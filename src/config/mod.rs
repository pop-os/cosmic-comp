// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Shell,
    state::{BackendData, State},
    wayland::protocols::{
        output_configuration::OutputConfigurationState, workspace::WorkspaceUpdateGuard,
    },
};
use cosmic_config::{ConfigGet, CosmicConfigEntry};
use cosmic_settings_config::window_rules::ApplicationException;
use cosmic_settings_config::{shortcuts, window_rules, Shortcuts};
use serde::{Deserialize, Serialize};
use smithay::wayland::xdg_activation::XdgActivationState;
pub use smithay::{
    backend::input::KeyState,
    input::keyboard::{keysyms as KeySyms, Keysym, ModifiersState},
    output::{Mode, Output},
    reexports::{
        calloop::LoopHandle,
        input::{
            AccelProfile, ClickMethod, Device as InputDevice, ScrollMethod, SendEventsMode,
            TapButtonMap,
        },
    },
    utils::{Logical, Physical, Point, Size, Transform},
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    fs::OpenOptions,
    path::PathBuf,
    sync::{atomic::AtomicBool, Arc, RwLock},
};
use tracing::{error, warn};

mod input_config;
pub mod key_bindings;
pub use key_bindings::{Action, PrivateAction};
mod types;
pub use self::types::*;
use cosmic::config::CosmicTk;
use cosmic_comp_config::{
    input::InputConfig, workspace::WorkspaceConfig, CosmicCompConfig, TileBehavior, XkbConfig,
};

#[derive(Debug)]
pub struct Config {
    pub dynamic_conf: DynamicConfig,
    pub cosmic_helper: cosmic_config::Config,
    /// cosmic-config comp configuration for `com.system76.CosmicComp`
    pub cosmic_conf: CosmicCompConfig,
    /// cosmic-config context for `com.system76.CosmicSettings.Shortcuts`
    pub settings_context: cosmic_config::Config,
    /// Key bindings from `com.system76.CosmicSettings.Shortcuts`
    pub shortcuts: Shortcuts,
    // Tiling exceptions from `com.system76.CosmicSettings.WindowRules`
    pub tiling_exceptions: Vec<ApplicationException>,
    /// System actions from `com.system76.CosmicSettings.Shortcuts`
    pub system_actions: BTreeMap<shortcuts::action::System, String>,
}

#[derive(Debug)]
pub struct DynamicConfig {
    outputs: (Option<PathBuf>, OutputsConfig),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct OutputsConfig {
    pub config: HashMap<Vec<OutputInfo>, Vec<OutputConfig>>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OutputInfo {
    pub connector: String,
    pub make: String,
    pub model: String,
}

impl From<Output> for OutputInfo {
    fn from(o: Output) -> OutputInfo {
        let physical = o.physical_properties();
        OutputInfo {
            connector: o.name(),
            make: physical.make,
            model: physical.model,
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum OutputState {
    #[serde(rename = "true")]
    Enabled,
    #[serde(rename = "false")]
    Disabled,
    Mirroring(String),
}

fn default_state() -> OutputState {
    OutputState::Enabled
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum AdaptiveSync {
    #[serde(rename = "true")]
    Enabled,
    #[serde(rename = "false")]
    Disabled,
    Force,
}

fn default_sync() -> AdaptiveSync {
    AdaptiveSync::Enabled
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct OutputConfig {
    pub mode: ((i32, i32), Option<u32>),
    #[serde(default = "default_sync")]
    pub vrr: AdaptiveSync,
    pub scale: f64,
    #[serde(with = "TransformDef")]
    pub transform: Transform,
    pub position: (u32, u32),
    #[serde(default = "default_state")]
    pub enabled: OutputState,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_bpc: Option<u32>,
}

impl Default for OutputConfig {
    fn default() -> OutputConfig {
        OutputConfig {
            mode: ((0, 0), None),
            vrr: AdaptiveSync::Enabled,
            scale: 1.0,
            transform: Transform::Normal,
            position: (0, 0),
            enabled: OutputState::Enabled,
            max_bpc: None,
        }
    }
}

impl OutputConfig {
    pub fn mode_size(&self) -> Size<i32, Physical> {
        self.mode.0.into()
    }

    pub fn mode_refresh(&self) -> u32 {
        self.mode.1.unwrap_or(60_000)
    }

    pub fn transformed_size(&self) -> Size<i32, Physical> {
        self.transform.transform_size(self.mode_size())
    }

    pub fn output_mode(&self) -> Mode {
        Mode {
            size: self.mode_size(),
            refresh: self.mode_refresh() as i32,
        }
    }
}

impl Config {
    pub fn load(loop_handle: &LoopHandle<'_, State>) -> Config {
        let config = cosmic_config::Config::new("com.system76.CosmicComp", 1).unwrap();
        let source = cosmic_config::calloop::ConfigWatchSource::new(&config).unwrap();
        loop_handle
            .insert_source(source, |(config, keys), (), state| {
                config_changed(config, keys, state);
            })
            .expect("Failed to add cosmic-config to the event loop");
        let xdg = xdg::BaseDirectories::new().ok();
        let workspace = get_config::<WorkspaceConfig>(&config, "workspaces");

        let cosmic_comp_config =
            CosmicCompConfig::get_entry(&config).unwrap_or_else(|(errs, c)| {
                for err in errs {
                    error!(?err, "");
                }
                c
            });

        // Listen for updates to the toolkit config
        if let Ok(tk_config) = cosmic_config::Config::new("com.system76.CosmicTk", 1) {
            fn handle_new_toolkit_config(config: CosmicTk, state: &mut State) {
                let mut workspace_guard = state.common.workspace_state.update();
                state.common.shell.write().unwrap().update_toolkit(
                    config,
                    &state.common.xdg_activation_state,
                    &mut workspace_guard,
                );
            }

            if let Ok(config) = CosmicTk::get_entry(&tk_config) {
                let _ = loop_handle.insert_idle(move |state| {
                    handle_new_toolkit_config(config, state);
                });
            }

            match cosmic_config::calloop::ConfigWatchSource::new(&tk_config) {
                Ok(source) => {
                    if let Err(err) =
                        loop_handle.insert_source(source, |(config, _keys), (), state| {
                            if let Ok(config) = CosmicTk::get_entry(&config) {
                                handle_new_toolkit_config(config, state);
                            }
                        })
                    {
                        warn!(?err, "Failed to watch com.system76.CosmicTk config");
                    }
                }
                Err(err) => warn!(
                    ?err,
                    "failed to create config watch source for com.system76.CosmicTk"
                ),
            }
        }

        // Source key bindings from com.system76.CosmicSettings.Shortcuts
        let settings_context = shortcuts::context().expect("Failed to load shortcuts config");
        let system_actions = shortcuts::system_actions(&config);
        let mut shortcuts = shortcuts::shortcuts(&settings_context);

        // Add any missing default shortcuts recommended by the compositor.
        key_bindings::add_default_bindings(&mut shortcuts, workspace.workspace_layout);

        // Listen for updates to the keybindings config.
        match cosmic_config::calloop::ConfigWatchSource::new(&settings_context) {
            Ok(source) => {
                if let Err(err) = loop_handle.insert_source(source, |(config, keys), (), state| {
                    for key in keys {
                        match key.as_str() {
                            // Reload the keyboard shortcuts config.
                            "custom" | "defaults" => {
                                let mut shortcuts = shortcuts::shortcuts(&config);
                                let layout = get_config::<WorkspaceConfig>(&config, "workspaces")
                                    .workspace_layout;
                                key_bindings::add_default_bindings(&mut shortcuts, layout);
                                state.common.config.shortcuts = shortcuts;
                            }

                            "system_actions" => {
                                state.common.config.system_actions =
                                    shortcuts::system_actions(&config);
                            }

                            _ => (),
                        }
                    }
                }) {
                    warn!(
                        ?err,
                        "Failed to watch com.system76.CosmicSettings.Shortcuts config"
                    );
                }
            }
            Err(err) => warn!(
                ?err,
                "failed to create config watch source for com.system76.CosmicSettings.Shortcuts"
            ),
        };

        let window_rules_context =
            window_rules::context().expect("Failed to load window rules config");
        let tiling_exceptions = window_rules::tiling_exceptions(&window_rules_context);

        match cosmic_config::calloop::ConfigWatchSource::new(&window_rules_context) {
            Ok(source) => {
                if let Err(err) = loop_handle.insert_source(source, |(config, keys), (), state| {
                    for key in keys {
                        match key.as_str() {
                            "tiling_exception_defaults" | "tiling_exception_custom" => {
                                let new_exceptions = window_rules::tiling_exceptions(&config);
                                state.common.config.tiling_exceptions = new_exceptions;
                                state
                                    .common
                                    .shell
                                    .write()
                                    .unwrap()
                                    .update_tiling_exceptions(
                                        state.common.config.tiling_exceptions.iter(),
                                    );
                            }
                            _ => (),
                        }
                    }
                }) {
                    warn!(
                        ?err,
                        "Failed to watch com.system76.CosmicSettings.WindowRules config"
                    );
                }
            }
            Err(err) => warn!(
                ?err,
                "failed to create config watch source for com.system76.CosmicSettings.WindowRules"
            ),
        };

        Config {
            dynamic_conf: Self::load_dynamic(xdg.as_ref()),
            cosmic_conf: cosmic_comp_config,
            cosmic_helper: config,
            settings_context,
            shortcuts,
            system_actions,
            tiling_exceptions,
        }
    }

    fn load_dynamic(xdg: Option<&xdg::BaseDirectories>) -> DynamicConfig {
        let output_path =
            xdg.and_then(|base| base.place_state_file("cosmic-comp/outputs.ron").ok());
        let outputs = Self::load_outputs(&output_path);

        DynamicConfig {
            outputs: (output_path, outputs),
        }
    }

    fn load_outputs(path: &Option<PathBuf>) -> OutputsConfig {
        if let Some(path) = path.as_ref() {
            if path.exists() {
                match ron::de::from_reader::<_, OutputsConfig>(
                    OpenOptions::new().read(true).open(path).unwrap(),
                ) {
                    Ok(mut config) => {
                        for (info, config) in config.config.iter_mut() {
                            let config_clone = config.clone();
                            for conf in config.iter_mut() {
                                if let OutputState::Mirroring(conn) = &conf.enabled {
                                    if let Some((j, _)) = info
                                        .iter()
                                        .enumerate()
                                        .find(|(_, info)| &info.connector == conn)
                                    {
                                        if config_clone[j].enabled != OutputState::Enabled {
                                            warn!("Invalid Mirroring tag, overriding with `Enabled` instead");
                                            conf.enabled = OutputState::Enabled;
                                        }
                                    } else {
                                        warn!("Invalid Mirroring tag, overriding with `Enabled` instead");
                                        conf.enabled = OutputState::Enabled;
                                    }
                                }
                            }
                        }
                        return config;
                    }
                    Err(err) => {
                        warn!(?err, "Failed to read output_config, resetting..");
                        if let Err(err) = std::fs::remove_file(path) {
                            error!(?err, "Failed to remove output_config.");
                        }
                    }
                };
            }
        }

        OutputsConfig {
            config: HashMap::new(),
        }
    }

    pub fn shortcut_for_action(&self, action: &shortcuts::Action) -> Option<String> {
        self.shortcuts.shortcut_for_action(action)
    }

    pub fn read_outputs(
        &mut self,
        output_state: &mut OutputConfigurationState<State>,
        backend: &mut BackendData,
        shell: &Arc<RwLock<Shell>>,
        loop_handle: &LoopHandle<'static, State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
        startup_done: Arc<AtomicBool>,
    ) {
        let outputs = output_state.outputs().collect::<Vec<_>>();
        let mut infos = outputs
            .iter()
            .cloned()
            .map(Into::<crate::config::OutputInfo>::into)
            .collect::<Vec<_>>();
        infos.sort();
        if let Some(configs) = self.dynamic_conf.outputs().config.get(&infos).cloned() {
            let known_good_configs = outputs
                .iter()
                .map(|output| {
                    output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .clone()
                })
                .collect::<Vec<_>>();

            let mut found_outputs = Vec::new();
            for (name, output_config) in infos.iter().map(|o| &o.connector).zip(configs.into_iter())
            {
                let output = outputs.iter().find(|o| &o.name() == name).unwrap().clone();
                let enabled = output_config.enabled.clone();
                *output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow_mut() = output_config;
                found_outputs.push((output.clone(), enabled));
            }

            if let Err(err) = backend.apply_config_for_outputs(
                false,
                loop_handle,
                shell.clone(),
                workspace_state,
                xdg_activation_state,
                startup_done.clone(),
            ) {
                warn!(?err, "Failed to set new config.");
                found_outputs.clear();
                for (output, output_config) in outputs
                    .clone()
                    .into_iter()
                    .zip(known_good_configs.into_iter())
                {
                    let enabled = output_config.enabled.clone();
                    *output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow_mut() = output_config;
                    found_outputs.push((output.clone(), enabled));
                }

                if let Err(err) = backend.apply_config_for_outputs(
                    false,
                    loop_handle,
                    shell.clone(),
                    workspace_state,
                    xdg_activation_state,
                    startup_done,
                ) {
                    error!(?err, "Failed to reset config.");
                } else {
                    for (output, enabled) in found_outputs {
                        if enabled == OutputState::Enabled {
                            output_state.enable_head(&output);
                        } else {
                            output_state.disable_head(&output);
                        }
                    }
                }
            } else {
                for (output, enabled) in found_outputs {
                    if enabled == OutputState::Enabled {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
                    }
                }
            }

            output_state.update();
            self.write_outputs(output_state.outputs());
        } else {
            if let Err(err) = backend.apply_config_for_outputs(
                false,
                loop_handle,
                shell.clone(),
                workspace_state,
                xdg_activation_state,
                startup_done,
            ) {
                warn!(?err, "Failed to set new config.",);
            } else {
                for output in outputs {
                    if output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .enabled
                        == OutputState::Enabled
                    {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
                    }
                }
            }

            output_state.update();
            self.write_outputs(output_state.outputs());
        }
    }

    pub fn write_outputs(
        &mut self,
        outputs: impl Iterator<Item = impl std::borrow::Borrow<Output>>,
    ) {
        let mut infos = outputs
            .map(|o| {
                let o = o.borrow();
                (
                    Into::<crate::config::OutputInfo>::into(o.clone()),
                    o.user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .clone(),
                )
            })
            .collect::<Vec<(OutputInfo, OutputConfig)>>();
        infos.sort_by(|(a, _), (b, _)| a.cmp(b));
        let (infos, configs) = infos.into_iter().unzip();
        self.dynamic_conf
            .outputs_mut()
            .config
            .insert(infos, configs);
    }

    pub fn xkb_config(&self) -> XkbConfig {
        self.cosmic_conf.xkb_config.clone()
    }

    pub fn read_device(&self, device: &mut InputDevice) {
        let (device_config, default_config) = self.get_device_config(device);
        input_config::update_device(device, device_config, default_config);
    }

    pub fn scroll_factor(&self, device: &InputDevice) -> f64 {
        let (device_config, default_config) = self.get_device_config(device);
        input_config::get_config(device_config, default_config, |x| {
            x.scroll_config.as_ref()?.scroll_factor
        })
        .map_or(1.0, |x| x.0)
    }

    pub fn map_to_output(&self, device: &InputDevice) -> Option<&str> {
        let (device_config, default_config) = self.get_device_config(device);
        Some(
            input_config::get_config(device_config, default_config, |x| {
                x.map_to_output.as_deref()
            })?
            .0,
        )
    }

    fn get_device_config(&self, device: &InputDevice) -> (Option<&InputConfig>, &InputConfig) {
        let default_config = if device.config_tap_finger_count() > 0 {
            &self.cosmic_conf.input_touchpad
        } else {
            &self.cosmic_conf.input_default
        };
        let device_config = self.cosmic_conf.input_devices.get(device.name());
        (device_config, default_config)
    }
}

pub struct PersistenceGuard<'a, T: Serialize>(Option<PathBuf>, &'a mut T);

impl<'a, T: Serialize> std::ops::Deref for PersistenceGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.1
    }
}

impl<'a, T: Serialize> std::ops::DerefMut for PersistenceGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.1
    }
}

impl<'a, T: Serialize> Drop for PersistenceGuard<'a, T> {
    fn drop(&mut self) {
        if let Some(path) = self.0.as_ref() {
            let writer = match OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(path)
            {
                Ok(writer) => writer,
                Err(err) => {
                    warn!(?err, "Failed to persist {}.", path.display());
                    return;
                }
            };
            if let Err(err) = ron::ser::to_writer_pretty(writer, &self.1, Default::default()) {
                warn!(?err, "Failed to persist {}", path.display());
            }
        }
    }
}

impl DynamicConfig {
    pub fn outputs(&self) -> &OutputsConfig {
        &self.outputs.1
    }

    pub fn outputs_mut(&mut self) -> PersistenceGuard<'_, OutputsConfig> {
        PersistenceGuard(self.outputs.0.clone(), &mut self.outputs.1)
    }
}

fn get_config<T: Default + serde::de::DeserializeOwned>(
    config: &cosmic_config::Config,
    key: &str,
) -> T {
    config.get(key).unwrap_or_else(|err| {
        error!(?err, "Failed to read config '{}'", key);
        T::default()
    })
}

fn update_input(state: &mut State) {
    if let BackendData::Kms(ref mut kms_state) = &mut state.backend {
        for device in kms_state.input_devices.values_mut() {
            state.common.config.read_device(device);
        }
    }
}

fn config_changed(config: cosmic_config::Config, keys: Vec<String>, state: &mut State) {
    for key in &keys {
        match key.as_str() {
            "xkb_config" => {
                let value = get_config::<XkbConfig>(&config, "xkb_config");
                let seats = state
                    .common
                    .shell
                    .read()
                    .unwrap()
                    .seats
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for seat in seats.into_iter() {
                    if let Some(keyboard) = seat.get_keyboard() {
                        keyboard.change_repeat_info(
                            (value.repeat_rate as i32).abs(), // Negative values are illegal
                            (value.repeat_delay as i32).abs(),
                        );
                        if let Err(err) = keyboard.set_xkb_config(state, xkb_config_to_wl(&value)) {
                            error!(?err, "Failed to load provided xkb config");
                            // TODO Revert to default?
                        }
                    }
                }
                state.common.atspi_ei.update_keymap(value.clone());
                state.common.config.cosmic_conf.xkb_config = value;
            }
            "input_default" => {
                let value = get_config::<InputConfig>(&config, "input_default");
                state.common.config.cosmic_conf.input_default = value;
                update_input(state);
            }
            "input_touchpad" => {
                let value = get_config::<InputConfig>(&config, "input_touchpad");
                state.common.config.cosmic_conf.input_touchpad = value;
                update_input(state);
            }
            "input_devices" => {
                let value = get_config::<HashMap<String, InputConfig>>(&config, "input_devices");
                state.common.config.cosmic_conf.input_devices = value;
                update_input(state);
            }
            "workspaces" => {
                state.common.config.cosmic_conf.workspaces =
                    get_config::<WorkspaceConfig>(&config, "workspaces");
                state.common.update_config();
            }
            "autotile" => {
                let new = get_config::<bool>(&config, "autotile");
                if new != state.common.config.cosmic_conf.autotile {
                    state.common.config.cosmic_conf.autotile = new;

                    let mut shell = state.common.shell.write().unwrap();
                    let shell_ref = &mut *shell;
                    shell_ref.workspaces.update_autotile(
                        new,
                        &mut state.common.workspace_state.update(),
                        shell_ref.seats.iter(),
                    );
                }
            }
            "autotile_behavior" => {
                let new = get_config::<TileBehavior>(&config, "autotile_behavior");
                if new != state.common.config.cosmic_conf.autotile_behavior {
                    state.common.config.cosmic_conf.autotile_behavior = new;

                    let mut shell = state.common.shell.write().unwrap();
                    let shell_ref = &mut *shell;
                    shell_ref.workspaces.update_autotile_behavior(
                        new,
                        &mut state.common.workspace_state.update(),
                        shell_ref.seats.iter(),
                    );
                }
            }
            "active_hint" => {
                let new = get_config::<bool>(&config, "active_hint");
                if new != state.common.config.cosmic_conf.active_hint {
                    state.common.config.cosmic_conf.active_hint = new;
                    state.common.update_config();
                }
            }
            "descale_xwayland" => {
                let new = get_config::<bool>(&config, "descale_xwayland");
                if new != state.common.config.cosmic_conf.descale_xwayland {
                    state.common.config.cosmic_conf.descale_xwayland = new;
                    state.common.update_xwayland_scale();
                }
            }
            "focus_follows_cursor" => {
                let new = get_config::<bool>(&config, "focus_follows_cursor");
                if new != state.common.config.cosmic_conf.focus_follows_cursor {
                    state.common.config.cosmic_conf.focus_follows_cursor = new;
                }
            }
            "cursor_follows_focus" => {
                let new = get_config::<bool>(&config, "cursor_follows_focus");
                if new != state.common.config.cosmic_conf.cursor_follows_focus {
                    state.common.config.cosmic_conf.cursor_follows_focus = new;
                }
            }
            "focus_follows_cursor_delay" => {
                let new = get_config::<u64>(&config, "focus_follows_cursor_delay");
                if new != state.common.config.cosmic_conf.focus_follows_cursor_delay {
                    state.common.config.cosmic_conf.focus_follows_cursor_delay = new;
                }
            }
            _ => {}
        }
    }
}

pub fn xkb_config_to_wl(config: &XkbConfig) -> WlXkbConfig<'_> {
    WlXkbConfig {
        rules: &config.rules,
        model: &config.model,
        layout: &config.layout,
        variant: &config.variant,
        options: config.options.clone(),
    }
}
