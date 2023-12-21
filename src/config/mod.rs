// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Shell,
    state::{BackendData, State},
    wayland::protocols::output_configuration::OutputConfigurationState,
};
use cosmic_config::ConfigGet;
use serde::{Deserialize, Serialize};
use smithay::input::Seat;
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
use std::{cell::RefCell, collections::HashMap, fs::OpenOptions, path::PathBuf};
use tracing::{debug, error, info, warn};

mod input_config;
mod key_bindings;
pub use key_bindings::{Action, KeyModifier, KeyModifiers, KeyPattern};
mod types;
pub use self::types::*;
use cosmic_comp_config::{
    input::InputConfig,
    workspace::{WorkspaceConfig, WorkspaceLayout},
    XkbConfig,
};

#[derive(Debug)]
pub struct Config {
    pub static_conf: StaticConfig,
    pub dynamic_conf: DynamicConfig,
    pub config: cosmic_config::Config,
    pub xkb: XkbConfig,
    pub input_default: InputConfig,
    pub input_touchpad: InputConfig,
    pub input_devices: HashMap<String, InputConfig>,
    pub workspace: WorkspaceConfig,
}

#[derive(Debug, Deserialize)]
pub struct StaticConfig {
    pub key_bindings: HashMap<key_bindings::KeyPattern, key_bindings::Action>,
    pub tiling_enabled: bool,
    pub data_control_enabled: bool,
}

impl StaticConfig {
    pub fn get_shortcut_for_action(&self, action: &Action) -> Option<String> {
        let possible_variants = self
            .key_bindings
            .iter()
            .filter(|(_, a)| *a == action)
            .map(|(b, _)| b)
            .collect::<Vec<_>>();

        possible_variants
            .iter()
            .find(|b| b.key.is_none()) // prefer short bindings
            .or_else(|| {
                possible_variants
                    .iter() // prefer bindings containing arrow keys
                    .find(|b| {
                        matches!(
                            b.key,
                            Some(Keysym::Down)
                                | Some(Keysym::Up)
                                | Some(Keysym::Left)
                                | Some(Keysym::Right)
                        )
                    })
            })
            .or_else(|| possible_variants.first()) // take the first one
            .map(|binding| binding.to_string())
    }
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

fn default_enabled() -> bool {
    true
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct OutputConfig {
    pub mode: ((i32, i32), Option<u32>),
    pub vrr: bool,
    pub scale: f64,
    #[serde(with = "TransformDef")]
    pub transform: Transform,
    pub position: (i32, i32),
    #[serde(default = "default_enabled")]
    pub enabled: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_bpc: Option<u32>,
}

impl Default for OutputConfig {
    fn default() -> OutputConfig {
        OutputConfig {
            mode: ((0, 0), None),
            vrr: false,
            scale: 1.0,
            transform: Transform::Normal,
            position: (0, 0),
            enabled: true,
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
        Config {
            static_conf: Self::load_static(xdg.as_ref(), workspace.workspace_layout),
            dynamic_conf: Self::load_dynamic(xdg.as_ref()),
            xkb: get_config(&config, "xkb_config"),
            input_default: get_config(&config, "input_default"),
            input_touchpad: get_config(&config, "input_touchpad"),
            input_devices: get_config(&config, "input_devices"),
            workspace,
            config,
        }
    }

    fn load_static(
        xdg: Option<&xdg::BaseDirectories>,
        workspace_layout: WorkspaceLayout,
    ) -> StaticConfig {
        let mut locations = if let Some(base) = xdg {
            vec![
                base.get_config_file("cosmic-comp.ron"),
                base.get_config_file("cosmic-comp/config.ron"),
            ]
        } else {
            Vec::with_capacity(3)
        };
        if cfg!(debug_assertions) {
            if let Ok(mut cwd) = std::env::current_dir() {
                cwd.push("config.ron");
                locations.push(cwd);
            }
        }
        locations.push(PathBuf::from("/etc/cosmic-comp/config.ron"));
        locations.push(PathBuf::from("/etc/cosmic-comp.ron"));

        for path in locations {
            debug!("Trying config location: {}", path.display());
            if path.exists() {
                info!("Using config at {}", path.display());
                let mut config: StaticConfig =
                    ron::de::from_reader(OpenOptions::new().read(true).open(path).unwrap())
                        .expect("Malformed config file");

                key_bindings::add_default_bindings(&mut config.key_bindings, workspace_layout);

                return config;
            }
        }

        StaticConfig {
            key_bindings: HashMap::new(),
            tiling_enabled: false,
            data_control_enabled: false,
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
                match ron::de::from_reader(OpenOptions::new().read(true).open(path).unwrap()) {
                    Ok(config) => return config,
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

    pub fn read_outputs(
        &mut self,
        output_state: &mut OutputConfigurationState<State>,
        backend: &mut BackendData,
        shell: &mut Shell,
        seats: impl Iterator<Item = Seat<State>>,
        loop_handle: &LoopHandle<'_, State>,
    ) {
        let seats = seats.collect::<Vec<_>>();
        let outputs = output_state.outputs().collect::<Vec<_>>();
        let mut infos = outputs
            .iter()
            .cloned()
            .map(Into::<crate::config::OutputInfo>::into)
            .collect::<Vec<_>>();
        infos.sort();
        if let Some(configs) = self.dynamic_conf.outputs().config.get(&infos).cloned() {
            let mut reset = false;
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

            for (name, output_config) in infos.iter().map(|o| &o.connector).zip(configs.into_iter())
            {
                let output = outputs.iter().find(|o| &o.name() == name).unwrap().clone();
                let enabled = output_config.enabled;
                *output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow_mut() = output_config;
                if let Err(err) = backend.apply_config_for_output(
                    &output,
                    false,
                    shell,
                    seats.iter().cloned(),
                    loop_handle,
                ) {
                    warn!(
                        ?err,
                        "Failed to set new config for output {}.",
                        output.name(),
                    );
                    reset = true;
                    break;
                } else {
                    if enabled {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
                    }
                }
            }

            if reset {
                for (output, output_config) in outputs
                    .clone()
                    .into_iter()
                    .zip(known_good_configs.into_iter())
                {
                    let enabled = output_config.enabled;
                    *output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow_mut() = output_config;
                    if let Err(err) = backend.apply_config_for_output(
                        &output,
                        false,
                        shell,
                        seats.iter().cloned(),
                        loop_handle,
                    ) {
                        error!(?err, "Failed to reset config for output {}.", output.name());
                    } else {
                        if enabled {
                            output_state.enable_head(&output);
                        } else {
                            output_state.disable_head(&output);
                        }
                    }
                }
            }

            output_state.update();
            self.write_outputs(output_state.outputs());
        } else {
            for output in outputs {
                if let Err(err) = backend.apply_config_for_output(
                    &output,
                    false,
                    shell,
                    seats.iter().cloned(),
                    loop_handle,
                ) {
                    warn!(
                        ?err,
                        "Failed to set new config for output {}.",
                        output.name(),
                    );
                } else {
                    if output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow()
                        .enabled
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
        infos.sort_by(|&(ref a, _), &(ref b, _)| a.cmp(b));
        let (infos, configs) = infos.into_iter().unzip();
        self.dynamic_conf
            .outputs_mut()
            .config
            .insert(infos, configs);
    }

    pub fn xkb_config(&self) -> XkbConfig {
        self.xkb.clone()
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

    fn get_device_config(&self, device: &InputDevice) -> (Option<&InputConfig>, &InputConfig) {
        let default_config = if device.config_tap_finger_count() > 0 {
            &self.input_touchpad
        } else {
            &self.input_default
        };
        let device_config = self.input_devices.get(device.name());
        (device_config, default_config)
    }
}

pub struct PersistenceGuard<'a, T: Serialize>(Option<PathBuf>, &'a mut T);

impl<'a, T: Serialize> std::ops::Deref for PersistenceGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.1
    }
}

impl<'a, T: Serialize> std::ops::DerefMut for PersistenceGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.1
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

    pub fn outputs_mut<'a>(&'a mut self) -> PersistenceGuard<'a, OutputsConfig> {
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
            "xkb-config" => {
                let value = get_config::<XkbConfig>(&config, "xkb-config");
                for seat in state.common.seats().cloned().collect::<Vec<_>>().iter() {
                    if let Some(keyboard) = seat.get_keyboard() {
                        if let Err(err) = keyboard.set_xkb_config(state, xkb_config_to_wl(&value)) {
                            error!(?err, "Failed to load provided xkb config");
                            // TODO Revert to default?
                        }
                    }
                }
                state.common.config.xkb = value;
            }
            "input_default" => {
                let value = get_config::<InputConfig>(&config, "input_default");
                state.common.config.input_default = value;
                update_input(state);
            }
            "input_touchpad" => {
                let value = get_config::<InputConfig>(&config, "input_touchpad");
                state.common.config.input_touchpad = value;
                update_input(state);
            }
            "input_devices" => {
                let value = get_config::<HashMap<String, InputConfig>>(&config, "input_devices");
                state.common.config.input_devices = value;
                update_input(state);
            }
            "workspaces" => {
                state.common.config.workspace =
                    get_config::<WorkspaceConfig>(&config, "workspaces");
                state.common.shell.update_config(&state.common.config);
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
