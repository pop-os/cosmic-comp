// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::Shell,
    state::{BackendData, State},
    utils::prelude::OutputExt,
    wayland::protocols::{
        output_configuration::OutputConfigurationState, workspace::WorkspaceUpdateGuard,
    },
};
use anyhow::Context;
use cosmic_config::{ConfigGet, CosmicConfigEntry};
use cosmic_settings_config::window_rules::ApplicationException;
use cosmic_settings_config::{Shortcuts, shortcuts, window_rules};
use serde::{Deserialize, Serialize};
use smithay::utils::{Clock, Monotonic};
use smithay::wayland::xdg_activation::XdgActivationState;
pub use smithay::{
    backend::input::{self as smithay_input, KeyState},
    input::keyboard::{Keysym, ModifiersState, keysyms as KeySyms},
    output::{Mode, Output},
    reexports::{
        calloop::LoopHandle,
        input::{
            AccelProfile, ClickMethod, Device as InputDevice, ScrollMethod, SendEventsMode,
            TapButtonMap,
        },
    },
    utils::{Logical, Physical, Point, SERIAL_COUNTER, Size, Transform},
};
use std::{
    cell::{Ref, RefCell},
    collections::{BTreeMap, HashMap},
    fs::OpenOptions,
    io::Write,
    path::PathBuf,
    sync::{Arc, atomic::AtomicBool},
};
use tracing::{error, warn};

mod input_config;
pub mod key_bindings;
mod types;

use cosmic::config::CosmicTk;
pub use cosmic_comp_config::EdidProduct;
use cosmic_comp_config::{
    AppearanceConfig, CosmicCompConfig, KeyboardConfig, TileBehavior, XkbConfig, XwaylandDescaling,
    XwaylandEavesdropping, ZoomConfig,
    input::{DeviceState as InputDeviceState, InputConfig, TouchpadOverride},
    output::comp::{
        OutputConfig, OutputInfo, OutputState, OutputsConfig, TransformDef, load_outputs,
    },
    workspace::WorkspaceConfig,
};
pub use key_bindings::{Action, PrivateAction};
use types::WlXkbConfig;

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
    numlock: (Option<PathBuf>, NumlockStateConfig),
    accessibility_filter: (Option<PathBuf>, ScreenFilter),
}

#[derive(Default, Debug, Deserialize, Serialize)]
pub struct NumlockStateConfig {
    pub last_state: bool,
}

pub struct CompOutputConfig<'a>(pub Ref<'a, OutputConfig>);

impl CompOutputConfig<'_> {
    pub fn mode_size(&self) -> Size<i32, Physical> {
        self.0.mode.0.into()
    }

    pub fn mode_refresh(&self) -> u32 {
        self.0.mode.1.unwrap_or(60_000)
    }

    pub fn transformed_size(&self) -> Size<i32, Physical> {
        self.transform().transform_size(self.mode_size())
    }

    pub fn output_mode(&self) -> Mode {
        Mode {
            size: self.mode_size(),
            refresh: self.mode_refresh() as i32,
        }
    }

    pub fn transform(&self) -> Transform {
        Transform::from(CompTransformDef(self.0.transform))
    }
}

pub struct CompTransformDef(pub TransformDef);

impl From<Transform> for CompTransformDef {
    fn from(transform: Transform) -> Self {
        let def = match transform {
            Transform::Normal => TransformDef::Normal,
            Transform::_90 => TransformDef::_90,
            Transform::_180 => TransformDef::_180,
            Transform::_270 => TransformDef::_270,
            Transform::Flipped => TransformDef::Flipped,
            Transform::Flipped90 => TransformDef::Flipped90,
            Transform::Flipped180 => TransformDef::Flipped180,
            Transform::Flipped270 => TransformDef::Flipped270,
        };
        CompTransformDef(def)
    }
}

impl From<CompTransformDef> for Transform {
    fn from(comp_transform: CompTransformDef) -> Self {
        match comp_transform.0 {
            TransformDef::Normal => Transform::Normal,
            TransformDef::_90 => Transform::_90,
            TransformDef::_180 => Transform::_180,
            TransformDef::_270 => Transform::_270,
            TransformDef::Flipped => Transform::Flipped,
            TransformDef::Flipped90 => Transform::Flipped90,
            TransformDef::Flipped180 => Transform::Flipped180,
            TransformDef::Flipped270 => Transform::Flipped270,
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, PartialEq)]
pub struct ScreenFilter {
    pub inverted: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color_filter: Option<ColorFilter>,
}

impl ScreenFilter {
    pub fn is_noop(&self) -> bool {
        !self.inverted && self.color_filter.is_none()
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
// these values need to match with offscreen.frag
pub enum ColorFilter {
    Greyscale = 1,
    Protanopia = 2,
    Deuteranopia = 3,
    Tritanopia = 4,
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
        let xdg = xdg::BaseDirectories::new();

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
                state.common.shell.write().update_toolkit(
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
        let system_actions = shortcuts::system_actions(&settings_context);
        let shortcuts = shortcuts::shortcuts(&settings_context);

        // Listen for updates to the keybindings config.
        match cosmic_config::calloop::ConfigWatchSource::new(&settings_context) {
            Ok(source) => {
                if let Err(err) = loop_handle.insert_source(source, |(config, keys), (), state| {
                    for key in keys {
                        match key.as_str() {
                            // Reload the keyboard shortcuts config.
                            "custom" | "defaults" => {
                                state.common.config.shortcuts = shortcuts::shortcuts(&config);
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
                                state.common.shell.write().update_tiling_exceptions(
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

        let _ = loop_handle.insert_idle(|state| {
            let filter_conf = state.common.config.dynamic_conf.screen_filter();
            state
                .common
                .a11y_state
                .set_screen_inverted(filter_conf.inverted);
            state
                .common
                .a11y_state
                .set_screen_filter(filter_conf.color_filter);
        });

        Config {
            dynamic_conf: Self::load_dynamic(&xdg),
            cosmic_conf: cosmic_comp_config,
            cosmic_helper: config,
            settings_context,
            shortcuts,
            system_actions,
            tiling_exceptions,
        }
    }

    fn load_dynamic(xdg: &xdg::BaseDirectories) -> DynamicConfig {
        let output_path = xdg.place_state_file("cosmic-comp/outputs.ron").ok();
        let outputs = load_outputs(output_path.as_ref());
        let numlock_path = xdg.place_state_file("cosmic-comp/numlock.ron").ok();
        let numlock = Self::load_numlock(&numlock_path);

        let filter_path = xdg
            .place_state_file("cosmic-comp/a11y_screen_filter.ron")
            .ok();
        let filter = Self::load_filter_state(&filter_path);

        DynamicConfig {
            outputs: (output_path, outputs),
            numlock: (numlock_path, numlock),
            accessibility_filter: (filter_path, filter),
        }
    }

    fn load_numlock(path: &Option<PathBuf>) -> NumlockStateConfig {
        path.as_deref()
            .filter(|path| path.exists())
            .and_then(|path| {
                ron::de::from_reader::<_, NumlockStateConfig>(
                    OpenOptions::new().read(true).open(path).unwrap(),
                )
                .map_err(|err| {
                    warn!(?err, "Failed to read numlock.ron, resetting..");
                    if let Err(err) = std::fs::remove_file(path) {
                        error!(?err, "Failed to remove numlock.ron.");
                    }
                })
                .ok()
            })
            .unwrap_or_default()
    }

    fn load_filter_state(path: &Option<PathBuf>) -> ScreenFilter {
        if let Some(path) = path.as_ref() {
            if path.exists() {
                match ron::de::from_reader::<_, ScreenFilter>(
                    OpenOptions::new().read(true).open(path).unwrap(),
                ) {
                    Ok(config) => return config,
                    Err(err) => {
                        warn!(?err, "Failed to read screen_filter state, resetting..");
                        if let Err(err) = std::fs::remove_file(path) {
                            error!(?err, "Failed to remove screen_filter state.");
                        }
                    }
                };
            }
        }

        ScreenFilter {
            inverted: false,
            color_filter: None,
        }
    }

    pub fn shortcut_for_action(&self, action: &shortcuts::Action) -> Option<String> {
        self.shortcuts.shortcut_for_action(action)
    }

    pub fn read_outputs(
        &mut self,
        output_state: &mut OutputConfigurationState<State>,
        backend: &mut BackendData,
        shell: &Arc<parking_lot::RwLock<Shell>>,
        loop_handle: &LoopHandle<'static, State>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
        startup_done: Arc<AtomicBool>,
        clock: &Clock<Monotonic>,
    ) -> anyhow::Result<()> {
        let outputs = output_state.outputs().collect::<Vec<_>>();
        let mut infos = outputs
            .iter()
            .cloned()
            .map(Into::<crate::config::CompOutputInfo>::into)
            .map(|i| i.0)
            .collect::<Vec<_>>();
        infos.sort();

        if let Some(configs) = self
            .dynamic_conf
            .outputs()
            .config
            .get(&infos)
            .filter(|configs| {
                if configs
                    .iter()
                    .all(|config| config.enabled == OutputState::Disabled)
                {
                    if !configs.is_empty() {
                        error!(
                            "Broken config, all outputs disabled. Resetting... {:?}",
                            configs
                        );
                    }
                    false
                } else {
                    true
                }
            })
            .cloned()
        {
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

            let mut backend = backend.lock();
            if let Err(err) = backend.apply_config_for_outputs(
                false,
                loop_handle,
                self.dynamic_conf.screen_filter(),
                shell.clone(),
                workspace_state,
                xdg_activation_state,
                startup_done.clone(),
                clock,
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

                backend
                    .apply_config_for_outputs(
                        false,
                        loop_handle,
                        self.dynamic_conf.screen_filter(),
                        shell.clone(),
                        workspace_state,
                        xdg_activation_state,
                        startup_done,
                        clock,
                    )
                    .context("Failed to reset config")?;

                for (output, enabled) in found_outputs {
                    if enabled == OutputState::Enabled {
                        output_state.enable_head(&output);
                    } else {
                        output_state.disable_head(&output);
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
            if outputs
                .iter()
                .all(|o| o.config().enabled == OutputState::Disabled)
            {
                for output in &outputs {
                    output.config_mut().enabled = OutputState::Enabled;
                }
            }

            // we don't have a config, so lets generate somewhat sane positions
            let mut w = 0;
            if !outputs.iter().any(|o| o.config().xwayland_primary) {
                // if we don't have a primary output for xwayland from a previous config, pick one
                if let Some(primary) = outputs.iter().find(|o| o.mirroring().is_none()) {
                    primary.config_mut().xwayland_primary = true;
                }
            }
            for output in outputs.iter().filter(|o| o.mirroring().is_none()) {
                {
                    let mut config = output.config_mut();
                    config.position = (w, 0);
                }
                w += output.geometry().size.w as u32;
            }

            let mut backend = backend.lock();
            backend
                .apply_config_for_outputs(
                    false,
                    loop_handle,
                    self.dynamic_conf.screen_filter(),
                    shell.clone(),
                    workspace_state,
                    xdg_activation_state,
                    startup_done.clone(),
                    clock,
                )
                .context("Failed to set new config")?;

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
            output_state.update();
            self.write_outputs(output_state.outputs());
        }

        Ok(())
    }

    pub fn write_outputs(
        &mut self,
        outputs: impl Iterator<Item = impl std::borrow::Borrow<Output>>,
    ) {
        let mut infos = outputs
            .map(|o| {
                let o = o.borrow();
                (
                    Into::<CompOutputInfo>::into(o.clone()).0,
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
        input_config::update_device(device, device_config.as_ref(), default_config);
    }

    pub fn scroll_factor(&self, device: &InputDevice) -> f64 {
        let (device_config, default_config) = self.get_device_config(device);
        input_config::get_config(device_config.as_ref(), default_config, |x| {
            x.scroll_config.as_ref()?.scroll_factor
        })
        .map_or(1.0, |x| x.0)
    }

    pub fn map_to_output(&self, device: &InputDevice) -> Option<String> {
        let (device_config, default_config) = self.get_device_config(device);
        Some(
            input_config::get_config(device_config.as_ref(), default_config, |x| {
                x.map_to_output.clone()
            })?
            .0,
        )
    }

    fn get_device_config(&self, device: &InputDevice) -> (Option<InputConfig>, &InputConfig) {
        let is_touchpad = device.config_tap_finger_count() > 0;

        let default_config = if is_touchpad {
            &self.cosmic_conf.input_touchpad
        } else {
            &self.cosmic_conf.input_default
        };

        let mut device_config = self.cosmic_conf.input_devices.get(device.name()).cloned();
        if is_touchpad && self.cosmic_conf.input_touchpad_override == TouchpadOverride::ForceDisable
        {
            device_config = Some({
                let mut config = device_config.unwrap_or_default();
                config.state = InputDeviceState::Disabled;
                config
            });
        }

        (device_config, default_config)
    }
}

pub struct PersistenceGuard<'a, T: Serialize>(Option<PathBuf>, &'a mut T);

impl<T: Serialize> std::ops::Deref for PersistenceGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.1
    }
}

impl<T: Serialize> std::ops::DerefMut for PersistenceGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.1
    }
}

impl<T: Serialize> Drop for PersistenceGuard<'_, T> {
    fn drop(&mut self) {
        if let Some(path) = self.0.as_ref() {
            let content = match ron::ser::to_string_pretty(&self.1, Default::default()) {
                Ok(content) => content,
                Err(err) => {
                    warn!("Failed to serialize: {:?}", err);
                    return;
                }
            };

            let mut writer = match OpenOptions::new()
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

            if let Err(err) = writer.write_all(content.as_bytes()) {
                warn!(?err, "Failed to persist {}", path.display());
            } else {
                let _ = writer.flush();
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

    pub fn numlock(&self) -> &NumlockStateConfig {
        &self.numlock.1
    }

    pub fn numlock_mut(&mut self) -> PersistenceGuard<'_, NumlockStateConfig> {
        PersistenceGuard(self.numlock.0.clone(), &mut self.numlock.1)
    }

    pub fn screen_filter(&self) -> &ScreenFilter {
        &self.accessibility_filter.1
    }

    pub fn screen_filter_mut(&mut self) -> PersistenceGuard<'_, ScreenFilter> {
        PersistenceGuard(
            self.accessibility_filter.0.clone(),
            &mut self.accessibility_filter.1,
        )
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
    if let BackendData::Kms(kms_state) = &mut state.backend {
        for device in kms_state.input_devices.values_mut() {
            state.common.config.read_device(device);
        }
    }
}

pub fn change_modifier_state(
    keyboard: &smithay::input::keyboard::KeyboardHandle<State>,
    scan_code: u32,
    state: &mut State,
) {
    /// Offset used to convert Linux scancode to X11 keycode.
    const X11_KEYCODE_OFFSET: u32 = 8;

    let mut input = |key_state, scan_code| {
        let time = state.common.clock.now().as_millis();
        let _ = keyboard.input(
            state,
            smithay_input::Keycode::new(scan_code + X11_KEYCODE_OFFSET),
            key_state,
            SERIAL_COUNTER.next_serial(),
            time,
            |_, _, _| smithay::input::keyboard::FilterResult::<()>::Forward,
        );
    };

    input(smithay_input::KeyState::Pressed, scan_code);
    input(smithay_input::KeyState::Released, scan_code);
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
                    .seats
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for seat in seats.into_iter() {
                    if let Some(keyboard) = seat.get_keyboard() {
                        let old_modifier_state = keyboard.modifier_state();
                        keyboard.change_repeat_info(
                            (value.repeat_rate as i32).abs(), // Negative values are illegal
                            (value.repeat_delay as i32).abs(),
                        );
                        if let Err(err) = keyboard.set_xkb_config(state, xkb_config_to_wl(&value)) {
                            error!(?err, "Failed to load provided xkb config");
                            // TODO Revert to default?
                        }

                        // Press and release the numlock key to update modifiers.
                        if old_modifier_state.num_lock != keyboard.modifier_state().num_lock {
                            const NUMLOCK_SCANCODE: u32 = 69;
                            change_modifier_state(&keyboard, NUMLOCK_SCANCODE, state);
                        }
                        if old_modifier_state.caps_lock != keyboard.modifier_state().caps_lock {
                            const CAPSLOCK_SCANCODE: u32 = 58;
                            change_modifier_state(&keyboard, CAPSLOCK_SCANCODE, state);
                        }
                    }
                }
                state.common.config.cosmic_conf.xkb_config = value;
            }
            "keyboard_config" => {
                let value = get_config::<KeyboardConfig>(&config, "keyboard_config");
                state.common.config.cosmic_conf.keyboard_config = value;
                let shell = state.common.shell.read();
                let seat = shell.seats.last_active();
                state.common.config.dynamic_conf.numlock_mut().last_state =
                    seat.get_keyboard().unwrap().modifier_state().num_lock;
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
            "input_touchpad_override" => {
                let value = get_config::<TouchpadOverride>(&config, "input_touchpad_override");
                state.common.config.cosmic_conf.input_touchpad_override = value;
                update_input(state)
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

                    let mut shell = state.common.shell.write();
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

                    let mut shell = state.common.shell.write();
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
                let new = get_config::<XwaylandDescaling>(&config, "descale_xwayland");
                if new != state.common.config.cosmic_conf.descale_xwayland {
                    state.common.config.cosmic_conf.descale_xwayland = new;
                    state.common.update_xwayland_scale();
                }
            }
            "xwayland_eavesdropping" => {
                let new = get_config::<XwaylandEavesdropping>(&config, "xwayland_eavesdropping");
                if new != state.common.config.cosmic_conf.xwayland_eavesdropping {
                    state.common.config.cosmic_conf.xwayland_eavesdropping = new;
                    state
                        .common
                        .xwayland_reset_eavesdropping(SERIAL_COUNTER.next_serial());
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
            "edge_snap_threshold" => {
                let new = get_config::<u32>(&config, "edge_snap_threshold");
                if new != state.common.config.cosmic_conf.edge_snap_threshold {
                    state.common.config.cosmic_conf.edge_snap_threshold = new;
                }
            }
            "accessibility_zoom" => {
                let new = get_config::<ZoomConfig>(&config, "accessibility_zoom");
                if new != state.common.config.cosmic_conf.accessibility_zoom {
                    state.common.config.cosmic_conf.accessibility_zoom = new;
                    state.common.update_config();
                }
            }
            "appearance_settings" => {
                let new = get_config::<AppearanceConfig>(&config, "appearance_settings");
                if new != state.common.config.cosmic_conf.appearance_settings {
                    state.common.config.cosmic_conf.appearance_settings = new;
                    state.common.update_config();
                    for output in state.common.shell.read().outputs() {
                        state.backend.schedule_render(output);
                    }
                }
            }
            _ => {}
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompOutputInfo(OutputInfo);

impl From<Output> for CompOutputInfo {
    fn from(o: Output) -> CompOutputInfo {
        let physical = o.physical_properties();
        CompOutputInfo(OutputInfo {
            connector: o.name(),
            make: physical.make,
            model: physical.model,
        })
    }
}
