// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{
        Shell, WorkspaceAmount,
    },
    state::{BackendData, Data, State},
    wayland::protocols::output_configuration::OutputConfigurationState,
};
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

mod key_bindings;
pub use key_bindings::{Action, KeyModifier, KeyModifiers, KeyPattern};
mod types;
pub use self::types::*;

#[derive(Debug)]
pub struct Config {
    pub static_conf: StaticConfig,
    pub dynamic_conf: DynamicConfig,
}

#[derive(Debug, Deserialize)]
pub struct StaticConfig {
    pub key_bindings: HashMap<key_bindings::KeyPattern, key_bindings::Action>,
    pub workspace_mode: WorkspaceMode,
    pub workspace_amount: WorkspaceAmount,
    #[serde(default = "default_workspace_layout")]
    pub workspace_layout: WorkspaceLayout,
    pub tiling_enabled: bool,
    #[serde(default = "default_active_hint")]
    pub active_hint: u8,
    #[serde(default = "default_gaps")]
    pub gaps: (u8, u8),
}

#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum WorkspaceMode {
    OutputBound,
    Global,
}

#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum WorkspaceLayout {
    Vertical,
    Horizontal,
}

#[derive(Debug)]
pub struct DynamicConfig {
    outputs: (Option<PathBuf>, OutputsConfig),
    inputs: (Option<PathBuf>, InputsConfig),
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

fn default_active_hint() -> u8 {
    4
}

fn default_gaps() -> (u8, u8) {
    (0, 4)
}

fn default_workspace_layout() -> WorkspaceLayout {
    WorkspaceLayout::Vertical
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

#[derive(Debug, Deserialize, Serialize)]
pub struct InputsConfig {
    xkb: XkbConfig,
    devices: HashMap<String, InputConfig>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InputConfig {
    state: DeviceState,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    acceleration: Option<AccelConfig>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    calibration: Option<[f32; 6]>,
    #[serde(with = "ClickMethodDef")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    click_method: Option<ClickMethod>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    disable_while_typing: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    left_handed: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    middle_button_emulation: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    rotation_angle: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    scroll_config: Option<ScrollConfig>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    tap_config: Option<TapConfig>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct AccelConfig {
    #[serde(with = "AccelProfileDef")]
    profile: Option<AccelProfile>,
    speed: f64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ScrollConfig {
    #[serde(with = "ScrollMethodDef")]
    method: Option<ScrollMethod>,
    natural_scroll: Option<bool>,
    scroll_button: Option<u32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DeviceState {
    Enabled,
    Disabled,
    DisabledOnExternalMouse,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TapConfig {
    enabled: bool,
    #[serde(with = "TapButtonMapDef")]
    button_map: Option<TapButtonMap>,
    drag: bool,
    drag_lock: bool,
}

impl Config {
    pub fn load() -> Config {
        let xdg = xdg::BaseDirectories::new().ok();
        Config {
            static_conf: Self::load_static(xdg.as_ref()),
            dynamic_conf: Self::load_dynamic(xdg.as_ref()),
        }
    }

    fn load_static(xdg: Option<&xdg::BaseDirectories>) -> StaticConfig {
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

                key_bindings::add_default_bindings(
                    &mut config.key_bindings,
                    config.workspace_layout,
                );

                return config;
            }
        }

        StaticConfig {
            key_bindings: HashMap::new(),
            workspace_mode: WorkspaceMode::Global,
            workspace_amount: WorkspaceAmount::Dynamic,
            workspace_layout: WorkspaceLayout::Vertical,
            tiling_enabled: false,
            active_hint: default_active_hint(),
            gaps: default_gaps(),
        }
    }

    fn load_dynamic(xdg: Option<&xdg::BaseDirectories>) -> DynamicConfig {
        let output_path =
            xdg.and_then(|base| base.place_state_file("cosmic-comp/outputs.ron").ok());
        let outputs = Self::load_outputs(&output_path);

        let input_path = xdg.and_then(|base| base.place_state_file("cosmic-comp/inputs.ron").ok());
        let inputs = Self::load_inputs(&input_path);

        DynamicConfig {
            outputs: (output_path, outputs),
            inputs: (input_path, inputs),
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

    fn load_inputs(path: &Option<PathBuf>) -> InputsConfig {
        if let Some(path) = path.as_ref() {
            if path.exists() {
                match ron::de::from_reader(OpenOptions::new().read(true).open(path).unwrap()) {
                    Ok(config) => return config,
                    Err(err) => {
                        warn!(?err, "Failed to read input_config, resetting..");
                        if let Err(err) = std::fs::remove_file(path) {
                            error!(?err, "Failed to remove input_config.");
                        }
                    }
                };
            }
        }

        InputsConfig {
            xkb: XkbConfig::default(),
            devices: HashMap::new(),
        }
    }

    pub fn read_outputs(
        &mut self,
        output_state: &mut OutputConfigurationState<State>,
        backend: &mut BackendData,
        shell: &mut Shell,
        seats: impl Iterator<Item = Seat<State>>,
        loop_handle: &LoopHandle<'_, Data>,
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
        self.dynamic_conf.inputs().xkb.clone()
    }

    pub fn read_device(&mut self, device: &mut InputDevice) {
        use std::collections::hash_map::Entry;

        let mut inputs = self.dynamic_conf.inputs_mut();
        match inputs.devices.entry(device.name().into()) {
            Entry::Occupied(entry) => {
                let config = entry.get();
                if let Err(err) = match config.state {
                    DeviceState::Enabled => {
                        device.config_send_events_set_mode(SendEventsMode::ENABLED)
                    }
                    DeviceState::Disabled => {
                        device.config_send_events_set_mode(SendEventsMode::DISABLED)
                    }
                    DeviceState::DisabledOnExternalMouse => device
                        .config_send_events_set_mode(SendEventsMode::DISABLED_ON_EXTERNAL_MOUSE),
                } {
                    warn!(
                        ?err,
                        "Failed to apply mode {:?} for device {:?}.",
                        config.state,
                        device.name(),
                    );
                }
                if let Some(accel) = config.acceleration.as_ref() {
                    if let Some(profile) = accel.profile {
                        if let Err(err) = device.config_accel_set_profile(profile) {
                            warn!(
                                ?err,
                                "Failed to apply acceleration profile {:?} for device {:?}.",
                                profile,
                                device.name(),
                            );
                        }
                    }
                    if let Err(err) = device.config_accel_set_speed(accel.speed) {
                        warn!(
                            ?err,
                            "Failed to apply acceleration speed {:?} for device {:?}.",
                            accel.speed,
                            device.name(),
                        );
                    }
                }
                if let Some(matrix) = config.calibration {
                    if let Err(err) = device.config_calibration_set_matrix(matrix) {
                        warn!(
                            ?err,
                            "Failed to apply calibration matrix {:?} for device {:?}.",
                            matrix,
                            device.name(),
                        );
                    }
                }
                if let Some(method) = config.click_method {
                    if let Err(err) = device.config_click_set_method(method) {
                        warn!(
                            ?err,
                            "Failed to apply click method {:?} for device {:?}.",
                            method,
                            device.name(),
                        );
                    }
                }
                if let Some(dwt) = config.disable_while_typing {
                    if let Err(err) = device.config_dwt_set_enabled(dwt) {
                        warn!(
                            ?err,
                            "Failed to apply disable-while-typing {:?} for device {:?}.",
                            dwt,
                            device.name(),
                        );
                    }
                }
                if let Some(left) = config.left_handed {
                    if let Err(err) = device.config_left_handed_set(left) {
                        warn!(
                            ?err,
                            "Failed to apply left-handed {:?} for device {:?}.",
                            left,
                            device.name(),
                        );
                    }
                }
                if let Some(middle) = config.middle_button_emulation {
                    if let Err(err) = device.config_middle_emulation_set_enabled(middle) {
                        warn!(
                            ?err,
                            "Failed to apply middle-button-emulation {:?} for device {:?}.",
                            middle,
                            device.name(),
                        );
                    }
                }
                if let Some(angle) = config.rotation_angle {
                    if let Err(err) = device.config_rotation_set_angle(angle) {
                        warn!(
                            ?err,
                            "Failed to apply rotation-angle {:?} for device {:?}",
                            angle,
                            device.name(),
                        );
                    }
                }
                if let Some(scroll) = config.scroll_config.as_ref() {
                    if let Some(method) = scroll.method {
                        if let Err(err) = device.config_scroll_set_method(method) {
                            warn!(
                                ?err,
                                "Failed to apply scroll method {:?} for device {:?}.",
                                method,
                                device.name(),
                            );
                        }
                    }
                    if let Some(natural) = scroll.natural_scroll {
                        if let Err(err) = device.config_scroll_set_natural_scroll_enabled(natural) {
                            warn!(
                                ?err,
                                "Failed to apply natural scrolling {:?} for device {:?}.",
                                natural,
                                device.name(),
                            );
                        }
                    }
                    if let Some(button) = scroll.scroll_button {
                        if let Err(err) = device.config_scroll_set_button(button) {
                            warn!(
                                ?err,
                                "Failed to apply scroll button {:?} for device {:?}.",
                                button,
                                device.name(),
                            );
                        }
                    }
                }
                if let Some(tap) = config.tap_config.as_ref() {
                    if let Err(err) = device.config_tap_set_enabled(tap.enabled) {
                        warn!(
                            ?err,
                            "Failed to apply tap-to-click {:?} for device {:?}.",
                            tap.enabled,
                            device.name(),
                        );
                    }
                    if let Some(button_map) = tap.button_map {
                        if let Err(err) = device.config_tap_set_button_map(button_map) {
                            warn!(
                                ?err,
                                "Failed to apply button map {:?} for device {:?}.",
                                button_map,
                                device.name(),
                            );
                        }
                    }
                    if let Err(err) = device.config_tap_set_drag_enabled(tap.drag) {
                        warn!(
                            ?err,
                            "Failed to apply tap-drag {:?} for device {:?}.",
                            tap.drag,
                            device.name(),
                        );
                    }
                    if let Err(err) = device.config_tap_set_drag_lock_enabled(tap.drag_lock) {
                        warn!(
                            ?err,
                            "Failed to apply tap-drag-lock {:?} for device {:?}.",
                            tap.drag_lock,
                            device.name(),
                        );
                    }
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(InputConfig {
                    state: match device.config_send_events_mode() {
                        x if x.contains(SendEventsMode::ENABLED) => DeviceState::Enabled,
                        x if x.contains(SendEventsMode::DISABLED_ON_EXTERNAL_MOUSE) => {
                            DeviceState::DisabledOnExternalMouse
                        }
                        x if x.contains(SendEventsMode::DISABLED) => DeviceState::Disabled,
                        _ => DeviceState::Disabled,
                    },
                    acceleration: if device.config_accel_is_available() {
                        Some(AccelConfig {
                            profile: device.config_accel_profile(),
                            speed: device.config_accel_speed(),
                        })
                    } else {
                        None
                    },
                    calibration: device.config_calibration_matrix(),
                    click_method: device.config_click_method(),
                    disable_while_typing: if device.config_dwt_is_available() {
                        Some(device.config_dwt_enabled())
                    } else {
                        None
                    },
                    left_handed: if device.config_left_handed_is_available() {
                        Some(device.config_left_handed())
                    } else {
                        None
                    },
                    middle_button_emulation: if device.config_middle_emulation_is_available() {
                        Some(device.config_middle_emulation_enabled())
                    } else {
                        None
                    },
                    rotation_angle: if device.config_rotation_is_available() {
                        Some(device.config_rotation_angle())
                    } else {
                        None
                    },
                    scroll_config: if device
                        .config_scroll_methods()
                        .iter()
                        .any(|x| *x != ScrollMethod::NoScroll)
                    {
                        Some(ScrollConfig {
                            method: device.config_scroll_method(),
                            natural_scroll: if device.config_scroll_has_natural_scroll() {
                                Some(device.config_scroll_natural_scroll_enabled())
                            } else {
                                None
                            },
                            scroll_button: if device.config_scroll_method()
                                == Some(ScrollMethod::OnButtonDown)
                            {
                                Some(device.config_scroll_button())
                            } else {
                                None
                            },
                        })
                    } else {
                        None
                    },
                    tap_config: if device.config_tap_finger_count() > 0 {
                        Some(TapConfig {
                            enabled: device.config_tap_enabled(),
                            button_map: device.config_tap_button_map(),
                            drag: device.config_tap_drag_enabled(),
                            drag_lock: device.config_tap_drag_lock_enabled(),
                        })
                    } else {
                        None
                    },
                });
            }
        }
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

    pub fn inputs(&self) -> &InputsConfig {
        &self.inputs.1
    }

    pub fn inputs_mut<'a>(&'a mut self) -> PersistenceGuard<'a, InputsConfig> {
        PersistenceGuard(self.inputs.0.clone(), &mut self.inputs.1)
    }
}
