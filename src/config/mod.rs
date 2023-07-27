// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{Shell, WorkspaceAmount},
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

mod input_config;
use input_config::InputConfig;
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
            Entry::Occupied(entry) => entry.get().update_device(device),
            Entry::Vacant(entry) => {
                entry.insert(InputConfig::for_device(device));
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
