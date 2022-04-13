// SPDX-License-Identifier: GPL-3.0-only

use crate::shell::layout::FocusDirection;
use serde::{Deserialize, Serialize};
use smithay::wayland::seat::Keysym;
pub use smithay::{
    backend::input::KeyState,
    utils::{Logical, Physical, Point, Size, Transform},
    wayland::{
        output::Output,
        seat::{keysyms as KeySyms, ModifiersState as KeyModifiers},
    },
};
use std::{collections::HashMap, fs::OpenOptions, path::PathBuf};
use xkbcommon::xkb;

pub struct Config {
    pub static_conf: StaticConfig,
    pub dynamic_conf: DynamicConfig,
}

#[derive(Debug, Deserialize)]
pub struct StaticConfig {
    pub key_bindings: HashMap<KeyPattern, Action>,
    pub workspace_mode: crate::shell::Mode,
}

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
}

#[derive(Serialize, Deserialize)]
#[serde(remote = "Transform")]
enum TransformDef {
    Normal,
    _90,
    _180,
    _270,
    Flipped,
    Flipped90,
    Flipped180,
    Flipped270,
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
            slog_scope::debug!("Trying config location: {}", path.display());
            if path.exists() {
                slog_scope::info!("Using config at {}", path.display());
                return ron::de::from_reader(OpenOptions::new().read(true).open(path).unwrap())
                    .expect("Malformed config file");
            }
        }

        StaticConfig {
            key_bindings: HashMap::new(),
            workspace_mode: crate::shell::Mode::global(),
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
                        slog_scope::warn!("Failed to read output_config ({}), resetting..", err);
                        if let Err(err) = std::fs::remove_file(path) {
                            slog_scope::error!("Failed to remove output_config {}", err);
                        }
                    }
                };
            }
        }

        OutputsConfig {
            config: HashMap::new(),
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
                    slog_scope::warn!("Failed to persist {}: {}", path.display(), err);
                    return;
                }
            };
            if let Err(err) = ron::ser::to_writer_pretty(writer, &self.1, Default::default()) {
                slog_scope::warn!("Failed to persist {}: {}", path.display(), err);
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

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub enum KeyModifier {
    Ctrl,
    Alt,
    Shift,
    Logo,
    CapsLock,
    NumLock,
}

impl std::ops::AddAssign<KeyModifier> for KeyModifiers {
    fn add_assign(&mut self, rhs: KeyModifier) {
        match rhs {
            KeyModifier::Ctrl => self.ctrl = true,
            KeyModifier::Alt => self.alt = true,
            KeyModifier::Shift => self.shift = true,
            KeyModifier::Logo => self.logo = true,
            KeyModifier::CapsLock => self.caps_lock = true,
            KeyModifier::NumLock => self.num_lock = true,
        };
    }
}

impl std::ops::BitOr for KeyModifier {
    type Output = KeyModifiers;

    fn bitor(self, rhs: KeyModifier) -> Self::Output {
        let mut modifiers = self.into();
        modifiers += rhs;
        modifiers
    }
}

impl Into<KeyModifiers> for KeyModifier {
    fn into(self) -> KeyModifiers {
        let mut modifiers = KeyModifiers {
            ctrl: false,
            alt: false,
            shift: false,
            caps_lock: false,
            logo: false,
            num_lock: false,
        };
        modifiers += self;
        modifiers
    }
}

#[derive(Deserialize)]
#[serde(transparent)]
struct KeyModifiersDef(Vec<KeyModifier>);

impl From<KeyModifiersDef> for KeyModifiers {
    fn from(src: KeyModifiersDef) -> Self {
        src.0.into_iter().fold(
            KeyModifiers {
                ctrl: false,
                alt: false,
                shift: false,
                caps_lock: false,
                logo: false,
                num_lock: false,
            },
            |mut modis, modi| {
                modis += modi;
                modis
            },
        )
    }
}

#[allow(non_snake_case)]
fn deserialize_KeyModifiers<'de, D>(deserializer: D) -> Result<KeyModifiers, D::Error>
where
    D: serde::Deserializer<'de>,
{
    KeyModifiersDef::deserialize(deserializer).map(Into::into)
}

#[allow(non_snake_case)]
fn deserialize_Keysym<'de, D>(deserializer: D) -> Result<Keysym, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::{Error, Unexpected};

    let name = String::deserialize(deserializer)?;
    //let name = format!("KEY_{}", code);
    match xkb::keysym_from_name(&name, xkb::KEYSYM_NO_FLAGS) {
        KeySyms::KEY_NoSymbol => match xkb::keysym_from_name(&name, xkb::KEYSYM_CASE_INSENSITIVE) {
            KeySyms::KEY_NoSymbol => Err(<D::Error as Error>::invalid_value(
                Unexpected::Str(&name),
                &"One of the keysym names of xkbcommon.h without the 'KEY_' prefix",
            )),
            x => {
                slog_scope::warn!(
                    "Key-Binding '{}' only matched case insensitive for {:?}",
                    name,
                    xkb::keysym_get_name(x)
                );
                Ok(x)
            }
        },
        x => Ok(x),
    }
}

/// Describtion of a key combination that might be
/// handled by the compositor.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Hash)]
#[serde(deny_unknown_fields)]
pub struct KeyPattern {
    /// What modifiers are expected to be pressed alongside the key
    #[serde(deserialize_with = "deserialize_KeyModifiers")]
    pub modifiers: KeyModifiers,
    /// The actual key, that was pressed
    #[serde(deserialize_with = "deserialize_Keysym")]
    pub key: u32,
}

impl KeyPattern {
    pub fn new(modifiers: impl Into<KeyModifiers>, key: u32) -> KeyPattern {
        KeyPattern {
            modifiers: modifiers.into(),
            key,
        }
    }
}

#[derive(Debug, Deserialize, Clone, PartialEq, Eq)]
pub enum Action {
    Terminate,
    Debug,
    Close,
    Workspace(u8),
    MoveToWorkspace(u8),
    Focus(FocusDirection),
    Orientation(crate::shell::layout::Orientation),
    Spawn(String),
}
