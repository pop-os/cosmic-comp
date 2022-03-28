// SPDX-License-Identifier: GPL-3.0-only

use serde::Deserialize;
use smithay::wayland::seat::Keysym;
pub use smithay::{
    backend::input::KeyState,
    wayland::seat::{keysyms as KeySyms, ModifiersState as KeyModifiers},
};
use std::{collections::HashMap, fs::OpenOptions, path::PathBuf};
use xkbcommon::xkb;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub key_bindings: HashMap<KeyPattern, Action>,
}

impl Config {
    pub fn load() -> Config {
        let mut locations = if let Ok(base) = xdg::BaseDirectories::new() {
            base.list_config_files_once("cosmic-comp.ron")
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
            if path.exists() {
                return ron::de::from_reader(OpenOptions::new().read(true).open(path).unwrap())
                    .expect("Malformed config file");
            }
        }

        Config {
            key_bindings: HashMap::new(),
        }
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
    Focus(FocusAction),
    Spawn(String),
}

#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum FocusAction {
    Left,
    Right,
    Up,
    Down,
}
