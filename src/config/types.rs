// SPDX-License-Identifier: GPL-3.0-only
#![allow(non_snake_case)]

use super::{KeyModifier, KeyModifiers};
use serde::{Deserialize, Serialize};
use smithay::reexports::x11rb::NO_SYMBOL;
pub use smithay::{
    input::keyboard::{Keysym, XkbConfig as WlXkbConfig},
    utils::Transform,
};
use tracing::warn;
use xkbcommon::xkb;

#[derive(Serialize, Deserialize)]
#[serde(remote = "Transform")]
pub enum TransformDef {
    Normal,
    _90,
    _180,
    _270,
    Flipped,
    Flipped90,
    Flipped180,
    Flipped270,
}

#[derive(Deserialize)]
#[serde(transparent)]
pub struct KeyModifiersDef(Vec<KeyModifier>);

impl From<KeyModifiersDef> for KeyModifiers {
    fn from(src: KeyModifiersDef) -> Self {
        src.0.into_iter().fold(
            KeyModifiers {
                ctrl: false,
                alt: false,
                shift: false,
                logo: false,
            },
            |mut modis, modi: KeyModifier| {
                modis += modi;
                modis
            },
        )
    }
}

#[allow(non_snake_case)]
pub fn deserialize_KeyModifiers<'de, D>(deserializer: D) -> Result<KeyModifiers, D::Error>
where
    D: serde::Deserializer<'de>,
{
    KeyModifiersDef::deserialize(deserializer).map(Into::into)
}

#[allow(non_snake_case)]
pub fn deserialize_Keysym<'de, D>(deserializer: D) -> Result<Option<Keysym>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::{Error, Unexpected};

    let name = String::deserialize(deserializer)?;
    //let name = format!("KEY_{}", code);
    match xkb::keysym_from_name(&name, xkb::KEYSYM_NO_FLAGS) {
        x if x.raw() == NO_SYMBOL => {
            match xkb::keysym_from_name(&name, xkb::KEYSYM_CASE_INSENSITIVE) {
                x if x.raw() == NO_SYMBOL => Err(<D::Error as Error>::invalid_value(
                    Unexpected::Str(&name),
                    &"One of the keysym names of xkbcommon.h without the 'KEY_' prefix",
                )),
                x => {
                    warn!(
                        "Key-Binding '{}' only matched case insensitive for {:?}",
                        name,
                        xkb::keysym_get_name(x)
                    );
                    Ok(Some(x))
                }
            }
        }
        x => Ok(Some(x)),
    }
}
