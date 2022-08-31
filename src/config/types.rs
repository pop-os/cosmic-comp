// SPDX-License-Identifier: GPL-3.0-only
#![allow(non_snake_case)]

use super::{KeyModifier, KeyModifiers};
use serde::{Deserialize, Serialize};
pub use smithay::{
    backend::input::KeyState,
    input::keyboard::{keysyms as KeySyms, Keysym, XkbConfig as WlXkbConfig},
    reexports::input::{AccelProfile, ClickMethod, ScrollMethod, TapButtonMap},
    utils::{Logical, Physical, Point, Size, Transform},
    wayland::output::{Mode, Output},
};
use xkbcommon::xkb;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct XkbConfig {
    pub rules: String,
    pub model: String,
    pub layout: String,
    pub variant: String,
    pub options: Option<String>,
}

impl Default for XkbConfig {
    fn default() -> XkbConfig {
        XkbConfig {
            rules: String::new(),
            model: String::new(),
            layout: String::new(),
            variant: String::new(),
            options: None,
        }
    }
}

impl<'a> Into<WlXkbConfig<'a>> for &'a XkbConfig {
    fn into(self) -> WlXkbConfig<'a> {
        WlXkbConfig {
            rules: &self.rules,
            model: &self.model,
            layout: &self.layout,
            variant: &self.variant,
            options: self.options.clone(),
        }
    }
}

pub mod ClickMethodDef {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use smithay::reexports::input::ClickMethod as ClickMethodOrig;

    #[derive(Debug, Serialize, Deserialize)]
    pub enum ClickMethod {
        ButtonAreas,
        Clickfinger,
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<ClickMethodOrig>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let o = Option::deserialize(deserializer)?;
        Ok(o.map(|x| match x {
            ClickMethod::ButtonAreas => ClickMethodOrig::ButtonAreas,
            ClickMethod::Clickfinger => ClickMethodOrig::Clickfinger,
        }))
    }

    pub fn serialize<S>(arg: &Option<ClickMethodOrig>, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let arg = match arg {
            Some(ClickMethodOrig::ButtonAreas) => Some(ClickMethod::ButtonAreas),
            Some(ClickMethodOrig::Clickfinger) => Some(ClickMethod::Clickfinger),
            Some(_) | None => None,
        };
        Option::serialize(&arg, ser)
    }
}

pub mod AccelProfileDef {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use smithay::reexports::input::AccelProfile as AccelProfileOrig;

    #[derive(Debug, Serialize, Deserialize)]
    enum AccelProfile {
        Flat,
        Adaptive,
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<AccelProfileOrig>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let o = Option::deserialize(deserializer)?;
        Ok(o.map(|x| match x {
            AccelProfile::Flat => AccelProfileOrig::Flat,
            AccelProfile::Adaptive => AccelProfileOrig::Adaptive,
        }))
    }

    pub fn serialize<S>(arg: &Option<AccelProfileOrig>, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let arg = match arg {
            Some(AccelProfileOrig::Flat) => Some(AccelProfile::Flat),
            Some(AccelProfileOrig::Adaptive) => Some(AccelProfile::Adaptive),
            Some(_) | None => None,
        };
        Option::serialize(&arg, ser)
    }
}

pub mod ScrollMethodDef {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use smithay::reexports::input::ScrollMethod as ScrollMethodOrig;

    #[derive(Debug, Serialize, Deserialize)]
    pub enum ScrollMethod {
        NoScroll,
        TwoFinger,
        Edge,
        OnButtonDown,
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<ScrollMethodOrig>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let o = Option::deserialize(deserializer)?;
        Ok(o.map(|x| match x {
            ScrollMethod::NoScroll => ScrollMethodOrig::NoScroll,
            ScrollMethod::TwoFinger => ScrollMethodOrig::TwoFinger,
            ScrollMethod::Edge => ScrollMethodOrig::Edge,
            ScrollMethod::OnButtonDown => ScrollMethodOrig::OnButtonDown,
        }))
    }

    pub fn serialize<S>(arg: &Option<ScrollMethodOrig>, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let arg = match arg {
            Some(ScrollMethodOrig::NoScroll) => Some(ScrollMethod::NoScroll),
            Some(ScrollMethodOrig::TwoFinger) => Some(ScrollMethod::TwoFinger),
            Some(ScrollMethodOrig::Edge) => Some(ScrollMethod::Edge),
            Some(ScrollMethodOrig::OnButtonDown) => Some(ScrollMethod::OnButtonDown),
            Some(_) | None => None,
        };
        Option::serialize(&arg, ser)
    }
}

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

pub mod TapButtonMapDef {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use smithay::reexports::input::TapButtonMap as TapButtonMapOrig;

    #[derive(Debug, Serialize, Deserialize)]
    pub enum TapButtonMap {
        LeftRightMiddle,
        LeftMiddleRight,
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<TapButtonMapOrig>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let o = Option::deserialize(deserializer)?;
        Ok(o.map(|x| match x {
            TapButtonMap::LeftRightMiddle => TapButtonMapOrig::LeftRightMiddle,
            TapButtonMap::LeftMiddleRight => TapButtonMapOrig::LeftMiddleRight,
        }))
    }

    pub fn serialize<S>(arg: &Option<TapButtonMapOrig>, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let arg = match arg {
            Some(TapButtonMapOrig::LeftRightMiddle) => Some(TapButtonMap::LeftRightMiddle),
            Some(TapButtonMapOrig::LeftMiddleRight) => Some(TapButtonMap::LeftMiddleRight),
            Some(_) | None => None,
        };
        Option::serialize(&arg, ser)
    }
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
                caps_lock: false,
                logo: false,
                num_lock: false,
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
pub fn deserialize_Keysym<'de, D>(deserializer: D) -> Result<Keysym, D::Error>
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
