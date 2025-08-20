// SPDX-License-Identifier: GPL-3.0-only

#![allow(non_snake_case)]

pub use input::{AccelProfile, ClickMethod, ScrollMethod, TapButtonMap};
use serde::{Deserialize, Serialize};

// Note: For the following values, None is used to represent the system default
// Configuration for input devices
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct InputConfig {
    pub state: DeviceState,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub acceleration: Option<AccelConfig>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub calibration: Option<[f32; 6]>,
    #[serde(with = "ClickMethodDef")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub click_method: Option<ClickMethod>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub disable_while_typing: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub left_handed: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub middle_button_emulation: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub rotation_angle: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub scroll_config: Option<ScrollConfig>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub tap_config: Option<TapConfig>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub map_to_output: Option<String>,
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct AccelConfig {
    #[serde(with = "AccelProfileDef")]
    pub profile: Option<AccelProfile>,
    pub speed: f64,
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct ScrollConfig {
    #[serde(with = "ScrollMethodDef")]
    pub method: Option<ScrollMethod>,
    pub natural_scroll: Option<bool>,
    pub scroll_button: Option<u32>,
    pub scroll_factor: Option<f64>,
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum DeviceState {
    Enabled,
    Disabled,
    DisabledOnExternalMouse,
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TouchpadOverride {
    #[default]
    None,
    ForceDisable,
}

impl Default for DeviceState {
    fn default() -> Self {
        Self::Enabled
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TapConfig {
    pub enabled: bool,
    #[serde(with = "TapButtonMapDef")]
    pub button_map: Option<TapButtonMap>,
    pub drag: bool,
    pub drag_lock: bool,
}

mod ClickMethodDef {
    use input::ClickMethod as ClickMethodOrig;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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

mod AccelProfileDef {
    use input::AccelProfile as AccelProfileOrig;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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

mod ScrollMethodDef {
    use input::ScrollMethod as ScrollMethodOrig;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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

mod TapButtonMapDef {
    use input::TapButtonMap as TapButtonMapOrig;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
