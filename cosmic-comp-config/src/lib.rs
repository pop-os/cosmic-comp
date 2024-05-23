// SPDX-License-Identifier: GPL-3.0-only

use cosmic_config::{cosmic_config_derive::CosmicConfigEntry, CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod input;
pub mod workspace;

#[derive(Clone, Debug, PartialEq, CosmicConfigEntry)]
#[version = 1]
pub struct CosmicCompConfig {
    pub workspaces: workspace::WorkspaceConfig,
    pub input_default: input::InputConfig,
    pub input_touchpad: input::InputConfig,
    pub input_devices: HashMap<String, input::InputConfig>,
    pub xkb_config: XkbConfig,
    /// Autotiling enabled
    pub autotile: bool,
    /// Determines the behavior of the autotile variable
    /// If set to Global, autotile applies to all windows in all workspaces
    /// If set to PerWorkspace, autotile only applies to new windows, and new workspaces
    pub autotile_behavior: TileBehavior,
    /// Active hint enabled
    pub active_hint: bool,
}

impl Default for CosmicCompConfig {
    fn default() -> Self {
        Self {
            workspaces: Default::default(),
            input_default: Default::default(),
            input_touchpad: Default::default(),
            input_devices: Default::default(),
            xkb_config: Default::default(),
            autotile: Default::default(),
            autotile_behavior: Default::default(),
            active_hint: true,
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Deserialize, Serialize)]
pub enum TileBehavior {
    #[default]
    Global,
    PerWorkspace,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct XkbConfig {
    pub rules: String,
    pub model: String,
    pub layout: String,
    pub variant: String,
    pub options: Option<String>,
    #[serde(default = "default_repeat_delay")]
    pub repeat_delay: u32,
    #[serde(default = "default_repeat_rate")]
    pub repeat_rate: u32,
}

impl Default for XkbConfig {
    fn default() -> XkbConfig {
        XkbConfig {
            rules: String::new(),
            model: String::new(),
            layout: String::new(),
            variant: String::new(),
            options: None,
            repeat_delay: default_repeat_delay(),
            repeat_rate: default_repeat_rate(),
        }
    }
}

fn default_repeat_rate() -> u32 {
    25
}

fn default_repeat_delay() -> u32 {
    600
}
