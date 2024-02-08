// SPDX-License-Identifier: GPL-3.0-only

use cosmic_config::{cosmic_config_derive::CosmicConfigEntry, CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod input;
pub mod workspace;

#[derive(Clone, Debug, Default, PartialEq, CosmicConfigEntry)]
#[version = 1]
pub struct CosmicCompConfig {
    pub workspaces: workspace::WorkspaceConfig,
    pub input_default: input::InputConfig,
    pub input_touchpad: input::InputConfig,
    pub input_devices: HashMap<String, input::InputConfig>,
    pub xkb_config: XkbConfig,
    /// Autotiling enabled
    pub autotile: bool,
    /// Tile windows on all workspaces
    /// If set to false, autotile only applies to new windows
    pub tile_all_windows: bool,
    /// Active hint enabled
    pub active_hint: bool,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
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
