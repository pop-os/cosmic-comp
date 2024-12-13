// SPDX-License-Identifier: GPL-3.0-only

use cosmic_config::{cosmic_config_derive::CosmicConfigEntry, CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};

pub mod input;
pub mod workspace;

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct KeyboardConfig {
    /// Boot state for numlock
    pub numlock_state: NumlockState,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum NumlockState {
    BootOn,
    #[default]
    BootOff,
    LastBoot,
}

#[derive(Clone, Debug, PartialEq, CosmicConfigEntry)]
#[version = 1]
pub struct CosmicCompConfig {
    pub workspaces: workspace::WorkspaceConfig,
    pub input_default: input::InputConfig,
    pub input_touchpad: input::InputConfig,
    pub input_devices: HashMap<String, input::InputConfig>,
    pub xkb_config: XkbConfig,
    pub keyboard_config: KeyboardConfig,
    /// Autotiling enabled
    pub autotile: bool,
    /// Determines the behavior of the autotile variable
    /// If set to Global, autotile applies to all windows in all workspaces
    /// If set to PerWorkspace, autotile only applies to new windows, and new workspaces
    pub autotile_behavior: TileBehavior,
    /// Active hint enabled
    pub active_hint: bool,
    /// Enables changing keyboard focus to windows when the cursor passes into them
    pub focus_follows_cursor: bool,
    /// Enables warping the cursor to the focused window when focus changes due to keyboard input
    pub cursor_follows_focus: bool,
    /// The delay in milliseconds before focus follows mouse (if enabled)
    pub focus_follows_cursor_delay: u64,
    /// Let X11 applications scale themselves
    pub descale_xwayland: bool,
    /// The threshold before windows snap themselves to output edges
    pub edge_snap_threshold: u32,
    /// Path to postprocess shader applied to whole screen
    pub postprocess_shader_path: Option<PathBuf>,
}

impl Default for CosmicCompConfig {
    fn default() -> Self {
        Self {
            workspaces: Default::default(),
            input_default: Default::default(),
            // By default, enable tap-to-click and disable-while-typing.
            input_touchpad: input::InputConfig {
                state: input::DeviceState::Enabled,
                click_method: Some(input::ClickMethod::Clickfinger),
                disable_while_typing: Some(true),
                tap_config: Some(input::TapConfig {
                    enabled: true,
                    button_map: Some(input::TapButtonMap::LeftRightMiddle),
                    drag: true,
                    drag_lock: false,
                }),
                ..Default::default()
            },
            input_devices: Default::default(),
            xkb_config: Default::default(),
            keyboard_config: Default::default(),
            autotile: Default::default(),
            autotile_behavior: Default::default(),
            active_hint: true,
            focus_follows_cursor: false,
            cursor_follows_focus: false,
            focus_follows_cursor_delay: 250,
            descale_xwayland: false,
            edge_snap_threshold: 0,
            postprocess_shader_path: None,
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
