// SPDX-License-Identifier: GPL-3.0-only

use cosmic_config::{cosmic_config_derive::CosmicConfigEntry, CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod input;
pub mod output;
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
    pub pinned_workspaces: Vec<workspace::PinnedWorkspace>,
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
    pub descale_xwayland: XwaylandDescaling,
    /// Let X11 applications snoop on certain key-presses to allow for global shortcuts
    pub xwayland_eavesdropping: XwaylandEavesdropping,
    /// The threshold before windows snap themselves to output edges
    pub edge_snap_threshold: u32,
    /// How far the pointer can travel before it's considered as moved
    pub pointer_moved_epsilon: u32,
    pub accessibility_zoom: ZoomConfig,
}

impl Default for CosmicCompConfig {
    fn default() -> Self {
        Self {
            workspaces: Default::default(),
            pinned_workspaces: Vec::new(),
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
            descale_xwayland: XwaylandDescaling::Fractional,
            xwayland_eavesdropping: XwaylandEavesdropping::default(),
            edge_snap_threshold: 0,
            pointer_moved_epsilon: 10,
            accessibility_zoom: ZoomConfig::default(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct ZoomConfig {
    pub start_on_login: bool,
    pub show_overlay: bool,
    pub increment: u32,
    pub view_moves: ZoomMovement,
    pub enable_mouse_zoom_shortcuts: bool,
}

impl Default for ZoomConfig {
    fn default() -> Self {
        ZoomConfig {
            start_on_login: false,
            show_overlay: true,
            increment: 50,
            view_moves: ZoomMovement::Continuously,
            enable_mouse_zoom_shortcuts: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum ZoomMovement {
    OnEdge,
    Centered,
    Continuously,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct XwaylandEavesdropping {
    pub keyboard: EavesdroppingKeyboardMode,
    pub pointer: bool,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum EavesdroppingKeyboardMode {
    #[default]
    None,
    Modifiers,
    Combinations,
    All,
}

#[derive(Debug, Deserialize, Serialize, Clone, Copy, Default, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum XwaylandDescaling {
    #[serde(rename = "true")]
    Enabled,
    #[serde(rename = "false")]
    Disabled,
    #[default]
    Fractional,
}
