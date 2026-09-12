// SPDX-License-Identifier: GPL-3.0-only

use cosmic_config::{CosmicConfigEntry, cosmic_config_derive::CosmicConfigEntry};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

use crate::input::TouchpadOverride;

pub mod input;
#[cfg(feature = "output")]
pub mod output;
pub mod workspace;

#[derive(Debug, Deserialize, Serialize, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdidProduct {
    pub manufacturer: [char; 3],
    pub product: u16,
    pub serial: Option<u32>,
    pub manufacture_week: i32,
    pub manufacture_year: i32,
    pub model_year: Option<i32>,
}

#[cfg(feature = "libdisplay-info")]
impl From<libdisplay_info::edid::VendorProduct> for EdidProduct {
    fn from(vp: libdisplay_info::edid::VendorProduct) -> Self {
        Self {
            manufacturer: vp.manufacturer,
            product: vp.product,
            serial: vp.serial,
            manufacture_week: vp.manufacture_week,
            manufacture_year: vp.manufacture_year,
            model_year: vp.model_year,
        }
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AppearanceConfig {
    pub clip_floating_windows: bool,
    pub clip_tiled_windows: bool,
    pub shadow_tiled_windows: bool,
}

impl Default for AppearanceConfig {
    fn default() -> Self {
        AppearanceConfig {
            clip_floating_windows: true,
            clip_tiled_windows: true,
            shadow_tiled_windows: false,
        }
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum DecorationPreference {
    #[default]
    ClientSide,
    ServerSide,
}

#[derive(Clone, Debug, PartialEq, CosmicConfigEntry)]
#[version = 1]
pub struct CosmicCompConfig {
    pub workspaces: workspace::WorkspaceConfig,
    pub pinned_workspaces: Vec<workspace::PinnedWorkspace>,
    pub input_default: input::InputConfig,
    pub input_touchpad: input::InputConfig,
    pub input_touchpad_override: TouchpadOverride,
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
    pub accessibility_zoom: ZoomConfig,
    pub appearance_settings: AppearanceConfig,
    /// When the cursor hides itself: idle, fullscreen idle, typing, touch
    pub cursor_hide: CursorHideConfig,
    /// Briefly magnify the cursor when the pointer is shaken, to help locate it
    pub cursor_shake_to_find: bool,
    pub activation_policy: ActivationPolicy,
    pub decoration_preference: DecorationPreference,
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
            input_touchpad_override: Default::default(),
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
            accessibility_zoom: ZoomConfig::default(),
            appearance_settings: AppearanceConfig::default(),
            cursor_hide: CursorHideConfig::default(),
            cursor_shake_to_find: true,
            activation_policy: ActivationPolicy::default(),
            decoration_preference: DecorationPreference::default(),
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

/// What the cursor idle timer should do when it fires.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HideDecision {
    /// Nothing applies in this context; drop the timer until the next activity.
    Drop,
    /// Hide the cursor now.
    Hide,
    /// Not yet — re-arm for this long.
    RearmAfter(Duration),
}

/// When the cursor hides itself. Every trigger is revealed by pointer input, so
/// these differ only in what arms the hide and after how long.
// `default`: a trigger added later must not make every existing file fail to
// deserialize, which `get_entry` would swallow as "reset everyone to defaults".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(default)]
pub struct CursorHideConfig {
    /// Seconds of pointer inactivity before hiding, anywhere. `None` disables.
    pub idle_timeout: Option<u32>,
    /// Seconds of pointer inactivity before hiding over a fullscreen window.
    /// Shortens `idle_timeout` in fullscreen; it never lengthens it.
    pub fullscreen_idle_timeout: Option<u32>,
    /// Hide as soon as a key is pressed.
    pub while_typing: bool,
    /// Hide on touch input, until the pointer next moves.
    pub after_touch: bool,
}

impl CursorHideConfig {
    /// The shortest timeout that applies right now, if any.
    pub fn effective_timeout(&self, fullscreen: bool) -> Option<Duration> {
        let secs = if fullscreen {
            match (self.idle_timeout, self.fullscreen_idle_timeout) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (a, b) => a.or(b),
            }
        } else {
            self.idle_timeout
        }?;
        Some(Duration::from_secs(secs as u64))
    }

    /// What to do when the idle timer fires after `elapsed` without pointer input.
    pub fn resolve(&self, elapsed: Duration, fullscreen: bool) -> HideDecision {
        match self.effective_timeout(fullscreen) {
            None => HideDecision::Drop,
            Some(timeout) if elapsed >= timeout => HideDecision::Hide,
            Some(timeout) => HideDecision::RearmAfter(timeout - elapsed),
        }
    }

    /// Whether any timer-driven hiding is configured at all.
    pub fn has_idle_trigger(&self) -> bool {
        self.idle_timeout.is_some() || self.fullscreen_idle_timeout.is_some()
    }

    /// The delay to arm the timer with, before context is known. Callers on the
    /// input path cannot read fullscreen state without deadlocking, so they arm
    /// pessimistically and `resolve` corrects it on fire.
    pub fn shortest_timeout(&self) -> Option<Duration> {
        self.effective_timeout(true)
    }
}

impl Default for CursorHideConfig {
    fn default() -> Self {
        CursorHideConfig {
            idle_timeout: None,
            fullscreen_idle_timeout: Some(3),
            while_typing: false,
            after_touch: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub struct ZoomConfig {
    pub start_on_login: bool,
    pub show_overlay: bool,
    pub increment: u32,
    pub view_moves: ZoomMovement,
    pub enable_mouse_zoom_shortcuts: bool,
}

impl ZoomConfig {
    pub const ZOOM_INCREMENT_PRESETS: &[u32] = &[10, 25, 50, 75, 100, 150, 200];
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
pub enum ActivationPolicy {
    #[default]
    Focus,
    FocusIfActiveWorkspace,
    Urgent,
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

#[cfg(test)]
mod test {
    use super::{CursorHideConfig, HideDecision};
    use std::time::Duration;

    const OFF: CursorHideConfig = CursorHideConfig {
        idle_timeout: None,
        fullscreen_idle_timeout: None,
        while_typing: false,
        after_touch: false,
    };

    fn secs(n: u64) -> Duration {
        Duration::from_secs(n)
    }

    #[test]
    fn no_timeout_never_hides() {
        assert_eq!(OFF.resolve(secs(600), false), HideDecision::Drop);
        assert_eq!(OFF.resolve(secs(600), true), HideDecision::Drop);
        assert!(!OFF.has_idle_trigger());
    }

    #[test]
    fn global_timeout_applies_everywhere() {
        let cfg = CursorHideConfig {
            idle_timeout: Some(10),
            ..OFF
        };
        assert_eq!(
            cfg.resolve(secs(4), false),
            HideDecision::RearmAfter(secs(6))
        );
        assert_eq!(cfg.resolve(secs(10), false), HideDecision::Hide);
        // A fullscreen window must not suppress an enabled global timeout.
        assert_eq!(cfg.resolve(secs(10), true), HideDecision::Hide);
        assert!(cfg.has_idle_trigger());
    }

    #[test]
    fn fullscreen_timeout_only_applies_in_fullscreen() {
        let cfg = CursorHideConfig {
            fullscreen_idle_timeout: Some(3),
            ..OFF
        };
        assert_eq!(cfg.resolve(secs(3), true), HideDecision::Hide);
        assert_eq!(cfg.resolve(secs(600), false), HideDecision::Drop);
    }

    #[test]
    fn fullscreen_shortens_but_never_lengthens() {
        let short_fs = CursorHideConfig {
            idle_timeout: Some(10),
            fullscreen_idle_timeout: Some(3),
            ..OFF
        };
        assert_eq!(short_fs.resolve(secs(3), true), HideDecision::Hide);
        assert_eq!(
            short_fs.resolve(secs(3), false),
            HideDecision::RearmAfter(secs(7))
        );

        let long_fs = CursorHideConfig {
            idle_timeout: Some(3),
            fullscreen_idle_timeout: Some(30),
            ..OFF
        };
        assert_eq!(long_fs.resolve(secs(3), true), HideDecision::Hide);

        // The arming delay is pessimistic: the shortest timeout that could
        // apply in any context, because arm-time code cannot read the shell.
        assert_eq!(short_fs.shortest_timeout(), Some(secs(3)));
        assert_eq!(long_fs.shortest_timeout(), Some(secs(3)));
        assert_eq!(OFF.shortest_timeout(), None);
    }

    #[test]
    fn boundary_is_inclusive() {
        let cfg = CursorHideConfig {
            idle_timeout: Some(5),
            ..OFF
        };
        assert_eq!(
            cfg.resolve(Duration::from_millis(4999), false),
            HideDecision::RearmAfter(Duration::from_millis(1))
        );
        assert_eq!(cfg.resolve(secs(5), false), HideDecision::Hide);
        assert_eq!(cfg.resolve(secs(6), false), HideDecision::Hide);
    }

    #[test]
    fn defaults_match_the_spec() {
        let cfg = CursorHideConfig::default();
        assert_eq!(cfg.idle_timeout, None);
        assert_eq!(cfg.fullscreen_idle_timeout, Some(3));
        assert!(!cfg.while_typing);
        assert!(cfg.after_touch);
    }

    #[test]
    fn config_round_trips_and_tolerates_missing_fields() {
        let cfg = CursorHideConfig {
            idle_timeout: Some(7),
            fullscreen_idle_timeout: None,
            while_typing: true,
            after_touch: false,
        };
        let encoded = ron::ser::to_string(&cfg).unwrap();
        assert_eq!(ron::from_str::<CursorHideConfig>(&encoded).unwrap(), cfg);

        // A file written before a field existed must keep the rest of the user's
        // settings rather than resetting the whole key.
        let partial: CursorHideConfig = ron::from_str("(idle_timeout: Some(5))").unwrap();
        assert_eq!(
            partial,
            CursorHideConfig {
                idle_timeout: Some(5),
                ..CursorHideConfig::default()
            }
        );
    }
}
