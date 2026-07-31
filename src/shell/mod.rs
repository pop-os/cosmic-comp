use calloop::LoopHandle;
use focus::target::WindowGroup;
use grabs::{MenuAlignment, SeatMoveGrabState};
use indexmap::IndexMap;
use layout::TilingExceptions;
use std::{
    collections::HashMap,
    sync::{Mutex, OnceLock, atomic::Ordering},
    thread,
    time::{Duration, Instant},
};
use wayland_backend::server::{ClientId, ObjectId};

/// Check if home mode feature is enabled via HOME_ENABLED env var.
/// This is cached on first access.
pub fn home_enabled() -> bool {
    static HOME_ENABLED: OnceLock<bool> = OnceLock::new();
    *HOME_ENABLED.get_or_init(|| {
        std::env::var("HOME_ENABLED")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false)
    })
}

use crate::{
    shell::{
        element::CosmicStack, focus::FocusTarget, grabs::fullscreen_items,
        layout::tiling::PlaceholderType,
    },
    utils,
    wayland::{
        handlers::data_device::{self, get_dnd_icon},
        protocols::workspace::{State as WState, WorkspaceCapabilities},
    },
};
use cosmic_comp_config::{
    AppearanceConfig, TileBehavior, ZoomConfig, ZoomMovement,
    workspace::{PinnedWorkspace, WorkspaceLayout, WorkspaceMode},
};
use cosmic_config::ConfigSet;
use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::TilingState;
use cosmic_settings_config::shortcuts::action::{Direction, FocusDirection, ResizeDirection};
use cosmic_settings_config::{shortcuts, window_rules::ApplicationException};
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::{input::TouchSlot, renderer::element::RenderElementStates},
    desktop::{
        LayerSurface, PopupKind, WindowSurface, WindowSurfaceType, layer_map_for_output,
        space::SpaceElement,
        utils::{
            OutputPresentationFeedback, surface_presentation_feedback_flags_from_states,
            surface_primary_scanout_output, take_presentation_feedback_surface_tree,
        },
    },
    input::{
        Seat,
        pointer::{
            CursorImageStatus, CursorImageSurfaceData, Focus, GrabStartData as PointerGrabStartData,
        },
    },
    output::{Output, WeakOutput},
    reexports::{
        wayland_protocols::ext::session_lock::v1::server::ext_session_lock_v1::ExtSessionLockV1,
        wayland_server::{Client, Resource, protocol::wl_surface::WlSurface},
    },
    utils::{IsAlive, Logical, Point, Rectangle, Serial, Size},
    wayland::{
        compositor::{SurfaceAttributes, get_parent, with_states},
        seat::WaylandFocus,
        session_lock::LockSurface,
        shell::{
            wlr_layer::{Anchor, KeyboardInteractivity, Layer, LayerSurfaceCachedState},
            xdg::{XDG_POPUP_ROLE, XdgPopupSurfaceData},
        },
        xdg_activation::XdgActivationState,
        xwayland_keyboard_grab::XWaylandKeyboardGrab,
    },
    xwayland::X11Surface,
};
use tracing::error;

use crate::{
    backend::render::animations::spring::{Spring, SpringParams},
    config::Config,
    utils::{prelude::*, process::workspaces_enabled, quirks::WORKSPACE_OVERVIEW_NAMESPACE},
    wayland::{
        handlers::{
            toplevel_management::minimize_rectangle, xdg_activation::ActivationContext,
            xdg_shell::popup::get_popup_toplevel,
        },
        protocols::{
            toplevel_info::{
                ToplevelInfoState, toplevel_enter_output, toplevel_enter_workspace,
                toplevel_leave_output, toplevel_leave_workspace,
            },
            workspace::{
                WorkspaceGroupHandle, WorkspaceHandle, WorkspaceState, WorkspaceUpdateGuard,
            },
        },
    },
};

pub mod auto_hide;
pub mod element;
pub mod focus;
pub mod grabs;
pub mod layer_open;
pub mod layer_resize_anim;
pub mod layer_slide;
pub mod layout;
mod seats;
mod workspace;
pub mod zoom;
pub use self::element::{CosmicMapped, CosmicMappedRenderElement, CosmicSurface};
pub use self::seats::*;
pub use self::workspace::*;
use self::zoom::{OutputZoomState, ZoomState};

use self::{
    element::{
        CosmicWindow, MaximizedState, resize_indicator::ResizeIndicator,
        swap_indicator::SwapIndicator,
    },
    focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    grabs::{
        GrabStartData, Item, MenuGrab, MoveGrab, ReleaseMode, ResizeEdge, ResizeGrab, tab_items,
        window_items,
    },
    layout::{
        floating::{FloatingLayout, ResizeState},
        tiling::{NodeDesc, ResizeForkGrab, TilingLayout},
    },
};

const GESTURE_MAX_LENGTH: f64 = 150.0;
const GESTURE_POSITION_THRESHOLD: f64 = 0.5;
const GESTURE_VELOCITY_THRESHOLD: f64 = 0.02;
const MOVE_GRAB_Y_OFFSET: f64 = 16.;
/// When dragging a zone-filling window, shrink it to this fraction of the zone.
const DRAG_UNMAXIMIZE_FRACTION: (i32, i32) = (2, 3);
const ACTIVATION_TOKEN_EXPIRE_TIME: Duration = Duration::from_secs(5);

#[derive(Debug, Clone)]
pub enum Trigger {
    KeyboardSwap(shortcuts::Binding, NodeDesc),
    KeyboardMove(shortcuts::Modifiers),
    Pointer(u32),
    Touch(TouchSlot),
}

#[derive(Debug, Clone)]
pub enum OverviewMode {
    None,
    Started(Trigger, Instant),
    Active(Trigger),
    Ended(Option<Trigger>, Instant),
}

impl OverviewMode {
    pub fn alpha(&self, animation: Duration) -> Option<f32> {
        match self {
            OverviewMode::Started(_, start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / animation.as_millis() as f32;
                Some(ease(EaseInOutCubic, 0.0, 1.0, percentage))
            }
            OverviewMode::Active(_) => Some(1.0),
            OverviewMode::Ended(_, end) => {
                let percentage = Instant::now().duration_since(*end).as_millis() as f32
                    / animation.as_millis() as f32;
                if percentage < 1.0 {
                    Some(ease(EaseInOutCubic, 1.0, 0.0, percentage))
                } else {
                    None
                }
            }
            OverviewMode::None => None,
        }
    }

    pub fn is_active(&self) -> bool {
        matches!(self, OverviewMode::Started(_, _) | OverviewMode::Active(_))
    }

    pub fn active_trigger(&self) -> Option<&Trigger> {
        if let OverviewMode::Started(trigger, _) | OverviewMode::Active(trigger) = self {
            Some(trigger)
        } else {
            None
        }
    }

    pub fn trigger(&self) -> Option<&Trigger> {
        self.active_trigger().or({
            if let OverviewMode::Ended(trigger, _) = self {
                trigger.as_ref()
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ResizeMode {
    None,
    Started(shortcuts::Binding, Instant, ResizeDirection),
    Active(shortcuts::Binding, ResizeDirection),
    Ended(Instant, ResizeDirection),
}

impl ResizeMode {
    pub fn alpha(&self, animation: Duration) -> Option<f32> {
        match self {
            ResizeMode::Started(_, start, _) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / animation.as_millis() as f32;
                Some(ease(EaseInOutCubic, 0.0, 1.0, percentage))
            }
            ResizeMode::Active(_, _) => Some(1.0),
            ResizeMode::Ended(end, _) => {
                let percentage = Instant::now().duration_since(*end).as_millis() as f32
                    / animation.as_millis() as f32;
                if percentage < 1.0 {
                    Some(ease(EaseInOutCubic, 1.0, 0.0, percentage))
                } else {
                    None
                }
            }
            ResizeMode::None => None,
        }
    }

    pub fn active_binding(&self) -> Option<&shortcuts::Binding> {
        if let ResizeMode::Started(binding, _, _) | ResizeMode::Active(binding, _) = self {
            Some(binding)
        } else {
            None
        }
    }

    pub fn active_direction(&self) -> Option<ResizeDirection> {
        if let ResizeMode::Started(_, _, direction) | ResizeMode::Active(_, direction) = self {
            Some(*direction)
        } else {
            None
        }
    }
}

/// Home mode state for layer-shell home visibility
/// Controls the visibility animation of "home-only" surfaces
#[derive(Debug, Clone, Default)]
pub enum HomeMode {
    /// Not in home mode - home-only surfaces are hidden
    #[default]
    None,
    /// Transitioning to home mode (fading in)
    FadingIn(Instant),
    /// Fully in home mode - home-only surfaces visible
    Active,
    /// Transitioning out of home mode (fading out)
    FadingOut(Instant),
}

impl HomeMode {
    /// Returns the current opacity for home-only surfaces (0.0 = hidden, 1.0 = visible)
    pub fn alpha(&self, animation: Duration) -> f32 {
        match self {
            HomeMode::None => 0.0,
            HomeMode::FadingIn(start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / animation.as_millis() as f32;
                ease(EaseInOutCubic, 0.0, 1.0, percentage.min(1.0))
            }
            HomeMode::Active => 1.0,
            HomeMode::FadingOut(start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / animation.as_millis() as f32;
                ease(EaseInOutCubic, 1.0, 0.0, percentage.min(1.0))
            }
        }
    }

    /// Returns true if home mode is active or transitioning to active
    pub fn is_active(&self) -> bool {
        matches!(self, HomeMode::FadingIn(_) | HomeMode::Active)
    }

    /// Returns true if an animation is in progress
    pub fn is_animating(&self, animation: Duration) -> bool {
        match self {
            HomeMode::FadingIn(start) | HomeMode::FadingOut(start) => {
                Instant::now().duration_since(*start) < animation
            }
            _ => false,
        }
    }

    /// Start transition to home mode
    pub fn enter(&mut self, animation: Duration) {
        match self {
            HomeMode::None => *self = HomeMode::FadingIn(Instant::now()),
            HomeMode::FadingOut(start) => {
                // Reverse the animation from current position
                let elapsed = Instant::now().duration_since(*start);
                let remaining = animation.saturating_sub(elapsed);
                *self = HomeMode::FadingIn(Instant::now() - remaining);
            }
            _ => {} // Already active or fading in
        }
    }

    /// Start transition out of home mode
    pub fn exit(&mut self, animation: Duration) {
        match self {
            HomeMode::Active => *self = HomeMode::FadingOut(Instant::now()),
            HomeMode::FadingIn(start) => {
                // Reverse the animation from current position
                let elapsed = Instant::now().duration_since(*start);
                let remaining = animation.saturating_sub(elapsed);
                *self = HomeMode::FadingOut(Instant::now() - remaining);
            }
            _ => {} // Already none or fading out
        }
    }

    /// Update animation state, transitioning to final state when complete
    pub fn update(&mut self, animation: Duration) {
        match self {
            HomeMode::FadingIn(start) => {
                if Instant::now().duration_since(*start) >= animation {
                    *self = HomeMode::Active;
                }
            }
            HomeMode::FadingOut(start) => {
                if Instant::now().duration_since(*start) >= animation {
                    *self = HomeMode::None;
                }
            }
            _ => {}
        }
    }
}

/// Voice mode animation duration for window fade
const VOICE_MODE_ANIMATION_DURATION: Duration = Duration::from_millis(200);
/// Duration to wait for orb to grow in before considering animation complete
const VOICE_MODE_ORB_GROW_DURATION: Duration = Duration::from_millis(400);
/// Duration to wait for orb to shrink out
const VOICE_MODE_ORB_SHRINK_DURATION: Duration = Duration::from_millis(300);

/// Voice mode state for window fading when voice orb is active
///
/// The animation sequence is:
/// - Enter: None -> FadingIn -> WaitingForOrbGrow -> Active
/// - Exit:  Active -> WaitingForOrbShrink -> FadingOut -> None
#[derive(Debug, Clone, Default)]
pub enum VoiceMode {
    /// Voice mode not active - windows at full opacity
    #[default]
    None,
    /// Step 1 (enter): Fading out windows before showing orb
    FadingIn(Instant),
    /// Step 2 (enter): Windows faded, waiting for orb grow-in animation
    WaitingForOrbGrow(Instant),
    /// Fully in voice mode - windows faded, orb visible
    Active,
    /// Step 1 (exit): Orb is shrinking, windows still faded
    WaitingForOrbShrink(Instant),
    /// Step 2 (exit): Orb hidden, fading in windows
    FadingOut(Instant),
}

impl VoiceMode {
    /// Returns the current fade-out alpha for windows (1.0 = full opacity, 0.0 = hidden)
    /// When voice mode is active, windows should fade to completely hidden
    pub fn window_alpha(&self) -> f32 {
        match self {
            VoiceMode::None => 1.0,
            VoiceMode::FadingIn(start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / VOICE_MODE_ANIMATION_DURATION.as_millis() as f32;
                // Fade from 1.0 to 0.0
                let t = percentage.min(1.0);
                1.0 - t
            }
            // Windows stay fully faded during orb animations and active state
            VoiceMode::WaitingForOrbGrow(_)
            | VoiceMode::Active
            | VoiceMode::WaitingForOrbShrink(_) => 0.0,
            VoiceMode::FadingOut(start) => {
                let percentage = Instant::now().duration_since(*start).as_millis() as f32
                    / VOICE_MODE_ANIMATION_DURATION.as_millis() as f32;
                // Fade from 0.0 to 1.0

                percentage.min(1.0)
            }
        }
    }

    /// Returns true if voice mode is active or transitioning to active
    pub fn is_active(&self) -> bool {
        matches!(
            self,
            VoiceMode::FadingIn(_)
                | VoiceMode::WaitingForOrbGrow(_)
                | VoiceMode::Active
                | VoiceMode::WaitingForOrbShrink(_)
        )
    }

    /// Returns true if an animation is in progress
    pub fn is_animating(&self) -> bool {
        !matches!(self, VoiceMode::None | VoiceMode::Active)
    }

    /// Returns true if the orb should be shown (windows have faded out)
    pub fn should_show_orb(&self) -> bool {
        matches!(
            self,
            VoiceMode::WaitingForOrbGrow(_) | VoiceMode::Active | VoiceMode::WaitingForOrbShrink(_)
        )
    }

    /// Returns true if the orb should be hidden (exiting voice mode)
    pub fn should_hide_orb(&self) -> bool {
        matches!(
            self,
            VoiceMode::WaitingForOrbShrink(_) | VoiceMode::FadingOut(_) | VoiceMode::None
        )
    }

    /// Start transition to voice mode (fade out windows, then orb grows)
    pub fn enter(&mut self) {
        match self {
            VoiceMode::None => {
                *self = VoiceMode::FadingIn(Instant::now());
            }
            VoiceMode::FadingOut(start) => {
                // Reverse the animation from current position
                let elapsed = Instant::now().duration_since(*start);
                let remaining = VOICE_MODE_ANIMATION_DURATION.saturating_sub(elapsed);
                *self = VoiceMode::FadingIn(Instant::now() - remaining);
            }
            VoiceMode::WaitingForOrbShrink(_) => {
                // Re-entering voice mode while orb was shrinking
                // Go directly to Active since orb is already visible
                *self = VoiceMode::Active;
            }
            _ => {} // Already active or fading in
        }
    }

    /// Start transition out of voice mode (orb shrinks, then windows fade in)
    pub fn exit(&mut self) {
        match self {
            VoiceMode::Active | VoiceMode::WaitingForOrbGrow(_) => {
                // Start orb shrink phase
                *self = VoiceMode::WaitingForOrbShrink(Instant::now());
            }
            VoiceMode::FadingIn(start) => {
                // If still fading in, reverse to fade out
                let elapsed = Instant::now().duration_since(*start);
                let remaining = VOICE_MODE_ANIMATION_DURATION.saturating_sub(elapsed);
                *self = VoiceMode::FadingOut(Instant::now() - remaining);
            }
            _ => {} // Already none, fading out, or waiting for orb shrink
        }
    }

    /// Start fading windows back in immediately (for attached mode transitions)
    /// Unlike exit(), this skips the WaitingForOrbShrink phase since the orb
    /// is bursting behind the window, not shrinking.
    pub fn fade_in_immediately(&mut self) {
        match self {
            VoiceMode::None | VoiceMode::FadingOut(_) => {
                // Already fading in or done
            }
            VoiceMode::FadingIn(start) => {
                // Reverse the fade-in animation
                let elapsed = Instant::now().duration_since(*start);
                let remaining = VOICE_MODE_ANIMATION_DURATION.saturating_sub(elapsed);
                *self = VoiceMode::FadingOut(Instant::now() - remaining);
            }
            _ => {
                // Go directly to FadingOut (skip WaitingForOrbShrink)
                *self = VoiceMode::FadingOut(Instant::now());
            }
        }
    }

    /// Exit voice mode immediately from attached state
    /// Unlike exit(), this skips all animation phases since windows were already
    /// visible during attached mode.
    pub fn exit_from_attached(&mut self) {
        *self = VoiceMode::None;
    }

    /// Update animation state, transitioning through the sequence
    pub fn update(&mut self) {
        match self {
            VoiceMode::FadingIn(start) => {
                if Instant::now().duration_since(*start) >= VOICE_MODE_ANIMATION_DURATION {
                    // Window fade complete, now wait for orb to grow
                    *self = VoiceMode::WaitingForOrbGrow(Instant::now());
                }
            }
            VoiceMode::WaitingForOrbGrow(start) => {
                if Instant::now().duration_since(*start) >= VOICE_MODE_ORB_GROW_DURATION {
                    *self = VoiceMode::Active;
                }
            }
            VoiceMode::WaitingForOrbShrink(start) => {
                if Instant::now().duration_since(*start) >= VOICE_MODE_ORB_SHRINK_DURATION {
                    // Orb shrink complete, now fade in windows
                    *self = VoiceMode::FadingOut(Instant::now());
                }
            }
            VoiceMode::FadingOut(start) => {
                if Instant::now().duration_since(*start) >= VOICE_MODE_ANIMATION_DURATION {
                    *self = VoiceMode::None;
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ActivationKey {
    Wayland(WlSurface),
    X11(u32),
}

impl From<&CosmicSurface> for ActivationKey {
    fn from(value: &CosmicSurface) -> Self {
        match value.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                ActivationKey::Wayland(toplevel.wl_surface().clone())
            }
            WindowSurface::X11(s) => ActivationKey::X11(s.window_id()),
        }
    }
}

#[derive(Debug)]
pub struct PendingWindow {
    pub surface: CosmicSurface,
    pub seat: Seat<State>,
    pub fullscreen: Option<Output>,
    pub maximized: bool,
    pub sticky: bool,
    pub frame_notified: bool,
}

#[derive(Debug)]
pub struct PendingLayer {
    pub surface: LayerSurface,
    pub seat: Seat<State>,
    pub output: Output,
}

/// Smallest the desktop/windows area may be reserved at while resizing the side
/// panel. Once the panel would shrink the windows below this, its exclusive zone is
/// capped here and the panel surface grows *over* the windows instead, so app
/// layouts don't get squished and break. (Logical px.)
const MIN_VIEWPORT_WIDTH: i32 = 500;

/// Smallest width the side panel may be resized to (logical px). Mirrors the client's
/// own `MIN_PANEL_WIDTH` — the client is a separate process, so the value is duplicated
/// here rather than shared.
const MIN_PANEL_WIDTH: i32 = 320;

/// Fraction of the output width at/above which the side panel counts as "maximized": an
/// edge double-click then restores it to its previous width instead of maximizing.
const MAXIMIZE_FRACTION: f32 = 0.99;

/// The side panel's current resize target. While a spring resize animation
/// ([`layer_resize_anim::LayerResizeAnim`]) plays, this is updated each frame to the
/// eased `width` and forced onto the surface (see [`Shell::override_active_layer_resize`]).
#[derive(Debug, Clone)]
pub struct LayerResize {
    /// The layer surface being resized.
    pub surface_id: ObjectId,
    /// The output the panel is on (its right/left edge is the fixed anchor edge).
    pub output: Output,
    /// `true` if anchored to the right edge (dragged edge is the left), `false` for
    /// a left-anchored panel.
    pub anchor_right: bool,
    /// Current target width in logical pixels.
    pub width: i32,
    /// Smallest allowed width.
    pub min: i32,
    /// Largest allowed width (the output's logical width).
    pub max: i32,
}

/// Edge double-click maximize/restore bookkeeping for the side panel. Persists across
/// resize animations (unlike [`LayerResize`], which is transient), so the two clicks of a
/// double-click can be timed and the pre-maximize width can be restored on the second toggle.
#[derive(Debug, Clone)]
pub struct LayerMaximizeState {
    /// The surface the toggle applies to.
    pub surface_id: ObjectId,
    /// Width to restore to when un-maximizing (captured at the last maximize).
    pub restore_width: i32,
    /// Timestamp + global X of the last edge press, for double-click detection.
    pub last_click: Option<(std::time::Instant, f64)>,
}

/// The side-panel edge currently hovered (pointer within the grab zone, no drag
/// in progress). Drives the hover sash indicator + the EW-resize cursor.
#[derive(Debug, Clone)]
pub struct EdgeHover {
    pub surface_id: ObjectId,
    /// Whether the panel is right-anchored (its draggable edge is on the left).
    pub anchor_right: bool,
}

/// An in-progress edge drag. The panel does **not** resize while dragging — only
/// this ghost `width` follows the pointer; on release the panel springs to it via
/// [`Shell::set_layer_resize_width`] (see [`grabs::EdgeResizeGrab`]).
#[derive(Debug, Clone)]
pub struct EdgeDragGhost {
    pub surface_id: ObjectId,
    pub output: Output,
    pub anchor_right: bool,
    /// Ghost width in logical px (clamped to the surface's `[min, max]`).
    pub width: i32,
}

/// What the render path should draw for a surface's edge sash, if anything.
pub enum EdgeIndicator {
    /// Subtle bar at the resting outer edge (hover, pre-drag).
    Hover { anchor_right: bool },
    /// Brighter bar at the dragged ghost edge; `ghost_width` is the panel width
    /// the bar represents (measured from the anchored edge).
    Drag {
        anchor_right: bool,
        ghost_width: i32,
    },
}

/// Native exclusive gaming-mode state.
///
/// Entered when a game-mode client enters game mode for an app over the
/// `one.playtron.GameMode` D-Bus interface (see [`crate::dbus::game_mode`]). Game
/// mode makes the game an exclusive fullscreen surface — which on its own turns
/// on cosmic-comp's fullscreen fast path (direct scanout + VRR) — and minimizes
/// everything else, giving the macOS-style "the desktop clears and the game takes
/// over" feel.
#[derive(Debug, Default)]
pub struct GameMode {
    pub active: bool,
    /// The Steam app id (`STEAM_GAME`) currently in game mode.
    pub app_id: Option<u32>,
    /// Pid of the process that launches games (the session manager). A game it
    /// spawns is a descendant of it, which lets a brand-new game window be
    /// recognized as game mode's at MAP time — before it has been tagged — and
    /// placed on the game-mode output rather than under the cursor.
    ///
    /// Currently always `None`: it cannot be resolved from the D-Bus caller,
    /// because asking the bus mid-method deadlocks the interface. The session
    /// manager has to supply it explicitly. Until then `game_mode_claims` matches
    /// on the game's own process tree only, which covers a running game's dialogs
    /// but not a game being launched fresh.
    pub controller_pid: Option<u32>,
    /// Base-layer priority published by the session manager on the X11 root
    /// (`GAMESCOPECTRL_BASELAYER_APPID`), highest priority FIRST.
    ///
    /// This is how the session manager expresses stacking without focusing
    /// anything: a window whose app id appears earlier in the list is stacked
    /// above one that appears later, so e.g. a custom webview shown over a
    /// running game sorts ahead of the game while the game keeps rendering
    /// behind it. Empty when the property is unset, in which case adoption
    /// order alone decides.
    pub baselayer_appids: Vec<u32>,
    /// The game surface we fullscreened, so we can un-fullscreen it on exit.
    pub game_surface: Option<CosmicSurface>,
    /// Windows that belong WITH the adopted game and are therefore allowed to
    /// render above it under strict control: its own dialogs, launcher/EULA
    /// windows and in-prefix login/browser windows. Membership is an allowlist
    /// rooted at `game_surface` (same `STEAM_GAME` id, same pid, or a
    /// direct `WM_TRANSIENT_FOR` pointing at it), so an unrelated window —
    /// including a game that raw-fullscreens itself before being adopted — stays
    /// hidden. FRONT-TO-BACK (topmost first), matching `Workspace::mapped()`;
    /// recomputed by `refresh_game_mode_state`.
    pub children: Vec<CosmicSurface>,
    /// The output the game is fullscreened on — display caps (refresh rate, VRR
    /// / tearing support, external) are reported for THIS output, not just the
    /// first one, so they're correct on multi-monitor setups.
    pub output: Option<Output>,
    /// The desktop workspace game mode was first entered from, restored on a full
    /// exit. Each game-mode app is fullscreened on its own (clean, auto-reaped)
    /// workspace and switching between apps is a workspace switch, so nothing is
    /// minimized; this only records where "normal desktop" was.
    pub home_workspace: Option<WorkspaceHandle>,
    /// Set when entry was requested but no window carrying that app id was mapped
    /// yet; resolved by `try_resolve_pending_game_mode` (the refresh tick and the
    /// `STEAM_GAME` property hook) once a matching window appears.
    pub pending_app_id: Option<u32>,
    /// Whether a gaming overlay is currently up over the game — either a real
    /// overlay window (`STEAM_OVERLAY`/`GAMESCOPE_EXTERNAL_OVERLAY`) or a client
    /// `SetOverlay(true)` assertion. Maintained by `refresh_overlay_visible`;
    /// read by the render path to drop the tearing/scanout fast path so the
    /// overlay composites cleanly.
    pub overlay_active: bool,
    /// The last `SetOverlay(visible)` assertion from a D-Bus client (the native
    /// QAM, which can't set the X11 overlay marker). OR'd with real overlay
    /// presence into `overlay_active`.
    pub overlay_asserted: bool,
    /// The window currently holding the input grab over the game (via
    /// `STEAM_INPUT_FOCUS` or `SetOverlay(blocking)`), so it can be released
    /// cleanly. Reset by `GameMode::default()` on exit.
    pub input_grab: Option<CosmicSurface>,
    /// While an overlay is up (`SetOverlay(true)`), the surface to composite
    /// over the game — the launcher (or a client overlay) window, resolved in
    /// `refresh_overlay_visible`. The render path stacks it above the game with
    /// its own per-pixel alpha (transparent except its panel), so the game shows
    /// through. Keyboard input to a blocking overlay is handled separately by the
    /// input grab; a non-blocking overlay just renders.
    pub overlay_surface: Option<CosmicSurface>,
}

#[derive(Debug)]
pub struct Shell {
    pub workspaces: Workspaces,

    // Can't make this into a HashSet. See https://github.com/pop-os/cosmic-comp/pull/1902
    pub pending_windows: Vec<PendingWindow>,
    pub pending_layers: Vec<PendingLayer>,
    pub pending_activations: HashMap<ActivationKey, ActivationContext>,
    pub override_redirect_windows: Vec<X11Surface>,
    pub session_lock: Option<SessionLock>,
    pub seats: Seats,
    pub previous_workspace_idx: Option<(Serial, WeakOutput, usize)>,
    pub xwayland_keyboard_grab: Option<XWaylandKeyboardGrab<State>>,

    theme: crate::comp_theme::CompTheme,
    pub active_hint: bool,
    overview_mode: OverviewMode,
    swap_indicator: Option<SwapIndicator>,
    /// Performance-capture badge, present only while an F12 capture is running.
    pub perf_badge: Option<crate::backend::render::perf_badge::PerfBadge>,
    resize_mode: ResizeMode,
    resize_state: Option<(
        KeyboardFocusTarget,
        ResizeDirection,
        ResizeEdge,
        i32,
        usize,
        Output,
    )>,
    resize_indicator: Option<ResizeIndicator>,
    zoom_state: Option<ZoomState>,
    pub game_mode: GameMode,
    /// Whether tearing (immediate/async page flips) is permitted while in game
    /// mode. Driven by the `one.playtron.GameMode` `SetTearing` D-Bus call; kept
    /// on `Shell` (not `GameMode`) so it persists across game-mode enter/exit.
    pub tearing_allowed: bool,
    /// Game-mode frame-rate cap (0 = uncapped). Read by the KMS surface thread
    /// for the output showing the fullscreen game, which caps its presentation
    /// rate. Driven by `one.playtron.GameMode.SetFpsLimit`.
    pub game_mode_fps_limit: u32,
    /// Game-mode VRR policy. Read by the KMS surface thread to override the
    /// output's adaptive-sync setting while a game is fullscreen. Driven by
    /// `one.playtron.GameMode.SetVrr`.
    pub game_mode_vrr: crate::dbus::game_mode::VrrMode,
    /// Live recent frame time (ns) of the output showing the fullscreen game,
    /// written each frame by the KMS surface thread. Shares its `Arc` with the
    /// game-mode D-Bus bridge so `AppFrametimeNs` (Auto-TDP) reads live values.
    /// Wired in `State::new` after the bridge is created; 0 when no game runs.
    pub game_mode_frametime_ns: std::sync::Arc<std::sync::atomic::AtomicU64>,
    /// Whether the game's output supports async-flip tearing — a device cap only
    /// the KMS surface thread can probe (`c.supports_tearing()`); it writes this
    /// each frame for the game's output, read back for `TearingSupported`.
    pub game_mode_tearing_supported: std::sync::Arc<std::sync::atomic::AtomicBool>,
    /// Latched by the KMS surface thread when a game-mode upscale (`scale_to`)
    /// failed to land on a DRM plane and had to composite (`primary_element ==
    /// Swapchain`) — detected only for a settled game on its own output with no
    /// overlay up. Game mode reads it to stop requesting the scale (letterbox
    /// instead of composited-to-black). Reset on entering game mode, so a new
    /// game / an app switch re-tries the scale.
    pub game_mode_scale_rejected: std::sync::Arc<std::sync::atomic::AtomicBool>,
    /// Render resolution + scaling mode requested for the game (`SetScaling`).
    ///
    /// A non-zero size is a RESOLUTION SPOOF: the game is configured to render at
    /// that size rather than the output's, and the result is scaled up to the
    /// output using `mode`. Zero size keeps the game at output resolution and only
    /// the mode applies (to a game that renders smaller by itself).
    pub game_mode_scaling: (u32, u32, crate::dbus::game_mode::ScalingMode),
    appearance_conf: AppearanceConfig,
    tiling_exceptions: TilingExceptions,
    /// Home mode state for animation (fading in/out of home screen)
    home_mode: HomeMode,
    /// Surface IDs that should only be visible when in home mode
    home_only_surfaces: std::collections::HashSet<ObjectId>,
    /// Surface IDs that should be hidden when in home mode (inverse of home_only)
    hide_on_home_surfaces: std::collections::HashSet<ObjectId>,
    /// Surface IDs that are explicitly hidden by client (layer_surface_visibility protocol)
    hidden_surfaces: std::collections::HashSet<ObjectId>,
    /// The last exclusive zone each layer surface's client actually committed,
    /// recorded before any compositor override. The slide animation scribbles
    /// animated values into `LayerSurfaceCachedState`, so this map is the only
    /// reliable record of the client's intent (e.g. for `detect_layer_slide_edge`
    /// when showing a hidden surface, or for restoring the zone after a slide).
    client_exclusive_zones:
        std::collections::HashMap<ObjectId, smithay::wayland::shell::wlr_layer::ExclusiveZone>,
    /// Surfaces minimized by home mode (to restore when exiting)
    home_minimized_surfaces: Vec<CosmicSurface>,

    /// Voice mode state for window fading animation
    voice_mode: VoiceMode,
    /// Voice orb rendering state
    pub voice_orb_state: crate::backend::render::voice_orb::VoiceOrbState,

    /// Layer surfaces currently fading in (surface ObjectId -> map instant)
    layer_fade_in: std::collections::HashMap<ObjectId, Instant>,
    /// Layer surfaces waiting for a buffer commit before starting their fade-in.
    /// Moved to `layer_fade_in` when the surface next commits a buffer.
    pending_layer_fade_in: std::collections::HashSet<ObjectId>,
    /// Layer surfaces currently fading out (surface ObjectId -> start instant).
    /// While fading out, the surface remains visible with decreasing alpha.
    /// When the animation completes, moved to `hidden_surfaces`.
    layer_fade_out: std::collections::HashMap<ObjectId, Instant>,

    /// Layer surfaces created without a specific wl_output.
    /// When these become visible via the visibility protocol, the compositor
    /// moves them to the output where the cursor currently is.
    pub output_agnostic_layers: std::collections::HashSet<ObjectId>,

    /// Layer surfaces that have already been granted Exclusive keyboard
    /// focus in the commit handler.  Cleared when interactivity drops
    /// below Exclusive or the surface is hidden/destroyed.
    pub exclusive_focus_granted: std::collections::HashSet<ObjectId>,

    /// Surfaces registered for compositor-driven auto-hide.
    pub auto_hide_surfaces: Vec<auto_hide::AutoHideSurface>,

    /// Layer surfaces with active slide animations (visibility-protocol triggered).
    pub layer_slides: Vec<layer_slide::LayerSlide>,

    /// The width the side panel is currently being forced to, set each frame while a
    /// spring resize animation ([`layer_resize_anim::LayerResizeAnim`]) plays. While
    /// set, [`Shell::override_active_layer_resize`] forces the surface's cached size +
    /// exclusive zone to `width` before every `arrange()`, so the compositor — not the
    /// client — owns the resize and windows reflow in lockstep.
    pub active_layer_resize: Option<LayerResize>,

    /// The final state of a *just-ended* resize grab, held until the client's buffer
    /// catches up to the committed width. While set, [`Shell::override_active_layer_resize`]
    /// keeps forcing the size + zone and [`Shell::get_layer_resize_offset`] keeps the
    /// anchored edge pinned — so a fast release never blinks to the trailing (lagging)
    /// width before the final buffer lands. Cleared by
    /// [`Shell::clear_layer_resize_settle_if_caught_up`] once the buffer matches.
    pub layer_resize_settle: Option<LayerResize>,

    /// The active spring resize animation for the side panel, if any (maximize /
    /// restore today, width presets later). Ticked in [`Shell::update_animations`];
    /// each tick sets [`Self::active_layer_resize`] to the eased width and on
    /// completion hands off to [`Self::layer_resize_settle`].
    pub active_layer_resize_anim: Option<layer_resize_anim::LayerResizeAnim>,

    /// The side-panel edge the pointer is currently hovering (no drag), if any —
    /// drives the hover sash indicator. Set/cleared by the input motion handler.
    pub edge_hover: Option<EdgeHover>,

    /// An in-progress edge drag (ghost-only; the panel resizes on release). Set by
    /// the input press handler, updated by [`grabs::EdgeResizeGrab`], drawn by the
    /// render path, cleared on release.
    pub edge_drag_ghost: Option<EdgeDragGhost>,

    /// Edge double-click maximize/restore state for the side panel. Persists across
    /// grabs so two separate clicks can be timed into a double-click and the
    /// pre-maximize width can be restored. See [`Shell::toggle_layer_resize_maximize`].
    pub layer_maximize: Option<LayerMaximizeState>,

    /// Layer surfaces currently playing the compositor-side OPEN animation
    /// (160ms easeInOut slide-up + scale + fade). This is the DEFAULT entrance
    /// for every surface that isn't edge-sliding (see [`Shell::map_layer`]).
    pub layer_opens: Vec<layer_open::LayerOpen>,
    /// Surfaces that will play the open animation but are waiting for their first
    /// buffer commit (auto_size geometry is 0 until then, and a re-shown surface
    /// must render its first frame before it fades in). Moved to `layer_opens`
    /// when the surface next commits a buffer (see `activate_pending_fade_in`).
    pending_layer_opens: std::collections::HashSet<ObjectId>,

    /// Layer surfaces currently playing the compositor-side CLOSE animation (the
    /// reverse of the open: 160ms easeInOut slide-down + scale-down + fade-out).
    /// The DEFAULT exit for every non-edge-sliding surface, triggered when it is
    /// hidden via the `layer_surface_visibility` protocol; the surface stays
    /// alive and rendered until the animation completes (then it's hidden, and
    /// the client typically destroys it shortly after).
    pub layer_closes: Vec<layer_open::LayerClose>,
    /// All currently-mapped surface IDs using the fade+rise (open/close)
    /// transition — i.e. every non-edge-sliding surface — so the
    /// visibility-protocol hide path plays the close (slide-down) animation
    /// instead of a plain cross-fade. Edge-sliding side panels (the chat panel,
    /// the dock) are NOT in this set; they animate via `layer_slides`.
    rise_surfaces: std::collections::HashSet<ObjectId>,

    /// Per-surface show/hide transition requested via the
    /// `layer_surface_visibility` protocol (`set_transition`). `Fade` forces the
    /// fade+rise animation even on an edge-anchored surface that would otherwise
    /// slide; `Slide` forces the edge slide (the dock). Surfaces absent from this
    /// map get the default: a left/right edge-anchored surface slides, everything
    /// else fades + rises.
    layer_transitions: std::collections::HashMap<
        ObjectId,
        crate::wayland::protocols::layer_surface_visibility::LayerTransition,
    >,

    /// Original X11 geometry at map time (before compositor configuration).
    /// Used to compute correct relative offsets for transient children.
    original_x11_positions: HashMap<u32, Rectangle<i32, Logical>>,

    #[cfg(feature = "debug")]
    pub debug_active: bool,
}

#[derive(Debug)]
pub struct SessionLock {
    pub ext_session_lock: ExtSessionLockV1,
    pub surfaces: HashMap<Output, LockSurface>,
}

#[derive(Debug, Clone, Copy)]
pub enum WorkspaceDelta {
    Shortcut(Instant),
    Gesture {
        percentage: f64,
        forward: bool,
    },
    GestureEnd {
        start: Instant,
        spring: Spring,
        forward: bool,
    },
    /// Time-driven cross-fade (no slide): the outgoing workspace stays opaque and
    /// the incoming one fades in over it. Used for the game-mode launcher<->game
    /// switch, where each workspace is a single fullscreen surface (so alpha
    /// blending is exact, with no overlapping-window double-exposure).
    Crossfade(Instant),
    // InvalidGesture(f64), TODO
    // InvalidGestureEnd(Instant, Spring), TODO
}

impl WorkspaceDelta {
    pub fn new_gesture(forward: bool) -> Self {
        WorkspaceDelta::Gesture {
            percentage: 0.0,
            forward,
        }
    }

    pub fn new_gesture_end(delta: f64, velocity: f64, forward: bool, params: SpringParams) -> Self {
        WorkspaceDelta::GestureEnd {
            start: Instant::now(),
            forward,
            spring: Spring {
                from: delta,
                to: 1.0,
                initial_velocity: velocity,
                params,
            },
        }
    }

    pub fn new_shortcut() -> Self {
        WorkspaceDelta::Shortcut(Instant::now())
    }

    pub fn new_crossfade() -> Self {
        WorkspaceDelta::Crossfade(Instant::now())
    }

    pub fn is_animating(&self) -> bool {
        matches!(
            self,
            WorkspaceDelta::Shortcut(_)
                | WorkspaceDelta::GestureEnd { .. }
                | WorkspaceDelta::Crossfade(_)
        )
    }
}

#[derive(Debug)]
pub struct WorkspaceSet {
    previously_active: Option<(usize, WorkspaceDelta)>,
    pub active: usize,
    pub group: WorkspaceGroupHandle,
    tiling_enabled: bool,
    output: Output,
    theme: crate::comp_theme::CompTheme,
    appearance: AppearanceConfig,
    pub sticky_layer: FloatingLayout,
    pub minimized_windows: Vec<MinimizedWindow>,
    pub workspaces: Vec<Workspace>,
}

fn create_workspace(
    state: &mut WorkspaceUpdateGuard<'_, State>,
    output: &Output,
    group_handle: &WorkspaceGroupHandle,
    active: bool,
    tiling: bool,
    theme: crate::comp_theme::CompTheme,
    appearance: AppearanceConfig,
) -> Workspace {
    let workspace_handle = state
        .create_workspace(
            group_handle,
            if tiling {
                TilingState::TilingEnabled
            } else {
                TilingState::FloatingOnly
            },
            // TODO Set id for persistent workspaces
            None,
        )
        .unwrap();
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        WorkspaceCapabilities::Activate
            | WorkspaceCapabilities::SetTilingState
            | WorkspaceCapabilities::Pin
            | WorkspaceCapabilities::Move,
    );
    Workspace::new(
        workspace_handle,
        output.clone(),
        tiling,
        theme.clone(),
        appearance,
    )
}

fn create_workspace_from_pinned(
    pinned: &PinnedWorkspace,
    state: &mut WorkspaceUpdateGuard<'_, State>,
    output: &Output,
    group_handle: &WorkspaceGroupHandle,
    active: bool,
    theme: crate::comp_theme::CompTheme,
    appearance: AppearanceConfig,
) -> Workspace {
    let workspace_handle = state
        .create_workspace(
            group_handle,
            if pinned.tiling_enabled {
                TilingState::TilingEnabled
            } else {
                TilingState::FloatingOnly
            },
            pinned.id.clone(),
        )
        .unwrap();
    state.add_workspace_state(&workspace_handle, WState::Pinned);
    if active {
        state.add_workspace_state(&workspace_handle, WState::Active);
    }
    state.set_workspace_capabilities(
        &workspace_handle,
        WorkspaceCapabilities::Activate
            | WorkspaceCapabilities::SetTilingState
            | WorkspaceCapabilities::Pin
            | WorkspaceCapabilities::Move,
    );

    if let Some(ref name) = pinned.name {
        state.set_workspace_name(&workspace_handle, name);
    }

    Workspace::from_pinned(
        pinned,
        workspace_handle,
        output.clone(),
        theme.clone(),
        appearance,
    )
}

/// Fold `workspace` into `into`, moving every window across and dropping the now-empty
/// workspace from the protocol.
///
/// Both workspaces must already sit on the same output: call [`Workspace::set_output`] on
/// `workspace` first, so the per-output toplevel bookkeeping is already correct by the time
/// we get here.
fn merge_workspaces(
    mut workspace: Workspace,
    into: &mut Workspace,
    workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    seats: &[Seat<State>],
) {
    // Re-parent the toplevel -> workspace association for everything we are about to move.
    // Collect first and de-duplicate: a maximized tiled window is mapped in both layers, and a
    // surface part-way through un-fullscreening is in `fullscreen_surfaces` as well as its
    // layer. `toplevel_enter_workspace` is a bare `Vec::push` with no dedup of its own, so
    // visiting a surface twice would emit `ext_workspace_enter` twice for it.
    let mut moving = Vec::new();
    let note = |toplevel: CosmicSurface, moving: &mut Vec<CosmicSurface>| {
        if !moving.contains(&toplevel) {
            moving.push(toplevel);
        }
    };
    for element in workspace.mapped() {
        for (toplevel, _) in element.windows() {
            note(toplevel, &mut moving);
        }
    }
    for minimized in &workspace.minimized_windows {
        for toplevel in minimized.windows() {
            note(toplevel, &mut moving);
        }
    }
    for fullscreen in &workspace.fullscreen_surfaces {
        note(fullscreen.surface.clone(), &mut moving);
    }
    for toplevel in &moving {
        toplevel_leave_workspace(toplevel, &workspace.handle);
        toplevel_enter_workspace(toplevel, &into.handle);
    }

    // Carry the per-seat focus history across. Without this the moved windows are absent from
    // `into`'s focus stack, and every lookup that resolves through it - `get_fullscreen` most
    // notably - stops finding them. `FocusStack::iter` yields newest-first and `append` pushes
    // onto the top, so walk it in reverse to preserve the relative order.
    for seat in seats {
        let previous = workspace
            .focus_stack
            .get(seat)
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        // Whatever held the top of `into`'s stack keeps it. `Workspace::render` gates the
        // fullscreen render path on `focus_stack.last()`, so letting the departing output's
        // most-recent window win would drop a live fullscreen - a game, in practice - behind
        // the windows we just merged in.
        let keep_on_top = into.focus_stack.get(seat).last().cloned();
        let mut stack = into.focus_stack.get_mut(seat);
        for target in previous.into_iter().rev() {
            stack.append(target);
        }
        if let Some(target) = keep_on_top {
            stack.append(target);
        }
    }

    into.minimized_windows
        .append(&mut workspace.minimized_windows);
    into.fullscreen_surfaces
        .append(&mut workspace.fullscreen_surfaces);
    into.tiling_layer.merge(workspace.tiling_layer);
    into.floating_layer.merge(workspace.floating_layer);
    workspace_state.remove_workspace(workspace.handle);
}

impl WorkspaceSet {
    fn new(
        state: &mut WorkspaceUpdateGuard<'_, State>,
        output: &Output,
        tiling_enabled: bool,
        theme: &crate::comp_theme::CompTheme,
        appearance: AppearanceConfig,
    ) -> WorkspaceSet {
        let group_handle = state.create_workspace_group();
        let sticky_layer = FloatingLayout::new(theme.clone(), appearance, output);

        WorkspaceSet {
            previously_active: None,
            active: 0,
            group: group_handle,
            tiling_enabled,
            theme: theme.clone(),
            sticky_layer,
            minimized_windows: Vec::new(),
            workspaces: Vec::new(),
            output: output.clone(),
            appearance,
        }
    }

    fn activate(
        &mut self,
        idx: usize,
        workspace_delta: WorkspaceDelta,
        state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<bool, InvalidWorkspaceIndex> {
        if idx >= self.workspaces.len() {
            return Err(InvalidWorkspaceIndex);
        }

        // Animate if workspaces overview isn't open
        let layer_map = layer_map_for_output(&self.output);
        let animate = !layer_map
            .layers()
            .any(|l| l.namespace() == WORKSPACE_OVERVIEW_NAMESPACE);

        if self.active != idx {
            let old_active = self.active;
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Active);
            state.remove_workspace_state(&self.workspaces[old_active].handle, WState::Urgent);
            state.remove_workspace_state(&self.workspaces[idx].handle, WState::Urgent);
            state.add_workspace_state(&self.workspaces[idx].handle, WState::Active);
            let dbg_crossfade = matches!(workspace_delta, WorkspaceDelta::Crossfade(_));
            self.previously_active = if animate {
                Some((old_active, workspace_delta))
            } else {
                None
            };
            self.active = idx;
            // Grey-slide anchor (bug 1): the moment a slide/crossfade to another
            // workspace begins. If the incoming workspace's game surface has no first
            // frame yet, this is when the empty grey workspace starts animating in.
            tracing::debug!(
                target: crate::logger::GAMING_TARGET,
                output = %self.output.name(),
                from = old_active,
                to = idx,
                crossfade = dbg_crossfade,
                animate,
                "workspace transition start"
            );
            Ok(true)
        } else {
            // snap to workspace, when in between workspaces due to swipe gesture
            if let Some((p_idx, p_delta)) = self.previously_active {
                if matches!(p_delta, WorkspaceDelta::Gesture { .. })
                    && matches!(workspace_delta, WorkspaceDelta::GestureEnd { .. })
                {
                    self.previously_active = Some((p_idx, workspace_delta));
                } else {
                    self.previously_active = None;
                }
                return Ok(true);
            }
            Ok(false)
        }
    }

    fn activate_previous(
        &mut self,
        workspace_delta: WorkspaceDelta,
        state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<bool, InvalidWorkspaceIndex> {
        if let Some((idx, _)) = self.previously_active {
            return self.activate(idx, workspace_delta, state);
        }
        Err(InvalidWorkspaceIndex)
    }

    fn update_workspace_delta(&mut self, delta: f64, forward: bool) {
        let easing = delta.clamp(0.0, GESTURE_MAX_LENGTH).abs() / GESTURE_MAX_LENGTH;
        if let Some((idx, _)) = self.previously_active {
            self.previously_active = Some((
                idx,
                WorkspaceDelta::Gesture {
                    percentage: easing,
                    forward,
                },
            ));
        }
    }

    fn set_output(&mut self, new_output: &Output, explicit: bool) {
        self.sticky_layer.set_output(new_output);
        for window in self.sticky_layer.windows() {
            toplevel_leave_output(&window, &self.output);
            toplevel_enter_output(&window, new_output);
        }
        for workspace in &mut self.workspaces {
            workspace.set_output(new_output, explicit);
        }
        self.output = new_output.clone();
    }

    fn refresh(&mut self) {
        if let Some((_, start)) = self.previously_active {
            match start {
                WorkspaceDelta::Shortcut(st) => {
                    if Instant::now().duration_since(st).as_millis() as f32
                        >= self.theme.motion.animation.as_millis() as f32
                    {
                        self.previously_active = None;
                    }
                }
                WorkspaceDelta::GestureEnd { start, spring, .. } => {
                    if Instant::now().duration_since(start).as_millis()
                        > spring.duration().as_millis()
                    {
                        self.previously_active = None;
                    }
                }
                WorkspaceDelta::Crossfade(st) => {
                    if Instant::now().duration_since(st).as_millis() as f32
                        >= self.theme.motion.slide_crossfade.as_millis() as f32
                    {
                        self.previously_active = None;
                    }
                }
                _ => {}
            }
        } else {
            self.workspaces[self.active].refresh();
        }
        self.sticky_layer.refresh();
    }

    fn add_empty_workspace(&mut self, state: &mut WorkspaceUpdateGuard<State>) {
        let workspace = create_workspace(
            state,
            &self.output,
            &self.group,
            false,
            self.tiling_enabled,
            self.theme.clone(),
            self.appearance,
        );
        workspace_set_idx(
            state,
            self.workspaces.len() as u8 + 1,
            &workspace.handle,
            workspace.name.as_deref(),
            // this method is only used by code paths related to dynamic workspaces, so this should be fine
        );
        self.workspaces.push(workspace);
    }

    fn ensure_last_empty(
        &mut self,
        state: &mut WorkspaceUpdateGuard<State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        // add empty at the end, if necessary
        if self
            .workspaces
            .last()
            .is_none_or(|last| !last.is_empty() || last.pinned)
        {
            self.add_empty_workspace(state);
        }

        // remove other empty workspaces
        let len = self.workspaces.len();
        let kept: Vec<bool> = self
            .workspaces
            .iter()
            .enumerate()
            .map(|(i, workspace)| {
                let previous_is_empty = i > 0
                    && self
                        .workspaces
                        .get(i - 1)
                        .is_some_and(|w| w.is_empty() && !w.pinned);
                let keep = if workspace.can_auto_remove(xdg_activation_state) {
                    // Keep empty workspace if it's active, or it's the last workspace,
                    // and the previous worspace is not both active and empty.
                    i == self.active
                        || (i == len - 1 && !(i == self.active + 1 && previous_is_empty))
                } else {
                    true
                };
                if !keep {
                    state.remove_workspace(workspace.handle);
                }
                keep
            })
            .collect();

        let mut iter = kept.iter();
        self.workspaces.retain(|_| *iter.next().unwrap());
        self.active -= kept
            .iter()
            .take(self.active + 1)
            .filter(|kept| !**kept)
            .count();

        if kept.iter().any(|val| !(*val)) {
            self.update_workspace_idxs(state);
        }
    }

    fn update_workspace_idxs(&self, state: &mut WorkspaceUpdateGuard<'_, State>) {
        for (i, workspace) in self.workspaces.iter().enumerate() {
            workspace_set_idx(
                state,
                i as u8 + 1,
                &workspace.handle,
                workspace.name.as_deref(),
            );
        }
    }

    fn post_remove_workspace(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        previous_active_handle: &WorkspaceHandle,
    ) {
        if self.workspaces.is_empty() {
            self.add_empty_workspace(workspace_state);
        }
        self.update_workspace_idxs(workspace_state);
        self.active = self
            .workspaces
            .iter()
            .position(|w| w.handle == *previous_active_handle)
            .unwrap_or_else(|| {
                let idx = self.workspaces.len() - 1;
                let workspace = &self.workspaces[idx];
                workspace_state.add_workspace_state(&workspace.handle, WState::Active);
                idx
            });
    }

    // Remove a workspace from the set, and return it, for adding to a different
    // workspace set
    fn remove_workspace(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        handle: &WorkspaceHandle,
    ) -> Option<Workspace> {
        let previous_active_handle = self.workspaces[self.active].handle;
        let idx = self.workspaces.iter().position(|w| w.handle == *handle)?;
        let workspace = self.workspaces.remove(idx);
        self.post_remove_workspace(workspace_state, &previous_active_handle);
        Some(workspace)
    }

    // Remove all workspaces matched by the callback from the set
    fn remove_workspaces(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        cb: impl Fn(&Workspace) -> bool,
    ) -> Vec<Workspace> {
        let previous_active_handle = self.workspaces[self.active].handle;
        let (prefers, doesnt) = self.workspaces.drain(..).partition(cb);
        self.workspaces = doesnt;
        self.post_remove_workspace(workspace_state, &previous_active_handle);
        prefers
    }

    pub fn surface_geometry_offset_from_toplevel(
        &self,
        surface: &WlSurface,
    ) -> Option<(Rectangle<i32, Local>, Point<i32, Logical>)> {
        let mut root = surface.clone();

        while let Some(parent) = get_parent(&root) {
            root = parent;
        }

        while smithay::wayland::compositor::get_role(&root) == Some(XDG_POPUP_ROLE) {
            let parent = with_states(&root, |states| {
                states
                    .data_map
                    .get::<XdgPopupSurfaceData>()
                    .and_then(|m| m.lock().unwrap().parent.as_ref().cloned())
            });
            if let Some(parent) = parent {
                root = parent;
            } else {
                break;
            }
        }

        self.sticky_layer
            .mapped()
            .find(|w| {
                w.windows()
                    .any(|(w, _)| w.wl_surface().as_deref() == Some(&root))
            })
            .and_then(|w| {
                self.sticky_layer
                    .element_geometry(w)
                    .zip(w.surface_offset(surface))
            })
            .or_else(|| {
                self.workspaces.iter().find_map(|workspace| {
                    workspace
                        .get_fullscreen_surfaces()
                        .find_map(|fs| {
                            (fs.surface.wl_surface().as_deref() == Some(&root))
                                .then(|| {
                                    fs.surface.surface_offset(surface).map(|offset| {
                                        (workspace.fullscreen_geometry_for(fs), offset)
                                    })
                                })
                                .flatten()
                        })
                        .or_else(|| {
                            workspace.mapped().find_map(|w| {
                                w.windows()
                                    .any(|(w, _)| w.wl_surface().as_deref() == Some(&root))
                                    .then(|| {
                                        workspace.element_geometry(w).zip(w.surface_offset(surface))
                                    })
                                    .flatten()
                            })
                        })
                })
            })
            .or_else(|| {
                layer_map_for_output(&self.output).layers().find_map(|l| {
                    (l.wl_surface() == &root)
                        .then(|| {
                            CosmicSurface::surface_tree_offset(l.wl_surface(), surface)
                                .map(|offset| (l.geometry().as_local(), offset))
                        })
                        .flatten()
                })
            })
    }
}

#[derive(Debug)]
pub struct Workspaces {
    pub sets: IndexMap<Output, WorkspaceSet>,
    backup_set: Option<WorkspaceSet>,
    pub layout: WorkspaceLayout,
    mode: WorkspaceMode,
    tiling_enabled: bool,
    autotile: bool,
    autotile_behavior: TileBehavior,
    theme: crate::comp_theme::CompTheme,
    appearance: AppearanceConfig,
    // Persisted workspace to add on first `output_add`
    persisted_workspaces: Vec<PinnedWorkspace>,
}

impl Workspaces {
    pub fn new(config: &Config, theme: crate::comp_theme::CompTheme) -> Workspaces {
        Workspaces {
            sets: IndexMap::new(),
            backup_set: None,
            layout: config.cosmic_conf.workspaces.workspace_layout,
            mode: config.cosmic_conf.workspaces.workspace_mode,
            tiling_enabled: config.cosmic_conf.tiling_enabled,
            autotile: config.cosmic_conf.autotile,
            autotile_behavior: config.cosmic_conf.autotile_behavior,
            theme,
            appearance: config.cosmic_conf.appearance_settings,
            persisted_workspaces: config.cosmic_conf.pinned_workspaces.clone(),
        }
    }

    pub fn add_output(
        &mut self,
        output: &Output,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        if self.sets.contains_key(output) {
            return;
        }

        let mut set = self
            .backup_set
            .take()
            .map(|mut set| {
                set.set_output(output, false);
                set
            })
            .unwrap_or_else(|| {
                WorkspaceSet::new(
                    workspace_state,
                    output,
                    self.effective_autotile(),
                    &self.theme,
                    self.appearance,
                )
            });
        workspace_state.add_group_output(&set.group, output);

        // If this is the first output added, create workspaces for pinned workspaces from config
        for pinned in std::mem::take(&mut self.persisted_workspaces) {
            let workspace = create_workspace_from_pinned(
                &pinned,
                workspace_state,
                output,
                &set.group,
                false,
                self.theme.clone(),
                self.appearance,
            );
            set.workspaces.push(workspace);
        }

        // Remove workspaces that prefer this output from other sets
        let mut moved_workspaces = self
            .sets
            .values_mut()
            .flat_map(|other_set| {
                other_set.remove_workspaces(workspace_state, |w| w.prefers_output(output))
            })
            .collect::<Vec<_>>();

        // Add `moved_workspaces` to set, and update output and index of workspaces
        for workspace in &mut moved_workspaces {
            workspace_state.remove_workspace_state(&workspace.handle, WState::Active);
            workspace_state.move_workspace_to_group(set.group, workspace.handle);
        }
        set.workspaces.extend(moved_workspaces);
        if set.workspaces.is_empty() {
            set.add_empty_workspace(workspace_state);
        }
        set.update_workspace_idxs(workspace_state);
        for (i, workspace) in set.workspaces.iter_mut().enumerate() {
            workspace.set_output(output, false);
            workspace.refresh();
            if i == set.active {
                workspace_state.add_workspace_state(&workspace.handle, WState::Active);
            }
        }
        self.sets.insert(output.clone(), set);
    }

    pub fn remove_output<'a>(
        &mut self,
        output: &Output,
        seats: impl Iterator<Item = &'a Seat<State>>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        if !self.sets.contains_key(output) {
            return;
        }

        let seats = seats.cloned().collect::<Vec<_>>();

        if let Some(set) = self.sets.shift_remove(output) {
            {
                let map = layer_map_for_output(output);
                for surface in map.layers() {
                    surface.layer_surface().send_close();
                }
            }

            // TODO: Heuristic which output to move to.
            // It is supposed to be the *most* internal, we just pick the first one for now
            // and hope enumeration order works in our favor.
            let new_output = self.sets.get_index(0).map(|(o, _)| o.clone());
            if let Some(new_output) = new_output {
                for seat in &seats {
                    if &seat.active_output() == output {
                        seat.set_active_output(&new_output);
                    }
                    if seat.focused_output().as_ref() == Some(output) {
                        seat.set_focused_output(None);
                    }
                }

                let new_set = self.sets.get_mut(&new_output).unwrap();
                let workspace_group = new_set.group;
                for (i, mut workspace) in set.workspaces.into_iter().enumerate() {
                    if workspace.can_auto_remove(xdg_activation_state) {
                        workspace_state.remove_workspace(workspace.handle);
                    } else if !workspaces_enabled() && !new_set.workspaces.is_empty() {
                        // Workspaces are disabled for this product (no `cosmic-workspaces`
                        // installed), so unplugging an output must not leave a second
                        // window-bearing workspace behind on the surviving one. Fold the
                        // windows into its active workspace instead of appending the whole
                        // workspace, which is what used to strand the user on "workspace 2"
                        // after a monitor round-trip.
                        workspace.set_output(&new_output, false);
                        let target = new_set.active.min(new_set.workspaces.len() - 1);
                        merge_workspaces(
                            workspace,
                            &mut new_set.workspaces[target],
                            workspace_state,
                            &seats,
                        );
                        new_set.workspaces[target].refresh();
                    } else {
                        // update workspace protocol state
                        workspace_state.remove_workspace_state(&workspace.handle, WState::Active);
                        workspace_state.move_workspace_to_group(workspace_group, workspace.handle);

                        // update mapping
                        workspace.set_output(&new_output, false);
                        workspace.refresh();
                        new_set.workspaces.push(workspace);

                        // If workspace was active, and the new set's active workspace is empty, make this workspace
                        // active on the new set. Instead of leaving an empty workspace active, and a previously active
                        // workspace hidden.
                        if i == set.active && new_set.workspaces[new_set.active].is_empty() {
                            workspace_state.remove_workspace_state(
                                &new_set.workspaces[new_set.active].handle,
                                WState::Active,
                            );
                            new_set.active = new_set.workspaces.len() - 1;
                            workspace_state.add_workspace_state(
                                &new_set.workspaces[new_set.active].handle,
                                WState::Active,
                            );
                        }
                    }
                }

                for window in set.sticky_layer.mapped() {
                    for (surface, _) in window.windows() {
                        toplevel_leave_output(&surface, output);
                        toplevel_enter_output(&surface, &new_output);
                    }
                }
                new_set.sticky_layer.merge(set.sticky_layer);
                for window in set.minimized_windows.iter() {
                    for surface in window.windows() {
                        toplevel_leave_output(&surface, output);
                        toplevel_enter_output(&surface, &new_output);
                    }
                }
                new_set.minimized_windows.extend(set.minimized_windows);

                if self.mode == WorkspaceMode::OutputBound {
                    workspace_state.remove_workspace_group(set.group);
                } else {
                    workspace_state.remove_group_output(&workspace_group, output);
                }
            } else {
                workspace_state.remove_group_output(&set.group, output);
                self.backup_set = Some(set);
            }

            self.refresh(workspace_state, xdg_activation_state)
        }
    }

    // Move workspace from one output to another, explicitly by the user
    pub fn migrate_workspace(
        &mut self,
        from: &Output,
        to: &Output,
        handle: &WorkspaceHandle,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        // If workspaces span across outputs, they can't be moved to a different output
        if self.mode == WorkspaceMode::Global {
            return;
        }

        if !self.sets.contains_key(to) || from == to {
            return;
        }

        if let Some(mut workspace) = self
            .sets
            .get_mut(from)
            .and_then(|set| set.remove_workspace(workspace_state, handle))
        {
            let new_set = self.sets.get_mut(to).unwrap();
            workspace_state.remove_workspace_state(&workspace.handle, WState::Active);
            workspace_state.move_workspace_to_group(new_set.group, workspace.handle);
            workspace.set_output(to, true);
            workspace.refresh();
            new_set.workspaces.insert(new_set.active + 1, workspace);
            new_set.update_workspace_idxs(workspace_state);
        }
    }

    // Move a workspace before/after a different workspace
    pub fn move_workspace(
        &mut self,
        handle: &WorkspaceHandle,
        other_handle: &WorkspaceHandle,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        after: bool,
    ) {
        if handle == other_handle {
            return;
        }

        let (Some(old_output), Some(new_output)) = (
            self.space_for_handle(handle).map(|w| w.output.clone()),
            self.space_for_handle(other_handle)
                .map(|w| w.output.clone()),
        ) else {
            return;
        };

        match self.mode {
            WorkspaceMode::OutputBound => {
                // Check which workspace is active on the new set; before removing from the
                // old set in cause we're moving an active workspace within the same set.
                let new_set = &mut self.sets[&new_output];
                let previous_active_handle = new_set.workspaces[new_set.active].handle;

                // Remove workspace from old set
                let old_set = &mut self.sets[&old_output];
                let mut workspace = if new_output != old_output {
                    old_set.remove_workspace(workspace_state, handle).unwrap()
                } else {
                    // If set is the same, just remove it here without adding empty workspace,
                    // updating `active`, etc.
                    let idx = old_set
                        .workspaces
                        .iter()
                        .position(|w| w.handle == *handle)
                        .unwrap();
                    old_set.workspaces.remove(idx)
                };

                let new_set = &mut self.sets[&new_output];

                if new_output != old_output {
                    workspace_state.remove_workspace_state(&workspace.handle, WState::Active);
                    workspace_state.move_workspace_to_group(new_set.group, workspace.handle);
                    workspace.set_output(&new_output, true);
                    workspace.refresh();
                }

                // Insert workspace into new set, relative to `other_handle`
                let idx = new_set
                    .workspaces
                    .iter()
                    .position(|w| w.handle == *other_handle)
                    .unwrap();
                let insert_idx = if after { idx + 1 } else { idx };
                new_set.workspaces.insert(insert_idx, workspace);

                new_set.active = new_set
                    .workspaces
                    .iter()
                    .position(|w| w.handle == previous_active_handle)
                    .unwrap();

                new_set.update_workspace_idxs(workspace_state);
            }
            WorkspaceMode::Global => {
                let old_set = &mut self.sets[&old_output];
                let old_idx = old_set
                    .workspaces
                    .iter()
                    .position(|w| w.handle == *handle)
                    .unwrap();

                let new_set = &mut self.sets[&new_output];
                let other_idx = new_set
                    .workspaces
                    .iter()
                    .position(|w| w.handle == *other_handle)
                    .unwrap();

                // Move workspace at given index on every output
                for set in self.sets.values_mut() {
                    if old_idx < set.workspaces.len() && other_idx < set.workspaces.len() {
                        let previous_active_handle = set.workspaces[set.active].handle;

                        if other_idx > old_idx {
                            let insert_idx = if after { other_idx } else { other_idx - 1 };
                            set.workspaces[old_idx..=insert_idx].rotate_left(1);
                        } else {
                            let insert_idx = if after { other_idx + 1 } else { other_idx };
                            set.workspaces[insert_idx..=old_idx].rotate_right(1);
                        }

                        set.active = set
                            .workspaces
                            .iter()
                            .position(|w| w.handle == previous_active_handle)
                            .unwrap();

                        set.update_workspace_idxs(workspace_state);
                    }
                }
            }
        }
    }

    pub fn update_config(
        &mut self,
        config: &Config,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        let old_mode = self.mode;
        self.mode = config.cosmic_conf.workspaces.workspace_mode;
        self.layout = config.cosmic_conf.workspaces.workspace_layout;
        self.appearance = config.cosmic_conf.appearance_settings;

        for set in self.sets.values_mut() {
            set.appearance = self.appearance;
            set.sticky_layer.appearance = self.appearance;
            for workspace in set.workspaces.iter_mut() {
                workspace.floating_layer.appearance = self.appearance;
                workspace.tiling_layer.appearance = self.appearance;
            }
        }

        if self.sets.len() <= 1 {
            return;
        }

        match (old_mode, self.mode) {
            (WorkspaceMode::Global, WorkspaceMode::OutputBound) => {
                // We basically just unlink the existing spaces, so nothing needs to be updated
            }
            (WorkspaceMode::OutputBound, WorkspaceMode::Global) => {
                // lets construct an iterator of all the pairs of workspaces we have to "merge"
                let mut pairs = Vec::new();
                if let Some(max) = self.sets.values().map(|set| set.workspaces.len()).max() {
                    let offset = self.sets.values().map(|set| set.active).max().unwrap();
                    for i in 0..max {
                        pairs.push(
                            self.sets
                                .values()
                                .map(|set| {
                                    let idx = set.active as isize + i as isize - offset as isize;
                                    if idx < 0 || idx >= set.workspaces.len() as isize {
                                        None
                                    } else {
                                        Some(idx)
                                    }
                                })
                                .collect::<Vec<_>>(),
                        );
                    }
                }

                for (j, pair) in pairs.iter().enumerate() {
                    for (i, x) in pair.iter().enumerate() {
                        // Fill up sets, where necessary
                        if x.is_none() {
                            // create missing workspace
                            let (output, set) = self.sets.get_index_mut(i).unwrap();
                            set.workspaces.insert(
                                j,
                                create_workspace(
                                    workspace_state,
                                    output,
                                    &set.group,
                                    false,
                                    config.cosmic_conf.autotile,
                                    self.theme.clone(),
                                    self.appearance,
                                ),
                            );
                        }
                        // Otherwise we are fine
                    }
                }
            }
            _ => {}
        };

        self.refresh(workspace_state, xdg_activation_state)
    }

    pub fn recalculate(&mut self) {
        for set in self.sets.values_mut() {
            set.sticky_layer.recalculate();
            set.workspaces.iter_mut().for_each(|w| w.recalculate());
        }
    }

    /// Recalculate only the workspace set of a single output. Used by the
    /// per-frame slide animation path to avoid relayouting unrelated outputs.
    pub fn recalculate_output(&mut self, output: &Output) {
        if let Some(set) = self.sets.get_mut(output) {
            set.sticky_layer.recalculate();
            set.workspaces.iter_mut().for_each(|w| w.recalculate());
        }
    }

    pub fn refresh(
        &mut self,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        xdg_activation_state: &XdgActivationState,
    ) {
        match self.mode {
            WorkspaceMode::Global => {
                let Some(max) = self.sets.values().map(|set| set.workspaces.len()).max() else {
                    return;
                };

                for set in self
                    .sets
                    .values_mut()
                    .filter(|set| set.workspaces.len() < max)
                {
                    while set.workspaces.len() < max {
                        set.add_empty_workspace(workspace_state)
                    }
                }

                // add empty at the end, if necessary
                if self
                    .sets
                    .values()
                    .flat_map(|set| set.workspaces.last())
                    .any(|w| !w.is_empty() || w.pinned)
                {
                    for set in self.sets.values_mut() {
                        set.add_empty_workspace(workspace_state);
                    }
                }

                // remove empty workspaces in between, if they are not active
                let len = self.sets[0].workspaces.len();
                let mut active = self.sets[0].active;
                let mut keep = vec![true; len];
                // false-positive: we iterate over multiple sets
                #[allow(clippy::needless_range_loop)]
                for i in 0..len {
                    let has_windows = self
                        .sets
                        .values()
                        .any(|s| !s.workspaces[i].can_auto_remove(xdg_activation_state));

                    if !has_windows && i != active && i != len - 1 {
                        for workspace in self.sets.values().map(|s| &s.workspaces[i]) {
                            workspace_state.remove_workspace(workspace.handle);
                        }
                        keep[i] = false;
                    }
                }

                self.sets.values_mut().for_each(|s| {
                    let mut iter = keep.iter();
                    s.workspaces.retain(|_| *iter.next().unwrap());
                });
                active -= keep.iter().take(active + 1).filter(|keep| !**keep).count();
                self.sets.values_mut().for_each(|s| {
                    s.active = active;
                });

                if keep.iter().any(|val| !(*val)) {
                    for set in self.sets.values_mut() {
                        set.update_workspace_idxs(workspace_state);
                    }
                }
            }
            WorkspaceMode::OutputBound => {
                for set in self.sets.values_mut() {
                    set.ensure_last_empty(workspace_state, xdg_activation_state);
                }
            }
        }

        for set in self.sets.values_mut() {
            set.refresh()
        }
    }

    pub fn get(&self, num: usize, output: &Output) -> Option<&Workspace> {
        self.sets
            .get(output)
            .and_then(|set| set.workspaces.get(num))
    }

    pub fn get_mut(&mut self, num: usize, output: &Output) -> Option<&mut Workspace> {
        self.sets
            .get_mut(output)
            .and_then(|set| set.workspaces.get_mut(num))
    }

    pub fn active(
        &self,
        output: &Output,
    ) -> Option<(Option<(&Workspace, WorkspaceDelta)>, &Workspace)> {
        self.sets
            .get(output)
            .or(self.backup_set.as_ref())
            .map(|set| {
                (
                    set.previously_active.and_then(|(idx, start)| {
                        set.workspaces.get(idx).map(|previous| (previous, start))
                    }),
                    &set.workspaces[set.active],
                )
            })
    }

    pub fn active_mut(&mut self, output: &Output) -> Option<&mut Workspace> {
        self.sets
            .get_mut(output)
            .or(self.backup_set.as_mut())
            .map(|set| &mut set.workspaces[set.active])
    }

    pub fn active_num(&self, output: &Output) -> (Option<usize>, usize) {
        let set = self.sets.get(output).or(self.backup_set.as_ref()).unwrap();
        (set.previously_active.map(|(idx, _)| idx), set.active)
    }

    pub fn idx_for_handle(&self, output: &Output, handle: &WorkspaceHandle) -> Option<usize> {
        let set = self.sets.get(output).unwrap();
        set.workspaces
            .iter()
            .enumerate()
            .find_map(|(i, w)| (&w.handle == handle).then_some(i))
    }

    pub fn len(&self, output: &Output) -> usize {
        let set = self.sets.get(output).unwrap();
        set.workspaces.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Output, &WorkspaceSet)> {
        self.sets.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&Output, &mut WorkspaceSet)> {
        self.sets.iter_mut()
    }

    pub fn spaces(&self) -> impl Iterator<Item = &Workspace> {
        self.sets.values().flat_map(|set| set.workspaces.iter())
    }

    pub fn space_for_handle(&self, handle: &WorkspaceHandle) -> Option<&Workspace> {
        self.spaces().find(|w| &w.handle == handle)
    }

    pub fn space_for_handle_mut(&mut self, handle: &WorkspaceHandle) -> Option<&mut Workspace> {
        self.spaces_mut().find(|w| &w.handle == handle)
    }

    pub fn spaces_for_output(&self, output: &Output) -> impl Iterator<Item = &Workspace> {
        self.sets
            .get(output)
            .into_iter()
            .flat_map(|set| set.workspaces.iter())
    }

    pub fn spaces_mut(&mut self) -> impl Iterator<Item = &mut Workspace> {
        Box::new(
            self.sets
                .values_mut()
                .flat_map(|set| set.workspaces.iter_mut()),
        )
    }

    pub fn set_theme(&mut self, theme: crate::comp_theme::CompTheme) {
        for (_, s) in &mut self.sets {
            s.theme = theme.clone();

            s.sticky_layer.theme = theme.clone();
            s.sticky_layer.mapped().for_each(|m| {
                m.update_theme(theme.clone());
            });

            for w in &mut s.workspaces {
                w.tiling_layer.theme = theme.clone();
                w.floating_layer.theme = theme.clone();

                w.mapped().for_each(|m| {
                    m.update_theme(theme.clone());
                });
            }
        }

        self.force_redraw();
    }

    pub fn force_redraw(&mut self) {
        for (_, s) in &mut self.sets {
            s.sticky_layer.mapped().for_each(|m| {
                m.force_redraw();
            });
            s.sticky_layer.refresh();

            for w in &mut s.workspaces {
                w.mapped().for_each(|m| {
                    m.force_redraw();
                });

                w.refresh();
                w.dirty.store(true, Ordering::Relaxed);
                w.recalculate();
            }
        }
    }

    pub fn update_autotile_behavior<'a>(
        &mut self,
        behavior: TileBehavior,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        self.autotile_behavior = behavior;
        self.apply_tile_change(guard, seats);
    }

    /// The effective tiling state: autotile is only active when the global
    /// tiling feature is enabled.
    fn effective_autotile(&self) -> bool {
        self.tiling_enabled && self.autotile
    }

    fn apply_tile_change<'a>(
        &mut self,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        let effective = self.effective_autotile();
        let seats = seats.cloned().collect::<Vec<_>>();
        for (_, set) in &mut self.sets {
            set.tiling_enabled = effective;

            if matches!(self.autotile_behavior, TileBehavior::Global) {
                // must apply change to all workspaces now
                for w in &mut set.workspaces {
                    if w.tiling_enabled == effective {
                        continue;
                    }
                    for s in &seats {
                        w.toggle_tiling(s, guard);
                    }
                }
            }
        }
    }

    pub fn update_autotile<'a>(
        &mut self,
        autotile: bool,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        self.autotile = autotile;
        self.apply_tile_change(guard, seats);
    }

    pub fn update_tiling_enabled<'a>(
        &mut self,
        enabled: bool,
        guard: &mut WorkspaceUpdateGuard<'_, State>,
        seats: impl Iterator<Item = &'a Seat<State>>,
    ) {
        self.tiling_enabled = enabled;
        self.apply_tile_change(guard, seats);
    }

    pub fn persist(&self, config: &Config) {
        let pinned_workspaces: Vec<PinnedWorkspace> = self
            .sets
            .values()
            .flat_map(|set| &set.workspaces)
            .flat_map(|w| w.to_pinned())
            .collect();
        let config = config.cosmic_helper.clone();
        thread::spawn(move || {
            if let Err(err) = config.set("pinned_workspaces", pinned_workspaces) {
                error!(?err, "Failed to update pinned_workspaces key");
            }
        });
    }
}

#[derive(Debug)]
pub struct InvalidWorkspaceIndex;

utils::id_gen!(next_output_id, OUTPUT_ID, OUTPUT_IDS);
pub struct OutputId(usize);

impl OutputId {
    pub fn namespace_for_workspace(&self, idx: usize) -> usize {
        self.0 | (idx << 32)
    }
}

impl Drop for OutputId {
    fn drop(&mut self) {
        OUTPUT_IDS.lock().unwrap().remove(&self.0);
    }
}

impl Common {
    pub fn add_output(&mut self, output: &Output) {
        let mut shell = self.shell.write();
        shell
            .workspaces
            .add_output(output, &mut self.workspace_state.update());

        output
            .user_data()
            .insert_if_missing_threadsafe(|| OutputId(next_output_id()));

        if let Some(state) = shell.zoom_state.as_ref() {
            output.user_data().insert_if_missing_threadsafe(|| {
                Mutex::new(OutputZoomState::new(
                    &state.seat,
                    output,
                    1.0,
                    state.increment,
                    state.movement,
                    self.event_loop_handle.clone(),
                    shell.theme.clone(),
                ))
            });
        }

        std::mem::drop(shell);
        self.refresh(); // fixes indicies of any moved workspaces
    }

    pub fn remove_output(&mut self, output: &Output) {
        let mut shell = self.shell.write();
        let shell_ref = &mut *shell;
        shell_ref.workspaces.remove_output(
            output,
            shell_ref.seats.iter(),
            &mut self.workspace_state.update(),
            &self.xdg_activation_state,
        );

        std::mem::drop(shell);

        // MERGE: dropped the per-output blur-cache purge (clear_cached_layer_surfaces /
        // clear_blur_textures_for_output / clear_layer_blur_textures_for_output). Those
        // lived in our `backend::render::blur` module, which upstream's frosted-glass
        // implementation replaces — it blits from the live framebuffer per frame and
        // keeps no per-output texture cache, so there is nothing left to purge here.

        self.refresh(); // cleans up excess of workspaces and empty workspaces
    }

    pub fn update_config(&mut self) {
        let mut shell = self.shell.write();
        let shell_ref = &mut *shell;
        shell_ref.active_hint = self.config.cosmic_conf.active_hint;
        shell_ref.appearance_conf = self.config.cosmic_conf.appearance_settings;
        if let Some(zoom_state) = shell_ref.zoom_state.as_mut() {
            zoom_state.increment = self.config.cosmic_conf.accessibility_zoom.increment;
            zoom_state.movement = self.config.cosmic_conf.accessibility_zoom.view_moves;
            zoom_state.show_overlay = self.config.cosmic_conf.accessibility_zoom.show_overlay;

            for output in shell_ref.workspaces.sets.keys() {
                let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
                let mut output_state_ref = output_state.lock().unwrap();
                let level = output_state_ref.level;
                output_state_ref.update(level, false, zoom_state.movement, zoom_state.increment);
            }
        }

        let mut workspace_state = self.workspace_state.update();
        shell_ref.workspaces.update_config(
            &self.config,
            &mut workspace_state,
            &self.xdg_activation_state,
        );

        for mapped in shell_ref.mapped() {
            mapped.update_appearance_conf(&self.config.cosmic_conf.appearance_settings);
        }
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.xdg_activation_state
            .retain_tokens(|_, data| data.timestamp.elapsed() < ACTIVATION_TOKEN_EXPIRE_TIME);
        self.shell.write().refresh(
            &self.xdg_activation_state,
            &mut self.workspace_state.update(),
        );
        self.popups.cleanup();
        self.toplevel_info_state.refresh(&self.workspace_state);
        self.refresh_idle_inhibit();
        if let Some(mut a11y_keyboard_monitor) = self.dbus_state.a11y_keyboard_monitor() {
            a11y_keyboard_monitor.refresh();
        }
        self.image_copy_capture_state.cleanup();
    }

    pub fn refresh_idle_inhibit(&mut self) {
        self.idle_inhibiting_surfaces.retain(|s| s.alive());

        let is_inhibited = self.idle_inhibiting_surfaces.iter().any(|surface| {
            with_states(surface, |states| {
                surface_primary_scanout_output(surface, states).is_some()
            })
        });
        self.idle_notifier_state.set_is_inhibited(is_inhibited);
    }

    #[profiling::function]
    pub fn on_commit(&mut self, surface: &WlSurface) {
        {
            let shell = self.shell.read();

            for seat in shell.seats.iter() {
                if let Some(move_grab) = seat.user_data().get::<SeatMoveGrabState>()
                    && let Some(grab_state) = move_grab.lock().unwrap().as_ref()
                {
                    let mapped = grab_state.element();
                    if mapped.active_window().wl_surface().as_deref() == Some(surface) {
                        mapped.on_commit(surface);
                    }
                }

                data_device::on_commit(surface, seat);
            }

            let is_cursor_image = shell.seats.iter().any(|seat| {
                    matches!(seat.cursor_image_status(), CursorImageStatus::Surface(ref cursor_surface) if cursor_surface == surface)
            });

            if is_cursor_image {
                with_states(surface, |states| {
                    let cursor_image_attributes = states.data_map.get::<CursorImageSurfaceData>();

                    if let Some(mut cursor_image_attributes) =
                        cursor_image_attributes.map(|attrs| attrs.lock().unwrap())
                    {
                        let buffer_delta = states
                            .cached_state
                            .get::<SurfaceAttributes>()
                            .current()
                            .buffer_delta
                            .take();
                        if let Some(buffer_delta) = buffer_delta {
                            cursor_image_attributes.hotspot -= buffer_delta;
                        }
                    }
                });
            }

            if let Some(mapped) = shell.element_for_surface(surface) {
                mapped.on_commit(surface);

                // Check if this is an embedded surface with animation sync active
                let surface_id = surface.id().to_string();
                if crate::wayland::handlers::surface_embed::has_embed_animation_sync(&surface_id) {
                    // Get the committed buffer size from the window geometry
                    let committed_size = mapped.active_window().geometry().size;
                    crate::wayland::handlers::surface_embed::record_embed_commit(
                        &surface_id,
                        committed_size,
                    );
                }
            }
            if let Some(fs) = shell
                .workspaces
                .spaces()
                .flat_map(|w| w.get_fullscreen_surfaces())
                .find(|f| f.surface == *surface)
            {
                fs.surface.on_commit()
            };
        }
        self.popups.commit(surface);
    }
}

impl Shell {
    pub fn new(config: &Config) -> Self {
        let theme = crate::comp_theme::CompTheme::from_current();

        let tiling_exceptions = layout::TilingExceptions::new(config.tiling_exceptions.iter());

        Shell {
            workspaces: Workspaces::new(config, theme.clone()),
            seats: Seats::new(),

            pending_windows: Vec::new(),
            pending_layers: Vec::new(),
            pending_activations: HashMap::new(),
            override_redirect_windows: Vec::new(),
            session_lock: None,
            previous_workspace_idx: None,
            xwayland_keyboard_grab: None,

            theme,
            active_hint: config.cosmic_conf.active_hint,
            overview_mode: OverviewMode::None,
            swap_indicator: None,
            perf_badge: None,
            resize_mode: ResizeMode::None,
            resize_state: None,
            resize_indicator: None,
            appearance_conf: config.cosmic_conf.appearance_settings,
            zoom_state: None,
            game_mode: GameMode::default(),
            tearing_allowed: false,
            game_mode_fps_limit: 0,
            game_mode_vrr: crate::dbus::game_mode::VrrMode::Auto,
            game_mode_frametime_ns: std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0)),
            game_mode_tearing_supported: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(
                false,
            )),
            game_mode_scaling: (0, 0, crate::dbus::game_mode::ScalingMode::Native),
            game_mode_scale_rejected: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(
                false,
            )),
            tiling_exceptions,
            // Start in home mode only if HOME_ENABLED is set
            home_mode: if home_enabled() {
                HomeMode::Active
            } else {
                HomeMode::None
            },
            home_only_surfaces: std::collections::HashSet::new(),
            hide_on_home_surfaces: std::collections::HashSet::new(),
            hidden_surfaces: std::collections::HashSet::new(),
            client_exclusive_zones: std::collections::HashMap::new(),
            home_minimized_surfaces: Vec::new(),

            // Voice mode state
            voice_mode: VoiceMode::None,
            voice_orb_state: Default::default(),

            // Layer surface fade-in tracking
            layer_fade_in: std::collections::HashMap::new(),
            pending_layer_fade_in: std::collections::HashSet::new(),
            layer_fade_out: std::collections::HashMap::new(),

            // Layer surfaces that follow the cursor to whichever output
            output_agnostic_layers: std::collections::HashSet::new(),

            // Exclusive keyboard focus already granted (avoid re-granting every frame)
            exclusive_focus_granted: std::collections::HashSet::new(),

            // Compositor-driven auto-hide surfaces
            auto_hide_surfaces: Vec::new(),

            // Layer slide animations (visibility-protocol triggered)
            layer_slides: Vec::new(),

            // No interactive side-panel resize in progress
            active_layer_resize: None,
            layer_resize_settle: None,
            active_layer_resize_anim: None,
            edge_hover: None,
            edge_drag_ghost: None,
            layer_maximize: None,

            // Fade+rise open/close animations (the default layer transition)
            layer_opens: Vec::new(),
            pending_layer_opens: std::collections::HashSet::new(),
            layer_closes: Vec::new(),
            rise_surfaces: std::collections::HashSet::new(),

            // Per-surface show/hide transition overrides
            layer_transitions: std::collections::HashMap::new(),

            // Original X11 geometry at map time
            original_x11_positions: HashMap::new(),

            #[cfg(feature = "debug")]
            debug_active: false,
        }
    }

    pub fn activate(
        &mut self,
        output: &Output,
        idx: usize,
        workspace_delta: WorkspaceDelta,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<Point<i32, Global>, InvalidWorkspaceIndex> {
        let result = match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    if matches!(
                        self.overview_mode.active_trigger(),
                        Some(Trigger::Pointer(_) | Trigger::Touch(_))
                    ) {
                        set.workspaces[set.active].tiling_layer.cleanup_drag();
                    }
                    set.activate(idx, workspace_delta, workspace_state)?;

                    let output_geo = output.geometry();
                    Ok(
                        output_geo.loc
                            + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)),
                    )
                } else {
                    Err(InvalidWorkspaceIndex)
                }
            }
            WorkspaceMode::Global => {
                for set in self.workspaces.sets.values_mut() {
                    set.activate(idx, workspace_delta, workspace_state)?;
                }
                let output_geo = output.geometry();
                Ok(output_geo.loc + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)))
            }
        };

        // Re-evaluate auto-hide state after workspace switch — the new
        // workspace may or may not have maximized/fullscreen windows.
        if result.is_ok() {
            self.refresh_auto_hide();
        }

        result
    }

    pub fn update_workspace_delta(&mut self, output: &Output, delta: f64, forward: bool) {
        match &mut self.workspaces.mode {
            WorkspaceMode::OutputBound => {
                if let Some(set) = self.workspaces.sets.get_mut(output) {
                    set.update_workspace_delta(delta, forward);
                }
            }
            WorkspaceMode::Global => {
                for set in self.workspaces.sets.values_mut() {
                    set.update_workspace_delta(delta, forward);
                }
            }
        }
    }

    pub fn end_workspace_swipe(
        &mut self,
        output: &Output,
        velocity: f64,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Result<Point<i32, Global>, InvalidWorkspaceIndex> {
        // Snapshot the theme's window spring for the velocity-seeded release.
        let window_spring = self.theme.motion.window_spring;
        let result =
            match &mut self.workspaces.mode {
                WorkspaceMode::OutputBound => {
                    if let Some(set) = self.workspaces.sets.get_mut(output) {
                        if matches!(
                            self.overview_mode.active_trigger(),
                            Some(Trigger::Pointer(_) | Trigger::Touch(_))
                        ) {
                            set.workspaces[set.active].tiling_layer.cleanup_drag();
                        }
                        if let Some((
                            _,
                            WorkspaceDelta::Gesture {
                                percentage: delta,
                                forward,
                            },
                        )) = set.previously_active
                        {
                            if (velocity > 0.0 && velocity.abs() >= GESTURE_VELOCITY_THRESHOLD)
                                || (velocity.abs() < GESTURE_VELOCITY_THRESHOLD
                                    && delta.abs() > GESTURE_POSITION_THRESHOLD)
                            {
                                set.activate(
                                    set.active,
                                    WorkspaceDelta::new_gesture_end(
                                        delta.abs(),
                                        velocity.abs(),
                                        forward,
                                        window_spring,
                                    ),
                                    workspace_state,
                                )?;
                            } else {
                                set.activate_previous(
                                    WorkspaceDelta::new_gesture_end(
                                        1.0 - delta.abs(),
                                        velocity.abs(),
                                        !forward,
                                        window_spring,
                                    ),
                                    workspace_state,
                                )?;
                            }
                        }

                        let output_geo = output.geometry();
                        Ok(output_geo.loc
                            + Point::from((output_geo.size.w / 2, output_geo.size.h / 2)))
                    } else {
                        Err(InvalidWorkspaceIndex)
                    }
                }
                WorkspaceMode::Global => {
                    for set in self.workspaces.sets.values_mut() {
                        if let Some((
                            _,
                            WorkspaceDelta::Gesture {
                                percentage: delta,
                                forward,
                            },
                        )) = set.previously_active
                        {
                            if (velocity > 0.0 && velocity.abs() >= GESTURE_VELOCITY_THRESHOLD)
                                || (velocity.abs() < GESTURE_VELOCITY_THRESHOLD
                                    && delta.abs() > GESTURE_POSITION_THRESHOLD)
                            {
                                set.activate(
                                    set.active,
                                    WorkspaceDelta::new_gesture_end(
                                        delta.abs(),
                                        velocity.abs(),
                                        forward,
                                        window_spring,
                                    ),
                                    workspace_state,
                                )?;
                            } else {
                                set.activate_previous(
                                    WorkspaceDelta::new_gesture_end(
                                        1.0 - delta.abs(),
                                        velocity.abs(),
                                        !forward,
                                        window_spring,
                                    ),
                                    workspace_state,
                                )?;
                            }
                        }
                    }
                    Err(InvalidWorkspaceIndex)
                }
            };

        // Re-evaluate auto-hide state after workspace swipe completes.
        if result.is_ok() {
            self.refresh_auto_hide();
        }

        result
    }

    /// Whether `pid` is `ancestor`, or descends from it.
    ///
    /// Walks `/proc/<pid>/stat`'s parent field upward. Bounded so a malformed or
    /// cyclic chain cannot spin.
    fn process_descends_from(pid: u32, ancestor: u32) -> bool {
        let mut current = pid;
        for _ in 0..32 {
            if current == ancestor {
                return true;
            }
            if current <= 1 {
                return false;
            }
            // Field 4 of /proc/<pid>/stat is the parent pid. The comm field (2) can
            // contain spaces and parentheses, so parse after the final ')'.
            let Ok(stat) = std::fs::read_to_string(format!("/proc/{current}/stat")) else {
                return false;
            };
            let Some(rest) = stat.rsplit_once(')').map(|(_, rest)| rest) else {
                return false;
            };
            let Some(ppid) = rest.split_whitespace().nth(1).and_then(|p| p.parse().ok()) else {
                return false;
            };
            current = ppid;
        }
        false
    }

    /// Whether a window that is being mapped belongs to game mode, and should
    /// therefore be placed on the output game mode owns rather than wherever the
    /// cursor happens to be.
    ///
    /// Checked at MAP time, so it cannot rely on `STEAM_GAME` — the session manager
    /// tags a window only after it appears. Instead a window qualifies when it is
    /// already tagged for the active app, or when its process descends from the
    /// game (its own dialogs) or from the client driving game mode (the game that
    /// client just launched).
    pub fn game_mode_claims(&self, surface: &CosmicSurface) -> bool {
        if !self.game_mode.active {
            return false;
        }
        if self.game_mode.app_id.is_some_and(|app_id| {
            app_id != 0 && crate::dbus::game_mode::app_id_of(surface) == app_id
        }) {
            return true;
        }
        let Some(pid) = surface.pid() else {
            return false;
        };
        let base_pid = self.game_mode.game_surface.as_ref().and_then(|s| s.pid());
        [base_pid, self.game_mode.controller_pid]
            .into_iter()
            .flatten()
            .any(|ancestor| Self::process_descends_from(pid, ancestor))
    }

    /// Whether strict game-mode control would refuse to RENDER `surface`: game
    /// mode is active and the surface shares the controlled surface's workspace
    /// without being the controlled surface itself.
    ///
    /// Such a window draws nothing, so it must not be granted focus either — the
    /// render path and the input path have to agree, otherwise the compositor
    /// hands the keyboard to a window the user cannot see.
    pub fn game_mode_hides(&self, surface: &CosmicSurface) -> bool {
        if !self.game_mode.active {
            return false;
        }
        let Some(controlled) = self.game_mode.game_surface.as_ref() else {
            return false;
        };
        if controlled == surface {
            return false;
        }
        // A window belonging WITH the game (its dialog, EULA or in-prefix login
        // window) IS rendered above it, so it must stay focusable. Evaluated
        // against the base rather than reading `GameMode::children`, which the
        // ~150ms refresh tick has not rebuilt yet for a window that just mapped.
        if self.game_mode.app_id.is_some_and(|app_id| {
            crate::dbus::game_mode::is_game_child(controlled, app_id, surface)
        }) {
            return false;
        }
        // Only the controlled surface's own workspace is under strict control;
        // other workspaces (even on the game's output) are a normal desktop.
        self.workspaces.spaces().any(|ws| {
            ws.get_fullscreen_surfaces()
                .any(|f| &f.surface == controlled)
                && (ws.get_fullscreen_surfaces().any(|f| &f.surface == surface)
                    || ws.mapped().any(|m| &m.active_window() == surface))
        })
    }

    pub fn active_space(&self, output: &Output) -> Option<&Workspace> {
        self.workspaces.active(output).map(|(_, active)| active)
    }

    pub fn active_space_mut(&mut self, output: &Output) -> Option<&mut Workspace> {
        self.workspaces.active_mut(output)
    }

    /// get the parent output of the window which has keyboard focus (for a given seat)
    pub fn get_output_for_focus(&self, seat: &Seat<State>) -> Option<Output> {
        let mut focus_target = seat.get_keyboard().unwrap().current_focus()?;

        if let KeyboardFocusTarget::Popup(popup) = &focus_target {
            let new_target = match popup {
                PopupKind::Xdg(popup) => {
                    if let Some(parent) = popup.get_parent_surface() {
                        self.element_for_surface(&parent).cloned()
                    } else {
                        None
                    }
                }
                PopupKind::InputMethod(popup) => {
                    if let Some(parent) = popup.get_parent() {
                        self.element_for_surface(&parent.surface).cloned()
                    } else {
                        None
                    }
                }
            }?;

            focus_target = KeyboardFocusTarget::Element(new_target);
        }

        match focus_target {
            KeyboardFocusTarget::Element(elem) => {
                if seat
                    .user_data()
                    .get::<SeatMoveGrabState>()
                    .is_some_and(|state| {
                        state
                            .lock()
                            .unwrap()
                            .as_ref()
                            .is_some_and(|state| state.element() == elem)
                    })
                {
                    return Some(seat.active_output());
                }

                self.outputs()
                    .find(|output| {
                        let is_sticky = self
                            .workspaces
                            .sets
                            .get(*output)
                            .unwrap()
                            .sticky_layer
                            .mapped()
                            .any(|m| m == &elem);

                        let workspace = self.active_space(output).unwrap();
                        let is_mapped = workspace.mapped().any(|m| m == &elem);

                        is_sticky || is_mapped
                    })
                    .cloned()
            }
            KeyboardFocusTarget::Fullscreen(elem) => self
                .outputs()
                .find(|output| {
                    let workspace = self.active_space(output).unwrap();
                    workspace
                        .get_fullscreen_surfaces()
                        .any(|f| f.surface == elem)
                })
                .cloned(),
            KeyboardFocusTarget::Group(WindowGroup { node, .. }) => self
                .outputs()
                .find(|output| {
                    self.workspaces
                        .active(output)
                        .unwrap()
                        .1
                        .tiling_layer
                        .has_node(&node)
                })
                .cloned(),
            KeyboardFocusTarget::LayerSurface(layer) => self
                .outputs()
                .find(|output| layer_map_for_output(output).layers().any(|l| l == &layer))
                .cloned(),
            KeyboardFocusTarget::LockSurface(surface) => self
                .session_lock
                .as_ref()?
                .surfaces
                .iter()
                .find_map(|(output, s)| (s == &surface).then_some(output))
                .cloned(),
            KeyboardFocusTarget::Popup(_) => unreachable!(),
        }
    }

    /// Coerce a keyboard focus target into a CosmicMapped element. This is useful when performing window specific
    /// actions, such as closing a window
    pub fn focused_element(&self, focus_target: &KeyboardFocusTarget) -> Option<CosmicMapped> {
        match focus_target {
            KeyboardFocusTarget::Element(window) => Some(window).cloned(),
            KeyboardFocusTarget::Popup(PopupKind::Xdg(popup)) => {
                if let Some(parent) = popup.get_parent_surface() {
                    self.element_for_surface(&parent).cloned()
                } else {
                    None
                }
            }
            KeyboardFocusTarget::Popup(PopupKind::InputMethod(popup)) => {
                if let Some(parent) = popup.get_parent() {
                    self.element_for_surface(&parent.surface).cloned()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Close the focused keyboard focus target
    /// If the target is an embedded window, close the parent instead
    pub fn close_focused(&self, focus_target: &KeyboardFocusTarget) {
        match focus_target {
            KeyboardFocusTarget::Group(_group) => {
                //TODO: decide if we want close actions to apply to groups
            }
            KeyboardFocusTarget::Fullscreen(surface) => {
                // Check if this surface is embedded - if so, close parent
                if let Some(parent_surface_id) =
                    crate::wayland::handlers::surface_embed::get_parent_surface_id(surface)
                {
                    // Find the parent element and close it
                    if let Some(parent) = self.element_for_surface_id(&parent_surface_id) {
                        parent.send_close();
                        return;
                    }
                }
                surface.close();
            }
            x => {
                if let Some(mapped) = self.focused_element(x) {
                    // Check if any window in the mapped element is embedded - if so, close parent
                    for (surface, _) in mapped.windows() {
                        if let Some(parent_surface_id) =
                            crate::wayland::handlers::surface_embed::get_parent_surface_id(&surface)
                        {
                            // Find the parent element and close it
                            if let Some(parent) = self.element_for_surface_id(&parent_surface_id) {
                                parent.send_close();
                                return;
                            }
                        }
                    }
                    mapped.send_close();
                }
            }
        }
    }

    pub fn refresh_active_space(&mut self, output: &Output) {
        if let Some(w) = self.workspaces.active_mut(output) {
            w.refresh()
        }
    }

    /// Find a layer surface by its wl_surface
    pub fn find_layer_surface_by_wl_surface(&self, surface: &WlSurface) -> Option<LayerSurface> {
        for output in self.outputs() {
            let map = layer_map_for_output(output);
            for layer in map.layers() {
                if layer.wl_surface() == surface {
                    return Some(layer.clone());
                }
            }
        }
        None
    }

    pub fn visible_output_for_surface(&self, surface: &WlSurface) -> Option<&Output> {
        if let Some(session_lock) = &self.session_lock {
            return session_lock
                .surfaces
                .iter()
                .find(|(_, v)| v.wl_surface() == surface)
                .map(|(k, _)| k);
        }

        self.outputs()
            // layer map surface?
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            // pending layer map surface?
            .or_else(|| {
                self.pending_layers.iter().find_map(|pending| {
                    let mut found = false;
                    pending.surface.with_surfaces(|s, _| {
                        if s == surface {
                            found = true;
                        }
                    });
                    found.then_some(&pending.output)
                })
            })
            // override redirect window?
            .or_else(|| {
                self.outputs().find(|o| {
                    self.override_redirect_windows.iter().any(|or| {
                        if or.wl_surface().as_ref() == Some(surface) {
                            or.geometry()
                                .as_global()
                                .intersection(o.geometry())
                                .is_some()
                        } else {
                            false
                        }
                    })
                })
            })
            // sticky window ?
            .or_else(|| {
                self.outputs().find(|o| {
                    self.workspaces.sets[*o]
                        .sticky_layer
                        .mapped()
                        .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                })
            })
            // normal window?
            .or_else(|| {
                self.outputs().find(|o| {
                    let workspace = self.active_space(o).unwrap();

                    workspace
                        .get_fullscreen_surfaces()
                        .any(|f| f.surface.has_surface(surface, WindowSurfaceType::ALL))
                        || workspace
                            .mapped()
                            .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                })
            })
            // cursor and drag surfaces
            .or_else(|| {
                self.outputs().find(|o| {
                    self.seats
                        .iter()
                        .filter(|seat| seat.active_output() == **o)
                        .any(|seat| {
                            let cursor_status = seat.cursor_image_status();
                            if let CursorImageStatus::Surface(s) = cursor_status
                                && s == *surface
                            {
                                return true;
                            }

                            if let Some(move_grab) = seat.user_data().get::<SeatMoveGrabState>()
                                && let Some(grab_state) = move_grab.lock().unwrap().as_ref()
                            {
                                for (window, _) in grab_state.element().windows() {
                                    let mut matches = false;
                                    window.0.with_surfaces(|s, _| {
                                        matches |= s == surface;
                                    });
                                    if matches {
                                        return true;
                                    }
                                }
                            }

                            get_dnd_icon(seat).is_some_and(|icon| icon.surface == *surface)
                        })
                })
            })
    }

    pub fn workspace_for_surface(&self, surface: &WlSurface) -> Option<(WorkspaceHandle, Output)> {
        match self.outputs().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL)
                .is_some()
        }) {
            Some(output) => self
                .workspaces
                .spaces()
                .find(move |workspace| workspace.output() == output)
                .map(|w| (w.handle, output.clone())),
            None => self
                .workspaces
                .spaces()
                .find(|w| {
                    w.get_fullscreen_surfaces()
                        .any(|f| f.surface.has_surface(surface, WindowSurfaceType::ALL))
                        || w.mapped()
                            .any(|e| e.has_surface(surface, WindowSurfaceType::ALL))
                        || w.minimized_windows.iter().any(|m| {
                            m.mapped()
                                .is_some_and(|m| m.has_surface(surface, WindowSurfaceType::ALL))
                        })
                })
                .map(|w| (w.handle, w.output().clone())),
        }
    }

    pub fn element_for_surface<S>(&self, surface: &S) -> Option<&CosmicMapped>
    where
        CosmicSurface: PartialEq<S>,
    {
        self.workspaces.sets.values().find_map(|set| {
            set.minimized_windows
                .iter()
                .find(|w| w.windows().any(|s| &s == surface))
                .and_then(|w| w.mapped())
                .or_else(|| {
                    set.sticky_layer
                        .mapped()
                        .find(|w| w.windows().any(|(s, _)| &s == surface))
                })
                .or_else(|| {
                    set.workspaces
                        .iter()
                        .find_map(|w| w.element_for_surface(surface))
                })
        })
    }

    pub fn element_for_x11_window_id(&self, x11_window_id: u32) -> Option<&CosmicMapped> {
        self.workspaces.sets.values().find_map(|set| {
            set.minimized_windows
                .iter()
                .find(|w| {
                    w.windows().any(|s| {
                        s.x11_surface()
                            .is_some_and(|x11| x11.window_id() == x11_window_id)
                    })
                })
                .and_then(|w| w.mapped())
                .or_else(|| {
                    set.sticky_layer.mapped().find(|w| {
                        w.windows().any(|(s, _)| {
                            s.x11_surface()
                                .is_some_and(|x11| x11.window_id() == x11_window_id)
                        })
                    })
                })
                .or_else(|| {
                    set.workspaces.iter().find_map(|w| {
                        w.mapped().find(|m| {
                            m.windows().any(|(s, _)| {
                                s.x11_surface()
                                    .is_some_and(|x11| x11.window_id() == x11_window_id)
                            })
                        })
                    })
                })
        })
    }

    /// Find a mapped element by its WlSurface ObjectId string
    /// Used for finding parent windows of embedded surfaces
    pub fn element_for_surface_id(&self, surface_id: &str) -> Option<&CosmicMapped> {
        self.workspaces.sets.values().find_map(|set| {
            set.minimized_windows
                .iter()
                .find(|w| {
                    w.windows().any(|s| {
                        s.wl_surface()
                            .map(|wl| wl.id().to_string() == surface_id)
                            .unwrap_or(false)
                    })
                })
                .and_then(|w| w.mapped())
                .or_else(|| {
                    set.sticky_layer.mapped().find(|w| {
                        w.windows().any(|(s, _)| {
                            s.wl_surface()
                                .map(|wl| wl.id().to_string() == surface_id)
                                .unwrap_or(false)
                        })
                    })
                })
                .or_else(|| {
                    set.workspaces.iter().find_map(|w| {
                        w.floating_layer
                            .mapped()
                            .find(|m| {
                                m.windows().any(|(s, _)| {
                                    s.wl_surface()
                                        .map(|wl| wl.id().to_string() == surface_id)
                                        .unwrap_or(false)
                                })
                            })
                            .or_else(|| {
                                w.tiling_layer.mapped().find_map(|(m, _)| {
                                    m.windows()
                                        .any(|(s, _)| {
                                            s.wl_surface()
                                                .map(|wl| wl.id().to_string() == surface_id)
                                                .unwrap_or(false)
                                        })
                                        .then_some(m)
                                })
                            })
                    })
                })
        })
    }

    /// Find elements by app_id (e.g., for chat windows)
    /// Returns iterator of (mapped element, geometry) pairs
    pub fn elements_by_app_id(
        &self,
        app_id: &str,
    ) -> Vec<(&CosmicMapped, Rectangle<i32, Logical>)> {
        let mut results = Vec::new();

        for set in self.workspaces.sets.values() {
            // Check sticky layer
            for mapped in set.sticky_layer.mapped() {
                if mapped.active_window().app_id() == app_id {
                    results.push((mapped, mapped.geometry()));
                }
            }

            // Check workspaces
            for workspace in &set.workspaces {
                // Floating layer
                for mapped in workspace.floating_layer.mapped() {
                    if mapped.active_window().app_id() == app_id {
                        results.push((mapped, mapped.geometry()));
                    }
                }

                // Tiling layer
                for (mapped, _) in workspace.tiling_layer.mapped() {
                    if mapped.active_window().app_id() == app_id {
                        results.push((mapped, mapped.geometry()));
                    }
                }
            }
        }

        results
    }

    pub fn is_surface_mapped<S>(&self, surface: &S) -> bool
    where
        CosmicSurface: PartialEq<S>,
    {
        self.workspaces.sets.values().any(|set| {
            set.minimized_windows
                .iter()
                .any(|w| w.windows().any(|s| &s == surface))
                || set
                    .sticky_layer
                    .mapped()
                    .any(|m| m.windows().any(|(s, _)| &s == surface))
                || set.workspaces.iter().any(|w| {
                    w.get_fullscreen_surfaces().any(|f| &f.surface == surface)
                        || w.minimized_windows
                            .iter()
                            .any(|m| m.windows().any(|s| &s == surface))
                        || w.floating_layer
                            .mapped()
                            .any(|m| m.windows().any(|(s, _)| &s == surface))
                        || w.tiling_layer
                            .mapped()
                            .any(|(m, _)| m.windows().any(|(s, _)| &s == surface))
                })
        })
    }

    pub fn space_for(&self, mapped: &CosmicMapped) -> Option<&Workspace> {
        self.workspaces.spaces().find(|workspace| {
            workspace.mapped().any(|m| m == mapped)
                || workspace
                    .minimized_windows
                    .iter()
                    .any(|m| m.mapped() == Some(mapped))
        })
    }

    pub fn space_for_mut(&mut self, mapped: &CosmicMapped) -> Option<&mut Workspace> {
        self.workspaces.spaces_mut().find(|workspace| {
            workspace.mapped().any(|m| m == mapped)
                || workspace
                    .minimized_windows
                    .iter()
                    .any(|m| m.mapped() == Some(mapped))
        })
    }

    pub fn outputs(&self) -> impl DoubleEndedIterator<Item = &Output> {
        self.workspaces.sets.keys()
    }

    pub fn next_output(&self, current_output: &Output, direction: Direction) -> Option<&Output> {
        let current_output_geo = current_output.geometry();
        self.outputs()
            .filter(|o| *o != current_output)
            .filter(|o| {
                let geo = o.geometry();
                match direction {
                    Direction::Left | Direction::Right => {
                        geo.loc.y < current_output_geo.loc.y + current_output_geo.size.h
                            && geo.loc.y + geo.size.h > current_output_geo.loc.y
                    }
                    Direction::Up | Direction::Down => {
                        geo.loc.x < current_output_geo.loc.x + current_output_geo.size.w
                            && geo.loc.x + geo.size.w > current_output_geo.loc.x
                    }
                }
            })
            .filter_map(|o| {
                let origin = o.geometry().loc;
                let res = match direction {
                    Direction::Up => current_output_geo.loc.y - origin.y,
                    Direction::Down => origin.y - current_output_geo.loc.y,
                    Direction::Left => current_output_geo.loc.x - origin.x,
                    Direction::Right => origin.x - current_output_geo.loc.x,
                };
                if res > 0 { Some((o, res)) } else { None }
            })
            .min_by_key(|(_, res)| *res)
            .map(|(o, _)| o)
    }

    pub fn builtin_output(&self) -> Option<&Output> {
        self.outputs().find(|output| output.is_internal())
    }

    pub fn global_space(&self) -> Rectangle<i32, Global> {
        self.outputs()
            .fold(
                Option::<Rectangle<i32, Global>>::None,
                |maybe_geo, output| match maybe_geo {
                    Some(rect) => Some(rect.merge(output.geometry())),
                    None => Some(output.geometry()),
                },
            )
            .unwrap_or_else(Rectangle::default)
    }

    pub fn animations_going(&self) -> bool {
        self.non_slide_animations_going()
            || self
                .layer_slides
                .iter()
                .any(|s| s.visibility.is_animating())
            // A slide-content crossfade can outlive the slide *motion* (the
            // client may not commit its reflowed buffer until after the slide
            // settles), and it advances only when rendered. Keep redraws flowing
            // until it finishes, otherwise the dissolve freezes and pops — a blink.
            || self.any_slide_fade_in_flight()
    }

    /// Outputs that need continuous redraws for ongoing animations. When only
    /// layer slides are animating, this is just the outputs hosting the
    /// sliding surfaces; any other animation type conservatively claims all
    /// outputs. Returns an empty list when nothing is animating.
    pub fn animating_outputs(&self) -> Vec<Output> {
        if self.non_slide_animations_going() {
            return self.outputs().cloned().collect();
        }
        let mut outputs: Vec<Output> = Vec::new();
        for slide in &self.layer_slides {
            if !slide.visibility.is_animating() {
                continue;
            }
            for output in self.outputs() {
                if outputs.contains(output) {
                    continue;
                }
                let map = layer_map_for_output(output);
                if map
                    .layers()
                    .any(|l| l.wl_surface().id() == slide.surface_id)
                {
                    outputs.push(output.clone());
                }
            }
        }
        // Outputs whose floating/sticky layer still has a crossfade in flight
        // need redraws even after the slide motion has settled (see
        // `animations_going`). Targeted so unrelated monitors stay idle.
        for (output, set) in self.workspaces.sets.iter() {
            if outputs.contains(output) {
                continue;
            }
            if set.sticky_layer.has_slide_fade_in_flight()
                || set
                    .workspaces
                    .iter()
                    .any(|w| w.floating_layer.has_slide_fade_in_flight())
            {
                outputs.push(output.clone());
            }
        }
        outputs
    }

    fn non_slide_animations_going(&self) -> bool {
        let workspace_sets = self.workspaces.sets.values().any(|set| {
            set.previously_active
                .as_ref()
                .is_some_and(|(_, delta)| delta.is_animating())
                || set.sticky_layer.animations_going()
        });
        let overview = !matches!(
            self.overview_mode,
            OverviewMode::None | OverviewMode::Active(_)
        );
        let resize = !matches!(
            self.resize_mode,
            ResizeMode::None | ResizeMode::Active(_, _)
        );
        let home = self.home_mode.is_animating(self.theme.motion.animation);
        let voice = self.voice_mode.is_animating();
        let voice_orb = self.voice_orb_state.needs_continuous_render();
        let workspaces = self
            .workspaces
            .spaces()
            .any(|workspace| workspace.animations_going());
        let zoom = self.zoom_state.as_ref().is_some_and(|_| {
            self.outputs().any(|o| {
                o.user_data()
                    .get::<Mutex<OutputZoomState>>()
                    .is_some_and(|state| state.lock().unwrap().is_animating())
            })
        });
        let auto_hide = self
            .auto_hide_surfaces
            .iter()
            .any(|s| s.visibility.is_animating());
        let layer_open = self.layer_opens.iter().any(|o| o.is_animating());
        let pending_open = !self.pending_layer_opens.is_empty();
        let layer_close = self.layer_closes.iter().any(|c| c.is_animating());
        let fade_in = !self.layer_fade_in.is_empty();
        let pending_fade = !self.pending_layer_fade_in.is_empty();
        let fade_out = !self.layer_fade_out.is_empty();
        let layer_resize = self
            .active_layer_resize_anim
            .as_ref()
            .is_some_and(|a| a.is_animating());

        workspace_sets
            || overview
            || resize
            || home
            || voice
            || voice_orb
            || workspaces
            || zoom
            || auto_hide
            || layer_open
            || pending_open
            || layer_close
            || fade_in
            || pending_fade
            || fade_out
            || layer_resize
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = HashMap::new();
        for set in self.workspaces.sets.values_mut() {
            set.sticky_layer.update_animation_state();
        }
        for workspace in self.workspaces.spaces_mut() {
            clients.extend(workspace.update_animations());
        }
        // Update home mode animation
        self.home_mode.update(self.theme.motion.animation);
        // Update voice mode fade animation and coordinate orb showing/hiding
        self.update_voice_mode_fade();
        // Update voice orb animation - track if shrinking_from_attached just completed
        let was_shrinking_from_attached = self.voice_orb_state.shrinking_from_attached;
        self.voice_orb_state.update();
        // If we just finished shrinking from attached mode, skip directly to None
        // (windows were already visible during the shrink, no need for fade-in)
        if was_shrinking_from_attached && !self.voice_orb_state.shrinking_from_attached {
            self.voice_mode.exit_from_attached();
        }
        // Update auto-hide animations and send visibility events
        self.update_auto_hide_animations();
        // Update layer slide animations (visibility-protocol side panels)
        let slide_completed = self.update_layer_slide_animations();
        // Override exclusive zones in cached state and re-arrange so ALL surfaces
        // (windows and other layer surfaces) animate with the panel.
        let has_active_slides = self
            .layer_slides
            .iter()
            .any(|s| s.visibility.is_animating());
        if has_active_slides {
            self.apply_slide_exclusive_zones();
            // Send any withheld slide-start configures whose snapshot is now
            // captured (window left pending_slide_snapshots). Runs AFTER the
            // render that does the capture, so the old buffer was already
            // snapshotted before the client is told to reflow.
            self.flush_deferred_slide_configures(false);
        // INVARIANT (no deferred-configure stranding): this settle branch is the
        // ONLY place that force-flushes the rest. `slide_completed` is true only
        // for slide-OUT; a slide-IN reaches here solely via `is_slide_active()`.
        // So `slide_active` must remain true from begin_slide_layout until here —
        // it is (only set false at settle and the transient toggle inside
        // begin_slide_layout). Don't add a path that clears slide_active without
        // a flush, or deferred configures could strand a window at its old size.
        } else if slide_completed || self.is_slide_active() {
            // Slides just finished (either slide-out completed, or slide-in finished
            // and entry was removed) — disable slide scaling and configure windows
            // to their final size (they were skipping configures during animation).
            self.set_slide_active(false);
            self.set_slide_fade(0.0);
            // Settle: force out any still-withheld configures (e.g. a window
            // that never rendered during the slide) so nothing is stranded at
            // the old size.
            self.flush_deferred_slide_configures(true);
            // The cached state still holds the last animated zone (off by up to
            // a pixel, and stale for surfaces that just became hidden). Restore
            // the client's true zones / zero hidden ones and re-arrange before
            // the final relayout.
            let outputs = self.outputs().cloned().collect::<Vec<_>>();
            for output in &outputs {
                self.override_slide_exclusive_zones(output);
                layer_map_for_output(output).arrange();
            }
            self.workspaces.recalculate();
        }
        // Advance the side-panel spring resize (maximize/restore, presets).
        self.update_layer_resize_animation();
        // Clean up completed layer surface fade-ins
        self.cleanup_layer_fade_ins();
        // Clean up completed open animations
        self.cleanup_layer_opens();
        // Complete close animations (moves to hidden_surfaces)
        self.cleanup_layer_closes();
        // Complete layer surface fade-outs (moves to hidden_surfaces)
        self.cleanup_layer_fade_outs();
        // MERGE: dropped the follow-up `update_layer_blur_state()` refresh that ran for
        // every output once a fade-out/close/slide finished. It repopulated our
        // `backend::render::blur` layer-blur cache, which upstream's frosted-glass
        // implementation replaces (it samples the live framebuffer per frame, so there
        // is no cache to invalidate). The animation cleanups above still run.
        clients
    }

    // -----------------------------------------------------------------------
    // Auto-hide methods
    // -----------------------------------------------------------------------

    /// Register a surface for compositor-driven auto-hide.
    pub fn register_auto_hide(
        &mut self,
        surface: &WlSurface,
        edge: auto_hide::AutoHideEdge,
        mode: auto_hide::AutoHideMode,
    ) {
        let surface_id = surface.id().protocol_id();
        // Remove any existing registration for this surface.
        // Compare by WlSurface identity (ObjectId), not protocol_id which
        // is only unique per-client.
        self.auto_hide_surfaces.retain(|s| s.surface != *surface);

        let mut entry = auto_hide::AutoHideSurface::new(surface, edge, mode);

        // Decide the initial visibility:
        // - "Always" mode: start hidden unless the workspace has no visible
        //   windows (so the dock stays visible on an empty desktop).
        // - "OnMaximize" mode: start hidden if maximized/fullscreen windows exist.
        // - Output not resolved yet: the surface registered for auto-hide before
        //   its first buffer commit (e.g. a panel that requests auto-hide on
        //   open), so we can't read the desktop state. Start hidden to avoid a
        //   visible startup flash — `refresh_auto_hide` re-evaluates once the
        //   surface is mapped (and `force_show`s it if the desktop is empty),
        //   so this lets clients start hidden without a 1×1 boot trick.
        let output = self.auto_hide_surface_output(surface);
        // `output` resolves only once the surface is MAPPED. An unmapped
        // (startup) registration can't read desktop state.
        let mapped = output.is_some();
        let should_hide = match output {
            Some(output) => match mode {
                auto_hide::AutoHideMode::Always => self.output_has_visible_windows(&output),
                auto_hide::AutoHideMode::OnMaximize => {
                    self.output_has_maximized_or_fullscreen(&output)
                }
            },
            None => true,
        };

        if should_hide {
            if mapped {
                // Re-registration of an already-mapped, visible bar (e.g. the
                // panel re-arming auto-hide after a popover closes): SLIDE OUT
                // (animated) instead of snapping. Reuses the same SlidingOut
                // transition as the normal cursor-leave hide; the terminal
                // `visibility_changed(false)` is emitted by
                // `update_auto_hide_animations` when the slide completes, so the
                // client clears its input region at the right moment.
                entry.visibility.start_hide(false);
            } else {
                // Unmapped/startup: snap fully hidden — no animation, no first-
                // frame flash. `refresh_auto_hide` re-evaluates once mapped.
                entry.visibility = auto_hide::AutoHideVisibility::Hidden;
                crate::wayland::protocols::layer_auto_hide::send_auto_hide_visibility(
                    surface, false,
                );
            }
        }

        tracing::info!(
            surface_id,
            ?edge,
            ?mode,
            should_hide,
            mapped,
            "auto_hide: registered surface"
        );
        self.auto_hide_surfaces.push(entry);
    }

    /// Unregister a surface from auto-hide. Shows it immediately if hidden.
    pub fn unregister_auto_hide(&mut self, surface: &WlSurface) {
        let surface_id = surface.id().protocol_id();
        self.auto_hide_surfaces.retain(|s| s.surface != *surface);
        tracing::info!(surface_id, "auto_hide: unregistered surface");
    }

    /// Check whether any toplevel on an output is maximized or fullscreen.
    pub fn output_has_maximized_or_fullscreen(&self, output: &Output) -> bool {
        if let Some(workspace) = self.active_space(output) {
            if !workspace.fullscreen_surfaces.is_empty() {
                return true;
            }
            // Check pending state (true) so we detect windows that are being
            // maximized but haven't committed the new state yet.
            return workspace.mapped().any(|m| m.is_maximized(true));
        }
        false
    }

    /// Check whether the active workspace on an output has any visible
    /// (non-minimized) windows.
    pub fn output_has_visible_windows(&self, output: &Output) -> bool {
        if let Some(workspace) = self.active_space(output) {
            if !workspace.fullscreen_surfaces.is_empty() {
                return true;
            }
            return workspace.mapped().next().is_some();
        }
        false
    }

    /// Find which output a layer surface belongs to.
    fn auto_hide_surface_output(&self, surface: &WlSurface) -> Option<Output> {
        let target_id = surface.id();
        for output in self.outputs() {
            let layer_map = layer_map_for_output(output);
            for layer in layer_map.layers() {
                if layer.wl_surface().id() == target_id {
                    return Some(output.clone());
                }
            }
        }
        None
    }

    /// Get the height of an auto-hide layer surface from the layer map.
    #[allow(dead_code)]
    fn auto_hide_surface_height(&self, surface: &WlSurface) -> Option<i32> {
        let target_id = surface.id();
        for output in self.outputs() {
            let layer_map = layer_map_for_output(output);
            for layer in layer_map.layers() {
                if layer.wl_surface().id() == target_id {
                    return layer_map.layer_geometry(layer).map(|geo| geo.size.h);
                }
            }
        }
        None
    }

    /// Get the margin rectangle between an auto-hide surface and the output
    /// edge.  For a bottom-edge dock with a bottom margin, this is the gap
    /// between the surface's bottom edge and the screen bottom.
    /// Returns the rectangle in global coordinates.
    fn auto_hide_surface_margin_rect(&self, surface: &WlSurface) -> Option<Rectangle<i32, Global>> {
        let target_id = surface.id();
        for output in self.outputs() {
            let layer_map = layer_map_for_output(output);
            for layer in layer_map.layers() {
                if layer.wl_surface().id() == target_id {
                    let local_geo = layer_map.layer_geometry(layer)?;
                    // Convert from Logical (output-local) to Global.
                    let global_geo = local_geo.as_local().to_global(output);
                    let output_geo = output.geometry();
                    let surface_bottom = global_geo.loc.y + global_geo.size.h;
                    let output_bottom = output_geo.loc.y + output_geo.size.h;
                    let margin_height = output_bottom - surface_bottom;
                    if margin_height <= 0 {
                        return None;
                    }
                    return Some(Rectangle::new(
                        Point::from((global_geo.loc.x, surface_bottom)),
                        Size::from((global_geo.size.w, margin_height)),
                    ));
                }
            }
        }
        None
    }

    /// Get the edge zone rectangle for an auto-hide surface.
    /// This is the thin strip at the screen edge used to trigger showing the
    /// dock when it is hidden.  Returns `None` when the surface has no edge
    /// zone configured or when the surface cannot be found.
    fn auto_hide_edge_zone_rect(&self, surface: &WlSurface) -> Option<Rectangle<i32, Global>> {
        let target_id = surface.id();
        for output in self.outputs() {
            let layer_map = layer_map_for_output(output);
            for layer in layer_map.layers() {
                if layer.wl_surface().id() == target_id {
                    let edge_zone =
                        crate::wayland::protocols::layer_auto_hide::get_surface_edge_zone(
                            layer.wl_surface(),
                        );
                    if edge_zone == 0 {
                        return None;
                    }
                    let local_geo = layer_map.layer_geometry(layer)?;
                    let global_geo = local_geo.as_local().to_global(output);
                    let output_geo = output.geometry();
                    let output_bottom = output_geo.loc.y + output_geo.size.h;
                    let zone_top = output_bottom - edge_zone as i32;
                    return Some(Rectangle::new(
                        Point::from((global_geo.loc.x, zone_top)),
                        Size::from((global_geo.size.w, edge_zone as i32)),
                    ));
                }
            }
        }
        None
    }

    /// Called after maximize/fullscreen/map/unmap/minimize state changes to
    /// update all auto-hide surfaces on the affected output.
    pub fn update_auto_hide_for_output(&mut self, output: &Output) {
        let has_max = self.output_has_maximized_or_fullscreen(output);
        let has_windows = self.output_has_visible_windows(output);
        let output_id = output.name();

        // Find which auto-hide surfaces belong to this output.
        // Use ObjectId for comparison (globally unique across clients).
        let matching_object_ids: Vec<smithay::reexports::wayland_server::backend::ObjectId> = self
            .auto_hide_surfaces
            .iter()
            .filter_map(|s| {
                let Ok(wl) = s.surface.upgrade() else {
                    return None;
                };
                let layer_map = layer_map_for_output(output);
                for layer in layer_map.layers() {
                    if layer.wl_surface().id() == wl.id() {
                        return Some(s.surface.id());
                    }
                }
                None
            })
            .collect();

        for surface in &mut self.auto_hide_surfaces {
            if !matching_object_ids.contains(&surface.surface.id()) {
                continue;
            }

            match surface.mode {
                auto_hide::AutoHideMode::Always => {
                    // "Always" mode: show the dock when the workspace has no
                    // visible windows (empty desktop), hide when windows exist.
                    if !has_windows {
                        // No visible windows — show the dock.
                        surface.visibility.force_show();
                    } else if !surface.cursor_over {
                        // Windows exist and cursor is not on the dock — hide.
                        surface.visibility.start_hide(false);
                    }
                    tracing::debug!(
                        surface_id = surface.surface_id,
                        has_windows,
                        output = %output_id,
                        "auto_hide: output window state changed (Always mode)"
                    );
                }
                auto_hide::AutoHideMode::OnMaximize => {
                    if has_max {
                        // Maximize detected — hide (with delay if cursor is on the surface).
                        if !surface.cursor_over {
                            surface.visibility.start_hide(false);
                        } else {
                            // Cursor is on the dock; delay the hide until cursor leaves.
                            surface.visibility.start_hide(true);
                        }
                    } else {
                        // No maximized/fullscreen windows — show immediately.
                        surface.visibility.force_show();
                    }
                    tracing::debug!(
                        surface_id = surface.surface_id,
                        has_maximized = has_max,
                        output = %output_id,
                        "auto_hide: output maximized state changed (OnMaximize mode)"
                    );
                }
            }
        }
    }

    /// Called when the cursor enters or leaves an auto-hide surface or its edge zone.
    pub fn update_auto_hide_cursor(
        &mut self,
        cursor_surface: Option<&WlSurface>,
        cursor_pos: Point<f64, Global>,
    ) {
        use smithay::reexports::wayland_server::backend::ObjectId;

        if self.auto_hide_surfaces.is_empty() {
            return;
        }

        let cursor_object_id: Option<ObjectId> = cursor_surface.map(|s| s.id());

        // Pre-compute per-output state to avoid borrow issues.
        let outputs_maximized: Vec<(Output, bool)> = self
            .workspaces
            .sets
            .keys()
            .map(|output| {
                let has_max = self.output_has_maximized_or_fullscreen(output);
                (output.clone(), has_max)
            })
            .collect();

        let outputs_has_windows: Vec<(Output, bool)> = self
            .workspaces
            .sets
            .keys()
            .map(|output| {
                let has_win = self.output_has_visible_windows(output);
                (output.clone(), has_win)
            })
            .collect();

        // Pre-compute surface-to-output mapping.
        let surface_outputs: Vec<(ObjectId, Option<Output>)> = self
            .auto_hide_surfaces
            .iter()
            .filter_map(|s| {
                let wl = s.surface.upgrade().ok()?;
                let output = self.auto_hide_surface_output(&wl);
                Some((s.surface.id(), output))
            })
            .collect();

        // Pre-compute margin rects (gap between surface bottom and output
        // bottom) so we can suppress hide when cursor is in that area.
        let margin_rects: Vec<(ObjectId, Option<Rectangle<i32, Global>>)> = self
            .auto_hide_surfaces
            .iter()
            .filter_map(|s| {
                let wl = s.surface.upgrade().ok()?;
                let rect = self.auto_hide_surface_margin_rect(&wl);
                Some((s.surface.id(), rect))
            })
            .collect();

        // Pre-compute edge zone rects (thin strip at output bottom) so we
        // can trigger show when the cursor enters the edge zone while the
        // surface is hidden.  This replaces the old approach of returning
        // the dock surface as a pointer hit target from surface_under().
        let edge_zone_rects: Vec<(ObjectId, Option<Rectangle<i32, Global>>)> = self
            .auto_hide_surfaces
            .iter()
            .filter_map(|s| {
                let wl = s.surface.upgrade().ok()?;
                let rect = self.auto_hide_edge_zone_rect(&wl);
                Some((s.surface.id(), rect))
            })
            .collect();

        // Helper: determine whether to hide based on mode and output state.
        let should_auto_hide = |mode: auto_hide::AutoHideMode, obj_id: &ObjectId| -> bool {
            let output = surface_outputs
                .iter()
                .find(|(id, _)| id == obj_id)
                .and_then(|(_, o)| o.as_ref());
            match mode {
                auto_hide::AutoHideMode::Always => output
                    .and_then(|o| {
                        outputs_has_windows
                            .iter()
                            .find(|(out, _)| out == o)
                            .map(|(_, has)| *has)
                    })
                    .unwrap_or(false),
                auto_hide::AutoHideMode::OnMaximize => output
                    .and_then(|o| {
                        outputs_maximized
                            .iter()
                            .find(|(out, _)| out == o)
                            .map(|(_, m)| *m)
                    })
                    .unwrap_or(false),
            }
        };

        for surface in &mut self.auto_hide_surfaces {
            let obj_id = surface.surface.id();
            // Cursor is "over" the surface if it's directly on the surface
            // OR if it's hovering the edge zone strip at the output bottom.
            let in_edge_zone = edge_zone_rects
                .iter()
                .find(|(id, _)| *id == obj_id)
                .and_then(|(_, rect)| rect.as_ref())
                .is_some_and(|rect| rect.to_f64().contains(cursor_pos));
            let is_over = cursor_object_id.as_ref() == Some(&obj_id) || in_edge_zone;
            let was_over = surface.cursor_over;

            if is_over && !was_over {
                // Cursor entered auto-hide surface/edge zone → show.
                surface.cursor_over = true;
                surface.visibility.start_show(true);
                tracing::debug!(surface_id = surface.surface_id, "auto_hide: cursor entered");
            } else if !is_over && was_over {
                // Cursor left the surface itself, but check if it moved
                // into the margin gap between surface and output edge.
                let in_margin = margin_rects
                    .iter()
                    .find(|(id, _)| *id == obj_id)
                    .and_then(|(_, rect)| rect.as_ref())
                    .is_some_and(|rect| rect.to_f64().contains(cursor_pos));

                if in_margin {
                    // Keep cursor_over true — cursor is still in the
                    // dock's margin area, don't trigger hide.
                    tracing::debug!(
                        surface_id = surface.surface_id,
                        "auto_hide: cursor in margin area, suppressing hide"
                    );
                } else {
                    surface.cursor_over = false;

                    if should_auto_hide(surface.mode, &obj_id) {
                        surface.visibility.start_hide(true);
                        tracing::debug!(
                            surface_id = surface.surface_id,
                            "auto_hide: cursor left, starting hide"
                        );
                    }
                }
            } else if !is_over && !was_over {
                // Cursor is not over the surface and wasn't before.
                // But check: was cursor_over kept true due to margin
                // suppression?  If so, verify cursor is still in margin.
                if surface.cursor_over {
                    let still_in_margin = margin_rects
                        .iter()
                        .find(|(id, _)| *id == obj_id)
                        .and_then(|(_, rect)| rect.as_ref())
                        .is_some_and(|rect| rect.to_f64().contains(cursor_pos));

                    if !still_in_margin {
                        surface.cursor_over = false;

                        if should_auto_hide(surface.mode, &obj_id) {
                            surface.visibility.start_hide(true);
                            tracing::debug!(
                                surface_id = surface.surface_id,
                                "auto_hide: cursor left margin area, starting hide"
                            );
                        }
                    }
                }
            } else {
                // is_over && was_over — no change needed.
                surface.cursor_over = true;
            }
        }
    }

    /// Update auto-hide animation state and send visibility events.
    fn update_auto_hide_animations(&mut self) {
        // Remove dead surfaces.
        self.auto_hide_surfaces
            .retain(|s| s.surface.upgrade().is_ok());

        for surface in &mut self.auto_hide_surfaces {
            if let Some(visible) = surface.visibility.update() {
                // State transition completed — send visibility_changed event.
                if let Ok(wl_surface) = surface.surface.upgrade() {
                    crate::wayland::protocols::layer_auto_hide::send_auto_hide_visibility(
                        &wl_surface,
                        visible,
                    );
                    tracing::debug!(
                        surface_id = surface.surface_id,
                        visible,
                        "auto_hide: visibility_changed event sent"
                    );
                }
            }
        }
    }

    /// Get the auto-hide render offset for a surface. Returns (0, 0) if the
    /// surface is not registered for auto-hide or is fully visible.
    pub fn get_auto_hide_offset(&self, surface: &WlSurface, surface_height: i32) -> (i32, i32) {
        for s in &self.auto_hide_surfaces {
            if s.surface == *surface {
                return s.render_offset(surface_height);
            }
        }
        (0, 0)
    }

    /// Get the slide render offset for a surface. Returns (0, 0) if the
    /// surface has no active slide animation.
    pub fn get_layer_slide_offset(&self, surface_id: &ObjectId) -> (i32, i32) {
        for s in &self.layer_slides {
            if s.surface_id == *surface_id {
                let offset = s.render_offset();
                if offset != (0, 0) {
                    tracing::debug!(
                        surface_protocol_id = surface_id.protocol_id(),
                        ?offset,
                        factor = s.visibility.factor(),
                        "get_layer_slide_offset: non-zero offset"
                    );
                }
                return offset;
            }
        }
        (0, 0)
    }

    /// Render offset that keeps the *anchored* edge of an actively-resized side panel
    /// pinned to the output edge while the client's buffer catches up to the new
    /// width. `arrange()` repositions a right-anchored surface to `output_right - width`
    /// the instant we override its size, but the client renders the matching buffer a
    /// frame later — so the still-narrow buffer would sit short of the screen edge
    /// (the right edge appears to trail the drag, then snap back). Shifting the buffer
    /// right by the width deficit re-pins the right edge every frame. Left-anchored
    /// panels need no shift (their anchored edge is the surface origin already).
    pub fn get_layer_resize_offset(&self, surface_id: &ObjectId, buffer_width: i32) -> (i32, i32) {
        if let Some(resize) = self
            .active_layer_resize
            .as_ref()
            .or(self.layer_resize_settle.as_ref())
            && &resize.surface_id == surface_id
            && resize.anchor_right
        {
            let deficit = resize.width - buffer_width;
            if deficit != 0 {
                return (deficit, 0);
            }
        }
        (0, 0)
    }

    /// Render offset that pins the BOTTOM edge of a bottom-anchored surface to its
    /// arranged position while the client's buffer height catches up to a just
    /// requested grow/shrink. The vertical twin of [`Self::get_layer_resize_offset`].
    ///
    /// `arrange()` repositions a bottom-anchored surface to `zone_bottom -
    /// configured_height` the instant the client commits a new `set_size`, but the
    /// matching buffer can land a frame later. The still-short (growing) or
    /// still-tall (shrinking) buffer, drawn at the new top, then floats up from — or
    /// overshoots below — the fixed bottom edge for a frame: the "content jumps then
    /// settles" blink when an auto-size popover expands/collapses an inline list.
    /// Shifting the buffer down by the height deficit (`configured - buffer`) re-pins
    /// the bottom edge every frame.
    ///
    /// The deficit is zero whenever the buffer already matches the configured size —
    /// i.e. in steady state and for every fixed-size surface (docks, the bottom bar) —
    /// so this is a no-op except during the one-or-few frames an auto-size surface is
    /// mid-resize. Only fires for surfaces anchored to the bottom and not stretched
    /// top-to-bottom (those grow upward from a fixed bottom).
    pub fn get_layer_bottom_pin_offset(
        &self,
        surface: &WlSurface,
        buffer_height: i32,
    ) -> (i32, i32) {
        let (anchor, configured_height) = with_states(surface, |states| {
            let mut state = states.cached_state.get::<LayerSurfaceCachedState>();
            let current = state.current();
            (current.anchor, current.size.h)
        });
        if configured_height > 0 && anchor.contains(Anchor::BOTTOM) && !anchor.contains(Anchor::TOP)
        {
            let deficit = configured_height - buffer_height;
            if deficit != 0 {
                return (0, deficit);
            }
        }
        (0, 0)
    }

    /// Whether `surface` is a layer surface anchored to the bottom edge and not
    /// stretched top-to-bottom — i.e. one whose bottom edge is the fixed reference
    /// and whose top moves when it resizes. The render path uses this to pin such a
    /// surface's physical bottom edge (see the sub-pixel bottom pin in `render`).
    pub fn is_layer_bottom_anchored(&self, surface: &WlSurface) -> bool {
        with_states(surface, |states| {
            let mut state = states.cached_state.get::<LayerSurfaceCachedState>();
            let anchor = state.current().anchor;
            anchor.contains(Anchor::BOTTOM) && !anchor.contains(Anchor::TOP)
        })
    }

    /// What edge sash, if any, the render path should draw for `surface_id`. The
    /// dragged ghost takes priority over a resting hover.
    pub fn get_layer_edge_indicator(&self, surface_id: &ObjectId) -> Option<EdgeIndicator> {
        if let Some(ghost) = self.edge_drag_ghost.as_ref()
            && &ghost.surface_id == surface_id
        {
            return Some(EdgeIndicator::Drag {
                anchor_right: ghost.anchor_right,
                ghost_width: ghost.width,
            });
        }
        if let Some(hover) = self.edge_hover.as_ref()
            && &hover.surface_id == surface_id
        {
            return Some(EdgeIndicator::Hover {
                anchor_right: hover.anchor_right,
            });
        }
        None
    }

    /// Clear the post-grab resize "settle" ([`Self::layer_resize_settle`]) once the
    /// client's buffer has caught up to the settled width. Called after `arrange()` on
    /// every layer commit; until it fires, the size override + edge-pin offset keep
    /// holding the final width so a fast release never blinks to the trailing width.
    pub fn clear_layer_resize_settle_if_caught_up(&mut self, output: &Output) {
        let Some(settle) = self.layer_resize_settle.as_ref() else {
            return;
        };
        if &settle.output != output {
            return;
        }
        let caught_up = layer_map_for_output(output)
            .layers()
            .any(|l| l.wl_surface().id() == settle.surface_id && l.bbox().size.w >= settle.width);
        if caught_up {
            self.layer_resize_settle = None;
        }
    }

    /// Check if a surface has an active slide animation (used to suppress
    /// the normal fade animation for sliding surfaces).
    pub fn is_layer_sliding(&self, surface_id: &ObjectId) -> bool {
        self.layer_slides
            .iter()
            .any(|s| s.surface_id == *surface_id)
    }

    /// Horizontal render scale to keep a full-width NEIGHBOR layer surface (e.g.
    /// the agentos-panel bottom bar, anchored Left+Right) glued to an actively
    /// sliding side panel's animated inner edge — instead of trailing it while
    /// its client lags a couple of frames behind the per-tick reconfigures.
    ///
    /// Returns `Some(scale_x)` only for a genuine full-width neighbor being shrunk
    /// by a RIGHT-anchored animating slide; the render path then squishes the
    /// bar's committed buffer about its (fixed) LEFT edge so its right edge lands
    /// exactly on the panel's animated left edge — the same `cached_factor` the
    /// panel itself uses, so they're pixel-locked. `None` (no scale, current
    /// behavior) for everything else, so this can never affect other surfaces.
    ///
    /// Map-free by construction: the render thread must NOT call
    /// `layer_map_for_output` (it uses cached layer surfaces), so anchor is read
    /// via `with_states` (per-surface, render-safe) and geometry is passed in.
    /// `bar_left`/`rendered_width` are the surface's OUTPUT-LOCAL left x and
    /// committed (bbox) width.
    pub fn get_layer_slide_neighbor_scale(
        &self,
        output: &Output,
        surface: &WlSurface,
        bar_left: i32,
        rendered_width: i32,
    ) -> Option<f64> {
        use smithay::wayland::shell::wlr_layer::Anchor;

        if rendered_width <= 0 {
            return None;
        }
        let output_w = output.geometry().size.w;
        // A full-width bar on an output WITHOUT the slide stays full width — only
        // squish one that's actually been shrunk (multi-output safety).
        if rendered_width >= output_w {
            return None;
        }
        let surface_id = surface.id();
        if self.hidden_surfaces.contains(&surface_id) {
            return None;
        }
        // Only RIGHT-edge slides are handled (the common chat-panel case): the bar
        // keeps its left edge and shrinks on the right. Left-edge slides would
        // need a right-edge origin — skipped (returns None → bar lags as before,
        // no regression). The slide is not this surface itself.
        //
        // LIMITATION (multi-output): `LayerSlide` has no output, so this picks the
        // first animating Right slide regardless of which output it's on. Safe for
        // the single-output case and for a bar on a non-slide output (caught by the
        // `rendered_width >= output_w` guard above). A misscale is only possible
        // with TWO simultaneous Right slides on different outputs AND a bar on the
        // other output independently shrunk below its width — rare; if it ever
        // matters, add an `output` to LayerSlide and filter on it here.
        let slide = self.layer_slides.iter().find(|s| {
            s.visibility.is_animating()
                && s.edge == layer_slide::SlideEdge::Right
                && s.surface_id != surface_id
        })?;
        // Only full-width bars (anchored to BOTH horizontal edges) are driven by a
        // horizontal exclusive zone; never scale a non-spanning surface.
        let anchor = with_states(surface, |states| {
            states
                .cached_state
                .get::<LayerSurfaceCachedState>()
                .current()
                .anchor
        });
        if !(anchor.contains(Anchor::LEFT) && anchor.contains(Anchor::RIGHT)) {
            return None;
        }
        // The panel's animated inner (left) edge, from the SAME cached_factor the
        // panel's render_offset uses → pixel-locked to the panel.
        let target_right = output_w - slide.effective_exclusive_zone();
        let target_width = (target_right - bar_left).max(1);
        let scale = target_width as f64 / rendered_width as f64;
        // Guard against absurd scales (capped zones / transient geometry).
        if !scale.is_finite() || scale <= 0.0 {
            return None;
        }
        Some(scale)
    }

    // -----------------------------------------------------------------------
    // Fade+rise open/close-animation methods (the default layer transition)
    // -----------------------------------------------------------------------

    /// True if this surface currently has an active open animation. Used to gate
    /// the translate/scale render path so settled layers are untouched.
    pub fn is_layer_opening(&self, surface_id: &ObjectId) -> bool {
        self.layer_opens.iter().any(|o| o.surface_id == *surface_id)
    }

    /// True while any layer surface is playing its open (scale + fade-in) or
    /// fade-out animation. The blur content hash only tracks commit counters and
    /// geometry — NOT the compositor-side alpha/scale these animations apply — so a
    /// blur group whose captured backdrop includes an animating surface must force a
    /// re-capture each frame while one runs. Otherwise the blurred backdrop freezes
    /// at the animation's first frame: e.g. a wallpaper captured at fade-in start
    /// (alpha≈0, scaled down) never updates to its settled state, and the glass on
    /// top stays dark until unrelated damage forces a re-blur.
    pub fn has_layer_open_or_fade_animations(&self) -> bool {
        self.layer_opens.iter().any(|o| o.is_animating())
            || !self.layer_fade_in.is_empty()
            || !self.layer_fade_out.is_empty()
    }

    /// The translate offset `(x, y)` (logical px) for an opening surface, or
    /// `(0, 0)` if it isn't opening. Folds into the layer-surface render offset.
    /// Full-output surfaces pure-fade (no rise) — see [`is_full_output_layer`].
    pub fn get_layer_open_offset(&self, surface_id: &ObjectId) -> (i32, i32) {
        if self.is_full_output_layer(surface_id) {
            return (0, 0);
        }
        for o in &self.layer_opens {
            if o.surface_id == *surface_id {
                return o.translate_offset();
            }
        }
        (0, 0)
    }

    /// The scale factor for an opening surface, or `1.0` if it isn't opening.
    /// Full-output surfaces pure-fade (no scale) — see [`is_full_output_layer`].
    pub fn get_layer_open_scale(&self, surface_id: &ObjectId) -> f32 {
        if self.is_full_output_layer(surface_id) {
            return 1.0;
        }
        for o in &self.layer_opens {
            if o.surface_id == *surface_id {
                return o.scale();
            }
        }
        1.0
    }

    /// True if this surface currently has an active CLOSE animation (a fade+rise
    /// surface being hidden). Gates the same translate/scale render path as the open.
    pub fn is_layer_closing(&self, surface_id: &ObjectId) -> bool {
        self.layer_closes
            .iter()
            .any(|c| c.surface_id == *surface_id)
    }

    /// The translate offset `(x, y)` (logical px) for a closing surface, or
    /// `(0, 0)` if it isn't closing. Slides DOWN `0 → +6px`. Full-output surfaces
    /// pure-fade (no rise) — see [`is_full_output_layer`].
    pub fn get_layer_close_offset(&self, surface_id: &ObjectId) -> (i32, i32) {
        if self.is_full_output_layer(surface_id) {
            return (0, 0);
        }
        for c in &self.layer_closes {
            if c.surface_id == *surface_id {
                return c.translate_offset();
            }
        }
        (0, 0)
    }

    /// The scale factor for a closing surface, or `1.0` if it isn't closing.
    /// Scales DOWN `1.0 → 0.97`. Full-output surfaces pure-fade (no scale).
    pub fn get_layer_close_scale(&self, surface_id: &ObjectId) -> f32 {
        if self.is_full_output_layer(surface_id) {
            return 1.0;
        }
        for c in &self.layer_closes {
            if c.surface_id == *surface_id {
                return c.scale();
            }
        }
        1.0
    }

    /// Reconcile the exclusive zones in smithay's cached state for all layer
    /// surfaces on `output` with the compositor's view, prior to an `arrange()`:
    ///
    /// - surfaces with an active slide animation get the interpolated zone,
    /// - hidden surfaces reserving space get `Exclusive(0)` — a hidden surface
    ///   must not shrink the workspace, even if its client commits a non-zero
    ///   zone while hidden (e.g. the chat panel re-claims its zone just before
    ///   requesting show, which used to snap the desktop and storm clients
    ///   with shrink+grow configures),
    /// - all other surfaces get the client's last committed value restored
    ///   (undoing stale animation overrides after a slide completes).
    ///
    /// Called before every `arrange()` (commit handler, animation tick, slide
    /// completion), so the client's raw values never cause layout jumps.
    pub fn override_slide_exclusive_zones(&self, output: &Output) {
        use smithay::wayland::shell::wlr_layer::ExclusiveZone;

        let map = layer_map_for_output(output);
        for layer in map.layers() {
            let id = layer.wl_surface().id();
            // Use the zone recorded by the last animation tick so commits
            // between ticks re-assert the exact value the layouts were
            // arranged against, rather than a fresh (slightly later) sample.
            let slide_ez = self
                .layer_slides
                .iter()
                .find(|s| s.surface_id == id && s.visibility.is_animating())
                .map(|s| {
                    s.last_applied_ez
                        .unwrap_or_else(|| s.effective_exclusive_zone())
                        .max(0) as u32
                });
            let hidden = self.hidden_surfaces.contains(&id);
            let client_ez = self.client_exclusive_zones.get(&id).copied();

            with_states(layer.wl_surface(), |states| {
                let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                let current = cached.current();
                let desired = if let Some(ez) = slide_ez {
                    Some(ExclusiveZone::Exclusive(ez))
                } else if hidden {
                    // Only suppress an actual reservation; Neutral/DontCare
                    // surfaces reserve nothing and keep their semantics.
                    match client_ez.unwrap_or(current.exclusive_zone) {
                        ExclusiveZone::Exclusive(n) if n > 0 => Some(ExclusiveZone::Exclusive(0)),
                        _ => None,
                    }
                } else {
                    client_ez
                };
                if let Some(desired) = desired {
                    current.exclusive_zone = desired;
                }
            });
        }
    }

    /// Force the actively-resized side panel's cached size + exclusive zone to the
    /// grab's current `width` before `arrange()`. Like [`Self::override_slide_exclusive_zones`],
    /// this runs on every layer commit (and from the resize grab's motion handler),
    /// so the client's own `set_size` can never fight the compositor-driven resize.
    pub fn override_active_layer_resize(&self, output: &Output) {
        use smithay::wayland::shell::wlr_layer::ExclusiveZone;

        let Some(resize) = self
            .active_layer_resize
            .as_ref()
            .or(self.layer_resize_settle.as_ref())
        else {
            return;
        };
        if &resize.output != output {
            return;
        }
        // The surface (size) may grow all the way to full screen, but the *reserved*
        // (exclusive) zone is capped so the desktop/windows area never shrinks below
        // MIN_VIEWPORT_WIDTH. Past that point the panel surface simply grows OVER the
        // windows (they stay frozen at the minimum) instead of squishing them further
        // and breaking their layouts.
        let output_width = output.geometry().size.w;
        let zone = resize
            .width
            .min((output_width - MIN_VIEWPORT_WIDTH).max(0))
            .max(0);
        let map = layer_map_for_output(output);
        for layer in map.layers() {
            if layer.wl_surface().id() != resize.surface_id {
                continue;
            }
            with_states(layer.wl_surface(), |states| {
                let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                let current = cached.current();
                tracing::debug!(
                    "RESIZE_DBG override prev_anchor={:?} prev_size=({},{}) -> new_width={} zone={} out_w={} anchor_right={}",
                    current.anchor,
                    current.size.w,
                    current.size.h,
                    resize.width,
                    zone,
                    output_width,
                    resize.anchor_right,
                );
                current.size = (resize.width, 0).into();
                current.exclusive_zone = ExclusiveZone::Exclusive(zone as u32);
            });
        }
    }

    /// Animate the side panel from `from_width` to `target_width` over the shared spring
    /// curve — used by the double-click maximize/restore toggle (and width presets later).
    /// Seeds a [`layer_resize_anim::LayerResizeAnim`]; its per-frame tick
    /// ([`Self::update_layer_resize_animation`]) forces each eased width onto the surface
    /// ([`Self::override_active_layer_resize`]) and re-arranges, so windows reflow in
    /// lockstep and the client adopts each `configure` through its `WindowResized` handler
    /// (re-asserting size + exclusive zone, debounce-saving) — exactly like a drag used to.
    /// Both widths are clamped to `[320, output width]`; a no-op change is ignored.
    pub fn set_layer_resize_width(
        &mut self,
        surface_id: &ObjectId,
        output: &Output,
        anchor_right: bool,
        from_width: i32,
        target_width: i32,
    ) {
        let max = output.geometry().size.w;
        let from = from_width.clamp(MIN_PANEL_WIDTH, max);
        let target = target_width.clamp(MIN_PANEL_WIDTH, max);
        if from == target {
            return;
        }
        let motion = self.theme.motion;
        self.active_layer_resize_anim = Some(layer_resize_anim::LayerResizeAnim::new(
            surface_id.clone(),
            output.clone(),
            anchor_right,
            from,
            target,
            motion,
        ));
        // Apply the first frame immediately so the motion starts this dispatch.
        self.update_layer_resize_animation();
    }

    /// Toggle the side panel between full width and its previous width on an edge
    /// double-click. The decision is purely positional — if the panel is already within
    /// 1% of the output width it *restores* to the saved previous width (falling back to a
    /// half-width if none is known); otherwise it *maximizes*, remembering `current_width`
    /// as the width to restore to. There is no separate "is maximized" flag to go stale
    /// after a manual drag, so a manual resize followed by a double-click always does the
    /// intuitive thing. `current_width` is the panel's present width (from
    /// [`Self::layer_resize_target`]).
    pub fn toggle_layer_resize_maximize(
        &mut self,
        surface_id: &ObjectId,
        output: &Output,
        anchor_right: bool,
        current_width: i32,
    ) {
        let max = output.geometry().size.w;
        let maximized_at = (max as f32 * MAXIMIZE_FRACTION) as i32;
        let near_max = current_width >= maximized_at;
        let target = if near_max {
            // RESTORE: jump back to the last pre-maximize width, or a sane half-width.
            self.layer_maximize
                .as_ref()
                .filter(|m| &m.surface_id == surface_id)
                .map(|m| m.restore_width)
                .filter(|w| *w >= MIN_PANEL_WIDTH && *w < maximized_at)
                .unwrap_or((max / 2).max(MIN_PANEL_WIDTH))
        } else {
            // MAXIMIZE: remember the width we are leaving so the next toggle can restore.
            match self.layer_maximize.as_mut() {
                Some(m) if &m.surface_id == surface_id => m.restore_width = current_width,
                _ => {
                    self.layer_maximize = Some(LayerMaximizeState {
                        surface_id: surface_id.clone(),
                        restore_width: current_width,
                        last_click: None,
                    });
                }
            }
            max
        };
        self.set_layer_resize_width(surface_id, output, anchor_right, current_width, target);
    }

    /// Clamp the agentos-chat-panel's exclusive zone so the desktop/windows area never
    /// shrinks below `MIN_VIEWPORT_WIDTH`, regardless of the width the client requests.
    /// Runs on every layer commit (before `arrange()`), so the cap persists after a
    /// resize grab ends — the client re-asserts its full width as the exclusive zone,
    /// and this clamps it (its surface stays full and simply overlaps the windows).
    pub fn cap_chat_panel_exclusive_zone(&self, output: &Output) {
        use smithay::wayland::shell::wlr_layer::ExclusiveZone;

        let cap = (output.geometry().size.w - MIN_VIEWPORT_WIDTH).max(0) as u32;
        let map = layer_map_for_output(output);
        for layer in map.layers() {
            // Only surfaces opted into edge resize (via the layer_edge_resize
            // protocol) get the viewport-floor cap on their exclusive zone.
            if crate::wayland::protocols::layer_edge_resize::get_surface_edge_resize(
                layer.wl_surface(),
            )
            .is_none()
            {
                continue;
            }
            with_states(layer.wl_surface(), |states| {
                let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                let current = cached.current();
                if let ExclusiveZone::Exclusive(n) = current.exclusive_zone {
                    tracing::debug!(
                        "RESIZE_DBG cap_zone cap={} current_zone={} size=({},{}) anchor={:?} will_cap={}",
                        cap,
                        n,
                        current.size.w,
                        current.size.h,
                        current.anchor,
                        n > cap,
                    );
                    if n > cap {
                        current.exclusive_zone = ExclusiveZone::Exclusive(cap);
                    }
                }
            });
        }
    }

    /// If `global_pos` is within `grab_px` of the inner (dragged) edge of the
    /// `agentos-chat-panel` side panel, return the data to start a resize grab.
    pub fn layer_resize_target(
        &self,
        global_pos: Point<f64, Global>,
        grab_px: f64,
    ) -> Option<LayerResize> {
        use smithay::wayland::shell::wlr_layer::Anchor;

        for output in self.outputs() {
            let output_geo = output.geometry();
            let map = layer_map_for_output(output);
            for layer in map.layers() {
                // Opt-in via the layer_edge_resize protocol (replaces the former
                // `namespace == "agentos-chat-panel"` hardcode). Width bounds come
                // from the client; `max_width == 0` means "full output width".
                let Some(cfg) =
                    crate::wayland::protocols::layer_edge_resize::get_surface_edge_resize(
                        layer.wl_surface(),
                    )
                else {
                    continue;
                };
                // Only a fully-shown panel exposes a draggable resize edge. A
                // hidden/closed panel still occupies its docked geometry in the
                // layer map (anchored at the output edge), so without this gate a
                // click at that x — now inside the neighbouring window — would be
                // swallowed and start a resize, applying an exclusive zone with no
                // panel visible. Also skip while a slide is animating (the edge is
                // moving and the panel is on its way in/out).
                let surface_id = layer.wl_surface().id();
                if self.hidden_surfaces.contains(&surface_id)
                    || self.layer_slides.iter().any(|s| s.surface_id == surface_id)
                {
                    continue;
                }
                let anchor = with_states(layer.wl_surface(), |states| {
                    states
                        .cached_state
                        .get::<LayerSurfaceCachedState>()
                        .current()
                        .anchor
                });
                let anchor_right = anchor.contains(Anchor::RIGHT) && !anchor.contains(Anchor::LEFT);
                let anchor_left = anchor.contains(Anchor::LEFT) && !anchor.contains(Anchor::RIGHT);
                if !anchor_right && !anchor_left {
                    continue;
                }
                let Some(geo) = map.layer_geometry(layer) else {
                    continue;
                };
                // `layer_geometry` is output-local; lift it into global space.
                let gx = output_geo.loc.x + geo.loc.x;
                let gy = output_geo.loc.y + geo.loc.y;
                if (global_pos.y as i32) < gy || (global_pos.y as i32) > gy + geo.size.h {
                    continue;
                }
                let inner_edge_x = if anchor_right { gx } else { gx + geo.size.w };
                if (global_pos.x - inner_edge_x as f64).abs() <= grab_px {
                    tracing::debug!(
                        "RESIZE_DBG layer_resize_target HIT anchor={:?} anchor_right={} anchor_left={} out_loc_x={} out_w={} panel_loc=({},{}) panel_size=({},{}) inner_edge_x={} pointer_x={:.1} start_width={}",
                        anchor,
                        anchor_right,
                        anchor_left,
                        output_geo.loc.x,
                        output_geo.size.w,
                        gx,
                        gy,
                        geo.size.w,
                        geo.size.h,
                        inner_edge_x,
                        global_pos.x,
                        geo.size.w,
                    );
                    let max = if cfg.max_width == 0 {
                        output_geo.size.w
                    } else {
                        cfg.max_width.min(output_geo.size.w)
                    };
                    return Some(LayerResize {
                        surface_id: layer.wl_surface().id(),
                        output: output.clone(),
                        anchor_right,
                        width: geo.size.w,
                        // MIN_PANEL_WIDTH is the hard floor (also enforced by
                        // set_layer_resize_width), so the drag ghost and the release
                        // clamp agree and a release never jumps the width.
                        min: cfg.min_width.max(MIN_PANEL_WIDTH).min(max),
                        max,
                    });
                }
            }
        }
        None
    }

    /// Update layer slide animations, completing finished ones.
    /// Returns true if any slide-out completed (surfaces moved to hidden_surfaces).
    fn update_layer_slide_animations(&mut self) -> bool {
        let mut completed_hidden = Vec::new();
        for slide in &mut self.layer_slides {
            if let Some(false) = slide.visibility.update() {
                // Slide-out complete — mark as fully hidden
                completed_hidden.push(slide.surface_id.clone());
            }
        }
        let had_completions = !completed_hidden.is_empty();
        // Move fully-hidden slides' surfaces to hidden_surfaces
        for surface_id in &completed_hidden {
            self.hidden_surfaces.insert(surface_id.clone());
        }
        // Remove completed slide entries (both fully Visible and fully Hidden).
        // Once animation completes, the cached state matches the client's value.
        self.layer_slides.retain(|s| s.visibility.is_animating());
        had_completions
    }

    /// Advance the side-panel spring resize animation, if one is active.
    ///
    /// Each tick sets [`Self::active_layer_resize`] to the eased width and forces it
    /// onto the surface via [`Self::override_active_layer_resize`] + `arrange()`,
    /// exactly like the old live drag — so windows reflow in lockstep and the client
    /// adopts each `configure`. On completion the final width is handed to
    /// [`Self::layer_resize_settle`] (keeping the anchored edge pinned until the
    /// client's buffer catches up) and the animation is cleared.
    fn update_layer_resize_animation(&mut self) {
        let Some(anim) = self.active_layer_resize_anim.as_ref() else {
            return;
        };
        let output = anim.output.clone();
        let surface_id = anim.surface_id.clone();
        let anchor_right = anim.anchor_right;
        let width = anim.current_width();
        let done = !anim.is_animating();

        // Keep `active_layer_resize` set to the eased width for the whole animation —
        // `override_active_layer_resize`, `get_layer_resize_offset` (edge-pin) and the
        // render path all read it, identical to a live drag grab.
        let max = output.geometry().size.w;
        self.active_layer_resize = Some(LayerResize {
            surface_id,
            output: output.clone(),
            anchor_right,
            width,
            min: MIN_PANEL_WIDTH,
            max,
        });
        self.override_active_layer_resize(&output);
        if layer_map_for_output(&output).arrange() {
            self.workspaces.recalculate();
        }

        if done {
            // Hold the final width (same as the end of a drag) so the edge-pin keeps
            // the anchored edge against the output until the client's wider buffer
            // lands; cleared by `clear_layer_resize_settle_if_caught_up`.
            self.layer_resize_settle = self.active_layer_resize.take();
            self.active_layer_resize_anim = None;
        }
    }

    /// Apply the animated exclusive zones of active slides to the layer maps
    /// and relayout the affected outputs. This ensures ALL surfaces (windows
    /// and other layer surfaces) animate smoothly with the panel.
    ///
    /// Skips all work when no slide's integer zone moved since the last
    /// application: this runs on every event-loop dispatch (input bursts,
    /// client commits), which is far more often than the eased zone moves a
    /// whole pixel — especially in the easing tail.
    fn apply_slide_exclusive_zones(&mut self) {
        // Drive the content crossfade every tick (it follows the eased motion,
        // so it must advance even when the integer zone hasn't moved).
        let fade = self
            .layer_slides
            .iter()
            .filter(|s| s.visibility.is_animating())
            .map(|s| s.visibility.remaining_fraction())
            .fold(0.0f32, f32::max);
        self.set_slide_fade(fade);

        let mut any_change = false;
        for slide in &mut self.layer_slides {
            if !slide.visibility.is_animating() {
                continue;
            }
            // Sample the live factor ONCE per tick and pin it as cached_factor,
            // which both render_offset (panel edge) and the window zone derive
            // from — so they're computed from one identical sample and never
            // drift between ticks (viewport-lag fix). Updated EVERY tick (not
            // only on a zone step): when surface_width == exclusive_zone (the
            // common case) render_offset's integer rounding only changes at the
            // same factor values the zone does, so they stay glued; but when the
            // panel buffer is WIDER than its reserved zone (a capped zone, e.g.
            // a very wide panel on a small output) the panel has finer pixel
            // granularity than the zone, and updating every tick lets it track
            // its own cadence instead of stair-stepping at the coarser zone rate.
            let factor = slide.visibility.factor();
            slide.cached_factor = factor;
            let ez = slide.ez_for_factor(factor);
            if slide.last_applied_ez != Some(ez) {
                slide.last_applied_ez = Some(ez);
                any_change = true;
            }
        }

        if !any_change {
            return;
        }

        // Collect the outputs hosting animating slide surfaces.
        let mut outputs_to_arrange: Vec<Output> = Vec::new();
        for slide in &self.layer_slides {
            if !slide.visibility.is_animating() {
                continue;
            }
            for output in self.outputs() {
                if outputs_to_arrange.contains(output) {
                    continue;
                }
                let map = layer_map_for_output(output);
                if map
                    .layers()
                    .any(|l| l.wl_surface().id() == slide.surface_id)
                {
                    outputs_to_arrange.push(output.clone());
                }
            }
        }

        // Mark floating layouts as slide-active BEFORE arrange, so any
        // recalculate triggered by zone changes skips configure sends.
        self.set_slide_active(true);

        // Write the animated zones and re-arrange so all layer surfaces
        // reposition, then relayout only the affected outputs' workspaces.
        for output in &outputs_to_arrange {
            self.override_slide_exclusive_zones(output);
            layer_map_for_output(output).arrange();
        }
        for output in &outputs_to_arrange {
            self.workspaces.recalculate_output(output);
        }
    }

    /// Set slide_active on all floating and tiling layouts (sticky + per-workspace).
    fn set_slide_active(&mut self, active: bool) {
        tracing::debug!(active, "[SLIDE] set_slide_active");
        for set in self.workspaces.sets.values_mut() {
            set.sticky_layer.slide_active = active;
            for workspace in &mut set.workspaces {
                workspace.floating_layer.slide_active = active;
                workspace.tiling_layer.slide_active = active;
            }
        }
    }

    /// Propagate the current slide crossfade fraction (1.0 = transition just
    /// started, 0.0 = settled) to all floating layouts.
    fn set_slide_fade(&mut self, fade: f32) {
        for set in self.workspaces.sets.values_mut() {
            set.sticky_layer.slide_fade = fade;
            for workspace in &mut set.workspaces {
                workspace.floating_layer.slide_fade = fade;
            }
        }
    }

    /// Mirror the current diagnostic slide epoch + start instant into every
    /// floating layout so the render path can stamp `[SLIDE_*]` logs/snapshots.
    /// Flush withheld slide-start configures across all floating layouts (see
    /// `FloatingLayout::flush_deferred_slide_configures`). Called every slide
    /// tick with `force_all=false` (sends each window's configure once its
    /// snapshot is captured) and once at settle with `force_all=true`.
    fn flush_deferred_slide_configures(&mut self, force_all: bool) {
        for set in self.workspaces.sets.values_mut() {
            set.sticky_layer.flush_deferred_slide_configures(force_all);
            for workspace in &mut set.workspaces {
                workspace
                    .floating_layer
                    .flush_deferred_slide_configures(force_all);
            }
        }
    }

    /// True while any floating layout holds a slide snapshot mid-crossfade. A
    /// crossfade can outlive the slide *motion* (the client may not commit its
    /// reflowed buffer until after the slide settles), so the redraw schedulers
    /// (`animations_going` / `animating_outputs`) consult this to keep rendering
    /// until the fade finishes — otherwise it freezes mid-dissolve (a blink).
    pub fn any_slide_fade_in_flight(&self) -> bool {
        self.workspaces.sets.values().any(|set| {
            set.sticky_layer.has_slide_fade_in_flight()
                || set
                    .workspaces
                    .iter()
                    .any(|w| w.floating_layer.has_slide_fade_in_flight())
        })
    }

    /// One-shot layout pass at the start (or reversal) of a layer slide:
    /// arrange the affected outputs at the slide's TERMINAL zone and send
    /// every floating window a single configure at its final size. The
    /// animation afterwards only moves visual bounds — clients render their
    /// final buffer once, and the render path crossfades the old content out
    /// over it, locked to the slide's motion. No further configures are sent
    /// until (at most) a no-op one when the slide settles.
    fn begin_slide_layout(&mut self, surface_id: &ObjectId) {
        use smithay::wayland::shell::wlr_layer::ExclusiveZone;

        let Some(slide) = self
            .layer_slides
            .iter()
            .find(|s| s.surface_id == *surface_id)
        else {
            return;
        };
        let terminal_ez = match slide.visibility {
            layer_slide::SlideVisibility::SlidingIn { .. }
            | layer_slide::SlideVisibility::Visible => slide.exclusive_zone.max(0),
            layer_slide::SlideVisibility::SlidingOut { .. }
            | layer_slide::SlideVisibility::Hidden => 0,
        };
        let initial_fade = slide.visibility.remaining_fraction();

        let outputs: Vec<Output> = self
            .outputs()
            .filter(|o| {
                layer_map_for_output(o)
                    .layers()
                    .any(|l| l.wl_surface().id() == *surface_id)
            })
            .cloned()
            .collect();
        if outputs.is_empty() {
            return;
        }

        // Final-layout pass with configures enabled. Written directly (not via
        // the reconcile helper, which would substitute the animated value).
        self.set_slide_active(false);
        for output in &outputs {
            let map = layer_map_for_output(output);
            for layer in map.layers() {
                if layer.wl_surface().id() == *surface_id {
                    with_states(layer.wl_surface(), |states| {
                        let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                        cached.current().exclusive_zone =
                            ExclusiveZone::Exclusive(terminal_ez as u32);
                    });
                }
            }
            drop(map);
            layer_map_for_output(output).arrange();
        }
        // Only floating layouts take part: tiled windows keep the existing
        // crop-and-configure-at-end flow (their buffers must keep covering the
        // animated slots). Every window that got a final-size configure is
        // armed for an old-content snapshot on the next render frame.
        for (output, set) in self.workspaces.sets.iter_mut() {
            if !outputs.contains(output) {
                continue;
            }
            // defer_configures=true: withhold the final-size configure until the
            // old content is snapshotted, so a fast client can't reflow ahead of
            // the capture (the slide-start snapshot race).
            let resized = set.sticky_layer.recalculate_collect_resized(true);
            set.sticky_layer.arm_slide_snapshots(resized);
            for workspace in &mut set.workspaces {
                let resized = workspace.floating_layer.recalculate_collect_resized(true);
                workspace.floating_layer.arm_slide_snapshots(resized);
            }
        }

        // Back to the animated state for visuals: re-apply the interpolated
        // zone immediately so no frame renders the final layout early.
        self.set_slide_active(true);
        self.set_slide_fade(initial_fade);
        for slide in &mut self.layer_slides {
            if slide.surface_id == *surface_id {
                slide.last_applied_ez = None;
            }
        }
        self.apply_slide_exclusive_zones();
    }

    /// Check if any floating/tiling layer still has slide_active set.
    fn is_slide_active(&self) -> bool {
        self.workspaces
            .sets
            .values()
            .any(|set| set.sticky_layer.slide_active)
    }

    /// Whether a surface is registered for compositor-driven auto-hide.
    pub fn is_auto_hide_surface(&self, surface: &WlSurface) -> bool {
        self.auto_hide_surfaces
            .iter()
            .any(|s| s.surface == *surface)
    }

    /// Re-evaluate auto-hide state for all outputs. Call after any
    /// maximize/fullscreen/un-maximize/un-fullscreen transition.
    pub fn refresh_auto_hide(&mut self) {
        if self.auto_hide_surfaces.is_empty() {
            return;
        }
        let outputs: Vec<Output> = self.outputs().cloned().collect();
        for output in outputs {
            self.update_auto_hide_for_output(&output);
        }
    }

    pub fn set_overview_mode(
        &mut self,
        enabled: Option<Trigger>,
        evlh: LoopHandle<'static, crate::state::State>,
    ) {
        if let Some(trigger) = enabled {
            if !matches!(
                self.overview_mode,
                OverviewMode::Started(_, _) | OverviewMode::Active(_)
            ) {
                if matches!(trigger, Trigger::KeyboardSwap(_, _)) {
                    self.swap_indicator = Some(SwapIndicator::new(evlh, self.theme.clone()));
                }
                self.overview_mode = OverviewMode::Started(trigger, Instant::now());
            }
        } else if matches!(
            self.overview_mode,
            OverviewMode::Started(_, _) | OverviewMode::Active(_)
        ) {
            let (reverse_duration, trigger) =
                if let OverviewMode::Started(trigger, start) = self.overview_mode.clone() {
                    (
                        self.theme.motion.animation
                            - Instant::now()
                                .duration_since(start)
                                .min(self.theme.motion.animation),
                        Some(trigger),
                    )
                } else {
                    (Duration::ZERO, self.overview_mode.active_trigger().cloned())
                };
            self.overview_mode = OverviewMode::Ended(trigger, Instant::now() - reverse_duration);
        }
    }

    pub fn overview_mode(&self) -> (OverviewMode, Option<SwapIndicator>) {
        if let OverviewMode::Started(trigger, timestamp) = &self.overview_mode
            && Instant::now().duration_since(*timestamp) > self.theme.motion.animation
        {
            return (
                OverviewMode::Active(trigger.clone()),
                self.swap_indicator.clone(),
            );
        }
        if let OverviewMode::Ended(_, timestamp) = &self.overview_mode
            && Instant::now().duration_since(*timestamp) > self.theme.motion.animation
        {
            return (OverviewMode::None, None);
        }

        (self.overview_mode.clone(), self.swap_indicator.clone())
    }

    /// Check if compositor is in home mode (active or transitioning to active)
    pub fn is_home(&self) -> bool {
        self.home_mode.is_active()
    }

    /// Get the current home mode state
    pub fn home_mode(&self) -> &HomeMode {
        &self.home_mode
    }

    /// Get the current opacity for home-only surfaces (0.0-1.0)
    pub fn home_alpha(&self) -> f32 {
        self.home_mode.alpha(self.theme.motion.animation)
    }

    /// Exit home mode visually only (fade out home surfaces without restoring windows)
    /// Use this when another mode like voice mode takes over
    pub fn exit_home_visual_only(&mut self) {
        self.home_mode.exit(self.theme.motion.animation);
    }

    /// Enter home mode (with animation) and minimize all windows
    pub fn enter_home(&mut self) {
        self.home_mode.enter(self.theme.motion.animation);

        // If voice mode is active and attached to a window, transition to floating
        // since the window will be minimized
        if self.voice_orb_state.orb_state
            == crate::wayland::protocols::voice_mode::OrbState::Attached
        {
            self.voice_orb_state.transition_to_floating();
            self.voice_mode.enter(); // Enable window fading (even though we're going to home)
        }

        // Minimize all visible windows across all workspaces
        self.minimize_all_windows();
    }

    /// Minimize all visible windows without entering home mode.
    ///
    /// Use this when you want to clear the screen without triggering
    /// the home mode animation.
    pub fn minimize_all_windows_only(&mut self) {
        // If voice mode is active and attached to a window, transition to floating
        // since the window will be minimized
        if self.voice_orb_state.orb_state
            == crate::wayland::protocols::voice_mode::OrbState::Attached
        {
            self.voice_orb_state.transition_to_floating();
        }

        // Minimize all visible windows across all workspaces
        self.minimize_all_windows();
    }

    /// Minimize all visible windows across all workspaces
    fn minimize_all_windows(&mut self) {
        // Clear any previously tracked surfaces
        self.home_minimized_surfaces.clear();

        // Collect all surfaces to minimize first (to avoid borrow conflicts)
        let mut surfaces_to_minimize = Vec::new();

        for set in self.workspaces.sets.values() {
            // Collect from sticky layer
            for mapped in set.sticky_layer.mapped() {
                surfaces_to_minimize.push(mapped.active_window());
            }

            // Collect from all workspaces
            for workspace in &set.workspaces {
                // Fullscreen windows
                for fullscreen in &workspace.fullscreen_surfaces {
                    surfaces_to_minimize.push(fullscreen.surface.clone());
                }

                // Mapped windows
                for mapped in workspace.mapped() {
                    surfaces_to_minimize.push(mapped.active_window());
                }
            }
        }

        // Store surfaces for restoration and minimize each one
        self.home_minimized_surfaces = surfaces_to_minimize.clone();
        for surface in surfaces_to_minimize {
            self.minimize_request(&surface);
        }
    }

    /// Exit home mode (with animation) and restore previously minimized windows
    pub fn exit_home(&mut self, seat: &Seat<State>, loop_handle: &LoopHandle<'static, State>) {
        self.home_mode.exit(self.theme.motion.animation);

        // Restore windows that were minimized by home mode
        let surfaces_to_restore = std::mem::take(&mut self.home_minimized_surfaces);
        for surface in surfaces_to_restore {
            self.unminimize_request(&surface, seat, loop_handle);
        }
    }

    /// Update home mode animation state
    pub fn update_home_animation(&mut self) {
        self.home_mode.update(self.theme.motion.animation);
    }

    /// Check if home mode animation is in progress
    pub fn home_animation_going(&self) -> bool {
        self.home_mode.is_animating(self.theme.motion.animation)
    }

    /// Get the set of home-only surface IDs
    pub fn home_only_surfaces(&self) -> &std::collections::HashSet<ObjectId> {
        &self.home_only_surfaces
    }

    /// Get the set of hide-on-home surface IDs
    pub fn hide_on_home_surfaces(&self) -> &std::collections::HashSet<ObjectId> {
        &self.hide_on_home_surfaces
    }

    /// Set a surface's visibility mode
    pub fn set_surface_visibility_mode(
        &mut self,
        surface_id: ObjectId,
        mode: crate::wayland::protocols::home_visibility::VisibilityMode,
    ) {
        use crate::wayland::protocols::home_visibility::VisibilityMode;
        // Remove from both sets first
        self.home_only_surfaces.remove(&surface_id);
        self.hide_on_home_surfaces.remove(&surface_id);

        // Add to appropriate set based on mode
        match mode {
            VisibilityMode::HomeOnly => {
                self.home_only_surfaces.insert(surface_id);
            }
            VisibilityMode::HideOnHome => {
                self.hide_on_home_surfaces.insert(surface_id);
            }
            VisibilityMode::Always => {
                // Already removed from both sets
            }
        }
    }

    /// Remove a surface from visibility tracking
    pub fn remove_surface_visibility(&mut self, surface_id: ObjectId) {
        self.home_only_surfaces.remove(&surface_id);
        self.hide_on_home_surfaces.remove(&surface_id);
    }

    // Client-hidden surface methods (layer_surface_visibility protocol)

    /// Get the set of explicitly hidden surface IDs
    pub fn hidden_surfaces(&self) -> &std::collections::HashSet<ObjectId> {
        &self.hidden_surfaces
    }

    /// Set a surface's show/hide transition (via layer_surface_visibility protocol).
    /// Surfaces that never call this fall back to the anchor-based heuristic.
    pub fn set_surface_transition(
        &mut self,
        surface_id: ObjectId,
        transition: crate::wayland::protocols::layer_surface_visibility::LayerTransition,
    ) {
        tracing::debug!(?surface_id, ?transition, "set_surface_transition");
        self.layer_transitions.insert(surface_id, transition);
    }

    /// Set a surface's hidden state (via layer_surface_visibility protocol)
    pub fn set_surface_hidden(&mut self, surface_id: ObjectId, hidden: bool) {
        use crate::wayland::protocols::layer_surface_visibility::LayerTransition;
        // Snapshot the theme's motion tokens once; the open/close animations
        // capture this so their sampling needs no theme handle.
        let motion = self.theme.motion;
        // Decide whether to slide or fade. A surface that explicitly requested
        // `Fade` never slides (even if edge-anchored); otherwise fall back to
        // the anchor-based heuristic.
        let slide_edge = match self.layer_transitions.get(&surface_id) {
            Some(LayerTransition::Fade) => None,
            _ => self.detect_layer_slide_edge(&surface_id),
        };

        // Hiding cancels any in-flight OPEN (entrance) animation. For a fade+rise
        // surface we first capture how "open" it currently is, so the close can start
        // from there (a seamless reverse) instead of snapping to full-open and
        // popping. `close_backdate_ms` is how far to back-date the LayerClose:
        //   - mid-open at linear progress p → (1 - p) · motion.layer_open (symmetry)
        //   - never shown (still pending first commit) → motion.layer_open (already hidden)
        //   - fully open / resting → 0 (full close from the top)
        let close_backdate_ms: u64 = if hidden {
            let backdate =
                if let Some(o) = self.layer_opens.iter().find(|o| o.surface_id == surface_id) {
                    let p = (o.start.elapsed().as_secs_f32() / motion.layer_open.as_secs_f32())
                        .clamp(0.0, 1.0);
                    ((1.0 - p) * motion.layer_open.as_millis() as f32) as u64
                } else if self.pending_layer_opens.contains(&surface_id) {
                    motion.layer_open.as_millis() as u64
                } else {
                    0
                };
            self.remove_layer_open(&surface_id);
            backdate
        } else {
            0
        };

        if let Some((edge, width, exclusive_zone)) = slide_edge {
            // Use slide animation for side-anchored panels
            if hidden {
                let was_fading_in = self.layer_fade_in.remove(&surface_id).is_some();
                let was_pending = self.pending_layer_fade_in.remove(&surface_id);
                tracing::debug!(
                    ?surface_id,
                    ?edge,
                    width,
                    exclusive_zone,
                    was_fading_in,
                    was_pending,
                    "set_surface_hidden(true): starting slide-out"
                );
                // Check if already sliding
                if let Some(existing) = self
                    .layer_slides
                    .iter_mut()
                    .find(|s| s.surface_id == surface_id)
                {
                    existing.visibility.start_hide(motion);
                } else {
                    let mut slide = layer_slide::LayerSlide::new(
                        surface_id.clone(),
                        edge,
                        width,
                        exclusive_zone,
                    );
                    slide.visibility.start_hide(motion);
                    self.layer_slides.push(slide);
                }
                // Configure all floating windows at their final size right
                // away; the animation only moves visual bounds from here.
                self.begin_slide_layout(&surface_id);
            } else {
                let was_hidden = self.hidden_surfaces.remove(&surface_id);
                let was_sliding_out = self.layer_slides.iter().any(|s| {
                    s.surface_id == surface_id
                        && matches!(
                            s.visibility,
                            layer_slide::SlideVisibility::SlidingOut { .. }
                                | layer_slide::SlideVisibility::Hidden
                        )
                });
                tracing::debug!(
                    ?surface_id,
                    ?edge,
                    width,
                    exclusive_zone,
                    was_hidden,
                    was_sliding_out,
                    "set_surface_hidden(false): starting slide-in"
                );
                // Only start slide-in if the surface was actually hidden or sliding out
                if was_hidden || was_sliding_out {
                    if let Some(existing) = self
                        .layer_slides
                        .iter_mut()
                        .find(|s| s.surface_id == surface_id)
                    {
                        existing.visibility.start_show(motion);
                    } else {
                        let mut slide = layer_slide::LayerSlide::new_hidden(
                            surface_id.clone(),
                            edge,
                            width,
                            exclusive_zone,
                        );
                        slide.visibility.start_show(motion);
                        self.layer_slides.push(slide);
                    }
                    // Configure all floating windows at their final size right
                    // away; the animation only moves visual bounds from here.
                    self.begin_slide_layout(&surface_id);
                }
            }
        } else {
            // Fade + rise: the DEFAULT animation for every surface that isn't
            // edge-sliding. Hiding plays the close (slide-DOWN + scale-DOWN +
            // fade-OUT); showing plays the open (slide-UP + scale-UP + fade-IN) —
            // the exact reverse, each from a single eased factor (see
            // `layer_open`). Alpha is funneled through `layer_open`/`layer_close`
            // (not the plain `layer_fade_in`/`layer_fade_out` timers) so it stays
            // perfectly synced with the translate + scale.
            if hidden {
                let was_fading_in = self.layer_fade_in.remove(&surface_id).is_some();
                let was_pending = self.pending_layer_fade_in.remove(&surface_id);
                // Any in-flight open was already folded into `close_backdate_ms`
                // and removed above via `remove_layer_open`.
                tracing::debug!(
                    ?surface_id,
                    was_fading_in,
                    was_pending,
                    "set_surface_hidden(true): starting close (slide-down) animation"
                );
                self.rise_surfaces.insert(surface_id.clone());
                self.layer_closes.retain(|c| c.surface_id != surface_id);
                self.layer_closes
                    .push(layer_open::LayerClose::new_backdated(
                        surface_id.clone(),
                        close_backdate_ms,
                        motion,
                    ));
            } else {
                let was_hidden = self.hidden_surfaces.remove(&surface_id);
                let was_fading_out = self.layer_fade_out.remove(&surface_id).is_some();
                // A close started by a just-superseded hide may still be in
                // flight; capture how far it got so we can reverse it instead of
                // letting it finish hiding the surface despite this show.
                let reversing_close = self
                    .layer_closes
                    .iter()
                    .find(|c| c.surface_id == surface_id)
                    .map(|c| {
                        (c.start.elapsed().as_secs_f32() / motion.layer_open.as_secs_f32())
                            .clamp(0.0, 1.0)
                    });
                self.rise_surfaces.insert(surface_id.clone());
                tracing::debug!(
                    ?surface_id,
                    was_hidden,
                    was_fading_out,
                    reversing_close = reversing_close.is_some(),
                    "set_surface_hidden(false): starting open (slide-up) animation"
                );
                if was_hidden {
                    // Defer the rise-in until the surface commits its first buffer
                    // (so neither content nor blur shows a stale frame). Held at
                    // alpha 0 until then by `layer_fade_in_alphas`.
                    self.pending_layer_opens.insert(surface_id);
                } else if let Some(close_progress) = reversing_close {
                    // Reverse the in-flight close into an open. Back-date the open
                    // so its first frame matches the close's current alpha/scale/
                    // offset (the easing is point-symmetric about (0.5, 0.5)),
                    // then it rises the rest of the way — no jump.
                    let backdate =
                        ((1.0 - close_progress) * motion.layer_open.as_millis() as f32) as u64;
                    self.layer_closes.retain(|c| c.surface_id != surface_id);
                    self.layer_opens.retain(|o| o.surface_id != surface_id);
                    self.layer_opens.push(layer_open::LayerOpen::new_backdated(
                        surface_id.clone(),
                        backdate,
                        motion,
                    ));
                } else if was_fading_out {
                    // Legacy plain fade-out still in flight — rise in from scratch.
                    self.layer_opens.retain(|o| o.surface_id != surface_id);
                    self.layer_opens
                        .push(layer_open::LayerOpen::new(surface_id, motion));
                }
            }
        }
    }

    /// Whether a layer surface fills its whole output (anchored to all four
    /// edges) — a full-screen backdrop / modal scrim. Scaling or rising such a
    /// surface about its centre only reveals gaps at the screen edges, so the
    /// open/close animations skip the scale + translate for it (keeping just the
    /// alpha fade), giving a clean fade-in with no "scale-in" wobble.
    fn is_full_output_layer(&self, surface_id: &ObjectId) -> bool {
        use smithay::wayland::shell::wlr_layer::Anchor;
        for output in self.outputs() {
            let map = layer_map_for_output(output);
            for layer in map.layers() {
                if layer.wl_surface().id() == *surface_id {
                    let anchor = with_states(layer.wl_surface(), |states| {
                        let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                        cached.current().anchor
                    });
                    return anchor.contains(Anchor::TOP)
                        && anchor.contains(Anchor::BOTTOM)
                        && anchor.contains(Anchor::LEFT)
                        && anchor.contains(Anchor::RIGHT);
                }
            }
        }
        false
    }

    /// Detect if a layer surface is anchored to a single lateral edge (Left or Right)
    /// and should use slide animation. Returns the edge and surface width if so.
    fn detect_layer_slide_edge(
        &self,
        surface_id: &ObjectId,
    ) -> Option<(layer_slide::SlideEdge, i32, i32)> {
        use smithay::wayland::shell::wlr_layer::{Anchor, ExclusiveZone};

        for output in self.outputs() {
            let map = layer_map_for_output(output);
            for layer in map.layers() {
                if layer.wl_surface().id() == *surface_id {
                    let (anchor, size, cached_ez) = with_states(layer.wl_surface(), |states| {
                        let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                        let current = cached.current();
                        (current.anchor, current.size, current.exclusive_zone)
                    });
                    // Prefer the client's recorded commit over cached state —
                    // the cache may hold a stale animation override (e.g. ~0
                    // after a completed slide-out), which would start the
                    // slide with no zone to animate.
                    let exclusive_zone = match self
                        .client_exclusive_zones
                        .get(surface_id)
                        .copied()
                        .unwrap_or(cached_ez)
                    {
                        ExclusiveZone::Exclusive(v) => v as i32,
                        _ => 0,
                    };
                    tracing::debug!(
                        ?surface_id,
                        ?anchor,
                        ?size,
                        exclusive_zone,
                        "detect_layer_slide_edge: found surface in layer map"
                    );
                    // Detect right-anchored panel: has RIGHT but not LEFT
                    // (typically RIGHT | TOP | BOTTOM for a full-height side panel)
                    if anchor.contains(Anchor::RIGHT) && !anchor.contains(Anchor::LEFT) {
                        let width = if size.w > 0 {
                            size.w
                        } else {
                            // Fallback to actual geometry
                            map.layer_geometry(layer).map(|g| g.size.w).unwrap_or(0)
                        };
                        if width > 0 {
                            return Some((layer_slide::SlideEdge::Right, width, exclusive_zone));
                        }
                    }
                    // Detect left-anchored panel: has LEFT but not RIGHT
                    if anchor.contains(Anchor::LEFT) && !anchor.contains(Anchor::RIGHT) {
                        let width = if size.w > 0 {
                            size.w
                        } else {
                            map.layer_geometry(layer).map(|g| g.size.w).unwrap_or(0)
                        };
                        if width > 0 {
                            return Some((layer_slide::SlideEdge::Left, width, exclusive_zone));
                        }
                    }
                    return None;
                }
            }
        }
        tracing::debug!(
            ?surface_id,
            "detect_layer_slide_edge: surface NOT found in any layer map"
        );
        None
    }

    /// Check if a surface is explicitly hidden
    pub fn is_surface_hidden(&self, surface_id: &ObjectId) -> bool {
        self.hidden_surfaces.contains(surface_id)
    }

    /// Check if a layer surface was created without a specific output
    pub fn is_output_agnostic_layer(&self, surface_id: &ObjectId) -> bool {
        self.output_agnostic_layers.contains(surface_id)
    }

    /// Remove a surface from hidden tracking (called when surface is destroyed)
    pub fn remove_hidden_surface(&mut self, surface_id: &ObjectId) {
        self.hidden_surfaces.remove(surface_id);
        self.layer_transitions.remove(surface_id);
    }

    /// Record the exclusive zone a layer surface's client last committed,
    /// before any compositor override is applied for animations.
    pub fn record_client_exclusive_zone(
        &mut self,
        surface_id: ObjectId,
        zone: smithay::wayland::shell::wlr_layer::ExclusiveZone,
    ) {
        self.client_exclusive_zones.insert(surface_id, zone);
    }

    /// Forget a destroyed layer surface's recorded exclusive zone.
    pub fn remove_client_exclusive_zone(&mut self, surface_id: &ObjectId) {
        self.client_exclusive_zones.remove(surface_id);
    }

    // Layer fade-in methods

    /// Get the map of layer surfaces currently fading in with their alpha values (read-only).
    pub fn layer_fade_in_alphas(&self) -> std::collections::HashMap<ObjectId, f32> {
        let now = Instant::now();
        let mut result: std::collections::HashMap<ObjectId, f32> = self
            .layer_fade_in
            .iter()
            .filter_map(|(surface_id, start)| {
                let elapsed = now.saturating_duration_since(*start);
                let progress = (elapsed.as_secs_f32()
                    / self.theme.motion.layer_fade_in.as_secs_f32())
                .clamp(0.0, 1.0);
                if progress >= 1.0 {
                    None
                } else {
                    // Ease-out cubic
                    let eased = 1.0 - (1.0 - progress).powi(3);
                    tracing::debug!(
                        ?surface_id,
                        elapsed_ms = elapsed.as_millis(),
                        progress = format!("{:.3}", progress),
                        alpha = format!("{:.3}", eased),
                        "layer_fade_in_alphas: surface fading in"
                    );
                    Some((surface_id.clone(), eased))
                }
            })
            .collect();
        // Surfaces waiting for a buffer commit before their fade-in starts
        // are held at alpha=0 so neither content nor blur is visible yet.
        for surface_id in &self.pending_layer_fade_in {
            result.entry(surface_id.clone()).or_insert(0.0);
        }
        // Open animations drive their alpha (0→1) from the SAME single eased
        // factor as their translate/scale. We funnel it through this map so
        // order.rs applies it to BOTH the surface alpha and the blur alpha — they
        // fade in perfectly synced with the slide-up + scale. (We suppressed the
        // plain layer_fade_in for these surfaces, so there is no double-fade.)
        for open in &self.layer_opens {
            result.insert(open.surface_id.clone(), open.alpha());
        }
        // Surfaces still waiting for their first buffer commit are held at 0.
        for surface_id in &self.pending_layer_opens {
            result.entry(surface_id.clone()).or_insert(0.0);
        }
        if !result.is_empty() {
            tracing::debug!(
                count = result.len(),
                "layer_fade_in_alphas: returning fading surfaces"
            );
        }
        result
    }

    /// Remove completed layer fade-in entries (called from update_animations)
    fn cleanup_layer_fade_ins(&mut self) {
        let now = Instant::now();
        self.layer_fade_in.retain(|_, start| {
            let elapsed = now.saturating_duration_since(*start);
            elapsed < self.theme.motion.layer_fade_in
        });
    }

    /// Remove completed open animations (called from update_animations).
    /// When an open finishes, the surface renders bare (scale 1.0 / alpha 1.0)
    /// because `is_layer_opening` returns false and it's no longer in the alpha map.
    fn cleanup_layer_opens(&mut self) {
        self.layer_opens.retain(|o| o.is_animating());
    }

    /// Complete close animations: when one finishes (160ms elapsed),
    /// move the surface to `hidden_surfaces` so it stops rendering (the client
    /// destroys it shortly after via `RemoveWindow`). Returns the completed IDs
    /// so the caller can refresh the layer blur cache (same as fade-outs).
    fn cleanup_layer_closes(&mut self) -> Vec<ObjectId> {
        let mut completed = Vec::new();
        self.layer_closes.retain(|c| {
            if c.is_animating() {
                true
            } else {
                tracing::debug!(
                    surface_id = ?c.surface_id,
                    "cleanup_layer_closes: close complete, moving to hidden_surfaces"
                );
                completed.push(c.surface_id.clone());
                false
            }
        });
        for surface_id in &completed {
            self.hidden_surfaces.insert(surface_id.clone());
        }
        completed
    }

    /// Remove a surface's close animation + rise registration (called when the
    /// surface is destroyed). Safe to call mid-animation: drops a still-running
    /// close so nothing lingers if the client destroys the surface early.
    pub fn remove_layer_close(&mut self, surface_id: &ObjectId) {
        self.layer_closes.retain(|c| c.surface_id != *surface_id);
        self.rise_surfaces.remove(surface_id);
    }

    /// Get the map of layer surfaces currently fading out with their alpha values.
    /// Alpha goes from 1.0 → 0.0 over the layer fade-out duration.
    pub fn layer_fade_out_alphas(&self) -> std::collections::HashMap<ObjectId, f32> {
        let now = Instant::now();
        let mut result: std::collections::HashMap<ObjectId, f32> = self
            .layer_fade_out
            .iter()
            .filter_map(|(surface_id, start)| {
                let elapsed = now.saturating_duration_since(*start);
                let progress = (elapsed.as_secs_f32()
                    / self.theme.motion.layer_fade_out.as_secs_f32())
                .clamp(0.0, 1.0);
                if progress >= 1.0 {
                    None
                } else {
                    // Ease-in cubic (decelerating fade)
                    let eased = 1.0 - progress.powi(3);
                    Some((surface_id.clone(), eased))
                }
            })
            .collect();
        // Close animations drive their alpha (1→0) from the SAME single
        // eased factor as their slide-down/scale-down. Funnel it through this map
        // so order.rs marks the surface visible (`is_fading_out`) and syncs the
        // blur alpha — it fades out perfectly in step with the slide + scale.
        for close in &self.layer_closes {
            result.insert(close.surface_id.clone(), close.alpha());
        }
        result
    }

    /// Complete fade-outs that have finished: move to hidden_surfaces
    fn cleanup_layer_fade_outs(&mut self) -> Vec<ObjectId> {
        let now = Instant::now();
        let mut completed = Vec::new();
        self.layer_fade_out.retain(|surface_id, start| {
            let elapsed = now.saturating_duration_since(*start);
            if elapsed >= self.theme.motion.layer_fade_out {
                tracing::debug!(
                    ?surface_id,
                    elapsed_ms = elapsed.as_millis(),
                    "cleanup_layer_fade_outs: fade-out complete, moving to hidden_surfaces"
                );
                completed.push(surface_id.clone());
                false
            } else {
                true
            }
        });
        for surface_id in &completed {
            self.hidden_surfaces.insert(surface_id.clone());
        }
        completed
    }

    /// Remove a surface from fade-in tracking (called when surface is destroyed)
    pub fn remove_layer_fade_in(&mut self, surface_id: &ObjectId) {
        self.layer_fade_in.remove(surface_id);
        self.pending_layer_fade_in.remove(surface_id);
    }

    /// Remove a surface from open-animation tracking (called when the
    /// surface is destroyed).
    pub fn remove_layer_open(&mut self, surface_id: &ObjectId) {
        self.layer_opens.retain(|o| o.surface_id != *surface_id);
        self.pending_layer_opens.remove(surface_id);
    }

    /// Remove a surface from fade-out tracking (called when surface is destroyed)
    pub fn remove_layer_fade_out(&mut self, surface_id: &ObjectId) {
        self.layer_fade_out.remove(surface_id);
    }

    /// Activate a pending fade-in for a surface.
    /// Called from the compositor `commit()` handler when a layer surface commits
    /// a buffer.  If the surface has a pending fade-in (was just un-hidden),
    /// this starts the actual animation so the blur fades in together with the
    /// freshly rendered content.
    pub fn activate_pending_fade_in(&mut self, surface_id: &ObjectId) {
        if self.pending_layer_fade_in.remove(surface_id) {
            tracing::debug!(
                ?surface_id,
                "activate_pending_fade_in: starting blur fade-in on buffer commit"
            );
            self.layer_fade_in
                .insert(surface_id.clone(), Instant::now());
        }

        // Rise surfaces start their compositor-side open animation on the SAME
        // first-buffer-commit hook (auto_size means geometry is only valid now,
        // and a re-shown surface must render its first frame first). Replace any
        // stale entry so a re-show restarts the animation cleanly.
        if self.pending_layer_opens.remove(surface_id) {
            tracing::debug!(
                ?surface_id,
                "activate_pending_fade_in: starting open (slide-up) animation on buffer commit"
            );
            self.layer_opens.retain(|o| o.surface_id != *surface_id);
            let motion = self.theme.motion;
            self.layer_opens
                .push(layer_open::LayerOpen::new(surface_id.clone(), motion));
        }
    }

    /// Restart the fade-in timer for a surface.
    /// Used when blur is first committed: the original timer starts at map_layer()
    /// time which is before the client commits any buffer, so by the time blur
    /// is ready the animation has expired. Restarting ensures a visible fade-in.
    /// Unconditionally inserts because the original entry may have already been
    /// cleaned up by cleanup_layer_fade_ins() if the animation expired.
    pub fn restart_layer_fade_in(&mut self, surface_id: ObjectId) {
        let is_already_fading_in = self.layer_fade_in.contains_key(&surface_id);
        let is_fading_out = self.layer_fade_out.contains_key(&surface_id);
        let is_pending = self.pending_layer_fade_in.contains(&surface_id);
        let is_hidden = self.hidden_surfaces.contains(&surface_id);

        // Only restart when the surface is pending its first fade-in or
        // reversing from a fade-out.  Skip when already fading in (resetting
        // the timer would drop alpha back to 0 and cause a visible blink) and
        // when fully visible (blur region update, no animation needed).
        if !is_pending && !is_fading_out {
            tracing::trace!(
                ?surface_id,
                is_already_fading_in,
                is_hidden,
                "restart_layer_fade_in: skipping — surface is {} fading in or fully visible",
                if is_already_fading_in {
                    "already"
                } else {
                    "not"
                }
            );
            return;
        }

        tracing::trace!(
            ?surface_id,
            is_already_fading_in,
            is_fading_out,
            is_pending,
            is_hidden,
            "restart_layer_fade_in: restarting fade-in for blur"
        );
        self.layer_fade_in.insert(surface_id, Instant::now());
    }

    // Voice mode methods

    /// Check if voice mode is currently active
    /// This checks both the orb visibility AND the voice mode state machine,
    /// so it returns true even during the window fade-in before the orb appears.
    pub fn is_voice_mode_active(&self) -> bool {
        self.voice_orb_state.is_active() || self.voice_mode.is_active()
    }

    /// Update voice mode animation state
    pub fn update_voice_mode_animation(&mut self) {
        self.voice_orb_state.update();
    }

    /// Enter voice mode (fade out windows)
    pub fn enter_voice_mode(&mut self) {
        self.voice_mode.enter();
    }

    /// Exit voice mode (fade in windows)
    pub fn exit_voice_mode(&mut self) {
        self.voice_mode.exit();
    }

    /// Exit voice mode immediately from attached state (no fade animation)
    pub fn exit_voice_mode_from_attached(&mut self) {
        self.voice_mode.exit_from_attached();
    }

    /// Get a debug representation of the current voice mode state
    pub fn voice_mode_debug(&self) -> String {
        format!("{:?}", self.voice_mode)
    }

    /// Fade windows in immediately (for attached mode transitions)
    pub fn voice_mode_fade_in_immediately(&mut self) {
        self.voice_mode.fade_in_immediately();
    }

    /// Handle focus change for voice mode - transitions between floating and attached orb
    /// Returns true if voice mode active and transition occurred
    /// Note: The actual receiver check is done at the protocol level via VoiceModeState
    pub fn handle_voice_mode_focus_change(
        &mut self,
        focused_element: Option<&CosmicMapped>,
        output: &Output,
        has_voice_receiver: bool,
    ) -> bool {
        // Only process if voice mode is active
        if !self.voice_orb_state.is_active() {
            return false;
        }

        let output_geo = output.geometry();

        match (has_voice_receiver, self.voice_orb_state.orb_state) {
            // Receiver window focused and orb is floating -> transition to attached
            (true, crate::wayland::protocols::voice_mode::OrbState::Floating) => {
                if let Some(mapped) = focused_element {
                    use smithay::desktop::space::SpaceElement;
                    let window_geo = SpaceElement::geometry(mapped);
                    let output_size = output_geo.size.as_logical();
                    self.voice_orb_state
                        .transition_to_attached(window_geo, output_size);
                    // Fade windows back in immediately (orb is bursting behind window)
                    self.voice_mode.fade_in_immediately();
                    tracing::debug!("Voice orb: transitioning to attached mode");
                    return true;
                }
            }
            // Receiver window focused and orb is frozen -> depends on prior state
            (true, crate::wayland::protocols::voice_mode::OrbState::Frozen) => {
                if self.voice_orb_state.frozen_was_attached {
                    // Orb was attached before freezing — stay frozen in place.
                    // The client will send voice_dismiss when transcription completes,
                    // which triggers dismiss_orb → request_hide → shrink animation.
                    tracing::debug!(
                        "Voice orb: frozen (was_attached) - ignoring focus change, waiting for client dismiss"
                    );
                    return true;
                }
                // Orb was floating before freezing — a new receiver window just opened.
                // Attach to it and fade out (e.g. chat-ui opens after desktop voice).
                if let Some(mapped) = focused_element {
                    use smithay::desktop::space::SpaceElement;
                    let window_geo = SpaceElement::geometry(mapped);
                    let output_size = output_geo.size.as_logical();
                    let surface_id = mapped
                        .active_window()
                        .wl_surface()
                        .map(|s| s.id().to_string())
                        .unwrap_or_default();
                    self.voice_orb_state.start_attach_and_transition(
                        window_geo,
                        output_size,
                        surface_id,
                    );
                    self.voice_mode.fade_in_immediately();
                    tracing::debug!(
                        "Voice orb: frozen (was_floating) -> attach_and_transition to newly focused window"
                    );
                    return true;
                }
            }
            // Receiver window lost focus and orb is attached -> transition to floating
            (false, crate::wayland::protocols::voice_mode::OrbState::Attached) => {
                self.voice_orb_state.transition_to_floating();
                // Enable window fading when floating
                self.voice_mode.enter();
                tracing::debug!("Voice orb: transitioning to floating mode");
                return true;
            }
            _ => {}
        }

        false
    }

    /// Get the current window alpha for voice mode (1.0 = full, 0.0 = hidden)
    /// When orb is attached to a window, windows are visible (orb bursts behind window)
    /// When orb is floating or frozen, windows are hidden
    pub fn voice_mode_window_alpha(&self) -> f32 {
        use crate::wayland::protocols::voice_mode::OrbState;

        // Windows stay visible only when attached or transitioning (orb behind window)
        // or when shrinking from attached state (window was already visible)
        // or when frozen from attached mode (windows were already visible before freeze)
        if self.voice_orb_state.orb_state == OrbState::Attached
            || self.voice_orb_state.orb_state == OrbState::Transitioning
            || self.voice_orb_state.shrinking_from_attached
            || (self.voice_orb_state.orb_state == OrbState::Frozen
                && self.voice_orb_state.frozen_was_attached)
        {
            // When attached/transitioning, windows should be visible
            // But respect the animation state for smooth transitions
            match &self.voice_mode {
                // Use animation alpha during FadingOut (fade-in animation)
                VoiceMode::FadingOut(_) => self.voice_mode.window_alpha(),
                // After animation complete or not started, windows fully visible
                VoiceMode::None => 1.0,
                // Still in other states but attached - show windows immediately
                // (this handles edge cases like attaching during FadingIn)
                _ => 1.0,
            }
        } else {
            // Floating, frozen, or hidden - use normal voice mode alpha (fades windows out)
            // BUT: if orb is hidden and we're fading out (transitioning to None),
            // windows should be visible (alpha = 1.0) because we're exiting voice mode.
            // The FadingOut animation is for when we're coming from floating/frozen mode
            // where windows were hidden and need to fade back in.
            // When attached mode exits quickly (scale was already 0), the window
            // was never hidden, so don't apply the fade animation.
            if self.voice_orb_state.orb_state == OrbState::Hidden
                && let VoiceMode::FadingOut(_) = &self.voice_mode
            {
                // Check if we were in attached mode before by seeing if attached_window is set
                // If attached_window is still Some, we were attached and window was visible
                if self.voice_orb_state.attached_window.is_some() {
                    return 1.0;
                }
            }

            self.voice_mode.window_alpha()
        }
    }

    /// Get the current layer shell alpha for voice mode (1.0 = full, 0.0 = hidden)
    /// Layer shells stay hidden during the BurstThenFadeOut animation so windows fade in first,
    /// then layer shells fade in after the burst completes.
    pub fn voice_mode_layer_shell_alpha(&self) -> f32 {
        // During BurstThenFadeOut animation, keep layer shells hidden until complete
        if self.voice_orb_state.is_in_burst_then_fade_out() {
            return 0.0;
        }

        // Otherwise, use the same alpha as windows
        self.voice_mode_window_alpha()
    }

    /// Check if voice mode animation is in progress
    pub fn voice_mode_animating(&self) -> bool {
        self.voice_mode.is_animating()
    }

    /// Update voice mode fade animation state and coordinate orb animation sequence
    pub fn update_voice_mode_fade(&mut self) {
        // Check if we should start showing the orb (window fade completed)
        if self.voice_mode.should_show_orb() && self.voice_orb_state.has_pending_show() {
            // Choose show method based on orb state
            if self.voice_orb_state.orb_state
                == crate::wayland::protocols::voice_mode::OrbState::Attached
            {
                self.voice_orb_state.show_attached();
            } else {
                self.voice_orb_state.show_floating();
            }
        }

        // Check if we should start hiding the orb (exit requested)
        if self.voice_mode.should_hide_orb() && self.voice_orb_state.has_pending_hide() {
            self.voice_orb_state.hide();
            self.voice_orb_state.clear_pending_hide();
        }

        // Update the voice mode state machine
        self.voice_mode.update();
    }

    /// Check if a surface should be rendered (for filtering)
    /// Returns (visible, alpha) where alpha is for animation blending
    pub fn surface_home_visibility(&self, surface_id: &ObjectId) -> (bool, f32) {
        if self.home_only_surfaces.contains(surface_id) {
            // Home-only surface: visible when at home or animating
            let alpha = self.home_mode.alpha(self.theme.motion.animation);
            (alpha > 0.0, alpha)
        } else if self.hide_on_home_surfaces.contains(surface_id) {
            // Hide-on-home surface: visible when NOT at home (inverse alpha)
            let alpha = 1.0 - self.home_mode.alpha(self.theme.motion.animation);
            (alpha > 0.0, alpha)
        } else {
            // Always-visible surface (default)
            (true, 1.0)
        }
    }

    /// Check if a surface should be rendered (for filtering)
    pub fn should_surface_be_visible(&self, surface_id: &ObjectId, is_home: bool) -> bool {
        if self.home_only_surfaces.contains(surface_id) {
            // Home-only surface: visible when at home or during animation
            is_home || self.home_mode.alpha(self.theme.motion.animation) > 0.0
        } else if self.hide_on_home_surfaces.contains(surface_id) {
            // Hide-on-home surface: visible when NOT at home or during animation
            !is_home || self.home_mode.alpha(self.theme.motion.animation) < 1.0
        } else {
            // Always-visible surface (default)
            true
        }
    }

    pub fn set_resize_mode(
        &mut self,
        enabled: Option<(shortcuts::Binding, ResizeDirection)>,
        config: &Config,
        evlh: LoopHandle<'static, crate::state::State>,
    ) {
        if let Some((pattern, direction)) = enabled {
            if let ResizeMode::Started(old_pattern, _, old_direction) = &mut self.resize_mode {
                *old_pattern = pattern;
                *old_direction = direction;
            } else {
                self.resize_mode = ResizeMode::Started(pattern, Instant::now(), direction);
            }
            self.resize_indicator = Some(ResizeIndicator::new(
                direction,
                config,
                evlh,
                self.theme.clone(),
            ));
        } else if let Some(direction) = self.resize_mode.active_direction() {
            self.resize_mode = ResizeMode::Ended(Instant::now(), direction);
            if let Some((_, direction, edge, _, _, _)) = self.resize_state.as_ref() {
                self.finish_resize(*direction, *edge);
            }
        }
    }

    pub fn resize_mode(&self) -> (ResizeMode, Option<ResizeIndicator>) {
        if let ResizeMode::Started(binding, timestamp, direction) = &self.resize_mode
            && Instant::now().duration_since(*timestamp) > self.theme.motion.animation
        {
            return (
                ResizeMode::Active(binding.clone(), *direction),
                self.resize_indicator.clone(),
            );
        }
        if let ResizeMode::Ended(timestamp, _) = self.resize_mode
            && Instant::now().duration_since(timestamp) > self.theme.motion.animation
        {
            return (ResizeMode::None, None);
        }

        (self.resize_mode.clone(), self.resize_indicator.clone())
    }

    pub fn stacking_indicator(
        &self,
        output: &Output,
        layer: ManagedLayer,
    ) -> Option<Rectangle<i32, Local>> {
        match layer {
            ManagedLayer::Sticky => self
                .workspaces
                .sets
                .get(output)
                .and_then(|set| set.sticky_layer.stacking_indicator()),
            ManagedLayer::Floating => self
                .active_space(output)?
                .floating_layer
                .stacking_indicator(),
            ManagedLayer::Tiling => self.active_space(output)?.tiling_layer.stacking_indicator(),
            ManagedLayer::Fullscreen => None,
        }
    }

    pub fn appearance_config(&self) -> AppearanceConfig {
        self.appearance_conf
    }

    pub fn trigger_zoom(
        &mut self,
        seat: &Seat<State>,
        output: Option<&Output>,
        level: f64,
        zoom_config: &ZoomConfig,
        animate: bool,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        if self.zoom_state.is_none() && level == 1. {
            return;
        }

        let outputs = output.map(|o| vec![o]).unwrap_or(self.outputs().collect());
        if self.zoom_state.is_none() {
            for output in self.outputs() {
                output.user_data().insert_if_missing_threadsafe(|| {
                    Mutex::new(OutputZoomState::new(
                        seat,
                        output,
                        1.0,
                        zoom_config.increment,
                        zoom_config.view_moves,
                        loop_handle.clone(),
                        self.theme.clone(),
                    ))
                });
            }
        }

        let mut toggled = self.zoom_state.is_none();
        if let Some(old_state) = self.zoom_state.as_ref()
            && &old_state.seat != seat
        {
            return;
        }

        for output in &outputs {
            let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
            output_state.lock().unwrap().update(
                level,
                animate,
                zoom_config.view_moves,
                zoom_config.increment,
            );
        }

        let all_outputs_off = self.outputs().all(|o| {
            o.user_data()
                .get::<Mutex<OutputZoomState>>()
                .unwrap()
                .lock()
                .unwrap()
                .current_level()
                == 1.0
        });
        toggled = toggled || all_outputs_off;

        if toggled {
            let value = !all_outputs_off;
            let _ = loop_handle.insert_idle(move |state| {
                state.common.a11y_state.set_screen_magnifier(value);
            });
        }

        self.zoom_state = Some(ZoomState {
            seat: seat.clone(),
            show_overlay: zoom_config.show_overlay,
            increment: zoom_config.increment,
            movement: zoom_config.view_moves,
        });

        self.update_focal_point(
            seat,
            seat.get_pointer().unwrap().current_location().as_global(),
            zoom_config.view_moves,
        );
    }

    pub fn update_focal_point(
        &mut self,
        seat: &Seat<State>,
        original_position: Point<f64, Global>,
        movement: ZoomMovement,
    ) {
        if let Some(state) = self.zoom_state.as_mut() {
            if &state.seat != seat {
                return;
            }

            let cursor_position = seat.get_pointer().unwrap().current_location().as_global();

            state.update_focal_point(
                &seat.active_output(),
                cursor_position,
                original_position,
                movement,
            );
        }
    }

    pub fn zoom_state(&self) -> Option<&ZoomState> {
        self.zoom_state.as_ref()
    }

    fn refresh(
        &mut self,
        xdg_activation_state: &XdgActivationState,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        match &self.overview_mode {
            OverviewMode::Started(trigger, timestamp)
                if Instant::now().duration_since(*timestamp) > self.theme.motion.animation =>
            {
                self.overview_mode = OverviewMode::Active(trigger.clone());
            }
            OverviewMode::Ended(_, timestamp)
                if Instant::now().duration_since(*timestamp) > self.theme.motion.animation =>
            {
                self.overview_mode = OverviewMode::None;
                self.swap_indicator = None;
            }
            _ => {}
        }

        match &self.resize_mode {
            ResizeMode::Started(binding, timestamp, direction)
                if Instant::now().duration_since(*timestamp) > self.theme.motion.animation =>
            {
                self.resize_mode = ResizeMode::Active(binding.clone(), *direction);
            }
            ResizeMode::Ended(timestamp, _)
                if Instant::now().duration_since(*timestamp) > self.theme.motion.animation =>
            {
                self.resize_mode = ResizeMode::None;
                self.resize_indicator = None;
            }
            _ => {}
        }

        if self.zoom_state.is_some() {
            let mut all_outputs_off = true;
            for output in self.outputs() {
                all_outputs_off = all_outputs_off
                    && output
                        .user_data()
                        .get::<Mutex<OutputZoomState>>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .refresh();
            }

            if all_outputs_off {
                self.zoom_state.take();
            }
        }

        self.workspaces
            .refresh(workspace_state, xdg_activation_state);

        for output in self.outputs() {
            let mut map = layer_map_for_output(output);
            map.cleanup();
        }

        self.override_redirect_windows.retain(|or| or.alive());
        self.override_redirect_windows
            .iter()
            .for_each(|or| or.refresh());

        self.pending_layers
            .retain(|pending| pending.surface.alive());
        self.pending_windows
            .retain(|pending| pending.surface.alive());
    }

    pub fn update_pointer_position(&mut self, location: Point<f64, Local>, output: &Output) {
        for (o, set) in self.workspaces.sets.iter_mut() {
            if o == output {
                set.sticky_layer.update_pointer_position(Some(location));
                for (i, workspace) in set.workspaces.iter_mut().enumerate() {
                    if i == set.active {
                        workspace
                            .update_pointer_position(Some(location), self.overview_mode.clone());
                    } else {
                        workspace.update_pointer_position(None, self.overview_mode.clone());
                    }
                }
            } else {
                set.sticky_layer.update_pointer_position(None);
                for workspace in &mut set.workspaces {
                    workspace.update_pointer_position(None, self.overview_mode.clone());
                }
            }
        }
    }

    pub fn remap_unfullscreened_window(
        &mut self,
        surface: CosmicSurface,
        mut state: Option<FullscreenRestoreState>,
        loop_handle: &LoopHandle<'static, State>,
    ) -> CosmicMapped {
        if let Some(FullscreenRestoreState::Stack { state: stack_state }) = &state {
            if let Some(mapped) = self.mapped().find(|m| **m == stack_state.stack)
                && let Some(stack) = mapped.stack_ref()
            {
                let idx = stack_state.idx.min(stack.len());
                stack.add_window(surface, Some(idx), None);
                return mapped.clone();
            } else {
                state = None;
            }
        }

        let window = if state.as_ref().is_some_and(|s| s.was_stack()) {
            CosmicMapped::from(CosmicStack::new(
                std::iter::once(surface),
                loop_handle.clone(),
                self.theme.clone(),
                self.appearance_conf,
            ))
        } else {
            CosmicMapped::from(CosmicWindow::new(
                surface,
                loop_handle.clone(),
                self.theme.clone(),
                self.appearance_conf,
            ))
        };

        if let Some(FullscreenRestoreState::Sticky { output, state, .. }) = &state {
            let output = output
                .upgrade()
                .unwrap_or_else(|| self.seats.last_active().active_output());
            toplevel_enter_output(&window.active_window(), &output);
            let set = self
                .workspaces
                .sets
                .get_mut(&output)
                .or(self.workspaces.backup_set.as_mut())
                .unwrap();
            set.sticky_layer.map_internal(
                window.clone(),
                Some(state.geometry.loc),
                Some(state.geometry.size.as_logical()),
                Some(set.output.geometry().to_local(&set.output)),
            );
            return window;
        }

        let seat = self.seats.last_active();
        let workspace = match &state {
            Some(FullscreenRestoreState::Floating { workspace, .. })
            | Some(FullscreenRestoreState::Tiling { workspace, .. }) => {
                let workspace = self.workspaces.space_for_handle_mut(workspace);
                let workspace = match workspace {
                    Some(workspace) => workspace,
                    None => self.workspaces.active_mut(&seat.active_output()).unwrap(),
                };
                toplevel_enter_output(&window.active_window(), &workspace.output);
                toplevel_enter_workspace(&window.active_window(), &workspace.handle);

                workspace
            }
            None => self.workspaces.active_mut(&seat.active_output()).unwrap(),
            Some(FullscreenRestoreState::Sticky { .. } | FullscreenRestoreState::Stack { .. }) => {
                unreachable!()
            }
        };
        let fullscreen_geometry = workspace.output.geometry().to_local(&workspace.output);

        match state {
            None => {
                toplevel_enter_output(&window.active_window(), &workspace.output);
                toplevel_enter_workspace(&window.active_window(), &workspace.handle);

                if workspace.tiling_enabled {
                    workspace.tiling_layer.remap(
                        window.clone(),
                        Some(fullscreen_geometry),
                        None,
                        Some(workspace.focus_stack.get(seat).iter()),
                    );
                } else {
                    workspace.floating_layer.map_internal(
                        window.clone(),
                        None,
                        None,
                        Some(fullscreen_geometry),
                    );
                }
            }
            Some(FullscreenRestoreState::Floating {
                state:
                    FloatingRestoreData {
                        was_maximized,
                        was_snapped,
                        geometry,
                        ..
                    },
                ..
            }) => {
                workspace.floating_layer.map_internal(
                    window.clone(),
                    Some(geometry.loc),
                    Some(geometry.size.as_logical()),
                    Some(fullscreen_geometry),
                );
                if was_maximized {
                    let mut state = window.maximized_state.lock().unwrap();
                    *state = Some(MaximizedState {
                        original_geometry: geometry,
                        original_layer: ManagedLayer::Floating,
                        original_snapped: was_snapped,
                    });
                    std::mem::drop(state);
                    workspace.floating_layer.map_maximized(
                        window.clone(),
                        fullscreen_geometry,
                        true,
                    );
                } else if let Some(corners) = was_snapped {
                    workspace.floating_layer.snap_to_corner(&window, &corners);
                }
            }
            Some(FullscreenRestoreState::Tiling {
                state:
                    TilingRestoreData {
                        state,
                        was_maximized,
                    },
                ..
            }) => {
                if workspace.tiling_enabled {
                    let focus_stack = workspace.focus_stack.get(seat);
                    workspace.tiling_layer.remap(
                        window.clone(),
                        Some(fullscreen_geometry),
                        state,
                        Some(focus_stack.iter()),
                    );
                    if was_maximized {
                        let previous_geometry =
                            workspace.tiling_layer.element_geometry(&window).unwrap();
                        let mut state = window.maximized_state.lock().unwrap();
                        *state = Some(MaximizedState {
                            original_geometry: previous_geometry,
                            original_layer: ManagedLayer::Tiling,
                            original_snapped: None,
                        });
                        std::mem::drop(state);
                        workspace.floating_layer.map_maximized(
                            window.clone(),
                            fullscreen_geometry,
                            true,
                        );
                    }
                } else {
                    workspace.floating_layer.map_internal(
                        window.clone(),
                        None,
                        None,
                        Some(fullscreen_geometry),
                    );

                    if was_maximized {
                        let geometry = workspace.floating_layer.element_geometry(&window).unwrap();
                        let mut state = window.maximized_state.lock().unwrap();
                        *state = Some(MaximizedState {
                            original_geometry: geometry,
                            original_layer: ManagedLayer::Floating,
                            original_snapped: None,
                        });
                        std::mem::drop(state);
                        workspace.floating_layer.map_maximized(
                            window.clone(),
                            fullscreen_geometry,
                            true,
                        );
                    }
                }
            }
            Some(FullscreenRestoreState::Sticky { .. } | FullscreenRestoreState::Stack { .. }) => {
                unreachable!()
            }
        }

        window
    }

    #[must_use]
    pub fn map_window(
        &mut self,
        window: &CosmicSurface,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
        workspace_state: &mut WorkspaceState<State>,
        loop_handle: &LoopHandle<'static, State>,
    ) -> Option<KeyboardFocusTarget> {
        let pos = self
            .pending_windows
            .iter()
            .position(|pending| &pending.surface == window)
            .unwrap();
        let PendingWindow {
            surface: window,
            seat,
            fullscreen: output,
            maximized: should_be_maximized,
            sticky: mut should_be_sticky,
            ..
        } = self.pending_windows.remove(pos);

        // Check if this window is embedded - if so, we need to place it on the same
        // output/workspace as the parent window to ensure proper embedding
        let embed_parent_output = crate::wayland::handlers::surface_embed::get_embed_render_info(&window)
            .and_then(|embed_info| {
                tracing::debug!(
                    embedded_app_id = %window.app_id(),
                    parent_surface_id = %embed_info.parent_surface_id,
                    "Looking for parent output for embedded window"
                );

                // Find the parent element by its surface ID - check all outputs and workspaces
                for output in self.outputs() {
                    tracing::debug!(
                        embedded_app_id = %window.app_id(),
                        output = %output.name(),
                        "Checking output for parent"
                    );

                    // Check workspaces on this output
                    for workspace in self.workspaces.spaces().filter(|s| &s.output == output) {
                        for mapped in workspace.mapped() {
                            let mapped_app_id = mapped.active_window().app_id();
                            let mapped_surface_id = mapped.active_window().wl_surface().map(|s| s.id().to_string());
                            tracing::debug!(
                                checking_mapped_app_id = %mapped_app_id,
                                checking_surface_id = ?mapped_surface_id,
                                looking_for_surface_id = %embed_info.parent_surface_id,
                                "Checking workspace mapped window"
                            );
                            if let Some(surface) = mapped.active_window().wl_surface()
                                && surface.id().to_string() == embed_info.parent_surface_id {
                                    tracing::info!(
                                        embedded_app_id = %window.app_id(),
                                        parent_app_id = %mapped.active_window().app_id(),
                                        output = %output.name(),
                                        "Found parent on workspace, using same output for embedded window"
                                    );
                                    return Some(output.clone());
                                }
                        }
                    }

                    // Check sticky layer on this output
                    if let Some(set) = self.workspaces.sets.get(output) {
                        for mapped in set.sticky_layer.mapped() {
                            let mapped_app_id = mapped.active_window().app_id();
                            let mapped_surface_id = mapped.active_window().wl_surface().map(|s| s.id().to_string());
                            tracing::debug!(
                                checking_mapped_app_id = %mapped_app_id,
                                checking_surface_id = ?mapped_surface_id,
                                looking_for_surface_id = %embed_info.parent_surface_id,
                                "Checking sticky layer mapped window"
                            );
                            if let Some(surface) = mapped.active_window().wl_surface()
                                && surface.id().to_string() == embed_info.parent_surface_id {
                                    tracing::info!(
                                        embedded_app_id = %window.app_id(),
                                        parent_app_id = %mapped.active_window().app_id(),
                                        output = %output.name(),
                                        "Found parent on sticky layer, using same output for embedded window"
                                    );
                                    return Some(output.clone());
                                }
                        }
                    }
                }

                tracing::warn!(
                    embedded_app_id = %window.app_id(),
                    parent_surface_id = %embed_info.parent_surface_id,
                    "Could not find parent for embedded window"
                );
                None
            });

        // Store the original X11 geometry before the compositor configures the window.
        // This is the app-requested position, needed for correct transient positioning.
        if let Some(x11) = window.x11_surface() {
            self.original_x11_positions
                .insert(x11.window_id(), x11.geometry());
        }

        let transient_for_id = window
            .x11_surface()
            .and_then(|surface| surface.is_transient_for());
        let transient_parent = transient_for_id
            .and_then(|parent_window_id| self.element_for_x11_window_id(parent_window_id))
            .cloned();

        let transient_parent_output = transient_parent.as_ref().and_then(|parent| {
            self.space_for(parent)
                .map(|workspace| workspace.output.clone())
                .or_else(|| {
                    self.workspaces.sets.iter().find_map(|(output, set)| {
                        set.sticky_layer
                            .mapped()
                            .any(|m| m == parent)
                            .then_some(output.clone())
                    })
                })
        });

        let transient_parent_workspace = transient_parent
            .as_ref()
            .and_then(|parent| self.space_for(parent).map(|workspace| workspace.handle));

        let transient_parent_is_sticky = transient_parent.as_ref().is_some_and(|parent| {
            self.workspaces
                .sets
                .values()
                .any(|set| set.sticky_layer.mapped().any(|m| m == parent))
        });

        // Inherit sticky state from a sticky parent (transient or toplevel parent).
        if !should_be_sticky {
            should_be_sticky = if let Some(toplevel) = window.0.toplevel() {
                if let Some(parent) = toplevel.parent() {
                    if let Some(elem) = self.element_for_surface(&parent) {
                        self.workspaces
                            .sets
                            .values()
                            .any(|set| set.sticky_layer.mapped().any(|m| m == elem))
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                transient_parent_is_sticky
            };
        }

        let pending_activation = self.pending_activations.remove(&(&window).into());
        let workspace_handle = match pending_activation {
            Some(ActivationContext::Workspace(handle)) => Some(handle),
            _ => transient_parent_workspace,
        };

        let should_be_fullscreen = output.is_some();
        // A window game mode claims belongs on the output game mode owns, decided
        // HERE rather than after adoption: everything below places the window, so
        // deferring would map it under the cursor, show it there for a frame, then
        // move it. `game_mode_claims` recognizes it by process ancestry, since the
        // session manager only tags a window after it appears.
        let game_mode_output = self
            .game_mode_claims(&window)
            .then(|| self.game_mode.output.clone())
            .flatten();
        // For embedded windows, use the parent's output; otherwise use fullscreen output or active output
        let mut output = game_mode_output
            .or(output)
            .or(embed_parent_output)
            .or(transient_parent_output)
            .unwrap_or_else(|| seat.active_output());

        // this is beyond stupid, just to make the borrow checker happy
        let workspace = if let Some(handle) = workspace_handle.filter(|handle| {
            self.workspaces
                .spaces()
                .any(|space| &space.handle == handle)
        }) {
            self.workspaces
                .spaces_mut()
                .find(|space| space.handle == handle)
                .unwrap()
        } else {
            self.workspaces.active_mut(&output).unwrap() // a seat's active output always has a workspace
        };
        if output != workspace.output {
            output = workspace.output.clone();
        }

        let active_handle = self.active_space(&output).unwrap().handle;
        let workspace = if let Some(handle) = workspace_handle.filter(|handle| {
            self.workspaces
                .spaces()
                .any(|space| &space.handle == handle)
        }) {
            self.workspaces
                .spaces_mut()
                .find(|space| space.handle == handle)
                .unwrap()
        } else {
            self.workspaces.active_mut(&output).unwrap()
        };

        toplevel_info.new_toplevel(&window, workspace_state);
        toplevel_enter_output(&window, &output);
        toplevel_enter_workspace(&window, &workspace.handle);

        let mut workspace_state = workspace_state.update();

        let workspace_output = workspace.output.clone();
        let was_activated = workspace_handle.is_some()
            && (workspace_output != seat.active_output() || active_handle != workspace.handle);
        let workspace_handle = workspace.handle;
        let is_dialog = layout::is_dialog(&window);
        let floating_exception = layout::has_floating_exception(&self.tiling_exceptions, &window);

        if should_be_fullscreen {
            workspace.map_fullscreen(&window, &seat, None, None);
            if was_activated {
                workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
            }

            return (workspace_output == seat.active_output() && active_handle == workspace_handle)
                .then_some(KeyboardFocusTarget::Fullscreen(window));
        }

        let maybe_focused = workspace.focus_stack.get(&seat).iter().next().cloned();
        if let Some(FocusTarget::Window(focused)) = maybe_focused
            && let Some(stack) = focused.stack_ref()
            && !is_dialog
            && !should_be_maximized
            && !(workspace.is_tiled(&focused.active_window()) && floating_exception)
        {
            stack.add_window(window, None, None);
            if was_activated {
                workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
            }
            return (workspace_output == seat.active_output() && active_handle == workspace_handle)
                .then_some(KeyboardFocusTarget::Element(focused));
        }

        let mapped = CosmicMapped::from(CosmicWindow::new(
            window.clone(),
            loop_handle.clone(),
            self.theme.clone(),
            self.appearance_conf,
        ));
        #[cfg(feature = "debug")]
        {
            mapped.set_debug(self.debug_active);
        }

        let workspace_empty = workspace.mapped().next().is_none();
        if is_dialog || floating_exception || !workspace.tiling_enabled {
            // For X11 transient children, use the X11 geometry as initial position
            // so they appear next to their parent (e.g. Android emulator side panel).
            let initial_position = window
                .x11_surface()
                .filter(|x| x.is_transient_for().is_some())
                .and_then(|x| {
                    let geo = x.geometry();
                    let parent_id = x.is_transient_for()?;
                    // Find the parent element's position in the workspace
                    let parent_elem = workspace.mapped().find(|m| {
                        m.active_window()
                            .x11_surface()
                            .is_some_and(|px| px.window_id() == parent_id)
                    });
                    let parent_geo = parent_elem.and_then(|p| workspace.element_geometry(p));
                    // Position relative to parent if we found it
                    if let Some(parent_geo) = parent_geo {
                        // The X11 geometry gives us the position relative to
                        // the root window. The parent's X11 geometry also uses
                        // root coordinates. Compute relative offset.
                        let parent_x11_geo = workspace
                            .mapped()
                            .find(|m| {
                                m.active_window()
                                    .x11_surface()
                                    .is_some_and(|px| px.window_id() == parent_id)
                            })
                            .and_then(|m| m.active_window().x11_surface().map(|px| px.geometry()));
                        // Get parent's SSD offset (element_geometry includes SSD,
                        // but the X11 content starts below the titlebar)
                        let parent_ssd_offset = parent_elem
                            .map(|p| p.active_window_offset())
                            .unwrap_or_default();
                        if let Some(parent_x11_geo) = parent_x11_geo {
                            let relative_x = geo.loc.x - parent_x11_geo.loc.x;
                            let relative_y = geo.loc.y - parent_x11_geo.loc.y;
                            // parent_geo.loc is the top-left of the decoration frame.
                            // Add parent's SSD offset to reach the X11 content origin,
                            // then subtract the child's own SSD offset since initial_position
                            // sets the element frame origin (not the content origin).
                            let child_ssd_height = mapped.ssd_height(false).unwrap_or(0);
                            Some(Point::<i32, Local>::from((
                                parent_geo.loc.x + parent_ssd_offset.x + relative_x,
                                parent_geo.loc.y + parent_ssd_offset.y + relative_y
                                    - child_ssd_height,
                            )))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });
            workspace
                .floating_layer
                .map(mapped.clone(), initial_position);
        } else {
            for mapped in workspace
                .mapped()
                .filter(|m| m.maximized_state.lock().unwrap().is_some())
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                workspace.unmaximize_request(&mapped);
            }
            let focus_stack = workspace.focus_stack.get(&seat);
            workspace
                .tiling_layer
                .map(mapped.clone(), Some(focus_stack.iter()), None);
        }

        if should_be_sticky {
            self.toggle_sticky(&seat, &mapped);
        }

        if should_be_maximized {
            // Fade-in animation for windows that start maximized
            // Window appears at full size and fades in (no geometry transition)
            self.maximize_request_fade_in(&mapped, &seat, loop_handle);
        }

        // If this is an embedded window, re-apply the geometry now that it's mapped
        // The initial configure in embed_geometry_changed happens before the window is mapped,
        // so we need to re-apply the size here to ensure the embedded window renders at the correct size
        if let Some(embed_info) =
            crate::wayland::handlers::surface_embed::get_embed_render_info(&window)
        {
            tracing::debug!(
                embedded_app_id = %window.app_id(),
                embed_geometry = ?embed_info.geometry,
                "Re-applying embed geometry after window mapped"
            );
            // Set the geometry and force a configure
            let global_geo = smithay::utils::Rectangle::new(
                (embed_info.geometry.loc.x, embed_info.geometry.loc.y).into(),
                (embed_info.geometry.size.w, embed_info.geometry.size.h).into(),
            );
            window.set_geometry(global_geo, 0);
            window.force_configure();
        }

        let new_target = if self.game_mode_hides(&window) {
            // Game mode renders ONLY its controlled surface on that workspace, so a
            // window it will not draw must not take the keyboard either: focusing an
            // invisible window looks exactly like a hung game (keystrokes vanish
            // into a black screen). Park it on the focus stack and mark the
            // workspace urgent instead, the same treatment as focus-stealing
            // prevention below.
            self.append_focus_stack(mapped, &seat);
            workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
            None
        } else if (workspace_output == seat.active_output() && active_handle == workspace_handle)
            || should_be_sticky
        {
            // Focus stealing prevention: only grant immediate focus if the window
            // was user-initiated (activation token), workspace was empty, or it's a dialog.
            if workspace_empty || was_activated || is_dialog {
                Some(KeyboardFocusTarget::from(mapped.clone()))
            } else {
                self.append_focus_stack(mapped, &seat);
                workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
                None
            }
        } else {
            if workspace_empty || was_activated {
                self.append_focus_stack(mapped, &seat);
                workspace_state.add_workspace_state(&workspace_handle, WState::Urgent);
            }
            None
        };

        let active_space = self.active_space(&output).unwrap();
        for mapped in active_space.mapped() {
            self.update_reactive_popups(mapped);
        }

        // Re-evaluate auto-hide — a new window was mapped.
        self.refresh_auto_hide();

        // After mapping an X11 window, check if any already-mapped windows are
        // transient children of this window that mapped before their parent.
        // If so, move them to the same output/workspace and make them floating.
        if let Some(x11_surface) = window.x11_surface() {
            let parent_window_id = x11_surface.window_id();
            self.reparent_orphaned_transient_children(parent_window_id, &output, workspace_handle);
        }

        new_target
    }

    /// After an X11 parent window maps, find any transient children that mapped
    /// before it and reposition them relative to the parent.
    /// Also moves children to the parent's output/workspace if needed.
    fn reparent_orphaned_transient_children(
        &mut self,
        parent_window_id: u32,
        _parent_output: &Output,
        parent_workspace_handle: WorkspaceHandle,
    ) {
        // Collect children that are transient to this parent
        let orphans: Vec<CosmicMapped> = self
            .workspaces
            .sets
            .values()
            .flat_map(|set| {
                set.workspaces
                    .iter()
                    .flat_map(|w| w.mapped())
                    .chain(set.sticky_layer.mapped())
            })
            .filter(|m| {
                m.active_window()
                    .x11_surface()
                    .and_then(|x| x.is_transient_for())
                    .is_some_and(|tid| tid == parent_window_id)
            })
            .cloned()
            .collect();

        if orphans.is_empty() {
            return;
        }

        // First, move any orphans that are on the wrong workspace
        for orphan in &orphans {
            let already_on_target = self
                .workspaces
                .spaces()
                .find(|w| w.handle == parent_workspace_handle)
                .is_some_and(|w| w.mapped().any(|m| m == orphan));

            if !already_on_target {
                // Remove from current workspace
                for workspace in self.workspaces.spaces_mut() {
                    if workspace.mapped().any(|m| m == orphan) {
                        workspace.unmap_element(orphan);
                        break;
                    }
                }

                // Map onto parent's workspace as floating (position set below)
                if let Some(workspace) = self
                    .workspaces
                    .spaces_mut()
                    .find(|w| w.handle == parent_workspace_handle)
                {
                    workspace.floating_layer.map(orphan.clone(), None);
                }
            }
        }

        // Now reposition all orphans relative to the parent.
        // The parent just mapped so its element_geometry is available.
        // Use ORIGINAL X11 geometries (stored at map time, before compositor configuration)
        // because the compositor may have placed the child at a default position.
        let workspace = match self
            .workspaces
            .spaces()
            .find(|w| w.handle == parent_workspace_handle)
        {
            Some(ws) => ws,
            None => return,
        };

        // Find parent element and its compositor geometry
        let parent_elem = workspace.mapped().find(|m| {
            m.active_window()
                .x11_surface()
                .is_some_and(|px| px.window_id() == parent_window_id)
        });
        let parent_geo = parent_elem.and_then(|p| workspace.element_geometry(p));
        let parent_ssd_offset = parent_elem
            .map(|p| p.active_window_offset())
            .unwrap_or_default();

        // Use stored original X11 geometry (app-requested, not compositor-configured)
        let parent_orig_x11_geo = self.original_x11_positions.get(&parent_window_id).copied();

        let (Some(parent_geo), Some(parent_orig_x11_geo)) = (parent_geo, parent_orig_x11_geo)
        else {
            tracing::warn!(
                parent_window_id = parent_window_id,
                has_parent_geo = parent_geo.is_some(),
                has_parent_orig_x11 = parent_orig_x11_geo.is_some(),
                "reparent_orphaned_transient_children: could not find parent geometry"
            );
            return;
        };

        // Collect positions using original X11 geometries
        let orphan_positions: Vec<_> = orphans
            .iter()
            .filter_map(|orphan| {
                let child_x11_id = orphan
                    .active_window()
                    .x11_surface()
                    .map(|x| x.window_id())?;
                let child_orig_x11_geo = self.original_x11_positions.get(&child_x11_id).copied()?;

                // Compute relative offset using ORIGINAL X11 positions
                let relative_x = child_orig_x11_geo.loc.x - parent_orig_x11_geo.loc.x;
                let relative_y = child_orig_x11_geo.loc.y - parent_orig_x11_geo.loc.y;

                // parent_geo.loc is the decoration frame origin.
                // Add parent SSD offset to reach the X11 content origin.
                // Subtract child's own SSD height since we're setting the element frame position.
                let child_ssd_height = orphan.ssd_height(false).unwrap_or(0);
                let new_loc = Point::<i32, Local>::from((
                    parent_geo.loc.x + parent_ssd_offset.x + relative_x,
                    parent_geo.loc.y + parent_ssd_offset.y + relative_y - child_ssd_height,
                ));

                // Update element geometry
                let new_geo = Rectangle::new(
                    new_loc.to_global(&workspace.output),
                    orphan.geometry().size.as_global(),
                );
                orphan.set_geometry(new_geo);

                Some((orphan.clone(), new_loc.as_logical()))
            })
            .collect();

        // Re-map at corrected positions (need mutable access)
        if let Some(workspace) = self
            .workspaces
            .spaces_mut()
            .find(|w| w.handle == parent_workspace_handle)
        {
            for (orphan, new_loc) in orphan_positions {
                workspace
                    .floating_layer
                    .space
                    .map_element(orphan, new_loc, false);
            }
        }
    }

    /// Collect X11 transient children of a window, compute their offsets from
    /// the parent, and unmap them from the workspace. Returns the children and
    /// their offsets for rendering in the grab state.
    pub fn collect_and_unmap_x11_transient_children(
        &mut self,
        parent: &CosmicMapped,
        parent_global_pos: Point<i32, Global>,
    ) -> Vec<(CosmicMapped, Point<i32, Logical>)> {
        let parent_x11_id = match parent.active_window().x11_surface().map(|x| x.window_id()) {
            Some(id) => id,
            None => return Vec::new(),
        };

        // First pass: find children and compute offsets
        let children: Vec<(CosmicMapped, Point<i32, Logical>)> = self
            .workspaces
            .spaces()
            .flat_map(|w| {
                w.mapped()
                    .filter(|m| {
                        m.active_window()
                            .x11_surface()
                            .and_then(|x| x.is_transient_for())
                            .is_some_and(|tid| tid == parent_x11_id)
                    })
                    .filter_map(|m| {
                        let child_geo = w.element_geometry(m)?;
                        let child_global = child_geo.loc.to_global(&w.output);
                        let offset = (child_global - parent_global_pos).as_logical();
                        Some((m.clone(), offset))
                    })
            })
            .collect();

        // Second pass: unmap children from their workspaces
        for (child, _) in &children {
            for workspace in self.workspaces.spaces_mut() {
                if workspace
                    .floating_layer
                    .space
                    .elements()
                    .any(|e| e == child)
                {
                    workspace.floating_layer.space.unmap_elem(child);
                    workspace.floating_layer.remove_animation(child);
                    break;
                }
            }
        }

        children
    }

    /// Remap X11 transient children that were unmapped during drag.
    /// Places them at parent_global_pos + offset on the appropriate workspace.
    pub fn remap_x11_transient_children(
        &mut self,
        parent_global_pos: Point<i32, Global>,
        children: &[(CosmicMapped, Point<i32, Logical>)],
    ) {
        for (child, offset) in children {
            let target_global: Point<i32, Global> =
                (parent_global_pos.as_logical() + *offset).as_global();

            // Find the output the target position falls on
            let target_output = self
                .outputs()
                .find(|o| {
                    o.geometry()
                        .as_logical()
                        .contains(target_global.as_logical())
                })
                .or_else(|| {
                    self.outputs().min_by_key(|o| {
                        let geo = o.geometry().as_logical();
                        let cx = geo.loc.x + geo.size.w / 2;
                        let cy = geo.loc.y + geo.size.h / 2;
                        let dx = target_global.x - cx;
                        let dy = target_global.y - cy;
                        dx * dx + dy * dy
                    })
                })
                .cloned();

            let Some(target_output) = target_output else {
                continue;
            };

            let target_local = target_global.to_local(&target_output);
            let target_ws = match self.active_space_mut(&target_output) {
                Some(ws) => ws,
                None => continue,
            };

            let new_geo = Rectangle::new(target_global, child.geometry().size.as_global());
            child.set_geometry(new_geo);
            target_ws.floating_layer.space.map_element(
                child.clone(),
                target_local.as_logical(),
                false,
            );

            child.output_enter(
                &target_output,
                Rectangle::from_size(target_output.geometry().size.as_logical()),
            );
        }
    }

    pub fn map_override_redirect(&mut self, window: X11Surface) {
        let geo = window.geometry();
        for (output, overlap) in self.outputs().cloned().filter_map(|o| {
            o.geometry()
                .as_logical()
                .intersection(geo)
                .map(|overlap| (o, overlap))
        }) {
            window.output_enter(&output, overlap);
        }

        self.override_redirect_windows.push(window);
    }

    #[must_use]
    pub fn map_layer(&mut self, layer_surface: &LayerSurface) -> Option<KeyboardFocusTarget> {
        let pos = self
            .pending_layers
            .iter()
            .position(|pending| &pending.surface == layer_surface)
            .unwrap();
        let pending = self.pending_layers.remove(pos);

        let surface_id = pending.surface.wl_surface().id();
        let is_hidden = self.hidden_surfaces.contains(&surface_id);

        let wants_focus = if is_hidden {
            // Surface was hidden before it was mapped (e.g. launcher daemon
            // GPU warm-up).  Don't grant focus to hidden surfaces.
            false
        } else {
            with_states(pending.surface.wl_surface(), |states| {
                let mut state = states.cached_state.get::<LayerSurfaceCachedState>();
                matches!(state.current().layer, Layer::Top | Layer::Overlay)
                    && state.current().keyboard_interactivity != KeyboardInteractivity::None
            })
        };

        // Map the surface first so the edge-slide detection below can read its
        // committed anchor / geometry from the output's layer map.
        {
            let mut map = layer_map_for_output(&pending.output);
            map.map_layer(&pending.surface).unwrap();
        }

        // Pick the entrance animation for a freshly-mapped, visible surface.
        // (Hidden surfaces — e.g. a daemon warming up the GPU — animate later,
        // when shown via the layer_surface_visibility protocol.)
        //
        // Two families:
        //   - Edge slide: a surface anchored to a single lateral edge (the chat
        //     panel) slides in from that edge with a coupled workspace push,
        //     UNLESS it opted into `Fade`. Needs a real exclusive zone to push.
        //   - Fade + rise (the DEFAULT, formerly gated to the popover/modal
        //     namespaces): every other surface fades in with a subtle upward
        //     slide + scale — the agentos-panel popover open animation. We hold
        //     it in `pending_layer_opens` (alpha 0) until the first buffer commit,
        //     then start the real `LayerOpen` in `activate_pending_fade_in`, and
        //     register it in `rise_surfaces` so the hide path plays the matching
        //     close (slide-down). Registering the open SUPPRESSES the plain
        //     fade-in, so alpha/translate/scale stay driven by one eased factor.
        if !is_hidden {
            let slide_in = (self.layer_transitions.get(&surface_id)
                != Some(
                    &crate::wayland::protocols::layer_surface_visibility::LayerTransition::Fade,
                ))
            .then(|| self.detect_layer_slide_edge(&surface_id))
            .flatten()
            .filter(|(_, _, exclusive_zone)| *exclusive_zone > 0);

            if let Some((edge, width, exclusive_zone)) = slide_in {
                use smithay::wayland::shell::wlr_layer::ExclusiveZone;
                tracing::debug!(
                    ?surface_id,
                    ?edge,
                    width,
                    exclusive_zone,
                    "map_layer: starting first-open slide-in"
                );
                // Start hidden, then immediately begin the slide-in.
                let motion = self.theme.motion;
                let mut slide = layer_slide::LayerSlide::new_hidden(
                    surface_id.clone(),
                    edge,
                    width,
                    exclusive_zone,
                );
                slide.visibility.start_show(motion);
                self.layer_slides.push(slide);
                // Override exclusive_zone to 0 for the start of the animation,
                // then re-arrange so other surfaces don't jump immediately.
                let wl_surface = pending.surface.wl_surface();
                with_states(wl_surface, |states| {
                    let mut cached = states.cached_state.get::<LayerSurfaceCachedState>();
                    cached.current().exclusive_zone = ExclusiveZone::Exclusive(0);
                });
                {
                    let mut map = layer_map_for_output(&pending.output);
                    map.arrange();
                }
            } else {
                self.rise_surfaces.insert(surface_id.clone());
                self.pending_layer_opens.insert(surface_id.clone());
            }
        }

        for workspace in self.workspaces.spaces_mut() {
            workspace.recalculate();
        }

        wants_focus.then(|| pending.surface.into())
    }

    pub fn unmap_surface<S>(
        &mut self,
        surface: &S,
        seat: &Seat<State>,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) -> Option<PendingWindow>
    where
        CosmicSurface: PartialEq<S>,
    {
        for set in self.workspaces.sets.values_mut() {
            let sticky_res = set.sticky_layer.mapped().find_map(|m| {
                m.windows()
                    .position(|(s, _)| &s == surface)
                    .map(|idx| (idx, m.clone()))
            });
            let surface = if let Some((idx, mapped)) = sticky_res {
                if let Some(stack) = mapped.stack_ref() {
                    stack.remove_idx(idx)
                } else {
                    set.sticky_layer.unmap(&mapped, None);
                    Some(mapped.active_window())
                }
            } else if let Some(idx) = set
                .minimized_windows
                .iter()
                .position(|w| w.windows().any(|s| &s == surface))
            {
                if let Some(stack) = set
                    .minimized_windows
                    .get_mut(idx)
                    .unwrap()
                    .mapped_mut()
                    .and_then(|m| m.stack_ref())
                {
                    let idx = stack.surfaces().position(|s| &s == surface);
                    idx.and_then(|idx| stack.remove_idx(idx))
                } else {
                    Some(
                        set.minimized_windows
                            .remove(idx)
                            .mapped()
                            .unwrap()
                            .active_window(),
                    )
                }
            } else if let Some((surface, _)) = set
                .workspaces
                .iter_mut()
                .find_map(|w| w.unmap_surface(surface))
            {
                Some(surface)
            } else {
                None
            };

            if let Some(surface) = surface {
                toplevel_info.remove_toplevel(&surface);
                // Re-evaluate auto-hide — the unmapped window may have been
                // the last maximized/fullscreen window on its output.
                self.refresh_auto_hide();
                return Some(PendingWindow {
                    surface,
                    seat: seat.clone(),
                    fullscreen: None,
                    maximized: false,
                    sticky: false,
                    frame_notified: false,
                });
            }
        }

        None
    }

    pub fn move_current(
        &mut self,
        seat: &Seat<State>,
        to: (&Output, Option<usize>),
        follow: bool,
        direction: Option<Direction>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        evlh: &LoopHandle<'static, State>,
    ) -> Result<Option<(KeyboardFocusTarget, Point<i32, Global>)>, InvalidWorkspaceIndex> {
        let (to_output, to_idx) = to;
        let to_idx = to_idx.unwrap_or(self.workspaces.active_num(to_output).1);
        let from_output = seat.focused_or_active_output();
        let from_idx = self.workspaces.active_num(&from_output).1;

        if &from_output == to_output && to_idx == self.workspaces.active_num(&from_output).1 {
            return Ok(None);
        }

        if &from_output == to_output
            && to_idx.checked_sub(1).is_some_and(|idx| idx == from_idx)
            && to_idx == self.workspaces.len(to_output) - 1
            && self
                .workspaces
                .get(from_idx, &from_output)
                .is_some_and(|w| w.len() == 1)
            && self
                .workspaces
                .get(to_idx, to_output)
                .is_some_and(|w| w.is_empty())
        {
            return Err(InvalidWorkspaceIndex);
        }

        let to = self
            .workspaces
            .get(to_idx, to_output)
            .map(|ws| ws.handle)
            .ok_or(InvalidWorkspaceIndex)?;

        let from_workspace = self
            .workspaces
            .active_mut(&from_output)
            .ok_or(InvalidWorkspaceIndex)?;
        let from = from_workspace.handle;

        match seat.get_keyboard().unwrap().current_focus() {
            Some(KeyboardFocusTarget::Group(WindowGroup {
                node, focus_stack, ..
            })) => {
                let new_pos = if follow {
                    seat.set_active_output(to_output);
                    self.workspaces
                        .idx_for_handle(to_output, &to)
                        .and_then(|to_idx| {
                            self.activate(
                                to_output,
                                to_idx,
                                WorkspaceDelta::new_shortcut(),
                                workspace_state,
                            )
                            .ok()
                        })
                } else {
                    None
                };

                let spaces = self.workspaces.spaces_mut();
                let (mut from_w, mut other_w) = spaces.partition::<Vec<_>, _>(|w| w.handle == from);
                if let Some(from_workspace) = from_w.get_mut(0)
                    && let Some(to_workspace) = other_w.iter_mut().find(|w| w.handle == to)
                {
                    {
                        let mut stack = to_workspace.focus_stack.get_mut(seat);
                        for elem in focus_stack.iter().flat_map(|node_id| {
                            from_workspace.tiling_layer.element_for_node(node_id)
                        }) {
                            stack.append(elem.clone());
                        }
                    }

                    if to_workspace.tiling_enabled {
                        for mapped in to_workspace
                            .mapped()
                            .filter(|m| m.maximized_state.lock().unwrap().is_some())
                            .cloned()
                            .collect::<Vec<_>>()
                            .into_iter()
                        {
                            to_workspace.unmaximize_request(&mapped);
                        }
                    }

                    let res = TilingLayout::move_tree(
                        &mut from_workspace.tiling_layer,
                        &mut to_workspace.tiling_layer,
                        &to,
                        seat,
                        to_workspace.focus_stack.get(seat).iter(),
                        NodeDesc {
                            handle: from,
                            node,
                            stack_window: None,
                            focus_stack,
                        },
                        direction,
                    );
                    from_workspace.refresh_focus_stack();
                    to_workspace.refresh_focus_stack();

                    if !to_workspace.tiling_enabled {
                        to_workspace.tiling_enabled = true;
                        for mapped in to_workspace
                            .tiling_layer
                            .mapped()
                            .map(|(mapped, _)| mapped.clone())
                            .collect::<Vec<_>>()
                            .into_iter()
                        {
                            to_workspace.toggle_floating_window(seat, &mapped);
                        }
                        to_workspace.tiling_enabled = false;
                    }

                    return Ok(res.zip(new_pos));
                }

                Ok(None)
            }
            Some(KeyboardFocusTarget::Fullscreen(surface)) => Ok(self.move_window(
                Some(seat),
                &surface,
                &from,
                &to,
                follow,
                direction,
                workspace_state,
                evlh,
            )),
            Some(KeyboardFocusTarget::Element(mapped)) => Ok(self.move_element(
                Some(seat),
                &mapped,
                &from,
                &to,
                follow,
                direction,
                workspace_state,
            )),
            _ => Ok(None),
        }
    }

    #[must_use]
    pub fn move_window(
        &mut self,
        seat: Option<&Seat<State>>,
        window: &CosmicSurface,
        from: &WorkspaceHandle,
        to: &WorkspaceHandle,
        follow: bool,
        direction: Option<Direction>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
        evlh: &LoopHandle<'static, State>,
    ) -> Option<(KeyboardFocusTarget, Point<i32, Global>)> {
        let from_output = self.workspaces.space_for_handle(from)?.output.clone();
        let to_output = self.workspaces.space_for_handle(to)?.output.clone();
        let to_is_tiling = self.workspaces.space_for_handle(to).unwrap().tiling_enabled;

        let from_workspace = self.workspaces.space_for_handle_mut(from).unwrap(); // checked above

        let is_minimized = window.is_minimized();
        let is_fullscreen = from_workspace
            .get_fullscreen_surfaces()
            .any(|f| &f.surface == window);
        let mut window_state = if is_fullscreen {
            let (_, previous_state, previous_geometry) =
                from_workspace.take_fullscreen(window).unwrap();
            WorkspaceRestoreData::Fullscreen(previous_state.zip(previous_geometry).map(
                |(previous_state, previous_geometry)| FullscreenRestoreData {
                    previous_state,
                    previous_geometry,
                },
            ))
        } else {
            from_workspace.unmap_surface(window)?.1
        };

        toplevel_leave_workspace(window, from);
        if from_output != to_output {
            toplevel_leave_output(window, &from_output);
            toplevel_enter_output(window, &to_output);
        }
        toplevel_enter_workspace(window, to);

        // we can't restore to a given position
        if let WorkspaceRestoreData::Tiling(state) = &mut window_state {
            state.state.take();
        }
        // update fullscreen state to restore to the new workspace
        if let WorkspaceRestoreData::Fullscreen(Some(FullscreenRestoreData {
            previous_state: previous,
            ..
        })) = &mut window_state
        {
            if to_is_tiling && !from_workspace.tiling_enabled {
                *previous = FullscreenRestoreState::Tiling {
                    workspace: *to,
                    state: TilingRestoreData {
                        state: None,
                        was_maximized: previous.was_maximized(),
                    },
                    was_stack: previous.was_stack(),
                };
            } else if let FullscreenRestoreState::Tiling { workspace, .. }
            | FullscreenRestoreState::Floating { workspace, .. } = previous
            {
                *workspace = *to;
            }
        }

        if is_minimized {
            let to_workspace = self.workspaces.space_for_handle_mut(to).unwrap(); // checked above
            let minimized_window = match window_state {
                WorkspaceRestoreData::Floating(previous) => {
                    let window = CosmicMapped::from(CosmicWindow::new(
                        window.clone(),
                        evlh.clone(),
                        self.theme.clone(),
                        self.appearance_conf,
                    ));
                    window.set_minimized(true);
                    MinimizedWindow::Floating { window, previous }
                }
                WorkspaceRestoreData::Tiling(previous) => {
                    let window = CosmicMapped::from(CosmicWindow::new(
                        window.clone(),
                        evlh.clone(),
                        self.theme.clone(),
                        self.appearance_conf,
                    ));
                    window.set_minimized(true);
                    MinimizedWindow::Tiling { window, previous }
                }
                WorkspaceRestoreData::Fullscreen(previous) => {
                    window.set_minimized(true);
                    MinimizedWindow::Fullscreen {
                        surface: window.clone(),
                        previous,
                    }
                }
                _ => {
                    unreachable!()
                } // MinimizedWindow always has restore data
            };
            to_workspace.minimized_windows.push(minimized_window);
            return None;
        }

        for mapped in from_workspace
            .mapped()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.update_reactive_popups(&mapped);
        }

        let new_pos = if follow {
            if let Some(seat) = seat {
                seat.set_active_output(&to_output);
            }
            self.workspaces
                .idx_for_handle(&to_output, to)
                .and_then(|to_idx| {
                    self.activate(
                        &to_output,
                        to_idx,
                        WorkspaceDelta::new_shortcut(),
                        workspace_state,
                    )
                    .ok()
                })
        } else {
            None
        };

        let to_workspace = self.workspaces.space_for_handle_mut(to).unwrap(); // checked above
        let to_mapped = to_workspace.mapped().cloned().collect::<Vec<_>>();

        let focus_target: KeyboardFocusTarget =
            if !matches!(window_state, WorkspaceRestoreData::Fullscreen(_))
                && !to_workspace.tiling_enabled
            {
                let mapped = CosmicMapped::from(CosmicWindow::new(
                    window.clone(),
                    evlh.clone(),
                    self.theme.clone(),
                    self.appearance_conf,
                ));
                let position = match window_state {
                    WorkspaceRestoreData::Floating(data) => Some(
                        data.position_relative(to_workspace.output.geometry().size.as_logical()),
                    ),
                    _ => None,
                };
                to_workspace.floating_layer.map(mapped.clone(), position);
                mapped.into()
            } else if !matches!(window_state, WorkspaceRestoreData::Fullscreen(_))
                && to_workspace.tiling_enabled
            {
                let mapped = CosmicMapped::from(CosmicWindow::new(
                    window.clone(),
                    evlh.clone(),
                    self.theme.clone(),
                    self.appearance_conf,
                ));
                for mapped in to_workspace
                    .mapped()
                    .filter(|m| m.maximized_state.lock().unwrap().is_some())
                    .cloned()
                    .collect::<Vec<_>>()
                    .into_iter()
                {
                    to_workspace.unmaximize_request(&mapped);
                }
                let focus_stack = seat.map(|seat| to_workspace.focus_stack.get(seat));
                to_workspace.tiling_layer.map(
                    mapped.clone(),
                    focus_stack.as_ref().map(|x| x.iter()),
                    direction,
                );
                mapped.into()
            } else if let WorkspaceRestoreData::Fullscreen(previous) = window_state {
                to_workspace.map_fullscreen(
                    window,
                    None,
                    previous.clone().map(|p| p.previous_state),
                    previous.map(|p| p.previous_geometry),
                );
                window.clone().into()
            } else {
                unreachable!() // TODO: sticky
            };

        for mapped in to_mapped.into_iter() {
            self.update_reactive_popups(&mapped);
        }

        // Re-evaluate auto-hide — window moved between outputs/workspaces.
        self.refresh_auto_hide();

        new_pos.map(|pos| (focus_target, pos))
    }

    #[must_use]
    pub fn move_element(
        &mut self,
        seat: Option<&Seat<State>>,
        mapped: &CosmicMapped,
        from: &WorkspaceHandle,
        to: &WorkspaceHandle,
        follow: bool,
        direction: Option<Direction>,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) -> Option<(KeyboardFocusTarget, Point<i32, Global>)> {
        let from_output = self.workspaces.space_for_handle(from)?.output.clone();
        let to_output = self.workspaces.space_for_handle(to)?.output.clone();

        let from_workspace = self.workspaces.space_for_handle_mut(from).unwrap(); // checked above
        let window_state = from_workspace.unmap_element(mapped)?;
        let elements = from_workspace.mapped().cloned().collect::<Vec<_>>();

        for (toplevel, _) in mapped.windows() {
            toplevel_leave_workspace(&toplevel, from);
            if from_output != to_output {
                toplevel_leave_output(&toplevel, &from_output);
            }
        }
        for mapped in elements.into_iter() {
            self.update_reactive_popups(&mapped);
        }
        let new_pos = if follow {
            if let Some(seat) = seat {
                seat.set_active_output(&to_output);
            }
            self.workspaces
                .idx_for_handle(&to_output, to)
                .and_then(|to_idx| {
                    self.activate(
                        &to_output,
                        to_idx,
                        WorkspaceDelta::new_shortcut(),
                        workspace_state,
                    )
                    .ok()
                })
        } else {
            None
        };

        let to_workspace = self.workspaces.space_for_handle_mut(to).unwrap(); // checked above
        if !to_workspace.tiling_enabled {
            let (position, was_maximized, was_snapped) = match &window_state {
                WorkspaceRestoreData::Floating(data) => (
                    Some(data.position_relative(to_workspace.output.geometry().size.as_logical())),
                    data.was_maximized,
                    data.was_snapped,
                ),
                _ => (None, false, None),
            };
            to_workspace.floating_layer.map(mapped.clone(), position);
            if was_maximized {
                let geometry = to_workspace
                    .floating_layer
                    .element_geometry(mapped)
                    .unwrap();
                *mapped.maximized_state.lock().unwrap() = Some(MaximizedState {
                    original_geometry: geometry,
                    original_layer: ManagedLayer::Floating,
                    original_snapped: was_snapped,
                });
                to_workspace
                    .floating_layer
                    .map_maximized(mapped.clone(), geometry, false);
            } else if let Some(corners) = was_snapped {
                to_workspace.floating_layer.snap_to_corner(mapped, &corners);
            }
        } else {
            for mapped in to_workspace
                .mapped()
                .filter(|m| m.maximized_state.lock().unwrap().is_some())
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                to_workspace.unmaximize_request(&mapped);
            }
            let focus_stack = seat.map(|seat| to_workspace.focus_stack.get(seat));
            to_workspace.tiling_layer.map(
                mapped.clone(),
                focus_stack.as_ref().map(|x| x.iter()),
                direction,
            );
        }

        let focus_target = KeyboardFocusTarget::from(mapped.clone());

        for mapped in to_workspace
            .mapped()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            self.update_reactive_popups(&mapped);
        }
        for (toplevel, _) in mapped.windows() {
            if from_output != to_output {
                toplevel_enter_output(&toplevel, &to_output);
            }
            toplevel_enter_workspace(&toplevel, to);
        }

        // Re-evaluate auto-hide — element moved between outputs/workspaces.
        self.refresh_auto_hide();

        new_pos.map(|pos| (focus_target, pos))
    }

    pub fn update_reactive_popups(&self, mapped: &CosmicMapped) {
        if let Some(workspace) = self.space_for(mapped)
            && let Some(element_loc) = workspace
                .element_geometry(mapped)
                .map(|geo| geo.loc.to_global(&workspace.output))
        {
            for (window, offset) in mapped.windows() {
                if let Some(toplevel) = window.0.toplevel() {
                    let window_geo_offset = window.geometry().loc.as_global();
                    update_reactive_popups(
                        toplevel,
                        element_loc + offset.as_global() + window_geo_offset,
                        self.outputs(),
                    );
                }
            }
        }
    }

    pub fn menu_request(
        &self,
        is_client_initiated: bool,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        location: Point<i32, Logical>,
        target_stack: bool,
        config: &Config,
        evlh: &LoopHandle<'static, State>,
    ) -> Option<(MenuGrab, Focus)> {
        let serial = serial.into();
        let Some(GrabStartData::Pointer(start_data)) =
            check_grab_preconditions(seat, serial, is_client_initiated.then_some(surface))
        else {
            return None; // TODO: an application can send a menu request for a touch event
        };

        let items_for_element = |mapped: &CosmicMapped,
                                 is_tiled: bool,
                                 is_sticky: bool,
                                 tiling_enabled: bool,
                                 edge: ResizeEdge| {
            let is_stacked = mapped.is_stack();

            if target_stack || !is_stacked {
                Box::new(
                    window_items(
                        mapped,
                        is_tiled,
                        is_stacked,
                        is_sticky,
                        tiling_enabled,
                        edge,
                        config,
                    )
                    .collect::<Vec<Item>>()
                    .into_iter(),
                ) as Box<dyn Iterator<Item = Item>>
            } else {
                let (tab, _) = mapped
                    .windows()
                    .find(|(s, _)| s.wl_surface().as_deref() == Some(surface))
                    .unwrap();
                Box::new(
                    tab_items(mapped, &tab, is_tiled, config)
                        .collect::<Vec<Item>>()
                        .into_iter(),
                ) as Box<dyn Iterator<Item = Item>>
            }
        };

        let (global_position, menu_items) = if let Some((set, mapped, relative_loc)) =
            self.workspaces.sets.values().find_map(|set| {
                set.sticky_layer
                    .mapped()
                    .find_map(|m| {
                        m.windows()
                            .find(|(w, _)| w == surface)
                            .map(|(_, loc)| (m, loc))
                    })
                    .map(|(mapped, relative_loc)| (set, mapped, relative_loc))
            }) {
            let output = set.output.clone();
            let global_position = (set.sticky_layer.element_geometry(mapped).unwrap().loc
                + relative_loc.as_local()
                + location.as_local())
            .to_global(&output);
            (
                global_position,
                items_for_element(mapped, false, true, false, ResizeEdge::all()),
            )
        } else if let Some((workspace, output)) = self.workspace_for_surface(surface) {
            let workspace = self.workspaces.space_for_handle(&workspace).unwrap();

            if let Some(fs) = workspace
                .get_fullscreen_surfaces()
                .find(|f| &f.surface == surface)
            {
                let window = &fs.surface;
                let global_position = (workspace.fullscreen_geometry_for(fs).loc
                    + location.as_local())
                .to_global(&output);

                (
                    global_position,
                    Box::new(fullscreen_items(window, config)) as Box<dyn Iterator<Item = Item>>,
                )
            } else {
                let mapped = workspace.element_for_surface(surface)?;
                let elem_geo = workspace.element_geometry(mapped)?;
                let relative_loc = mapped.active_window_geometry().loc;
                let global_position =
                    (elem_geo.loc + relative_loc.as_local() + location.as_local())
                        .to_global(&output);
                let is_tiled = workspace.is_tiled(&mapped.active_window());
                let edge = if is_tiled {
                    mapped
                        .tiling_node_id
                        .lock()
                        .unwrap()
                        .clone()
                        .map(|node_id| {
                            TilingLayout::possible_resizes(workspace.tiling_layer.tree(), node_id)
                        })
                        .unwrap_or(ResizeEdge::empty())
                } else {
                    ResizeEdge::all()
                };

                (
                    global_position,
                    items_for_element(mapped, is_tiled, false, workspace.tiling_enabled, edge),
                )
            }
        } else {
            return None;
        };

        let theme = self.theme.clone();
        let grab = MenuGrab::new(
            GrabStartData::Pointer(start_data),
            seat,
            menu_items,
            global_position,
            MenuAlignment::CORNER,
            None,
            evlh.clone(),
            theme,
        );

        Some((grab, Focus::Keep))
    }

    /// If a window's size fills the given zone, return the zone size scaled
    /// down by [`DRAG_UNMAXIMIZE_FRACTION`] for use as a drag target size.
    fn drag_shrink_size(
        win_w: i32,
        win_h: i32,
        zone: Rectangle<i32, Logical>,
    ) -> Option<Size<i32, Logical>> {
        if win_w >= zone.size.w && win_h >= zone.size.h {
            let (n, d) = DRAG_UNMAXIMIZE_FRACTION;
            Some(Size::from((zone.size.w * n / d, zone.size.h * n / d)))
        } else {
            None
        }
    }

    pub fn move_request(
        &mut self,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        release: ReleaseMode,
        move_out_of_stack: bool,
        config: &Config,
        evlh: &LoopHandle<'static, State>,
        client_initiated: bool,
    ) -> Option<(MoveGrab, Focus)> {
        if self.overview_mode().0.is_active() {
            return None;
        }

        let serial = serial.into();
        let mut element_geo = None;

        let mut start_data =
            check_grab_preconditions(seat, serial, client_initiated.then_some(surface))?;

        if client_initiated
            && start_data.distance(seat.get_pointer().unwrap().current_location()) < 1.
        {
            return Some((
                MoveGrab::delayed(
                    start_data,
                    surface,
                    seat,
                    serial,
                    release,
                    move_out_of_stack,
                ),
                Focus::Keep,
            ));
        }

        let maybe_fullscreen_workspace = self
            .workspaces
            .spaces_mut()
            .find(|w| w.get_fullscreen_surfaces().any(|f| &f.surface == surface));
        if let Some(workspace) = maybe_fullscreen_workspace {
            let fs = workspace
                .get_fullscreen_surfaces()
                .find(|f| &f.surface == surface)
                .unwrap();
            element_geo = Some(workspace.fullscreen_geometry_for(fs));
            let (surface, state, _) = workspace.remove_fullscreen_surface(surface).unwrap();
            self.remap_unfullscreened_window(surface, state, evlh);
        };

        let old_mapped = self.element_for_surface(surface).cloned()?;
        if old_mapped.is_minimized() {
            return None;
        }

        // Block move requests for embedded windows - they should not be draggable
        if old_mapped
            .windows()
            .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w))
        {
            return None;
        }

        let (window, _) = old_mapped
            .windows()
            .find(|(w, _)| w.wl_surface().as_deref() == Some(surface))
            .unwrap();

        let mapped = if move_out_of_stack {
            let new_mapped: CosmicMapped = CosmicWindow::new(
                window.clone(),
                evlh.clone(),
                self.theme.clone(),
                self.appearance_conf,
            )
            .into();
            if old_mapped.is_maximized(false) {
                new_mapped.set_maximized(false);
            }
            start_data.set_focus(new_mapped.focus_under(
                (0., 0.).into(),
                WindowSurfaceType::ALL,
                seat,
            ));
            new_mapped
        } else {
            old_mapped.clone()
        };

        if move_out_of_stack {
            // Update focus stack to set focus to the window being dragged out of
            // the stack.
            if let Some(workspace) = self.space_for_mut(&old_mapped) {
                let mut stack = workspace.focus_stack.get_mut(seat);
                stack.append(mapped.clone());
            }
        }

        let trigger = match &start_data {
            GrabStartData::Pointer(start_data) => Trigger::Pointer(start_data.button),
            GrabStartData::Touch(start_data) => Trigger::Touch(start_data.slot),
        };
        let active_hint = if config.cosmic_conf.active_hint {
            self.theme.active_hint as u8
        } else {
            0
        };
        let pointer = seat.get_pointer().unwrap();
        let pos = pointer.current_location().as_global();

        let cursor_output = if let Some(output) = self
            .outputs()
            .find(|output| {
                output
                    .geometry()
                    .as_logical()
                    .overlaps_or_touches(Rectangle::new(
                        start_data.location().to_i32_floor(),
                        (0, 0).into(),
                    ))
            })
            .cloned()
        {
            output
        } else {
            seat.active_output()
        };

        let (initial_window_location, layer, workspace_handle, target_size) =
            if let Some(workspace) = self.space_for_mut(&old_mapped) {
                let elem_geo = element_geo.or_else(|| workspace.element_geometry(&old_mapped))?;
                let mut initial_window_location = elem_geo.loc.to_global(workspace.output());

                let mut new_size =
                    if let Some(max_state) = old_mapped.maximized_state.lock().unwrap().take() {
                        // Unmaximize directly instead of going through the pipelined
                        // `unmaximize_request`. The element is about to be unmapped for
                        // the grab, so the deferred animation would never complete;
                        // worse, that path emits a configure that re-asserts the
                        // maximized state right before we clear it. A client-decorated
                        // window that sees maximized re-asserted keeps its resize
                        // handles hidden, so pulling it away from the top left it
                        // unresizable. Clear the flags and configure to the restore
                        // geometry in one clean step, mirroring the sticky branch below.
                        old_mapped.set_maximized(false);
                        old_mapped.set_tiled(false);

                        // If the restore geometry is as large as (or larger than) the
                        // output zone, shrink to 2/3 so the window actually appears
                        // unmaximized when dropped.
                        let zone = layer_map_for_output(workspace.output()).non_exclusive_zone();
                        let mut restore_geo = max_state.original_geometry;
                        if let Some(s) =
                            Self::drag_shrink_size(restore_geo.size.w, restore_geo.size.h, zone)
                        {
                            restore_geo.size = s.as_local();
                        }

                        old_mapped.set_geometry(restore_geo.to_global(workspace.output()));
                        old_mapped.configure();
                        Some(restore_geo.size.as_logical())
                    } else {
                        // Not maximized, but if the window fills the output zone,
                        // shrink it on drag so the user can reposition it.
                        let zone = layer_map_for_output(workspace.output()).non_exclusive_zone();
                        if let Some(new_size) =
                            Self::drag_shrink_size(elem_geo.size.w, elem_geo.size.h, zone)
                        {
                            old_mapped.set_geometry(Rectangle::new(
                                elem_geo.loc.to_global(workspace.output()),
                                new_size.as_local().as_global(),
                            ));
                            old_mapped.configure();
                            Some(new_size)
                        } else {
                            None
                        }
                    };

                let layer = if if mapped == old_mapped {
                    let was_floating = workspace.floating_layer.unmap(&mapped, None);
                    let was_tiled = workspace
                        .tiling_layer
                        .unmap_as_placeholder(&mapped, PlaceholderType::GrabbedWindow);
                    assert!(was_floating.is_some() != was_tiled.is_some());
                    if was_floating.is_some_and(|geo| geo.size != elem_geo.size) {
                        new_size = was_floating.map(|geo| geo.size.as_logical());
                    }
                    was_tiled.is_some()
                } else {
                    workspace
                        .tiling_layer
                        .mapped()
                        .any(|(m, _)| m == &old_mapped)
                } {
                    ManagedLayer::Tiling
                } else {
                    ManagedLayer::Floating
                };

                // if this changed the width, the window was tiled in floating mode
                if let Some(new_size) = new_size {
                    let output = workspace.output();
                    let ratio = pos.to_local(output).x / (elem_geo.loc.x + elem_geo.size.w) as f64;

                    initial_window_location = Point::from((
                        pos.x - (new_size.w as f64 * ratio),
                        pos.y - MOVE_GRAB_Y_OFFSET,
                    ))
                    .to_i32_round();
                }

                (initial_window_location, layer, workspace.handle, new_size)
            } else {
                let sticky_layer = self
                    .workspaces
                    .sets
                    .get_mut(&cursor_output)
                    .filter(|set| set.sticky_layer.mapped().any(|m| m == &old_mapped))
                    .map(|set| &mut set.sticky_layer)?;
                let elem_geo = sticky_layer.element_geometry(&old_mapped).unwrap();
                let mut initial_window_location = elem_geo.loc.to_global(&cursor_output);

                let mut new_size =
                    if let Some(state) = old_mapped.maximized_state.lock().unwrap().take() {
                        // If surface is maximized then unmaximize it
                        old_mapped.set_maximized(false);
                        old_mapped.set_tiled(false);
                        old_mapped.configure();
                        let mut new_size = state.original_geometry.size.as_logical();

                        // Clamp to 2/3 of zone if original size fills the output
                        let zone = layer_map_for_output(&cursor_output).non_exclusive_zone();
                        if let Some(s) = Self::drag_shrink_size(new_size.w, new_size.h, zone) {
                            new_size = s;
                        }

                        sticky_layer.map_internal(
                            mapped.clone(),
                            Some(state.original_geometry.loc),
                            Some(new_size),
                            None,
                        );

                        Some(new_size)
                    } else {
                        // Not maximized, but if the window fills the output zone,
                        // shrink it on drag so the user can reposition it.
                        let zone = layer_map_for_output(&cursor_output).non_exclusive_zone();
                        if let Some(new_size) =
                            Self::drag_shrink_size(elem_geo.size.w, elem_geo.size.h, zone)
                        {
                            old_mapped.set_geometry(Rectangle::new(
                                elem_geo.loc.to_global(&cursor_output),
                                new_size.as_local().as_global(),
                            ));
                            old_mapped.configure();
                            Some(new_size)
                        } else {
                            None
                        }
                    };

                if mapped == old_mapped
                    && let Some(geo) = sticky_layer.unmap(&mapped, None)
                    && geo.size != elem_geo.size
                {
                    new_size = Some(geo.size.as_logical());
                }

                if let Some(new_size) = new_size {
                    let ratio =
                        pos.to_local(&cursor_output).x / (elem_geo.loc.x + elem_geo.size.w) as f64;
                    initial_window_location = Point::<f64, _>::from((
                        pos.x - (new_size.w as f64 * ratio),
                        pos.y - MOVE_GRAB_Y_OFFSET,
                    ))
                    .to_i32_round();
                }

                (
                    initial_window_location,
                    ManagedLayer::Sticky,
                    self.active_space(&cursor_output).unwrap().handle,
                    new_size,
                )
            };

        toplevel_leave_workspace(&window, &workspace_handle);
        toplevel_leave_output(&window, &cursor_output);

        if move_out_of_stack {
            old_mapped.stack_ref().unwrap().remove_window(&window);
            self.workspaces
                .space_for_handle_mut(&workspace_handle)
                .unwrap()
                .refresh();
        }

        mapped.set_activate(true);
        mapped.configure();

        // Collect and unmap X11 transient children so they can be rendered
        // alongside the parent during drag with zero drift.
        let transient_children =
            self.collect_and_unmap_x11_transient_children(&mapped, initial_window_location);

        let grab = MoveGrab::new(
            start_data,
            mapped,
            seat,
            initial_window_location,
            cursor_output,
            active_hint,
            config.cosmic_conf.edge_snap_threshold as f64,
            layer,
            release,
            evlh.clone(),
            transient_children,
            target_size,
        );

        if grab.is_tiling_grab() {
            self.set_overview_mode(Some(trigger), evlh.clone());
        }

        Some((grab, Focus::Clear))
    }

    // Just to avoid a longer lived shell reference
    /// Get the window geometry of a keyboard focus target
    pub fn focused_geometry(&self, target: &KeyboardFocusTarget) -> Option<Rectangle<i32, Global>> {
        match target {
            KeyboardFocusTarget::Fullscreen(surface) => surface
                .wl_surface()
                .and_then(|s| self.workspace_for_surface(&s))
                .and_then(|(handle, _)| self.workspaces.space_for_handle(&handle))
                .map(|workspace| {
                    workspace
                        .fullscreen_geometry_for_surface(surface)
                        .to_global(workspace.output())
                }),
            _ => {
                if let Some(element) = self.focused_element(target) {
                    self.element_geometry(&element)
                } else {
                    None
                }
            }
        }
    }

    pub fn element_geometry(&self, mapped: &CosmicMapped) -> Option<Rectangle<i32, Global>> {
        if let Some(set) = self
            .workspaces
            .sets
            .values()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set
                .sticky_layer
                .element_geometry(mapped)
                .unwrap()
                .to_global(&set.output);
            Some(geometry)
        } else if let Some(workspace) = self.space_for(mapped) {
            let geometry = workspace
                .element_geometry(mapped)
                .unwrap()
                .to_global(workspace.output());
            Some(geometry)
        } else {
            None
        }
    }

    #[must_use]
    pub fn next_focus(&self, direction: FocusDirection, seat: &Seat<State>) -> FocusResult {
        let overview = self.overview_mode().0;
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return FocusResult::None;
        };
        let output = seat.active_output();

        if matches!(target, KeyboardFocusTarget::Fullscreen(_)) {
            return FocusResult::None;
        }

        let set = self.workspaces.sets.get(&output).unwrap();
        let sticky_layer = &set.sticky_layer;
        let workspace = &set.workspaces[set.active];

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(_) => get_popup_toplevel(&popup),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return FocusResult::None;
                };
                sticky_layer
                    .space
                    .elements()
                    .chain(workspace.mapped())
                    .find(|elem| elem.wl_surface().as_deref() == Some(&toplevel_surface))
            }
            KeyboardFocusTarget::Element(elem) => sticky_layer
                .space
                .elements()
                .chain(workspace.mapped())
                .find(|e| *e == &elem),
            KeyboardFocusTarget::Group { .. } => {
                let focus_stack = workspace.focus_stack.get(seat);
                let swap_desc = match overview.active_trigger() {
                    Some(Trigger::KeyboardSwap(_, desc)) => Some(desc.clone()),
                    _ => None,
                };

                return workspace.tiling_layer.next_focus(
                    direction,
                    seat,
                    focus_stack.iter(),
                    swap_desc,
                );
            }
            _ => None,
        })
        .cloned() else {
            return FocusResult::None;
        };

        if focused.handle_focus(seat, direction, None) {
            return FocusResult::Handled;
        }

        if workspace.is_tiled(&focused.active_window()) {
            if focused.is_maximized(false) {
                return FocusResult::None;
            }

            let focus_stack = workspace.focus_stack.get(seat);
            let swap_desc = match overview.active_trigger() {
                Some(Trigger::KeyboardSwap(_, desc)) => Some(desc.clone()),
                _ => None,
            };

            workspace
                .tiling_layer
                .next_focus(direction, seat, focus_stack.iter(), swap_desc)
        } else {
            let floating_layer = &set.workspaces[set.active].floating_layer;

            let geometry = sticky_layer
                .space
                .element_geometry(&focused)
                .or_else(|| floating_layer.space.element_geometry(&focused))
                .unwrap();

            let elements = sticky_layer
                .space
                .elements()
                .chain(floating_layer.space.elements())
                .filter(|elem| *elem != &focused)
                .map(|elem| {
                    (
                        elem,
                        sticky_layer
                            .space
                            .element_geometry(elem)
                            .or_else(|| floating_layer.space.element_geometry(elem))
                            .unwrap(),
                    )
                });

            let next = match direction {
                FocusDirection::Up => elements
                    .filter(|(_, other_geo)| other_geo.loc.y <= geometry.loc.y)
                    .min_by_key(|(_, other_geo)| {
                        let res = geometry.loc.y - other_geo.loc.y;
                        if res.is_positive() { res } else { i32::MAX }
                    }),
                FocusDirection::Down => elements
                    .filter(|(_, other_geo)| other_geo.loc.y > geometry.loc.y)
                    .max_by_key(|(_, other_geo)| {
                        let res = geometry.loc.y - other_geo.loc.y;
                        if res.is_negative() { res } else { i32::MIN }
                    }),
                FocusDirection::Left => elements
                    .filter(|(_, other_geo)| other_geo.loc.x <= geometry.loc.x)
                    .min_by_key(|(_, other_geo)| {
                        let res = geometry.loc.x - other_geo.loc.x;
                        if res.is_positive() { res } else { i32::MAX }
                    }),
                FocusDirection::Right => elements
                    .filter(|(_, other_geo)| other_geo.loc.x > geometry.loc.x)
                    .max_by_key(|(_, other_geo)| {
                        let res = geometry.loc.x - other_geo.loc.x;
                        if res.is_negative() { res } else { i32::MIN }
                    }),
                _ => return FocusResult::None,
            }
            .map(|(other, _)| other);

            next.map(|elem| FocusResult::Some(KeyboardFocusTarget::Element(elem.clone())))
                .unwrap_or(FocusResult::None)
        }
    }

    #[must_use]
    pub fn move_current_element(&mut self, direction: Direction, seat: &Seat<State>) -> MoveResult {
        let Some(output) = seat.focused_output() else {
            return MoveResult::None;
        };
        let workspace = self.active_space(&output).unwrap();
        let focus_stack = workspace.focus_stack.get(seat);
        match focus_stack.last().cloned() {
            Some(FocusTarget::Fullscreen(surface)) => {
                MoveResult::MoveFurther(KeyboardFocusTarget::Fullscreen(surface))
            }
            Some(FocusTarget::Window(mapped)) => {
                if let Some(set) = self
                    .workspaces
                    .sets
                    .values_mut()
                    .find(|set| set.sticky_layer.mapped().any(|m| &mapped == m))
                {
                    set.sticky_layer.move_current_element(
                        direction,
                        seat,
                        ManagedLayer::Sticky,
                        self.theme.clone(),
                    )
                } else {
                    let theme = self.theme.clone();
                    if mapped
                        .maximized_state
                        .lock()
                        .unwrap()
                        .as_ref()
                        .is_some_and(|state| state.original_layer == ManagedLayer::Tiling)
                    {
                        self.unmaximize_request(&mapped);
                    }

                    let workspace = self.active_space_mut(&output).unwrap();
                    workspace
                        .floating_layer
                        .move_current_element(direction, seat, ManagedLayer::Floating, theme)
                        .or_else(|| workspace.tiling_layer.move_current_node(direction, seat))
                }
            }
            _ => MoveResult::None,
        }
    }

    pub fn menu_resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        edge: ResizeEdge,
        edge_snap_threshold: u32,
    ) -> Option<(
        (
            Option<(PointerFocusTarget, Point<f64, Logical>)>,
            Point<i32, Global>,
        ),
        (ResizeGrab, Focus),
    )> {
        if mapped.is_fullscreen(true) || mapped.is_maximized(true) {
            return None;
        }

        let mut start_data = check_grab_preconditions(seat, None, None)?;

        let (floating_layer, geometry) = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set
                .sticky_layer
                .element_geometry(mapped)
                .unwrap()
                .to_global(&set.output);
            (&mut set.sticky_layer, geometry)
        } else {
            let workspace = self.space_for_mut(mapped)?;
            let geometry = workspace
                .element_geometry(mapped)
                .unwrap()
                .to_global(workspace.output());
            (&mut workspace.floating_layer, geometry)
        };

        let new_loc = if edge.contains(ResizeEdge::LEFT) {
            Point::<i32, Global>::from((geometry.loc.x, geometry.loc.y + (geometry.size.h / 2)))
        } else if edge.contains(ResizeEdge::RIGHT) {
            Point::<i32, Global>::from((
                geometry.loc.x + geometry.size.w,
                geometry.loc.y + (geometry.size.h / 2),
            ))
        } else if edge.contains(ResizeEdge::TOP) {
            Point::<i32, Global>::from((geometry.loc.x + (geometry.size.w / 2), geometry.loc.y))
        } else if edge.contains(ResizeEdge::BOTTOM) {
            Point::<i32, Global>::from((
                geometry.loc.x + (geometry.size.w / 2),
                geometry.loc.y + geometry.size.h,
            ))
        } else {
            return None;
        };

        let element_offset = (new_loc - geometry.loc).as_logical();
        let focus = mapped
            .focus_under(element_offset.to_f64(), WindowSurfaceType::ALL, seat)
            .map(|(target, surface_offset)| (target, (surface_offset + element_offset.to_f64())));
        start_data.set_location(new_loc.as_logical().to_f64());
        start_data.set_focus(focus.clone());

        let grab: ResizeGrab = if let Some(grab) = floating_layer.resize_request(
            mapped,
            seat,
            start_data.clone(),
            edge,
            edge_snap_threshold,
            ReleaseMode::Click,
        ) {
            grab.into()
        } else {
            let ws = self.space_for_mut(mapped)?;
            let node_id = mapped.tiling_node_id.lock().unwrap().clone()?;
            let (node, left_up_idx, orientation) = ws.tiling_layer.resize_request(node_id, edge)?;
            ResizeForkGrab::new(
                start_data,
                new_loc.to_f64(),
                node,
                left_up_idx,
                orientation,
                ws.output.downgrade(),
                ReleaseMode::Click,
            )
            .into()
        };

        Some(((focus, new_loc), (grab, Focus::Keep)))
    }

    pub fn maximize_toggle(
        &mut self,
        window: &CosmicMapped,
        seat: &Seat<State>,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        // Dispatch on `maximized_state`, not the toplevel's protocol flag: both
        // branches below act on `maximized_state`, so keying off the flag makes a
        // desync unrecoverable (unmaximize finds no state and no-ops, and the flag
        // stays set, so maximize is never reached again).
        if window.maximized_state.lock().unwrap().is_some() {
            self.unmaximize_request_with_options(window);
        } else {
            if window.is_fullscreen(true) {
                return;
            }
            self.maximize_request_with_options(window, seat, true, false, loop_handle);
        }
    }

    pub fn minimize_request<S>(&mut self, surface: &S)
    where
        CosmicSurface: PartialEq<S>,
    {
        if let Some((set, mapped)) = self.workspaces.sets.values_mut().find_map(|set| {
            let mapped = set
                .sticky_layer
                .mapped()
                .find(|m| &m.active_window() == surface)
                .cloned();
            mapped.map(|m| (set, m))
        }) {
            let to = minimize_rectangle(&set.output, &mapped.active_window());
            let geo = set.sticky_layer.unmap(&mapped, Some(to)).unwrap();
            set.minimized_windows.push(MinimizedWindow::Floating {
                window: mapped.clone(),
                previous: FloatingRestoreData {
                    geometry: geo,
                    output_size: set.output.geometry().size.as_logical(),
                    was_maximized: false,
                    was_snapped: None,
                },
            });
        } else if let Some((workspace, window)) =
            self.workspaces.sets.values_mut().find_map(|set| {
                set.workspaces.iter_mut().find_map(|workspace| {
                    let window = workspace
                        .get_fullscreen_surfaces()
                        .map(|f| f.surface.clone())
                        .chain(workspace.mapped().map(|m| m.active_window()))
                        .find(|s| s == surface);
                    window.map(|s| (workspace, s))
                })
            })
        {
            let to = minimize_rectangle(workspace.output(), &window);
            if let Some(minimized) = workspace.minimize(surface, to) {
                workspace.minimized_windows.push(minimized);
            }
        }
        // Re-evaluate auto-hide — minimizing may leave the workspace empty.
        self.refresh_auto_hide();
    }

    pub fn unminimize_request<S>(
        &mut self,
        surface: &S,
        seat: &Seat<State>,
        loop_handle: &LoopHandle<'static, State>,
    ) where
        CosmicSurface: PartialEq<S>,
    {
        if let Some((set, window)) = self.workspaces.sets.values_mut().find_map(|set| {
            set.minimized_windows
                .iter()
                .position(|m| m.windows().any(|s| &s == surface))
                .map(|i| set.minimized_windows.swap_remove(i))
                .map(|window| (set, window))
        }) {
            let MinimizedWindow::Floating { window, previous } = window else {
                unreachable!("None sticky window in WorkspaceSet minimized_windows");
            };

            let from = minimize_rectangle(&set.output, &window.active_window());
            let previous_position =
                previous.position_relative(set.output.geometry().size.as_logical());
            if window.is_stack() {
                window.set_active(surface);
            }
            set.sticky_layer
                .remap_minimized(window, from, previous_position);
        } else {
            let Some((workspace, window)) = self.workspaces.spaces_mut().find_map(|w| {
                w.minimized_windows
                    .iter()
                    .position(|m| m.windows().any(|s| &s == surface))
                    .map(|i| w.minimized_windows.swap_remove(i))
                    .map(|window| (w, window))
            }) else {
                return;
            };

            if window.mapped().is_some_and(|m| m.is_stack()) {
                window.mapped().unwrap().set_active(surface);
            }
            let from = minimize_rectangle(workspace.output(), &window.active_window());
            if let Some((surface, restore, _)) = workspace.unminimize(window, from, seat) {
                toplevel_leave_output(&surface, &workspace.output);
                toplevel_leave_workspace(&surface, &workspace.handle);
                self.remap_unfullscreened_window(surface, restore, loop_handle);
            }
        }
        // Re-evaluate auto-hide — unminimizing adds a visible window.
        self.refresh_auto_hide();
    }

    pub fn maximize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        animate: bool,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        self.maximize_request_with_options(mapped, seat, animate, false, loop_handle)
    }

    /// Maximize with fade-in animation only (no geometry transition).
    /// Used for windows that start maximized - they appear at full size and fade in.
    pub fn maximize_request_fade_in(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        self.maximize_request_with_options(mapped, seat, true, true, loop_handle)
    }

    /// Maximize with pipelined client-driven resize animation.
    pub fn maximize_request_with_options(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        animate: bool,
        fade_in_only: bool,
        loop_handle: &LoopHandle<'static, State>,
    ) {
        // Don't allow maximizing embedded windows
        if mapped
            .windows()
            .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w))
        {
            return;
        }

        self.unminimize_request(&mapped.active_window(), seat, loop_handle);

        let (original_layer, floating_layer, mut original_geometry) = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set.sticky_layer.element_geometry(mapped).unwrap();
            (ManagedLayer::Sticky, &mut set.sticky_layer, geometry)
        } else if let Some(workspace) = self.space_for_mut(mapped) {
            let layer = if workspace.is_tiled(&mapped.active_window()) {
                ManagedLayer::Tiling
            } else {
                ManagedLayer::Floating
            };
            let geometry = workspace.element_geometry(mapped).unwrap();
            (layer, &mut workspace.floating_layer, geometry)
        } else {
            return;
        };

        // If the window has a pending size (e.g., it was created with a specific size
        // but immediately maximized), use that as the original geometry size.
        // This ensures windows that start maximized remember their intended windowed size.
        if let Some(pending_size) = mapped.pending_size() {
            original_geometry.size = pending_size.as_local();
        }

        let mut state = mapped.maximized_state.lock().unwrap();
        if state.is_none() {
            *state = Some(MaximizedState {
                original_geometry,
                original_layer,
                original_snapped: None,
            });
            std::mem::drop(state);
            if fade_in_only && animate {
                floating_layer.map_maximized_fade_in(mapped.clone(), original_geometry);
            } else if animate {
                floating_layer.start_pipelined_maximize(mapped.clone(), original_geometry);
            } else {
                floating_layer.map_maximized(mapped.clone(), original_geometry, false);
            }
            // Trigger auto-hide for surfaces on the same output.
            self.refresh_auto_hide();
        }
    }

    pub fn unmaximize_request(&mut self, mapped: &CosmicMapped) -> Option<Size<i32, Logical>> {
        self.unmaximize_request_with_options(mapped)
    }

    /// Unmaximize with pipelined client-driven resize animation.
    pub fn unmaximize_request_with_options(
        &mut self,
        mapped: &CosmicMapped,
    ) -> Option<Size<i32, Logical>> {
        if let Some(set) = self.workspaces.sets.values_mut().find(|set| {
            set.sticky_layer.mapped().any(|m| m == mapped)
                || set
                    .minimized_windows
                    .iter()
                    .any(|m| m.mapped().is_some_and(|m| m == mapped))
        }) {
            let mut state = mapped.maximized_state.lock().unwrap();
            if let Some(state) = state.take() {
                assert_eq!(state.original_layer, ManagedLayer::Sticky);

                if let Some(minimized) = set
                    .minimized_windows
                    .iter_mut()
                    .find(|m| m.mapped().is_some_and(|m| m == mapped))
                {
                    minimized.unmaximize(state.original_geometry, state.original_snapped);
                } else {
                    set.sticky_layer
                        .start_pipelined_unmaximize(mapped.clone(), state.original_geometry);
                }
                // Trigger auto-hide update after unmaximize.
                self.refresh_auto_hide();
                Some(state.original_geometry.size.as_logical())
            } else {
                None
            }
        } else if let Some(workspace) = self.space_for_mut(mapped) {
            let result = workspace
                .unmaximize_request_with_options(mapped)
                .map(|geo| geo.size.as_logical());
            // Trigger auto-hide update after unmaximize.
            self.refresh_auto_hide();
            result
        } else {
            None
        }
    }

    pub fn resize_request(
        &mut self,
        surface: &WlSurface,
        seat: &Seat<State>,
        serial: impl Into<Option<Serial>>,
        edges: ResizeEdge,
        edge_snap_threshold: u32,
        client_initiated: bool,
    ) -> Option<(ResizeGrab, Focus)> {
        let serial = serial.into();
        let start_data =
            check_grab_preconditions(seat, serial, client_initiated.then_some(surface))?;
        let mapped = self.element_for_surface(surface).cloned()?;
        if mapped.is_maximized(true) {
            return None;
        }

        if mapped
            .windows()
            .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w))
        {
            return None;
        }

        // Reject duplicate resize requests (e.g. Steam sends two
        // _NET_WM_MOVERESIZE messages). Creating a new grab while one is
        // already active would corrupt the resize state when the old grab's
        // ungrab() overwrites the freshly-set Resizing state.
        if let Some(ResizeState::Resizing(data)) = *mapped.resize_state.lock().unwrap() {
            tracing::warn!(
                app_id = mapped.active_window().app_id(),
                active_edges = ?data.edges,
                requested_edges = ?edges,
                "Rejecting duplicate resize request while resize is already active",
            );
            return None;
        }

        let floating_layer = if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == &mapped))
        {
            &mut set.sticky_layer
        } else {
            let workspace = self.space_for_mut(&mapped)?;
            &mut workspace.floating_layer
        };

        let grab: ResizeGrab = if let Some(grab) = floating_layer.resize_request(
            &mapped,
            seat,
            start_data.clone(),
            edges,
            edge_snap_threshold,
            ReleaseMode::NoMouseButtons,
        ) {
            grab.into()
        } else {
            let ws = self.space_for_mut(&mapped)?;
            let node_id = mapped.tiling_node_id.lock().unwrap().clone()?;
            let (node, left_up_idx, orientation) =
                ws.tiling_layer.resize_request(node_id, edges)?;
            ResizeForkGrab::new(
                start_data,
                seat.get_pointer().unwrap().current_location().as_global(),
                node,
                left_up_idx,
                orientation,
                ws.output.downgrade(),
                ReleaseMode::NoMouseButtons,
            )
            .into()
        };

        Some((grab, Focus::Clear))
    }

    pub fn resize(&mut self, seat: &Seat<State>, direction: ResizeDirection, edge: ResizeEdge) {
        let Some(output) = seat.focused_output() else {
            return;
        };
        let (_, idx) = self.workspaces.active_num(&output);
        let Some(focused) = seat.get_keyboard().unwrap().current_focus() else {
            return;
        };
        let amount = (self
            .resize_state
            .take()
            .map(|(_, _, _, amount, _, _)| amount)
            .unwrap_or(10)
            + 2)
        .min(20);

        if self
            .workspaces
            .sets
            .get_mut(&output)
            .unwrap()
            .sticky_layer
            .resize(&focused, direction, edge, amount)
        {
            self.resize_state = Some((focused, direction, edge, amount, idx, output));
        } else if let Some(workspace) = self.workspaces.get_mut(idx, &output)
            && workspace.resize(&focused, direction, edge, amount)
        {
            self.resize_state = Some((focused, direction, edge, amount, idx, output));
        }
    }

    pub fn finish_resize(&mut self, direction: ResizeDirection, edge: ResizeEdge) {
        if let Some((old_focused, old_direction, old_edge, _, idx, output)) =
            self.resize_state.take()
            && old_direction == direction
            && old_edge == edge
        {
            let Some(toplevel) = old_focused.toplevel() else {
                return;
            };
            let Some(mapped) = self
                .workspaces
                .sets
                .values()
                .find_map(|set| {
                    set.sticky_layer
                        .mapped()
                        .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                })
                .cloned()
                .or_else(|| {
                    let workspace = self.workspaces.get(idx, &output).unwrap();
                    workspace
                        .mapped()
                        .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
                        .cloned()
                })
            else {
                return;
            };

            let mut resize_state = mapped.resize_state.lock().unwrap();
            if let Some(ResizeState::Resizing(data)) = *resize_state {
                mapped.set_resizing(false);
                *resize_state = Some(ResizeState::WaitingForCommit(data));
            }
        }
    }

    #[must_use]
    pub fn toggle_stacking(
        &mut self,
        seat: &Seat<State>,
        window: &CosmicMapped,
    ) -> Option<KeyboardFocusTarget> {
        if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == window))
        {
            let workspace = &mut set.workspaces[set.active];
            set.sticky_layer
                .toggle_stacking(window, workspace.focus_stack.get_mut(seat))
        } else if let Some(workspace) = self.space_for_mut(window) {
            if workspace.tiling_layer.mapped().any(|(m, _)| m == window) {
                workspace
                    .tiling_layer
                    .toggle_stacking(window, workspace.focus_stack.get_mut(seat))
            } else if workspace.floating_layer.mapped().any(|w| w == window) {
                workspace
                    .floating_layer
                    .toggle_stacking(window, workspace.focus_stack.get_mut(seat))
            } else {
                None
            }
        } else {
            None
        }
    }

    #[must_use]
    pub fn toggle_stacking_focused(
        &mut self,
        seat: &Seat<State>,
        loop_handle: &LoopHandle<'static, State>,
    ) -> Option<KeyboardFocusTarget> {
        let focused_output = seat.focused_output()?;
        let set = self.workspaces.sets.get_mut(&focused_output).unwrap();
        let workspace = &mut set.workspaces[set.active];

        if matches!(
            seat.get_keyboard().unwrap().current_focus(),
            Some(KeyboardFocusTarget::Fullscreen(_))
        ) {
            return None;
        }

        let maybe_window = workspace.focus_stack.get(seat).iter().next().cloned();
        if let Some(FocusTarget::Window(window)) = maybe_window {
            let was_maximized = window.is_maximized(false);
            if was_maximized {
                workspace.unmaximize_request(&window);
            }

            let res = if set.sticky_layer.mapped().any(|m| m == &window) {
                set.sticky_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else if workspace.tiling_layer.mapped().any(|(m, _)| m == &window) {
                workspace
                    .tiling_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else if workspace.floating_layer.mapped().any(|w| w == &window) {
                workspace
                    .floating_layer
                    .toggle_stacking_focused(seat, workspace.focus_stack.get_mut(seat))
            } else {
                None
            };

            if was_maximized && let Some(KeyboardFocusTarget::Element(mapped)) = res.as_ref() {
                self.maximize_request(mapped, seat, false, loop_handle);
            }

            res
        } else {
            None
        }
    }

    pub fn toggle_sticky(&mut self, seat: &Seat<State>, mapped: &CosmicMapped) {
        // clean from focus-stacks
        for workspace in self.workspaces.spaces_mut() {
            for seat in self.seats.iter() {
                let mut stack = workspace.focus_stack.get_mut(seat);
                stack.remove(mapped);
            }
        }

        if let Some(workspace) = self.space_for_mut(mapped) {
            let previous_layer = if workspace.is_tiled(&mapped.active_window()) {
                workspace.toggle_floating_window(seat, mapped);
                ManagedLayer::Tiling
            } else {
                ManagedLayer::Floating
            };
            let Some(geometry) = workspace.element_geometry(mapped) else {
                return;
            };
            workspace.unmap_element(mapped);

            *mapped.previous_layer.lock().unwrap() = Some(previous_layer);
            let output = workspace.output().clone();
            let handle = workspace.handle;

            for (window, _) in mapped.windows() {
                window.set_sticky(true);
                toplevel_leave_workspace(&window, &handle);
            }

            let set = self.workspaces.sets.get_mut(&output).unwrap();
            set.sticky_layer.map(mapped.clone(), geometry.loc);

            let mut state = mapped.maximized_state.lock().unwrap();
            if let Some(MaximizedState {
                original_geometry,
                original_layer: _,
                original_snapped,
            }) = *state
            {
                *state = Some(MaximizedState {
                    original_geometry,
                    original_layer: ManagedLayer::Sticky,
                    original_snapped,
                });
                std::mem::drop(state);
                set.workspaces[set.active].floating_layer.map_maximized(
                    mapped.clone(),
                    geometry,
                    false,
                );
            }
        } else if let Some(set) = self
            .workspaces
            .sets
            .values_mut()
            .find(|set| set.sticky_layer.mapped().any(|m| m == mapped))
        {
            let geometry = set.sticky_layer.unmap(mapped, None).unwrap();

            let workspace = &mut set.workspaces[set.active];
            for (window, _) in mapped.windows() {
                toplevel_enter_workspace(&window, &workspace.handle);
                window.set_sticky(false);
            }
            let previous_layer = mapped
                .previous_layer
                .lock()
                .unwrap()
                .take()
                .unwrap_or(ManagedLayer::Floating);

            match previous_layer {
                ManagedLayer::Tiling if workspace.tiling_enabled => {
                    let focus_stack = workspace.focus_stack.get(seat);
                    workspace
                        .tiling_layer
                        .map(mapped.clone(), Some(focus_stack.iter()), None);
                }
                ManagedLayer::Sticky => unreachable!(),
                _ => workspace.floating_layer.map(mapped.clone(), geometry.loc),
            }

            let mut state = mapped.maximized_state.lock().unwrap();
            if let Some(MaximizedState {
                original_geometry,
                original_layer: _,
                original_snapped,
            }) = *state
            {
                *state = Some(MaximizedState {
                    original_geometry,
                    original_layer: previous_layer,
                    original_snapped,
                });
                std::mem::drop(state);
                workspace
                    .floating_layer
                    .map_maximized(mapped.clone(), geometry, false);
            }
        }

        self.append_focus_stack(mapped.clone(), seat);
    }

    pub fn toggle_sticky_current(&mut self, seat: &Seat<State>) {
        if matches!(
            seat.get_keyboard().unwrap().current_focus(),
            Some(KeyboardFocusTarget::Fullscreen(_))
        ) {
            return;
        }
        let set = self.workspaces.sets.get_mut(&seat.active_output()).unwrap();
        let workspace = &mut set.workspaces[set.active];

        let maybe_window = workspace.focus_stack.get(seat).iter().next().cloned();
        if let Some(FocusTarget::Window(mapped)) = maybe_window {
            self.toggle_sticky(seat, &mapped);
        }
    }

    #[must_use]
    pub fn fullscreen_request<S>(
        &mut self,
        surface: &S,
        output: Output,
        _loop_handle: &LoopHandle<'static, State>,
    ) -> Option<KeyboardFocusTarget>
    where
        CosmicSurface: PartialEq<S>,
    {
        let mapped = self.element_for_surface(surface).cloned()?;

        // Don't allow fullscreening embedded windows
        if mapped
            .windows()
            .any(|(w, _)| crate::wayland::handlers::surface_embed::is_surface_embedded(&w))
        {
            return None;
        }

        let seat = self.seats.last_active().clone();
        let window;

        if let Some((old_output, set)) = self
            .workspaces
            .sets
            .iter_mut()
            .find(|(_, set)| set.sticky_layer.mapped().any(|m| m == &mapped))
        {
            let mut from = set.sticky_layer.element_geometry(&mapped).unwrap();
            let mut was_maximized = false;
            let mut restore_state = None;
            let was_stack = mapped.is_stack();
            window = if let Some(stack) = mapped.stack_ref()
                && stack.len() > 1
            {
                let idx = stack.surfaces().position(|s| &s == surface)?;
                let surface = stack.remove_idx(idx)?;
                restore_state = Some(FullscreenRestoreState::Stack {
                    state: StackRestoreData {
                        stack: mapped.key(),
                        idx,
                    },
                });
                surface
            } else {
                // Must be set before `map_internal`/`unmap` below, as both may call
                // intermediate `configure()`, which would send a configure event without the
                // fullscreen state, causing clients like Chromium to cancel the transition.
                mapped.set_fullscreen(true);

                if let Some(state) = mapped.maximized_state.lock().unwrap().take() {
                    mapped.set_maximized(false);
                    set.sticky_layer.map_internal(
                        mapped.clone(),
                        Some(state.original_geometry.loc),
                        Some(state.original_geometry.size.as_logical()),
                        None,
                    );
                    was_maximized = true;
                }

                from = set.sticky_layer.unmap(&mapped, None).unwrap();
                mapped.active_window()
            };

            toplevel_leave_output(&window, old_output);
            let old_output = old_output.downgrade();
            let workspace_handle = self.active_space(&output).unwrap().handle;
            toplevel_enter_output(&window, &output);
            toplevel_enter_workspace(&window, &workspace_handle);

            let workspace = self.active_space_mut(&output).unwrap();
            workspace.map_fullscreen(
                &window,
                &seat,
                Some(restore_state.unwrap_or(FullscreenRestoreState::Sticky {
                    output: old_output,
                    state: FloatingRestoreData {
                        geometry: from,
                        output_size: workspace.output.geometry().size.as_logical(),
                        was_maximized,
                        was_snapped: None,
                    },
                    was_stack,
                })),
                Some(from),
            );
        } else {
            let workspace = self.space_for_mut(&mapped)?;
            if mapped.is_minimized() {
                // TODO: Rewrite the `MinimizedWindow` to restore to fullscreen
                return None;
            }

            // Must be set before `unmap_surface()`.
            // `Workspace::unmap_surface` may call intermediate `configure()` internally, which would send
            // a configure event without the fullscreen state, causing clients like Chromium to cancel the transition.
            mapped.set_fullscreen(true);

            let from = workspace.element_geometry(&mapped).unwrap();
            let (surface, state) = workspace.unmap_surface(surface).unwrap();
            window = surface;
            let handle = workspace.handle;

            toplevel_leave_output(&window, &workspace.output);
            toplevel_leave_workspace(&window, &workspace.handle);

            let workspace = self.active_space_mut(&output).unwrap();
            toplevel_enter_output(&window, &output);
            toplevel_enter_workspace(&window, &workspace.handle);

            workspace.map_fullscreen(
                &window,
                &seat,
                match state {
                    WorkspaceRestoreData::Floating(floating_state) => {
                        Some(FullscreenRestoreState::Floating {
                            workspace: handle,
                            state: floating_state,
                            was_stack: mapped.is_stack(),
                        })
                    }
                    WorkspaceRestoreData::Tiling(tiling_state) => {
                        Some(FullscreenRestoreState::Tiling {
                            workspace: handle,
                            state: tiling_state,
                            was_stack: mapped.is_stack(),
                        })
                    }
                    WorkspaceRestoreData::Stack(stack_state) => {
                        Some(FullscreenRestoreState::Stack { state: stack_state })
                    }
                    WorkspaceRestoreData::Fullscreen(_) => unreachable!(),
                },
                Some(from),
            );
        };

        // Trigger auto-hide for surfaces on the same output.
        self.refresh_auto_hide();

        Some(KeyboardFocusTarget::Fullscreen(window))
    }

    pub fn unfullscreen_request<S>(
        &mut self,
        surface: &S,
        loop_handle: &LoopHandle<'static, State>,
    ) -> Option<KeyboardFocusTarget>
    where
        CosmicSurface: PartialEq<S>,
    {
        let maybe_workspace = self.workspaces.iter_mut().find_map(|(_, s)| {
            s.workspaces
                .iter_mut()
                .find(|w| w.get_fullscreen_surfaces().any(|f| &f.surface == surface))
        });

        if let Some(workspace) = maybe_workspace {
            let (old_fullscreen, restore, _) =
                workspace.remove_fullscreen_surface(surface).unwrap();
            toplevel_leave_output(&old_fullscreen, &workspace.output);
            toplevel_leave_workspace(&old_fullscreen, &workspace.handle);

            let window = self.remap_unfullscreened_window(old_fullscreen, restore, loop_handle);
            // Trigger auto-hide update after un-fullscreen.
            self.refresh_auto_hide();
            Some(KeyboardFocusTarget::Element(window))
        } else {
            None
        }
    }

    pub fn update_toolkit(
        &mut self,
        toolkit: crate::toolkit_config::ToolkitConfig,
        xdg_activation_state: &XdgActivationState,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        let mut container = crate::toolkit_config::TOOLKIT_CONFIG.write().unwrap();
        if *container != toolkit {
            *container = toolkit;
            drop(container);
            self.refresh(xdg_activation_state, workspace_state);
            self.workspaces.force_redraw();
        }
    }

    pub fn set_theme(
        &mut self,
        theme: crate::comp_theme::CompTheme,
        xdg_activation_state: &XdgActivationState,
        workspace_state: &mut WorkspaceUpdateGuard<'_, State>,
    ) {
        self.theme = theme.clone();
        self.refresh(xdg_activation_state, workspace_state);

        // Update all mapped windows (SSDs, tab bars, etc.)
        self.workspaces.set_theme(theme.clone());

        // Update transient shell UI elements
        if let Some(ref indicator) = self.swap_indicator {
            indicator.set_theme(theme.clone());
        }
        if let Some(ref indicator) = self.resize_indicator {
            indicator.set_theme(theme.clone());
        }

        // Update zoom UI elements on all outputs
        if self.zoom_state.is_some() {
            for output in self.outputs().cloned().collect::<Vec<_>>() {
                if let Some(state) = output.user_data().get::<Mutex<OutputZoomState>>() {
                    state.lock().unwrap().element.set_theme(theme.clone());
                }
            }
        }
    }

    pub fn theme(&self) -> &crate::comp_theme::CompTheme {
        &self.theme
    }

    pub fn update_tiling_exceptions<'a, I>(&mut self, exceptions: I)
    where
        I: Iterator<Item = &'a ApplicationException>,
    {
        self.tiling_exceptions = layout::TilingExceptions::new(exceptions);
    }

    pub fn take_presentation_feedback(
        &self,
        output: &Output,
        render_element_states: &RenderElementStates,
    ) -> OutputPresentationFeedback {
        let mut output_presentation_feedback = OutputPresentationFeedback::new(output);

        if let Some(active) = self.active_space(output) {
            active.mapped().for_each(|mapped| {
                mapped.active_window().take_presentation_feedback(
                    &mut output_presentation_feedback,
                    surface_primary_scanout_output,
                    |surface, _| {
                        surface_presentation_feedback_flags_from_states(
                            surface,
                            None,
                            render_element_states,
                        )
                    },
                );
            });
        }

        self.override_redirect_windows.iter().for_each(|or| {
            if let Some(wl_surface) = or.wl_surface() {
                take_presentation_feedback_surface_tree(
                    &wl_surface,
                    &mut output_presentation_feedback,
                    surface_primary_scanout_output,
                    |surface, _| {
                        surface_presentation_feedback_flags_from_states(
                            surface,
                            None,
                            render_element_states,
                        )
                    },
                )
            }
        });

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            let namespace = self.workspaces.active_num(output).1;
            layer_surface.take_presentation_feedback(
                &mut output_presentation_feedback,
                surface_primary_scanout_output,
                |surface, _| {
                    surface_presentation_feedback_flags_from_states(
                        surface,
                        Some(namespace),
                        render_element_states,
                    )
                },
            );
        }

        output_presentation_feedback
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.workspaces.iter().flat_map(|(_, set)| {
            set.sticky_layer
                .mapped()
                .chain(set.minimized_windows.iter().flat_map(|m| m.mapped()))
                .chain(set.workspaces.iter().flat_map(|w| {
                    w.mapped()
                        .chain(w.minimized_windows.iter().flat_map(|m| m.mapped()))
                }))
        })
    }
}

fn workspace_set_idx(
    state: &mut WorkspaceUpdateGuard<'_, State>,
    idx: u8,
    handle: &WorkspaceHandle,
    name: Option<&str>,
) {
    state.set_workspace_name(handle, name.unwrap_or(&format!("{}", idx)));
    state.set_workspace_coordinates(handle, &[idx as u32]);
}

pub fn check_grab_preconditions(
    seat: &Seat<State>,
    serial: Option<Serial>,
    client_initiated: Option<&WlSurface>,
) -> Option<GrabStartData> {
    use smithay::reexports::wayland_server::Resource;

    let pointer = seat.get_pointer().unwrap();
    let touch = seat.get_touch().unwrap();

    let start_data =
        if serial.is_some_and(|serial| touch.has_grab(serial)) {
            GrabStartData::Touch(touch.grab_start_data().unwrap())
        } else {
            GrabStartData::Pointer(pointer.grab_start_data().unwrap_or_else(|| {
                PointerGrabStartData {
                    focus: pointer.current_focus().map(|f| (f, Point::from((0., 0.)))),
                    button: 0x110,
                    location: pointer.current_location(),
                }
            }))
        };

    if let Some(surface) = client_initiated {
        // Check that this surface has a click or touch down grab.
        if !match serial {
            Some(serial) => pointer.has_grab(serial) || touch.has_grab(serial),
            None => pointer.is_grabbed() | touch.is_grabbed(),
        } {
            return None;
        }

        // If the focus was for a different surface, ignore the request.
        if start_data.focus().is_none()
            || !start_data.focus().unwrap().0.same_client_as(&surface.id())
        {
            return None;
        }
    }

    Some(start_data)
}
