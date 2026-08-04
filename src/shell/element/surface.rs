use iced_core::Shadow;
use smithay::backend::renderer::gles::element::PixelShaderElement;
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface;

use crate::{
    backend::render::{
        element::AsGlowRenderer,
        shadow::ShadowShader,
        wayland::{SurfaceRenderElement, push_render_elements_from_surface_tree},
    },
    shell::focus::target::PointerFocusTarget,
    utils::prelude::*,
    wayland::handlers::{
        background_effect::ComputedBlurRegionCachedState, compositor::frame_time_filter_fn,
        corner_radius::surface_corners,
    },
    wayland::protocols::layer_shadow::surface_has_shadow,
};
use std::{
    borrow::Cow,
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            ImportAll, Renderer, buffer_has_alpha,
            element::{Kind, RenderElementStates, surface::KindEvaluation},
            utils::RendererSurfaceStateUserData,
        },
    },
    desktop::{
        PopupManager, WeakWindow, Window, WindowSurface, WindowSurfaceType, space::SpaceElement,
        utils::OutputPresentationFeedback,
    },
    input::{
        Seat,
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
    },
    output::Output,
    reexports::{
        wayland_protocols::{
            wp::presentation_time::server::wp_presentation_feedback::Kind as PresentationKind,
            xdg::{
                decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode,
                shell::server::xdg_toplevel::State as ToplevelState,
            },
        },
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration::Mode as KdeMode,
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{
        IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size, user_data::UserDataMap,
    },
    wayland::{
        alpha_modifier::AlphaModifierSurfaceCachedState,
        compositor::{
            SubsurfaceCachedState, SurfaceData, TraversalAction, get_parent, with_states,
            with_surface_tree_downward,
        },
        dmabuf::get_dmabuf,
        seat::WaylandFocus,
        shell::xdg::{
            SurfaceCachedState, ToplevelCachedState, ToplevelSurface, XdgPopupSurfaceData,
            XdgToplevelSurfaceData,
        },
    },
    xwayland::{
        X11Surface,
        xwm::{WmWindowType, X11Relatable},
    },
};
use tracing::trace;

use crate::{
    state::{State, SurfaceDmabufFeedback},
    wayland::handlers::{
        compositor::FRAME_TIME_FILTER,
        decoration::{KdeDecorationData, PreferredDecorationMode},
    },
};

/// The [`DrmNode`] the surface's currently committed buffer was allocated on, if it is a dmabuf.
fn buffer_node(data: &SurfaceData) -> Option<DrmNode> {
    let surface_state = data.data_map.get::<RendererSurfaceStateUserData>()?;
    let surface_state = surface_state.lock().unwrap();
    surface_state
        .buffer()
        .and_then(|buffer| get_dmabuf(buffer).ok())
        .and_then(|dmabuf| dmabuf.node())
}

fn is_likely_translucent(alpha: f32, data: &SurfaceData) -> bool {
    if alpha < 1.0 {
        return true;
    }

    let mut alpha_modifier_state = data.cached_state.get::<AlphaModifierSurfaceCachedState>();
    let alpha_multiplier = alpha_modifier_state
        .current()
        .multiplier_f32()
        .unwrap_or(1.0);
    if alpha_multiplier < 1.0 {
        return true;
    }

    let Some(surface_state) = data.data_map.get::<RendererSurfaceStateUserData>() else {
        return false;
    };
    let surface_state = surface_state.lock().unwrap();
    if surface_state
        .buffer()
        .is_none_or(|buffer| !buffer_has_alpha(buffer).unwrap_or(true))
    {
        return false;
    }

    let mut blur_state = data.cached_state.get::<ComputedBlurRegionCachedState>();
    blur_state
        .current()
        .blur_region
        .as_ref()
        .is_some_and(|region| !region.is_empty())
}

/// Build the [`KindEvaluation`] for a window's surface tree.
///
/// `scanout_node`, when set, is the scan-out target [`DrmNode`] of the output currently being
/// rendered: only buffers allocated on that node may be promoted to overlay scan-out candidates.
/// It is `None` for render passes that never scan out to a plane (e.g. screen-copy).
fn scanout_kind_eval(
    scanout_override: Option<bool>,
    scanout_node: Option<DrmNode>,
    alpha: f32,
) -> KindEvaluation {
    match (scanout_override, scanout_node) {
        // Forced off.
        (Some(false), _) => Kind::Unspecified.into(),
        // No node restriction: preserve the previous behaviour exactly.
        (Some(true), None) => Kind::ScanoutCandidate.into(),
        (None, None) => FRAME_TIME_FILTER,
        // Node restriction in effect: only buffers on the scan-out node may be candidates.
        (Some(true), Some(node)) => KindEvaluation::Closure(Box::new(move |data| {
            if buffer_node(data) == Some(node) && !is_likely_translucent(alpha, data) {
                Kind::ScanoutCandidate
            } else {
                Kind::Unspecified
            }
        })),
        (None, Some(node)) => KindEvaluation::Closure(Box::new(move |data| {
            if buffer_node(data) == Some(node) && !is_likely_translucent(alpha, data) {
                frame_time_filter_fn(data)
            } else {
                Kind::Unspecified
            }
        })),
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct CosmicSurface(pub Window);

#[derive(Debug, Clone)]
pub struct WeakCosmicSurface(pub WeakWindow);

impl From<ToplevelSurface> for CosmicSurface {
    fn from(s: ToplevelSurface) -> Self {
        CosmicSurface(Window::new_wayland_window(s))
    }
}

impl From<Window> for CosmicSurface {
    fn from(w: Window) -> Self {
        CosmicSurface(w)
    }
}

impl From<X11Surface> for CosmicSurface {
    fn from(s: X11Surface) -> Self {
        CosmicSurface(Window::new_x11_window(s))
    }
}

impl PartialEq<WlSurface> for CosmicSurface {
    fn eq(&self, other: &WlSurface) -> bool {
        self.wl_surface().is_some_and(|s| &*s == other)
    }
}

impl PartialEq<ToplevelSurface> for CosmicSurface {
    fn eq(&self, other: &ToplevelSurface) -> bool {
        self.wl_surface().is_some_and(|s| &*s == other.wl_surface())
    }
}

impl PartialEq<X11Surface> for CosmicSurface {
    fn eq(&self, other: &X11Surface) -> bool {
        self.x11_surface() == Some(other)
    }
}

impl PartialEq<WeakCosmicSurface> for CosmicSurface {
    fn eq(&self, other: &WeakCosmicSurface) -> bool {
        other.upgrade().is_some_and(|other| other == *self)
    }
}

#[derive(Default)]
struct Minimized(AtomicBool);

#[derive(Default)]
struct Sticky(AtomicBool);

#[derive(Default)]
struct GlobalGeometry(Mutex<Option<Rectangle<i32, Global>>>);

/// How to draw the shadow behind a popup that asked for one.
///
/// The element is built where the popup's geometry is worked out, but handed
/// back rather than pushed: `SurfaceRenderElement` cannot carry a shader
/// element, and the callers that can are the ones whose render element type
/// already accepts one.
pub struct PopupShadow<'a> {
    /// Layers to draw, in the order the theme lists them — furthest from the
    /// surface first, so later ones land on top.
    pub layers: &'a [Shadow],
    /// Where each element built goes.
    pub push: &'a mut dyn FnMut(PixelShaderElement),
}

impl CosmicSurface {
    pub fn title(&self) -> String {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .title
                    .clone()
                    .unwrap_or_default()
            }),
            WindowSurface::X11(surface) => surface.title().replace('\0', ""),
        }
    }

    /// Whether this surface asked for a blurred backdrop.
    ///
    /// Kept across the move to the background-effect protocol because callers use
    /// it for more than drawing the blur: a blurred window suppresses its drop
    /// shadow and changes how its corners are clipped. Only the source of the
    /// answer changed -- it now comes from the surface's computed blur region
    /// rather than the old per-surface query.
    pub fn has_blur(&self) -> bool {
        self.wl_surface().is_some_and(|surface| {
            with_states(&surface, |states| {
                let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
                blur_state
                    .current()
                    .blur_region
                    .as_ref()
                    .is_some_and(|region| !region.is_empty())
            })
        })
    }

    pub fn corner_radius(&self, geometry_size: Size<i32, Logical>) -> Option<[u8; 4]> {
        self.wl_surface().and_then(|surface| {
            with_states(&surface, |states| surface_corners(states, geometry_size))
        })
    }

    pub fn app_id(&self) -> String {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .app_id
                    .clone()
                    .unwrap_or_default()
            }),
            WindowSurface::X11(surface) => surface.class().replace('\0', ""),
        }
    }

    /// Steam app id (`STEAM_GAME` X11 property) for this surface, if set. Only
    /// XWayland windows carry it; native Wayland toplevels return `None`.
    pub fn steam_appid(&self) -> Option<u32> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.steam_game(),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// Whether this is a Steam or external gaming overlay (`STEAM_OVERLAY` /
    /// `GAMESCOPE_EXTERNAL_OVERLAY` — e.g. the Steam overlay or MangoHud). These
    /// must survive game mode (not minimized) and sit above the game. Only
    /// XWayland windows carry the markers.
    pub fn is_overlay(&self) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => {
                surface.steam_overlay().is_some_and(|v| v != 0)
                    || surface.external_overlay().is_some_and(|v| v != 0)
            }
            WindowSurface::Wayland(_) => false,
        }
    }

    /// The `STEAM_INPUT_FOCUS` mode for this surface, if set (non-zero = this
    /// window grabs keyboard/pointer input over the game). `None` for native
    /// Wayland toplevels.
    pub fn steam_input_focus(&self) -> Option<u32> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.steam_input_focus(),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// Whether this is the Steam client / Big Picture window (`STEAM_BIGPICTURE`),
    /// which game mode treats as the base-layer launcher.
    pub fn is_steam_client(&self) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.steam_bigpicture().is_some_and(|v| v != 0),
            WindowSurface::Wayland(_) => false,
        }
    }

    /// PID of the client owning this surface (`_NET_WM_PID`). `None` for native
    /// Wayland toplevels. Game mode uses this to relate a window to the adopted
    /// game: a popup may only composite over the game when it comes from the SAME
    /// process.
    pub fn pid(&self) -> Option<u32> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.pid(),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// The X11 window this surface is transient for (`WM_TRANSIENT_FOR`), if any.
    pub fn transient_for(&self) -> Option<u32> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.is_transient_for(),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// This surface's X11 window id, if it is an XWayland window.
    pub fn x11_window_id(&self) -> Option<u32> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => Some(surface.window_id()),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// Whether this is an X11 override-redirect window (menus, tooltips, the
    /// Steam overlay) — it bypasses the window manager and positions itself.
    pub fn is_override_redirect(&self) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.is_override_redirect(),
            WindowSurface::Wayland(_) => false,
        }
    }

    /// `_NET_WM_WINDOW_TYPE`, if the client set one.
    pub fn window_type(&self) -> Option<WmWindowType> {
        match self.0.underlying_surface() {
            WindowSurface::X11(surface) => surface.window_type(),
            WindowSurface::Wayland(_) => None,
        }
    }

    /// Whether this window is too insubstantial to ever be the game or a popup
    /// worth compositing: a 1x1/zero-area stub. Games map 1x1 IME/input helpers and
    /// offscreen login stubs, which must never win focus.
    pub fn is_useless(&self) -> bool {
        let size = self.bbox().size;
        size.w <= 1 || size.h <= 1
    }

    /// Whether this window looks like a menu/dropdown/transient artifact rather
    /// than a real toplevel.
    /// Override-redirect and the popup-ish `_NET_WM_WINDOW_TYPE`s count; so does
    /// a non-fullscreen window hidden from BOTH the taskbar and the pager (a
    /// helper surface), which is how Wine marks many of its transient widgets.
    pub fn maybe_a_dropdown(&self) -> bool {
        let WindowSurface::X11(surface) = self.0.underlying_surface() else {
            // Native Wayland popups are xdg_popups, tracked separately.
            return false;
        };
        if surface.is_override_redirect() {
            return true;
        }
        if matches!(
            surface.window_type(),
            Some(
                WmWindowType::DropdownMenu
                    | WmWindowType::PopupMenu
                    | WmWindowType::Menu
                    | WmWindowType::Tooltip
                    | WmWindowType::Combo
                    | WmWindowType::Splash
                    | WmWindowType::Notification
                    | WmWindowType::Dnd
            )
        ) {
            return true;
        }
        surface.is_skip_taskbar() && surface.is_skip_pager() && !surface.is_fullscreen()
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| state.size),
            WindowSurface::X11(surface) => Some(surface.geometry().size),
        }
    }

    pub fn has_pending_changes(&self) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.has_pending_changes(),
            WindowSurface::X11(_surface) => false,
        }
    }

    /// Returns the number of configures sent but not yet ack'd by the client.
    pub fn pending_configure_count(&self) -> usize {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .pending_configures()
                    .len()
            }),
            WindowSurface::X11(_) => 0,
        }
    }

    pub fn last_server_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                let attributes = states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap();
                attributes.current_server_state().size
            }),
            WindowSurface::X11(_) => None,
        }
    }

    pub fn global_geometry(&self) -> Option<Rectangle<i32, Global>> {
        *self
            .0
            .user_data()
            .get_or_insert_threadsafe(GlobalGeometry::default)
            .0
            .lock()
            .unwrap()
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>, ssd_height: u32) {
        {
            let mut geo = geo;
            geo.size.h += ssd_height as i32;
            geo.loc.y -= ssd_height as i32;

            *self
                .0
                .user_data()
                .get_or_insert_threadsafe(GlobalGeometry::default)
                .0
                .lock()
                .unwrap() = Some(geo);
        }
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                toplevel.with_pending_state(|state| state.size = Some(geo.size.as_logical()))
            }
            WindowSurface::X11(surface) => {
                let _ = surface.configure(geo.as_logical() + surface.frame_extents());
            }
        }
    }

    pub fn set_bounds(&self, size: impl Into<Option<Size<i32, Logical>>>) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                toplevel.with_pending_state(|state| state.bounds = size.into())
            }
            WindowSurface::X11(_surface) => {}
        }
    }

    pub fn is_activated(&self, pending: bool) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_toplevel_state(toplevel, pending, |state| {
                state.is_some_and(|state| state.states.contains(ToplevelState::Activated))
            }),
            WindowSurface::X11(surface) => surface.is_activated(),
        }
    }

    pub fn set_activated(&self, activated: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| {
                if activated {
                    state.states.set(ToplevelState::Activated);
                } else {
                    state.states.unset(ToplevelState::Activated);
                }
            }),
            WindowSurface::X11(surface) => {
                let _ = surface.set_activated(activated);
            }
        }
    }

    pub fn is_decorated(&self, pending: bool) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let kde_state = with_states(toplevel.wl_surface(), |states| {
                    states
                        .data_map
                        .get::<KdeDecorationData>()
                        .and_then(|data| data.lock().unwrap().mode.map(|m| m != KdeMode::Server))
                });

                let xdg_state = with_toplevel_state(toplevel, pending, |state| {
                    state.and_then(|state| {
                        state
                            .decoration_mode
                            .map(|mode| mode == DecorationMode::ClientSide)
                    })
                });

                kde_state.or(xdg_state).unwrap_or(true)
            }
            WindowSurface::X11(surface) => surface.is_decorated(),
        }
    }

    pub fn try_force_undecorated(&self, enable: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                if enable {
                    let previous_decoration_state = toplevel.with_committed_state(|state| {
                        state.map_or_else(Default::default, |state| state.decoration_mode)
                    });
                    if PreferredDecorationMode::is_unset(&self.0) {
                        PreferredDecorationMode::update(&self.0, previous_decoration_state);
                    }
                    toplevel.with_pending_state(|pending| {
                        pending.decoration_mode = Some(DecorationMode::ServerSide);
                    });
                    with_states(toplevel.wl_surface(), |data| {
                        if let Some(kde_data) = data.data_map.get::<KdeDecorationData>() {
                            for obj in kde_data.lock().unwrap().objs.iter() {
                                obj.mode(KdeMode::Server);
                            }
                        }
                    })
                } else {
                    let previous_mode = PreferredDecorationMode::mode(&self.0);
                    toplevel.with_pending_state(|pending| {
                        pending.decoration_mode = previous_mode;
                    });
                    with_states(toplevel.wl_surface(), |data| {
                        if let Some(kde_data) = data.data_map.get::<KdeDecorationData>() {
                            for obj in kde_data.lock().unwrap().objs.iter() {
                                obj.mode(KdeMode::Server);
                            }
                        }
                    })
                }
            }
            WindowSurface::X11(_surface) => {}
        }
    }

    /// Force client-side decorations (no server-side decorations)
    /// Used for embedded windows that should not have window decorations
    pub fn force_client_side_decorations(&self) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                // Store the previous mode so we can restore it later if needed
                let previous_decoration_state = toplevel.with_committed_state(|state| {
                    state.map_or_else(Default::default, |state| state.decoration_mode)
                });
                if PreferredDecorationMode::is_unset(&self.0) {
                    PreferredDecorationMode::update(&self.0, previous_decoration_state);
                }
                // Set to ClientSide decorations
                toplevel.with_pending_state(|pending| {
                    pending.decoration_mode = Some(DecorationMode::ClientSide);
                });
                // Also notify KDE decoration protocol
                with_states(toplevel.wl_surface(), |data| {
                    if let Some(kde_data) = data.data_map.get::<KdeDecorationData>() {
                        for obj in kde_data.lock().unwrap().objs.iter() {
                            obj.mode(KdeMode::Client);
                        }
                    }
                })
            }
            WindowSurface::X11(_surface) => {}
        }
    }

    pub fn is_resizing(&self, pending: bool) -> Option<bool> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_toplevel_state(toplevel, pending, |state| {
                    state.is_some_and(|state| state.states.contains(ToplevelState::Resizing))
                }))
            }
            WindowSurface::X11(surface) => surface.pending_configure().map(|_| true),
        }
    }

    pub fn set_resizing(&self, resizing: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| {
                if resizing {
                    state.states.set(ToplevelState::Resizing);
                } else {
                    state.states.unset(ToplevelState::Resizing);
                }
            }),
            WindowSurface::X11(_surface) => {}
        }
    }

    pub fn is_tiled(&self, pending: bool) -> Option<bool> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_toplevel_state(toplevel, pending, |state| {
                    state.is_some_and(|state| state.states.contains(ToplevelState::TiledLeft))
                }))
            }
            WindowSurface::X11(_surface) => None,
        }
    }

    pub fn set_tiled(&self, tiled: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| {
                if tiled {
                    state.states.set(ToplevelState::TiledLeft);
                    state.states.set(ToplevelState::TiledRight);
                    state.states.set(ToplevelState::TiledTop);
                    state.states.set(ToplevelState::TiledBottom);
                } else {
                    state.states.unset(ToplevelState::TiledLeft);
                    state.states.unset(ToplevelState::TiledRight);
                    state.states.unset(ToplevelState::TiledTop);
                    state.states.unset(ToplevelState::TiledBottom);
                }
            }),
            WindowSurface::X11(_surface) => {}
        }
    }

    pub fn is_fullscreen(&self, pending: bool) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_toplevel_state(toplevel, pending, |state| {
                state.is_some_and(|state| state.states.contains(ToplevelState::Fullscreen))
            }),
            WindowSurface::X11(surface) => surface.is_fullscreen(),
        }
    }

    pub fn set_fullscreen(&self, fullscreen: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| {
                if fullscreen {
                    state.states.set(ToplevelState::Fullscreen);
                } else {
                    state.states.unset(ToplevelState::Fullscreen);
                }
            }),
            WindowSurface::X11(surface) => {
                let _ = surface.set_fullscreen(fullscreen);
            }
        }
    }

    pub fn is_maximized(&self, pending: bool) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_toplevel_state(toplevel, pending, |state| {
                state.is_some_and(|state| state.states.contains(ToplevelState::Maximized))
            }),
            WindowSurface::X11(surface) => surface.is_maximized(),
        }
    }

    pub fn set_maximized(&self, maximized: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| {
                if maximized {
                    state.states.set(ToplevelState::Maximized);
                } else {
                    state.states.unset(ToplevelState::Maximized);
                }
            }),
            WindowSurface::X11(surface) => {
                let _ = surface.set_maximized(maximized);
            }
        }
    }

    pub fn is_minimized(&self) -> bool {
        self.0
            .user_data()
            .get_or_insert_threadsafe(Minimized::default)
            .0
            .load(Ordering::SeqCst)
    }

    pub fn set_minimized(&self, minimized: bool) {
        self.0
            .user_data()
            .get_or_insert_threadsafe(Minimized::default)
            .0
            .store(minimized, Ordering::SeqCst);
        if let WindowSurface::X11(surface) = self.0.underlying_surface() {
            let _ = surface.set_hidden(minimized);
        }
    }

    pub fn is_sticky(&self) -> bool {
        self.0
            .user_data()
            .get_or_insert_threadsafe(Sticky::default)
            .0
            .load(Ordering::SeqCst)
    }

    pub fn set_sticky(&self, sticky: bool) {
        self.0
            .user_data()
            .get_or_insert_threadsafe(Sticky::default)
            .0
            .store(sticky, Ordering::SeqCst);
        if let WindowSurface::X11(surface) = self.0.underlying_surface() {
            let _ = surface.set_sticky(sticky);
        }
    }

    pub fn set_suspended(&self, suspended: bool) {
        if let WindowSurface::Wayland(window) = self.0.underlying_surface() {
            window.with_pending_state(|state| {
                if suspended {
                    state.states.set(ToplevelState::Suspended);
                } else {
                    state.states.unset(ToplevelState::Suspended);
                }
            });
        }
    }

    pub fn min_size_without_ssd(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_states(toplevel.wl_surface(), |states| {
                    states
                        .cached_state
                        .get::<SurfaceCachedState>()
                        .current()
                        .min_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            WindowSurface::X11(surface) => surface.min_size(),
        }
    }

    pub fn max_size_without_ssd(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_states(toplevel.wl_surface(), |states| {
                    states
                        .cached_state
                        .get::<SurfaceCachedState>()
                        .current()
                        .max_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            WindowSurface::X11(surface) => surface.max_size(),
        }
    }

    pub fn serial_acked(&self, serial: &Serial) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                let attrs = states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap();
                attrs
                    .last_acked
                    .as_ref()
                    .is_some_and(|configure| configure.serial >= *serial)
            }),
            WindowSurface::X11(_surface) => true,
        }
    }

    pub fn serial_past(&self, serial: &Serial) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                let mut guard = states.cached_state.get::<ToplevelCachedState>();
                guard
                    .current()
                    .last_acked
                    .as_ref()
                    .is_some_and(|configure| configure.serial >= *serial)
            }),
            WindowSurface::X11(_surface) => true,
        }
    }

    pub fn latest_size_committed(&self) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                with_states(toplevel.wl_surface(), |states| {
                    let attributes = states
                        .data_map
                        .get::<XdgToplevelSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap();

                    let current_server = attributes.current_server_state();
                    let mut guard = states.cached_state.get::<ToplevelCachedState>();
                    if guard
                        .current()
                        .last_acked
                        .as_ref()
                        .is_some_and(|configure| configure.state.size == current_server.size)
                    {
                        // The window had committed for our previous size change, so we can
                        // change the size again.
                        trace!(
                            "current size matches server size: {:?}",
                            guard.current().last_acked.as_ref().unwrap().state.size
                        );
                        true
                    } else {
                        // The window had not committed for our previous size change yet.
                        // This throttling is done because some clients do not batch size requests,
                        // leading to bad behavior with very fast input devices (i.e. a 1000 Hz
                        // mouse). This throttling also helps interactive resize transactions
                        // preserve visual consistency.
                        trace!("throttling resize");
                        false
                    }
                })
            }
            WindowSurface::X11(surface) => surface.pending_configure().is_none(),
        }
    }

    pub fn force_configure(&self) -> Option<Serial> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => Some(toplevel.send_configure()),
            WindowSurface::X11(surface) => {
                let _ = surface.configure(None);
                None
            }
        }
    }

    pub fn send_configure(&self) -> Option<Serial> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.send_pending_configure(),
            WindowSurface::X11(_) => None,
        }
    }

    pub fn close(&self) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.send_close(),
            WindowSurface::X11(surface) => {
                let _ = surface.close();
            }
        }
    }

    pub fn has_surface(&self, surface: &WlSurface, surface_type: WindowSurfaceType) -> bool {
        let Some(toplevel) = self.wl_surface() else {
            return false;
        };

        if surface_type.contains(WindowSurfaceType::TOPLEVEL) && *toplevel == *surface {
            return true;
        }

        if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
            use std::sync::atomic::Ordering;

            let found = AtomicBool::new(false);
            with_surface_tree_downward(
                &toplevel,
                surface,
                |_, _, search| TraversalAction::DoChildren(search),
                |s, _, search| {
                    found.fetch_or(s == *search, Ordering::SeqCst);
                },
                |_, _, _| !found.load(Ordering::SeqCst),
            );
            if found.load(Ordering::SeqCst) {
                return true;
            }
        }

        if surface_type.contains(WindowSurfaceType::POPUP) {
            PopupManager::popups_for_surface(&toplevel).any(|(p, _)| p.wl_surface() == surface)
        } else {
            false
        }
    }

    pub fn surface_offset(&self, surface: &WlSurface) -> Option<Point<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Self::surface_tree_offset(toplevel.wl_surface(), surface)
            }
            WindowSurface::X11(surface_x11) => {
                if surface_x11.wl_surface().as_ref() == Some(surface) {
                    Some(Point::default())
                } else {
                    None
                }
            }
        }
    }

    pub fn surface_tree_offset(
        root: &WlSurface,
        surface: &WlSurface,
    ) -> Option<Point<i32, Logical>> {
        let mut offset = Point::<i32, Logical>::default();
        let mut parent = surface.clone();
        loop {
            if parent == *root {
                return Some(offset);
            } else if let Some(s) = get_parent(&parent) {
                offset += with_states(&parent, |states| {
                    states
                        .cached_state
                        .get::<SubsurfaceCachedState>()
                        .current()
                        .location
                });
                parent = s;
            } else {
                // `parent` is now root of subsurface tree; `surface` is not a subsurface child of `root`
                break;
            }
        }

        for (popup, popup_offset) in PopupManager::popups_for_surface(root) {
            if let Some(offset) = Self::surface_tree_offset(popup.wl_surface(), surface) {
                return Some(popup_offset + offset);
            }
        }

        None
    }

    pub fn focus_under(
        &self,
        relative_pos: Point<f64, Logical>,
        surface_type: WindowSurfaceType,
    ) -> Option<(PointerFocusTarget, Point<f64, Logical>)> {
        if let Some(xsurface) = self.x11_surface() {
            xsurface
                .surface_under(relative_pos, Point::default(), surface_type)
                .map(|(_surface, surface_offset)| {
                    (
                        PointerFocusTarget::X11Surface {
                            surface: xsurface.clone(),
                            toplevel: Some(self.clone()),
                        },
                        surface_offset.to_f64(),
                    )
                })
        } else {
            self.0
                .surface_under(relative_pos, surface_type)
                .map(|(surface, surface_offset)| {
                    (
                        PointerFocusTarget::WlSurface {
                            surface,
                            toplevel: Some(self.clone().into()),
                        },
                        surface_offset.to_f64(),
                    )
                })
        }
    }

    pub fn on_commit(&self) {
        self.0.on_commit();
    }

    pub fn send_frame<T, F>(
        &self,
        output: &Output,
        time: T,
        throttle: Option<Duration>,
        primary_scan_out_output: F,
    ) where
        T: Into<Duration>,
        F: FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
    {
        self.0
            .send_frame(output, time, throttle, primary_scan_out_output);
    }

    pub fn send_dmabuf_feedback<F1>(
        &self,
        output: &Output,
        feedback: &SurfaceDmabufFeedback,
        _render_element_states: &RenderElementStates,
        primary_scan_out_output: F1,
    ) where
        F1: FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
    {
        let is_fullscreen = self.is_fullscreen(false);

        self.0
            .send_dmabuf_feedback(output, primary_scan_out_output, |_, data| {
                if is_fullscreen {
                    &feedback.primary_scanout_feedback
                } else if frame_time_filter_fn(data) == Kind::ScanoutCandidate {
                    feedback
                        .overlay_scanout_feedback
                        .as_ref()
                        .unwrap_or(&feedback.render_feedback)
                } else {
                    &feedback.render_feedback
                }
            })
    }

    pub fn take_presentation_feedback<F1, F2>(
        &self,
        output_feedback: &mut OutputPresentationFeedback,
        primary_scan_out_output: F1,
        presentation_feedback_flags: F2,
    ) where
        F1: FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
        F2: FnMut(&WlSurface, &SurfaceData) -> PresentationKind + Copy,
    {
        self.0.take_presentation_feedback(
            output_feedback,
            primary_scan_out_output,
            presentation_feedback_flags,
        )
    }

    pub fn with_surfaces<F>(&self, processor: F)
    where
        F: FnMut(&WlSurface, &SurfaceData),
    {
        self.0.with_surfaces(processor)
    }

    pub fn user_data(&self) -> &UserDataMap {
        self.0.user_data()
    }

    pub fn push_popup_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_node: Option<DrmNode>,
        blur_strength: usize,
        push: &mut dyn FnMut(SurfaceRenderElement<R>),
        mut shadow: Option<PopupShadow<'_>>,
    ) where
        R: Renderer + ImportAll + AsGlowRenderer,
        R::TextureId: Clone + 'static,
    {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let surface = toplevel.wl_surface();
                for (popup, popup_offset) in PopupManager::popups_for_surface(surface) {
                    // Check for compositor-driven tooltip position override
                    let tooltip_override = crate::wayland::protocols::tooltip::get_tooltip_position(
                        popup.wl_surface(),
                    );

                    let mut geometry = popup.geometry().to_f64();
                    let offset = if let Some(override_data) = tooltip_override {
                        // During show-delay the override has a future show_at —
                        // push no elements so the popup is invisible until then.
                        if let Some(show_at) = override_data.show_at
                            && std::time::Instant::now() < show_at
                        {
                            continue;
                        }

                        let popup_geo = popup.geometry();

                        let popup_size = if popup_geo.size.w > 0 && popup_geo.size.h > 0 {
                            popup_geo.size
                        } else {
                            with_states(popup.wl_surface(), |states| {
                                states
                                    .data_map
                                    .get::<XdgPopupSurfaceData>()
                                    .and_then(|data| {
                                        let attrs = data.lock().ok()?;
                                        let size = attrs.current_server_state().geometry.size;
                                        if size.w > 0 && size.h > 0 {
                                            Some(size)
                                        } else {
                                            None
                                        }
                                    })
                                    .unwrap_or_default()
                            })
                        };

                        let mut pos = override_data.parent_relative;
                        // Adjust for popup geometry offset
                        pos.x -= popup_geo.loc.x;
                        pos.y -= popup_geo.loc.y;

                        // Apply anchor-based offset so the correct corner aligns.
                        override_data.anchor.adjust_position(
                            &mut pos.x,
                            &mut pos.y,
                            popup_size.w,
                            popup_size.h,
                        );

                        // Mirror the non-overridden branch below, with the override
                        // position standing in for the protocol popup offset.
                        geometry.loc += location.to_f64().to_logical(scale) + pos.to_f64();
                        pos.to_physical_precise_round(scale)
                    } else {
                        geometry.loc += location.to_f64().to_logical(scale) + popup_offset.to_f64();
                        (self.0.geometry().loc + popup_offset - popup.geometry().loc)
                            .to_physical_precise_round(scale)
                    };

                    let radii = with_states(popup.wl_surface(), |states| {
                        surface_corners(states, geometry.size.to_i32_round())
                    })
                    .unwrap_or([0; 4]);

                    // Behind the popup's own content, and only when the client
                    // asked for it over the shadow protocol. Pushed first
                    // because elements are collected front to back.
                    if let Some(shadow) = shadow.as_mut()
                        && surface_has_shadow(popup.wl_surface())
                    {
                        Self::push_popup_shadow(
                            renderer,
                            popup.wl_surface(),
                            geometry,
                            scale,
                            alpha,
                            radii,
                            shadow,
                        );
                    }

                    push_render_elements_from_surface_tree(
                        renderer,
                        popup.wl_surface(),
                        location + offset,
                        geometry,
                        scale,
                        alpha,
                        false,
                        radii,
                        None,
                        blur_strength,
                        scanout_kind_eval(None, scanout_node, alpha),
                        push,
                        None,
                    )
                }
            }
            WindowSurface::X11(_) => {}
        }
    }

    /// Build the shadow layers behind one popup.
    ///
    /// One element per layer, in the theme's order, so a multi-layer shadow
    /// composites the way it does when an application draws it — taking only
    /// the first layer, as the layer-shell paths do, leaves a menu shadow
    /// almost invisible.
    fn push_popup_shadow<R>(
        renderer: &mut R,
        surface: &wl_surface::WlSurface,
        geometry: Rectangle<f64, Logical>,
        scale: Scale<f64>,
        alpha: f32,
        radii: [u8; 4],
        shadow: &mut PopupShadow<'_>,
    ) where
        R: Renderer + AsGlowRenderer,
        R::TextureId: Clone + 'static,
    {
        // The shader works in the same logical space the geometry is already
        // in; `Local` is that space tagged as output-relative, which is what
        // the caller has resolved by this point.
        let geo = geometry.to_i32_round().as_local();
        let surface_id = surface.id();

        for (index, layer) in shadow.layers.iter().enumerate() {
            let element = ShadowShader::layer_element(
                renderer,
                &surface_id,
                // Its own cache slot, or each layer would evict the last and
                // all three would be rebuilt every frame.
                u8::try_from(index).unwrap_or(u8::MAX),
                geo,
                radii,
                // The popup's own alpha, and only that. The shader multiplies
                // the colour — which already carries the layer's opacity — by
                // this, so folding the opacity in here as well would square it
                // and leave a 4% shadow drawing at 0.16%.
                alpha,
                scale.x,
                [layer.color.r, layer.color.g, layer.color.b, layer.color.a],
                [layer.offset.x, layer.offset.y],
                layer.blur_radius,
            );
            (shadow.push)(element);
        }
    }

    pub fn push_render_elements<R>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_override: Option<bool>,
        scanout_node: Option<DrmNode>,
        should_clip: bool,
        radii: [u8; 4],
        blur_strength: usize,
        push_above: &mut dyn FnMut(SurfaceRenderElement<R>),
        push_below: Option<&mut dyn FnMut(SurfaceRenderElement<R>)>,
    ) where
        R: Renderer + ImportAll + AsGlowRenderer,
        R::TextureId: Clone + 'static,
    {
        let mut geometry = self.0.geometry().to_f64();
        geometry.loc += location.to_f64().to_logical(scale);

        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let surface = toplevel.wl_surface();

                push_render_elements_from_surface_tree(
                    renderer,
                    surface,
                    location,
                    geometry,
                    scale,
                    alpha,
                    should_clip,
                    radii,
                    None,
                    blur_strength,
                    scanout_kind_eval(scanout_override, scanout_node, alpha),
                    push_above,
                    push_below,
                )
            }
            WindowSurface::X11(surface) => {
                let Some(surface) = surface.wl_surface() else {
                    return;
                };

                push_render_elements_from_surface_tree(
                    renderer,
                    &surface,
                    location,
                    geometry,
                    scale,
                    alpha,
                    should_clip,
                    radii,
                    None,
                    blur_strength,
                    scanout_kind_eval(scanout_override, scanout_node, alpha),
                    push_above,
                    push_below,
                )
            }
        }
    }

    pub fn x11_surface(&self) -> Option<&X11Surface> {
        self.0.x11_surface()
    }

    pub fn downgrade(&self) -> WeakCosmicSurface {
        WeakCosmicSurface(self.0.downgrade())
    }
}

impl WeakCosmicSurface {
    pub fn upgrade(&self) -> Option<CosmicSurface> {
        self.0.upgrade().map(CosmicSurface)
    }
}

impl IsAlive for CosmicSurface {
    fn alive(&self) -> bool {
        self.0.alive()
    }
}

impl SpaceElement for CosmicSurface {
    fn geometry(&self) -> Rectangle<i32, Logical> {
        SpaceElement::geometry(&self.0)
    }

    fn bbox(&self) -> Rectangle<i32, Logical> {
        SpaceElement::bbox(&self.0)
    }

    fn is_in_input_region(&self, point: &Point<f64, smithay::utils::Logical>) -> bool {
        SpaceElement::is_in_input_region(&self.0, point)
    }

    fn z_index(&self) -> u8 {
        SpaceElement::z_index(&self.0)
    }

    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.0, activated)
    }

    fn output_enter(
        &self,
        output: &Output,
        overlap: smithay::utils::Rectangle<i32, smithay::utils::Logical>,
    ) {
        SpaceElement::output_enter(&self.0, output, overlap);
    }

    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
    }

    #[profiling::function]
    fn refresh(&self) {
        SpaceElement::refresh(&self.0);
    }
}

impl KeyboardTarget<State> for CosmicSurface {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        mut keys: Vec<KeysymHandle<'_>>,
        serial: smithay::utils::Serial,
    ) {
        if self.0.is_x11() {
            keys = vec![];
        }

        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                KeyboardTarget::enter(toplevel.wl_surface(), seat, data, keys, serial)
            }
            WindowSurface::X11(x11) => KeyboardTarget::enter(x11, seat, data, keys, serial),
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: smithay::utils::Serial) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                KeyboardTarget::leave(toplevel.wl_surface(), seat, data, serial)
            }
            WindowSurface::X11(x11) => KeyboardTarget::leave(x11, seat, data, serial),
        }
    }

    fn key(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        key: KeysymHandle<'_>,
        state: smithay::backend::input::KeyState,
        serial: smithay::utils::Serial,
        time: u32,
    ) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                KeyboardTarget::key(toplevel.wl_surface(), seat, data, key, state, serial, time)
            }
            WindowSurface::X11(x11) => {
                KeyboardTarget::key(x11, seat, data, key, state, serial, time)
            }
        }
    }

    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: smithay::utils::Serial,
    ) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                KeyboardTarget::modifiers(toplevel.wl_surface(), seat, data, modifiers, serial)
            }
            WindowSurface::X11(x11) => {
                KeyboardTarget::modifiers(x11, seat, data, modifiers, serial)
            }
        }
    }
}

impl WaylandFocus for CosmicSurface {
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        self.0.wl_surface()
    }
}

impl X11Relatable for CosmicSurface {
    fn is_window(&self, window: &X11Surface) -> bool {
        self.x11_surface() == Some(window)
    }
}

fn with_toplevel_state<T, F: FnOnce(Option<&smithay::wayland::shell::xdg::ToplevelState>) -> T>(
    toplevel: &ToplevelSurface,
    pending: bool,
    cb: F,
) -> T {
    if pending {
        toplevel.with_pending_state(|pending| cb(Some(pending)))
    } else {
        toplevel.with_committed_state(cb)
    }
}
