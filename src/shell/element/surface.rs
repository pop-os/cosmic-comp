use std::{
    borrow::Cow,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
    time::Duration,
};

use smithay::{
    backend::renderer::{
        element::{
            self,
            surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
            utils::select_dmabuf_feedback,
            AsRenderElements, RenderElementStates,
        },
        ImportAll, Renderer,
    },
    desktop::{
        space::SpaceElement, utils::OutputPresentationFeedback, PopupManager, Window,
        WindowSurface, WindowSurfaceType,
    },
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        Seat,
    },
    output::Output,
    reexports::{
        wayland_protocols::{
            wp::presentation_time::server::wp_presentation_feedback::Kind,
            xdg::{
                decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode,
                shell::server::xdg_toplevel::State as ToplevelState,
            },
        },
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration::Mode as KdeMode,
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{
        user_data::UserDataMap, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size,
    },
    wayland::{
        compositor::{with_states, with_surface_tree_downward, SurfaceData, TraversalAction},
        seat::WaylandFocus,
        shell::xdg::{SurfaceCachedState, ToplevelSurface, XdgToplevelSurfaceData},
    },
    xwayland::{xwm::X11Relatable, X11Surface},
};
use tracing::trace;

use crate::{
    state::{State, SurfaceDmabufFeedback},
    utils::prelude::*,
    wayland::handlers::decoration::{KdeDecorationData, PreferredDecorationMode},
};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct CosmicSurface(pub Window);

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
        self.wl_surface().map_or(false, |s| &*s == other)
    }
}

impl PartialEq<ToplevelSurface> for CosmicSurface {
    fn eq(&self, other: &ToplevelSurface) -> bool {
        self.wl_surface()
            .map_or(false, |s| &*s == other.wl_surface())
    }
}

impl PartialEq<X11Surface> for CosmicSurface {
    fn eq(&self, other: &X11Surface) -> bool {
        self.x11_surface().map_or(false, |s| s == other)
    }
}

#[derive(Default)]
struct Minimized(AtomicBool);

#[derive(Default)]
struct Sticky(AtomicBool);

#[derive(Default)]
struct GlobalGeometry(Mutex<Option<Rectangle<i32, Global>>>);

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

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| state.size),
            WindowSurface::X11(surface) => Some(surface.geometry().size),
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
                let _ = surface.configure(geo.as_logical());
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
            WindowSurface::Wayland(toplevel) => {
                if pending {
                    toplevel.with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Activated)
                    })
                } else {
                    toplevel
                        .current_state()
                        .states
                        .contains(ToplevelState::Activated)
                }
            }
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

                let xdg_state = if pending {
                    toplevel.with_pending_state(|pending| {
                        pending
                            .decoration_mode
                            .map(|mode| mode == DecorationMode::ClientSide)
                    })
                } else {
                    toplevel
                        .current_state()
                        .decoration_mode
                        .map(|mode| mode == DecorationMode::ClientSide)
                };

                kde_state.or(xdg_state).unwrap_or(true)
            }
            WindowSurface::X11(surface) => surface.is_decorated(),
        }
    }

    pub fn try_force_undecorated(&self, enable: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                if enable {
                    let previous_decoration_state =
                        toplevel.current_state().decoration_mode.clone();
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

    pub fn is_resizing(&self, pending: bool) -> Option<bool> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                if pending {
                    Some(toplevel.with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Resizing)
                    }))
                } else {
                    Some(
                        toplevel
                            .current_state()
                            .states
                            .contains(ToplevelState::Resizing),
                    )
                }
            }
            WindowSurface::X11(_surface) => None,
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
                if pending {
                    Some(toplevel.with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::TiledLeft)
                    }))
                } else {
                    Some(
                        toplevel
                            .current_state()
                            .states
                            .contains(ToplevelState::TiledLeft),
                    )
                }
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
            WindowSurface::Wayland(toplevel) => {
                if pending {
                    toplevel.with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Fullscreen)
                    })
                } else {
                    toplevel
                        .current_state()
                        .states
                        .contains(ToplevelState::Fullscreen)
                }
            }
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
            WindowSurface::Wayland(toplevel) => {
                if pending {
                    toplevel.with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Maximized)
                    })
                } else {
                    toplevel
                        .current_state()
                        .states
                        .contains(ToplevelState::Maximized)
                }
            }
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
        if !minimized {
            if let WindowSurface::X11(surface) = self.0.underlying_surface() {
                let _ = surface.set_mapped(false);
                let _ = surface.set_mapped(true);
            }
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
    }

    pub fn set_suspended(&self, suspended: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(window) => window.with_pending_state(|state| {
                if suspended {
                    state.states.set(ToplevelState::Suspended);
                } else {
                    state.states.unset(ToplevelState::Suspended);
                }
            }),
            WindowSurface::X11(surface) => {
                let _ = surface.set_suspended(suspended);
            }
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
                    .configure_serial
                    .as_ref()
                    .map(|s| s >= serial)
                    .unwrap_or(false)
            }),
            WindowSurface::X11(_surface) => true,
        }
    }

    pub fn serial_past(&self, serial: &Serial) -> bool {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => with_states(toplevel.wl_surface(), |states| {
                let attrs = states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap();
                attrs
                    .current_serial
                    .as_ref()
                    .map(|s| s >= serial)
                    .unwrap_or(false)
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
                    if attributes.current.size == current_server.size {
                        // The window had committed for our previous size change, so we can
                        // change the size again.
                        trace!(
                            "current size matches server size: {:?}",
                            attributes.current.size
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
            WindowSurface::X11(_) => true,
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

        if surface_type.contains(WindowSurfaceType::TOPLEVEL) {
            if *toplevel == *surface {
                return true;
            }
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
        render_element_states: &RenderElementStates,
        primary_scan_out_output: F1,
    ) where
        F1: FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
    {
        let is_fullscreen = self.is_fullscreen(false);

        self.0
            .send_dmabuf_feedback(output, primary_scan_out_output, |surface, _| {
                select_dmabuf_feedback(
                    surface,
                    render_element_states,
                    &feedback.render_feedback,
                    if is_fullscreen {
                        &feedback.primary_scanout_feedback
                    } else {
                        &feedback.scanout_feedback
                    },
                )
            })
    }

    pub fn take_presentation_feedback<F1, F2>(
        &self,
        output_feedback: &mut OutputPresentationFeedback,
        primary_scan_out_output: F1,
        presentation_feedback_flags: F2,
    ) where
        F1: FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
        F2: FnMut(&WlSurface, &SurfaceData) -> Kind + Copy,
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

    pub fn popup_render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C>
    where
        R: Renderer + ImportAll,
        R::TextureId: Clone + 'static,
        C: From<WaylandSurfaceRenderElement<R>>,
    {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let surface = toplevel.wl_surface();
                PopupManager::popups_for_surface(surface)
                    .flat_map(move |(popup, popup_offset)| {
                        let offset = (self.0.geometry().loc + popup_offset - popup.geometry().loc)
                            .to_physical_precise_round(scale);

                        render_elements_from_surface_tree(
                            renderer,
                            popup.wl_surface(),
                            location + offset,
                            scale,
                            alpha,
                            element::Kind::Unspecified,
                        )
                    })
                    .collect()
            }
            WindowSurface::X11(_) => Vec::new(),
        }
    }

    pub fn render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C>
    where
        R: Renderer + ImportAll,
        R::TextureId: Clone + 'static,
        C: From<WaylandSurfaceRenderElement<R>>,
    {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let surface = toplevel.wl_surface();

                render_elements_from_surface_tree(
                    renderer,
                    surface,
                    location,
                    scale,
                    alpha,
                    element::Kind::Unspecified,
                )
            }
            WindowSurface::X11(surface) => {
                surface.render_elements(renderer, location, scale, alpha)
            }
        }
    }

    pub fn x11_surface(&self) -> Option<&X11Surface> {
        self.0.x11_surface()
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
        SpaceElement::output_enter(&self.0, output, overlap)
    }

    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output)
    }

    #[profiling::function]
    fn refresh(&self) {
        SpaceElement::refresh(&self.0)
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

impl<R> AsRenderElements<R> for CosmicSurface
where
    R: Renderer + ImportAll,
    R::TextureId: Clone + 'static,
{
    type RenderElement = WaylandSurfaceRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        self.0.render_elements(renderer, location, scale, alpha)
    }
}
