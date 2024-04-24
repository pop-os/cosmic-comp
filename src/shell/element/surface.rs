use std::{
    sync::atomic::{AtomicBool, Ordering},
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
        space::SpaceElement, utils::OutputPresentationFeedback, PopupManager, Window, WindowSurface,
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
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{user_data::UserDataMap, IsAlive, Logical, Rectangle, Serial, Size},
    wayland::{
        compositor::{with_states, SurfaceData},
        seat::WaylandFocus,
        shell::xdg::{SurfaceCachedState, ToplevelSurface, XdgToplevelSurfaceData},
    },
    xwayland::{xwm::X11Relatable, X11Surface},
};

use crate::{
    state::{State, SurfaceDmabufFeedback},
    utils::prelude::*,
    wayland::handlers::decoration::PreferredDecorationMode,
};

#[derive(Debug, Clone, PartialEq)]
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
        self.wl_surface().map_or(false, |s| &s == other)
    }
}

impl PartialEq<X11Surface> for CosmicSurface {
    fn eq(&self, other: &X11Surface) -> bool {
        self.x11_surface().map_or(false, |s| s == other)
    }
}

#[derive(Default)]
struct Minimized(AtomicBool);

pub const SSD_HEIGHT: i32 = 48;
pub const RESIZE_BORDER: i32 = 10;

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
            WindowSurface::X11(surface) => surface.title(),
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
            WindowSurface::X11(surface) => surface.class(),
        }
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => toplevel.with_pending_state(|state| state.size),
            WindowSurface::X11(surface) => Some(surface.geometry().size),
        }
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>) {
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
                if pending {
                    toplevel.with_pending_state(|pending| {
                        pending
                            .decoration_mode
                            .map(|mode| mode == DecorationMode::ClientSide)
                            .unwrap_or(true)
                    })
                } else {
                    toplevel
                        .current_state()
                        .decoration_mode
                        .map(|mode| mode == DecorationMode::ClientSide)
                        .unwrap_or(true)
                }
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
                } else {
                    let previous_mode = PreferredDecorationMode::mode(&self.0);
                    toplevel.with_pending_state(|pending| {
                        pending.decoration_mode = previous_mode;
                    });
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
        match self.0.underlying_surface() {
            WindowSurface::Wayland(_) => self
                .0
                .user_data()
                .get_or_insert_threadsafe(Minimized::default)
                .0
                .load(Ordering::SeqCst),
            WindowSurface::X11(surface) => surface.is_minimized(),
        }
    }

    pub fn set_minimized(&self, minimized: bool) {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(_) => self
                .0
                .user_data()
                .get_or_insert_threadsafe(Minimized::default)
                .0
                .store(minimized, Ordering::SeqCst),
            WindowSurface::X11(surface) => {
                let _ = surface.set_minimized(minimized);
            }
        }
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
            _ => {}
        }
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_states(toplevel.wl_surface(), |states| {
                    states.cached_state.current::<SurfaceCachedState>().min_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            WindowSurface::X11(surface) => surface.min_size(),
        }
        .map(|size| {
            if self.is_decorated(false) {
                size + (0, SSD_HEIGHT).into()
            } else {
                size
            }
        })
    }

    pub fn max_size(&self) -> Option<Size<i32, Logical>> {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                Some(with_states(toplevel.wl_surface(), |states| {
                    states.cached_state.current::<SurfaceCachedState>().max_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            WindowSurface::X11(surface) => surface.max_size(),
        }
        .map(|size| {
            if self.is_decorated(false) {
                size + (0, SSD_HEIGHT).into()
            } else {
                size
            }
        })
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
        self.0
            .send_dmabuf_feedback(output, primary_scan_out_output, |surface, _| {
                select_dmabuf_feedback(
                    surface,
                    render_element_states,
                    &feedback.render_feedback,
                    &feedback.scanout_feedback,
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

    pub fn split_render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: smithay::utils::Point<i32, smithay::utils::Physical>,
        scale: smithay::utils::Scale<f64>,
        alpha: f32,
    ) -> (Vec<C>, Vec<C>)
    where
        R: Renderer + ImportAll,
        <R as Renderer>::TextureId: Clone + 'static,
        C: From<WaylandSurfaceRenderElement<R>>,
    {
        match self.0.underlying_surface() {
            WindowSurface::Wayland(toplevel) => {
                let surface = toplevel.wl_surface();

                let popup_render_elements = PopupManager::popups_for_surface(surface)
                    .flat_map(|(popup, popup_offset)| {
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
                    .collect();

                let window_render_elements = render_elements_from_surface_tree(
                    renderer,
                    surface,
                    location,
                    scale,
                    alpha,
                    element::Kind::Unspecified,
                );

                (window_render_elements, popup_render_elements)
            }
            WindowSurface::X11(surface) => (
                surface.render_elements(renderer, location, scale, alpha),
                Vec::new(),
            ),
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

    fn is_in_input_region(
        &self,
        point: &smithay::utils::Point<f64, smithay::utils::Logical>,
    ) -> bool {
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
    fn wl_surface(&self) -> Option<WlSurface> {
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
    <R as Renderer>::TextureId: Clone + 'static,
{
    type RenderElement = WaylandSurfaceRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: smithay::utils::Point<i32, smithay::utils::Physical>,
        scale: smithay::utils::Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        self.0.render_elements(renderer, location, scale, alpha)
    }
}
