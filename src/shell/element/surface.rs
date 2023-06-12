use std::{cell::RefCell, time::Duration};

use smithay::{
    backend::renderer::{
        element::{
            surface::WaylandSurfaceRenderElement, utils::select_dmabuf_feedback, AsRenderElements,
            RenderElementStates,
        },
        ImportAll, Renderer,
    },
    desktop::{
        utils::{
            send_dmabuf_feedback_surface_tree, send_frames_surface_tree,
            take_presentation_feedback_surface_tree, with_surfaces_surface_tree,
            OutputPresentationFeedback,
        },
        Window,
    },
    input::{keyboard::KeyboardTarget, pointer::PointerTarget},
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
    space_elements,
    utils::{user_data::UserDataMap, Logical, Rectangle, Serial, Size},
    wayland::{
        compositor::{with_states, SurfaceData},
        seat::WaylandFocus,
        shell::xdg::{ToplevelSurface, XdgToplevelSurfaceData},
    },
    xwayland::{xwm::X11Relatable, X11Surface},
};

use crate::state::SurfaceDmabufFeedback;

space_elements! {
    #[derive(Debug, Clone, PartialEq)]
    pub CosmicSurface;
    Wayland=Window,
    X11=X11Surface,
}

impl From<ToplevelSurface> for CosmicSurface {
    fn from(s: ToplevelSurface) -> Self {
        CosmicSurface::Wayland(Window::new(s))
    }
}

impl From<Window> for CosmicSurface {
    fn from(w: Window) -> Self {
        CosmicSurface::Wayland(w)
    }
}

impl From<X11Surface> for CosmicSurface {
    fn from(s: X11Surface) -> Self {
        CosmicSurface::X11(s)
    }
}

pub const SSD_HEIGHT: i32 = 48;

impl CosmicSurface {
    pub fn title(&self) -> String {
        match self {
            CosmicSurface::Wayland(window) => {
                with_states(window.toplevel().wl_surface(), |states| {
                    states
                        .data_map
                        .get::<XdgToplevelSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .title
                        .clone()
                        .unwrap_or_default()
                })
            }
            CosmicSurface::X11(surface) => surface.title(),
            _ => unreachable!(),
        }
    }

    pub fn app_id(&self) -> String {
        match self {
            CosmicSurface::Wayland(window) => {
                with_states(window.toplevel().wl_surface(), |states| {
                    states
                        .data_map
                        .get::<XdgToplevelSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .app_id
                        .clone()
                        .unwrap_or_default()
                })
            }
            CosmicSurface::X11(surface) => surface.class(),
            _ => unreachable!(),
        }
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Logical>) {
        match self {
            CosmicSurface::Wayland(window) => window
                .toplevel()
                .with_pending_state(|state| state.size = Some(geo.size)),
            CosmicSurface::X11(surface) => {
                let _ = surface.configure(geo);
            }
            _ => {}
        }
    }

    pub fn set_bounds(&self, size: impl Into<Option<Size<i32, Logical>>>) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
                state.bounds = size.into();
            }),
            _ => {}
        }
    }

    pub fn is_activated(&self, pending: bool) -> bool {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    window.toplevel().with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Activated)
                    })
                } else {
                    window
                        .toplevel()
                        .current_state()
                        .states
                        .contains(ToplevelState::Activated)
                }
            }
            CosmicSurface::X11(surface) => surface.is_activated(),
            _ => unreachable!(),
        }
    }

    pub fn set_activated(&self, activated: bool) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
                if activated {
                    state.states.set(ToplevelState::Activated);
                } else {
                    state.states.unset(ToplevelState::Activated);
                }
            }),
            CosmicSurface::X11(surface) => {
                let _ = surface.set_activated(activated);
            }
            _ => unreachable!(),
        }
    }

    pub fn is_decorated(&self, pending: bool) -> bool {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    window.toplevel().with_pending_state(|pending| {
                        pending
                            .decoration_mode
                            .map(|mode| mode == DecorationMode::ClientSide)
                            .unwrap_or(true)
                    })
                } else {
                    window
                        .toplevel()
                        .current_state()
                        .decoration_mode
                        .map(|mode| mode == DecorationMode::ClientSide)
                        .unwrap_or(true)
                }
            }
            CosmicSurface::X11(surface) => surface.is_decorated(),
            _ => unreachable!(),
        }
    }

    pub fn try_force_undecorated(&self, enable: bool) {
        match self {
            CosmicSurface::Wayland(window) => {
                if enable {
                    let previous_decoration_state =
                        window.toplevel().current_state().decoration_mode.clone();
                    window
                        .user_data()
                        .insert_if_missing(|| RefCell::new(Option::<DecorationMode>::None));
                    *window
                        .user_data()
                        .get::<RefCell<Option<DecorationMode>>>()
                        .unwrap()
                        .borrow_mut() = previous_decoration_state;
                    window.toplevel().with_pending_state(|pending| {
                        pending.decoration_mode = Some(DecorationMode::ServerSide);
                    });
                } else {
                    let previous_mode = window
                        .user_data()
                        .get::<RefCell<Option<DecorationMode>>>()
                        .and_then(|m| m.borrow().clone());
                    window.toplevel().with_pending_state(|pending| {
                        pending.decoration_mode = previous_mode;
                    });
                }
            }
            _ => {}
        }
    }

    pub fn is_resizing(&self, pending: bool) -> Option<bool> {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    Some(window.toplevel().with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Resizing)
                    }))
                } else {
                    Some(
                        window
                            .toplevel()
                            .current_state()
                            .states
                            .contains(ToplevelState::Resizing),
                    )
                }
            }
            _ => None,
        }
    }

    pub fn set_resizing(&self, resizing: bool) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
                if resizing {
                    state.states.set(ToplevelState::Resizing);
                } else {
                    state.states.unset(ToplevelState::Resizing);
                }
            }),
            _ => {}
        }
    }

    pub fn is_tiled(&self, pending: bool) -> Option<bool> {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    Some(window.toplevel().with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::TiledLeft)
                    }))
                } else {
                    Some(
                        window
                            .toplevel()
                            .current_state()
                            .states
                            .contains(ToplevelState::TiledLeft),
                    )
                }
            }
            _ => None,
        }
    }

    pub fn set_tiled(&self, tiled: bool) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
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
            _ => {}
        }
    }

    pub fn is_fullscreen(&self, pending: bool) -> bool {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    window.toplevel().with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Fullscreen)
                    })
                } else {
                    window
                        .toplevel()
                        .current_state()
                        .states
                        .contains(ToplevelState::Fullscreen)
                }
            }
            CosmicSurface::X11(surface) => surface.is_fullscreen(),
            _ => unreachable!(),
        }
    }

    pub fn set_fullscreen(&self, fullscreen: bool) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
                if fullscreen {
                    state.states.set(ToplevelState::Fullscreen);
                } else {
                    state.states.unset(ToplevelState::Fullscreen);
                }
            }),
            CosmicSurface::X11(surface) => {
                let _ = surface.set_fullscreen(fullscreen);
            }
            _ => unreachable!(),
        }
    }

    pub fn is_maximized(&self, pending: bool) -> bool {
        match self {
            CosmicSurface::Wayland(window) => {
                if pending {
                    window.toplevel().with_pending_state(|pending| {
                        pending.states.contains(ToplevelState::Maximized)
                    })
                } else {
                    window
                        .toplevel()
                        .current_state()
                        .states
                        .contains(ToplevelState::Maximized)
                }
            }
            CosmicSurface::X11(surface) => surface.is_maximized(),
            _ => unreachable!(),
        }
    }

    pub fn set_maximized(&self, maximized: bool) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().with_pending_state(|state| {
                if maximized {
                    state.states.set(ToplevelState::Maximized);
                } else {
                    state.states.unset(ToplevelState::Maximized);
                }
            }),
            CosmicSurface::X11(surface) => {
                let _ = surface.set_maximized(maximized);
            }
            _ => unreachable!(),
        }
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        match self {
            CosmicSurface::Wayland(window) => {
                Some(with_states(window.toplevel().wl_surface(), |states| {
                    let attrs = states
                        .data_map
                        .get::<XdgToplevelSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attrs.min_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            CosmicSurface::X11(surface) => surface.min_size(),
            _ => unreachable!(),
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
        match self {
            CosmicSurface::Wayland(window) => {
                Some(with_states(window.toplevel().wl_surface(), |states| {
                    let attrs = states
                        .data_map
                        .get::<XdgToplevelSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attrs.max_size
                }))
                .filter(|size| !(size.w == 0 && size.h == 0))
            }
            CosmicSurface::X11(surface) => surface.max_size(),
            _ => unreachable!(),
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
        match self {
            CosmicSurface::Wayland(window) => {
                with_states(window.toplevel().wl_surface(), |states| {
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
                })
            }
            _ => true,
        }
    }

    pub fn force_configure(&self) -> Option<Serial> {
        match self {
            CosmicSurface::Wayland(window) => Some(window.toplevel().send_configure()),
            CosmicSurface::X11(surface) => {
                let _ = surface.configure(None);
                None
            }
            _ => unreachable!(),
        }
    }

    pub fn send_configure(&self) -> Option<Serial> {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().send_pending_configure(),
            CosmicSurface::X11(surface) => {
                let _ = surface.configure(None);
                None
            }
            _ => unreachable!(),
        }
    }

    pub fn close(&self) {
        match self {
            CosmicSurface::Wayland(window) => window.toplevel().send_close(),
            CosmicSurface::X11(surface) => {
                let _ = surface.close();
            }
            _ => unreachable!(),
        }
    }

    pub fn on_commit(&self) {
        match self {
            CosmicSurface::Wayland(window) => window.on_commit(),
            _ => {}
        }
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
        match self {
            CosmicSurface::Wayland(window) => {
                window.send_frame(output, time, throttle, primary_scan_out_output);
            }
            CosmicSurface::X11(surface) => {
                if let Some(wl_surface) = surface.wl_surface() {
                    send_frames_surface_tree(
                        &wl_surface,
                        output,
                        time,
                        throttle,
                        primary_scan_out_output,
                    )
                }
            }
            _ => unreachable!(),
        }
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
        match self {
            CosmicSurface::Wayland(window) => {
                window.send_dmabuf_feedback(output, primary_scan_out_output, |surface, _| {
                    select_dmabuf_feedback(
                        surface,
                        render_element_states,
                        &feedback.render_feedback,
                        &feedback.scanout_feedback,
                    )
                })
            }
            CosmicSurface::X11(surface) => {
                if let Some(wl_surface) = surface.wl_surface() {
                    send_dmabuf_feedback_surface_tree(
                        &wl_surface,
                        output,
                        primary_scan_out_output,
                        |surface, _| {
                            select_dmabuf_feedback(
                                surface,
                                render_element_states,
                                &feedback.render_feedback,
                                &feedback.scanout_feedback,
                            )
                        },
                    )
                }
            }
            _ => unreachable!(),
        }
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
        match self {
            CosmicSurface::Wayland(window) => window.take_presentation_feedback(
                output_feedback,
                primary_scan_out_output,
                presentation_feedback_flags,
            ),
            CosmicSurface::X11(surface) => {
                if let Some(wl_surface) = surface.wl_surface() {
                    take_presentation_feedback_surface_tree(
                        &wl_surface,
                        output_feedback,
                        primary_scan_out_output,
                        presentation_feedback_flags,
                    )
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn with_surfaces<F>(&self, processor: F)
    where
        F: FnMut(&WlSurface, &SurfaceData) + Copy,
    {
        match self {
            CosmicSurface::Wayland(window) => window.with_surfaces(processor),
            CosmicSurface::X11(surface) => {
                if let Some(wl_surface) = surface.wl_surface() {
                    with_surfaces_surface_tree(&wl_surface, processor)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn user_data(&self) -> &UserDataMap {
        match self {
            CosmicSurface::Wayland(window) => window.user_data(),
            CosmicSurface::X11(surface) => surface.user_data(),
            _ => unreachable!(),
        }
    }
}

impl KeyboardTarget<crate::state::State> for CosmicSurface {
    fn enter(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        keys: Vec<smithay::input::keyboard::KeysymHandle<'_>>,
        serial: smithay::utils::Serial,
    ) {
        match self {
            CosmicSurface::Wayland(window) => {
                KeyboardTarget::enter(window, seat, data, keys, serial)
            }
            CosmicSurface::X11(surface) => {
                KeyboardTarget::enter(surface, seat, data, vec![], serial)
            }
            _ => unreachable!(),
        }
    }

    fn leave(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        serial: smithay::utils::Serial,
    ) {
        match self {
            CosmicSurface::Wayland(window) => KeyboardTarget::leave(window, seat, data, serial),
            CosmicSurface::X11(surface) => KeyboardTarget::leave(surface, seat, data, serial),
            _ => unreachable!(),
        }
    }

    fn key(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        key: smithay::input::keyboard::KeysymHandle<'_>,
        state: smithay::backend::input::KeyState,
        serial: smithay::utils::Serial,
        time: u32,
    ) {
        match self {
            CosmicSurface::Wayland(window) => {
                KeyboardTarget::key(window, seat, data, key, state, serial, time)
            }
            CosmicSurface::X11(surface) => {
                KeyboardTarget::key(surface, seat, data, key, state, serial, time)
            }
            _ => unreachable!(),
        }
    }

    fn modifiers(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        modifiers: smithay::input::keyboard::ModifiersState,
        serial: smithay::utils::Serial,
    ) {
        match self {
            CosmicSurface::Wayland(window) => {
                KeyboardTarget::modifiers(window, seat, data, modifiers, serial)
            }
            CosmicSurface::X11(surface) => {
                KeyboardTarget::modifiers(surface, seat, data, modifiers, serial)
            }
            _ => unreachable!(),
        }
    }
}

impl PointerTarget<crate::state::State> for CosmicSurface {
    fn enter(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        event: &smithay::input::pointer::MotionEvent,
    ) {
        match self {
            CosmicSurface::Wayland(window) => PointerTarget::enter(window, seat, data, event),
            CosmicSurface::X11(surface) => PointerTarget::enter(surface, seat, data, event),
            _ => unreachable!(),
        }
    }

    fn motion(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        event: &smithay::input::pointer::MotionEvent,
    ) {
        match self {
            CosmicSurface::Wayland(window) => PointerTarget::motion(window, seat, data, event),
            CosmicSurface::X11(surface) => PointerTarget::motion(surface, seat, data, event),
            _ => unreachable!(),
        }
    }

    fn relative_motion(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        event: &smithay::input::pointer::RelativeMotionEvent,
    ) {
        match self {
            CosmicSurface::Wayland(window) => {
                PointerTarget::relative_motion(window, seat, data, event)
            }
            CosmicSurface::X11(surface) => {
                PointerTarget::relative_motion(surface, seat, data, event)
            }
            _ => unreachable!(),
        }
    }

    fn button(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        event: &smithay::input::pointer::ButtonEvent,
    ) {
        match self {
            CosmicSurface::Wayland(window) => PointerTarget::button(window, seat, data, event),
            CosmicSurface::X11(surface) => PointerTarget::button(surface, seat, data, event),
            _ => unreachable!(),
        }
    }

    fn axis(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        frame: smithay::input::pointer::AxisFrame,
    ) {
        match self {
            CosmicSurface::Wayland(window) => PointerTarget::axis(window, seat, data, frame),
            CosmicSurface::X11(surface) => PointerTarget::axis(surface, seat, data, frame),
            _ => unreachable!(),
        }
    }

    fn leave(
        &self,
        seat: &smithay::input::Seat<crate::state::State>,
        data: &mut crate::state::State,
        serial: smithay::utils::Serial,
        time: u32,
    ) {
        match self {
            CosmicSurface::Wayland(window) => {
                PointerTarget::leave(window, seat, data, serial, time)
            }
            CosmicSurface::X11(surface) => PointerTarget::leave(surface, seat, data, serial, time),
            _ => unreachable!(),
        }
    }
}

impl WaylandFocus for CosmicSurface {
    fn wl_surface(&self) -> Option<WlSurface> {
        match self {
            CosmicSurface::Wayland(window) => window.wl_surface(),
            CosmicSurface::X11(surface) => surface.wl_surface(),
            _ => unreachable!(),
        }
    }
}

impl<R> AsRenderElements<R> for CosmicSurface
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = WaylandSurfaceRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: smithay::utils::Point<i32, smithay::utils::Physical>,
        scale: smithay::utils::Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        match self {
            CosmicSurface::Wayland(window) => {
                window.render_elements(renderer, location, scale, alpha)
            }
            CosmicSurface::X11(surface) => {
                surface.render_elements(renderer, location, scale, alpha)
            }
            _ => unreachable!(),
        }
    }
}

impl X11Relatable for CosmicSurface {
    fn is_window(&self, window: &X11Surface) -> bool {
        match self {
            CosmicSurface::X11(surface) => surface == window,
            _ => false,
        }
    }
}
