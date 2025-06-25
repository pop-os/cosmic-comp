use std::{borrow::Cow, sync::Weak, time::Duration};

use crate::{
    shell::{
        element::{CosmicMapped, CosmicStack, CosmicWindow},
        layout::tiling::ResizeForkTarget,
        zoom::ZoomFocusTarget,
        CosmicSurface, SeatExt,
    },
    utils::prelude::*,
    wayland::handlers::{screencopy::SessionHolder, xdg_shell::popup::get_popup_toplevel},
};
use id_tree::NodeId;
use smithay::{
    backend::input::KeyState,
    desktop::{space::SpaceElement, LayerSurface, PopupKind, WindowSurface, WindowSurfaceType},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            MotionEvent as PointerMotionEvent, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent, TouchTarget,
            UpEvent,
        },
        Seat,
    },
    reexports::wayland_server::{
        backend::ObjectId, protocol::wl_surface::WlSurface, Client, Resource,
    },
    utils::{IsAlive, Logical, Point, Serial, Transform},
    wayland::{seat::WaylandFocus, session_lock::LockSurface},
    xwayland::xwm::XwmId,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusTarget {
    WlSurface {
        surface: WlSurface,
        toplevel: Option<PointerFocusToplevel>,
    },
    StackUI(CosmicStack),
    WindowUI(CosmicWindow),
    ResizeFork(ResizeForkTarget),
    ZoomUI(ZoomFocusTarget),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusToplevel {
    Surface(CosmicSurface),
    Popup(PopupKind),
}

impl From<CosmicSurface> for PointerFocusToplevel {
    fn from(value: CosmicSurface) -> Self {
        PointerFocusToplevel::Surface(value)
    }
}

impl From<PopupKind> for PointerFocusToplevel {
    fn from(value: PopupKind) -> Self {
        PointerFocusToplevel::Popup(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyboardFocusTarget {
    Element(CosmicMapped),
    Fullscreen(CosmicSurface),
    Group(WindowGroup),
    LayerSurface(LayerSurface),
    Popup(PopupKind),
    LockSurface(LockSurface),
    XWaylandGrab(WlSurface),
}

// TODO: This should be TryFrom, but PopupGrab needs to be able to convert. Fix this in smithay
impl From<KeyboardFocusTarget> for PointerFocusTarget {
    fn from(target: KeyboardFocusTarget) -> Self {
        match target {
            KeyboardFocusTarget::Element(elem) => {
                let window = elem.active_window();
                let surface = window.wl_surface().unwrap();
                PointerFocusTarget::WlSurface {
                    surface: surface.into_owned(),
                    toplevel: Some(window.into()),
                }
            }
            KeyboardFocusTarget::Fullscreen(elem) => PointerFocusTarget::WlSurface {
                surface: elem.wl_surface().unwrap().into_owned(),
                toplevel: Some(elem.into()),
            },
            KeyboardFocusTarget::LayerSurface(layer) => PointerFocusTarget::WlSurface {
                surface: layer.wl_surface().clone(),
                toplevel: None,
            },
            KeyboardFocusTarget::Popup(popup) => PointerFocusTarget::WlSurface {
                surface: popup.wl_surface().clone(),
                toplevel: Some(popup.into()),
            },
            KeyboardFocusTarget::LockSurface(lock) => PointerFocusTarget::WlSurface {
                surface: lock.wl_surface().clone(),
                toplevel: None,
            },
            _ => unreachable!("A group cannot start a popup grab"),
        }
    }
}

impl PointerFocusTarget {
    pub fn under_surface<P: Into<Point<f64, Logical>>>(
        surface: &CosmicSurface,
        point: P,
    ) -> Option<(Self, Point<i32, Logical>)> {
        match surface.0.underlying_surface() {
            WindowSurface::Wayland(_toplevel) => surface
                .0
                .surface_under(point, WindowSurfaceType::ALL)
                .map(|(wl_surface, point)| {
                    (
                        Self::WlSurface {
                            surface: wl_surface,
                            toplevel: Some(surface.clone().into()),
                        },
                        point,
                    )
                }),
            WindowSurface::X11(x11_surface) => Some((
                Self::WlSurface {
                    surface: x11_surface.wl_surface()?,
                    toplevel: Some(surface.clone().into()),
                },
                Point::default(),
            )),
        }
    }

    pub fn toplevel(&self, shell: &Shell) -> Option<CosmicSurface> {
        match &self {
            PointerFocusTarget::WlSurface {
                toplevel: Some(PointerFocusToplevel::Surface(surface)),
                ..
            } => Some(surface.clone()),
            PointerFocusTarget::WlSurface {
                toplevel: Some(PointerFocusToplevel::Popup(PopupKind::Xdg(popup))),
                ..
            } => get_popup_toplevel(popup)
                .and_then(|s| shell.element_for_surface(&s).map(|mapped| (mapped, s)))
                .and_then(|(m, s)| {
                    m.windows()
                        .find(|(w, _)| w.wl_surface().map(|s2| s == *s2).unwrap_or(false))
                        .map(|(s, _)| s)
                }),
            PointerFocusTarget::StackUI(stack) => Some(stack.active()),
            PointerFocusTarget::WindowUI(window) => Some(window.surface()),
            _ => None,
        }
    }

    pub fn is_client(&self, client: &Client) -> bool {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                surface.client().is_some_and(|c| c == *client)
            }
            _ => false,
        }
    }
}

impl KeyboardFocusTarget {
    pub fn toplevel(&self) -> Option<Cow<'_, WlSurface>> {
        match self {
            KeyboardFocusTarget::Element(mapped) => mapped.wl_surface(),
            KeyboardFocusTarget::Popup(PopupKind::Xdg(xdg)) => {
                get_popup_toplevel(&xdg).map(Cow::Owned)
            }
            _ => None,
        }
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        match self {
            KeyboardFocusTarget::Element(mapped) => Box::new(mapped.windows().map(|(s, _)| s))
                as Box<dyn Iterator<Item = CosmicSurface>>,
            KeyboardFocusTarget::Fullscreen(surface) => Box::new(std::iter::once(surface.clone())),
            _ => Box::new(std::iter::empty()),
        }
    }

    pub fn is_xwm(&self, xwm: XwmId) -> bool {
        match self {
            KeyboardFocusTarget::Element(mapped) => {
                if let Some(surface) = mapped.active_window().x11_surface() {
                    return surface.xwm_id().unwrap() == xwm;
                }
            }
            KeyboardFocusTarget::Fullscreen(surface) => {
                if let Some(surface) = surface.x11_surface() {
                    return surface.xwm_id().unwrap() == xwm;
                }
            }
            _ => {}
        }

        false
    }
}

#[derive(Debug, Clone)]
pub struct WindowGroup {
    pub node: NodeId,
    pub alive: Weak<()>,
    pub focus_stack: Vec<NodeId>,
}

impl PartialEq for WindowGroup {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && Weak::ptr_eq(&self.alive, &other.alive)
    }
}

impl IsAlive for PointerFocusTarget {
    fn alive(&self) -> bool {
        match self {
            // XXX? does this change anything
            PointerFocusTarget::WlSurface { surface, .. } => surface.alive(),
            PointerFocusTarget::StackUI(e) => e.alive(),
            PointerFocusTarget::WindowUI(e) => e.alive(),
            PointerFocusTarget::ResizeFork(f) => f.alive(),
            PointerFocusTarget::ZoomUI(_) => true,
        }
    }
}

impl IsAlive for KeyboardFocusTarget {
    fn alive(&self) -> bool {
        match self {
            KeyboardFocusTarget::Element(e) => e.alive(),
            KeyboardFocusTarget::Fullscreen(f) => f.alive(),
            KeyboardFocusTarget::Group(g) => g.alive.upgrade().is_some(),
            KeyboardFocusTarget::LayerSurface(l) => l.alive(),
            KeyboardFocusTarget::Popup(p) => p.alive(),
            KeyboardFocusTarget::LockSurface(l) => l.alive(),
            KeyboardFocusTarget::XWaylandGrab(g) => g.alive(),
        }
    }
}

impl PointerTarget<State> for PointerFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let toplevel = self.toplevel(&*data.common.shell.read());
        if let Some(element) = toplevel {
            for session in element.cursor_sessions() {
                session.set_cursor_pos(Some(
                    event
                        .location
                        .to_buffer(1.0, Transform::Normal, &element.geometry().size.to_f64())
                        .to_i32_round(),
                ));
                if let Some((_, hotspot)) = seat
                    .cursor_geometry((0.0, 0.0), Duration::from_millis(event.time as u64).into())
                {
                    session.set_cursor_hotspot(hotspot);
                } else {
                    session.set_cursor_hotspot((0, 0));
                }
            }
        }

        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::enter(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::enter(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::enter(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::enter(f, seat, data, event),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::enter(e, seat, data, event),
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let toplevel = self.toplevel(&*data.common.shell.read());
        if let Some(element) = toplevel {
            for session in element.cursor_sessions() {
                session.set_cursor_pos(Some(
                    event
                        .location
                        .to_buffer(1.0, Transform::Normal, &element.geometry().size.to_f64())
                        .to_i32_round(),
                ));
                if let Some((_, hotspot)) = seat
                    .cursor_geometry((0.0, 0.0), Duration::from_millis(event.time as u64).into())
                {
                    session.set_cursor_hotspot(hotspot);
                } else {
                    session.set_cursor_hotspot((0, 0));
                }
            }
        }

        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::motion(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::motion(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::motion(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::motion(f, seat, data, event),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::motion(e, seat, data, event),
        }
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::relative_motion(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::relative_motion(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::relative_motion(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::relative_motion(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => PointerTarget::relative_motion(e, seat, data, event),
        }
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::button(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::button(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::button(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::button(f, seat, data, event),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::button(e, seat, data, event),
        }
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::axis(surface, seat, data, frame)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::axis(u, seat, data, frame),
            PointerFocusTarget::WindowUI(u) => PointerTarget::axis(u, seat, data, frame),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::axis(f, seat, data, frame),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::axis(e, seat, data, frame),
        }
    }
    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::frame(surface, seat, data)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::frame(u, seat, data),
            PointerFocusTarget::WindowUI(u) => PointerTarget::frame(u, seat, data),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::frame(f, seat, data),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::frame(e, seat, data),
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        let toplevel = self.toplevel(&*data.common.shell.read());
        if let Some(element) = toplevel {
            for session in element.cursor_sessions() {
                session.set_cursor_pos(None);
                session.set_cursor_hotspot((0, 0));
            }
        }

        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::leave(surface, seat, data, serial, time)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::leave(u, seat, data, serial, time),
            PointerFocusTarget::WindowUI(u) => PointerTarget::leave(u, seat, data, serial, time),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::leave(f, seat, data, serial, time),
            PointerFocusTarget::ZoomUI(e) => PointerTarget::leave(e, seat, data, serial, time),
        }
    }
    fn gesture_swipe_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_swipe_begin(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_begin(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_begin(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => {
                PointerTarget::gesture_swipe_begin(e, seat, data, event)
            }
        }
    }
    fn gesture_swipe_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeUpdateEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_swipe_update(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_update(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_update(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_update(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => {
                PointerTarget::gesture_swipe_update(e, seat, data, event)
            }
        }
    }
    fn gesture_swipe_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeEndEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_swipe_end(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_end(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_end(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_end(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => PointerTarget::gesture_swipe_end(e, seat, data, event),
        }
    }
    fn gesture_pinch_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_pinch_begin(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_begin(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_begin(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => {
                PointerTarget::gesture_pinch_begin(e, seat, data, event)
            }
        }
    }
    fn gesture_pinch_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchUpdateEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_pinch_update(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_update(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_update(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_update(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => {
                PointerTarget::gesture_pinch_update(e, seat, data, event)
            }
        }
    }
    fn gesture_pinch_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchEndEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_pinch_end(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_end(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_end(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_end(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => PointerTarget::gesture_pinch_end(e, seat, data, event),
        }
    }
    fn gesture_hold_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureHoldBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_hold_begin(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_hold_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_hold_begin(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_hold_begin(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => {
                PointerTarget::gesture_hold_begin(e, seat, data, event)
            }
        }
    }
    fn gesture_hold_end(&self, seat: &Seat<State>, data: &mut State, event: &GestureHoldEndEvent) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                PointerTarget::gesture_hold_end(surface, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::gesture_hold_end(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_hold_end(u, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_hold_end(f, seat, data, event)
            }
            PointerFocusTarget::ZoomUI(e) => PointerTarget::gesture_hold_end(e, seat, data, event),
        }
    }
}

impl TouchTarget<State> for PointerFocusTarget {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::down(surface, seat, data, event, seq)
            }
            PointerFocusTarget::WindowUI(window) => {
                TouchTarget::down(window, seat, data, event, seq)
            }
            PointerFocusTarget::StackUI(stack) => TouchTarget::down(stack, seat, data, event, seq),
            PointerFocusTarget::ResizeFork(fork) => TouchTarget::down(fork, seat, data, event, seq),
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::down(elem, seat, data, event, seq),
        }
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::up(surface, seat, data, event, seq)
            }
            PointerFocusTarget::WindowUI(window) => TouchTarget::up(window, seat, data, event, seq),
            PointerFocusTarget::StackUI(stack) => TouchTarget::up(stack, seat, data, event, seq),
            PointerFocusTarget::ResizeFork(fork) => TouchTarget::up(fork, seat, data, event, seq),
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::up(elem, seat, data, event, seq),
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::motion(surface, seat, data, event, seq)
            }
            PointerFocusTarget::WindowUI(window) => {
                TouchTarget::motion(window, seat, data, event, seq)
            }
            PointerFocusTarget::StackUI(stack) => {
                TouchTarget::motion(stack, seat, data, event, seq)
            }
            PointerFocusTarget::ResizeFork(fork) => {
                TouchTarget::motion(fork, seat, data, event, seq)
            }
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::motion(elem, seat, data, event, seq),
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::frame(surface, seat, data, seq)
            }
            PointerFocusTarget::WindowUI(window) => TouchTarget::frame(window, seat, data, seq),
            PointerFocusTarget::StackUI(stack) => TouchTarget::frame(stack, seat, data, seq),
            PointerFocusTarget::ResizeFork(fork) => TouchTarget::frame(fork, seat, data, seq),
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::frame(elem, seat, data, seq),
        }
    }

    fn cancel(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::cancel(surface, seat, data, seq)
            }
            PointerFocusTarget::WindowUI(window) => TouchTarget::cancel(window, seat, data, seq),
            PointerFocusTarget::StackUI(stack) => TouchTarget::cancel(stack, seat, data, seq),
            PointerFocusTarget::ResizeFork(fork) => TouchTarget::cancel(fork, seat, data, seq),
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::cancel(elem, seat, data, seq),
        }
    }

    fn shape(&self, seat: &Seat<State>, data: &mut State, event: &ShapeEvent, seq: Serial) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::shape(surface, seat, data, event, seq)
            }
            PointerFocusTarget::WindowUI(window) => {
                TouchTarget::shape(window, seat, data, event, seq)
            }
            PointerFocusTarget::StackUI(stack) => TouchTarget::shape(stack, seat, data, event, seq),
            PointerFocusTarget::ResizeFork(fork) => {
                TouchTarget::shape(fork, seat, data, event, seq)
            }
            PointerFocusTarget::ZoomUI(elem) => TouchTarget::shape(elem, seat, data, event, seq),
        }
    }

    fn orientation(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &OrientationEvent,
        seq: Serial,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                TouchTarget::orientation(surface, seat, data, event, seq)
            }
            PointerFocusTarget::WindowUI(window) => {
                TouchTarget::orientation(window, seat, data, event, seq)
            }
            PointerFocusTarget::StackUI(stack) => {
                TouchTarget::orientation(stack, seat, data, event, seq)
            }
            PointerFocusTarget::ResizeFork(fork) => {
                TouchTarget::orientation(fork, seat, data, event, seq)
            }
            PointerFocusTarget::ZoomUI(elem) => {
                TouchTarget::orientation(elem, seat, data, event, seq)
            }
        }
    }
}

impl KeyboardTarget<State> for KeyboardFocusTarget {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        match self {
            KeyboardFocusTarget::Element(w) => KeyboardTarget::enter(w, seat, data, keys, serial),
            KeyboardFocusTarget::Fullscreen(w) => {
                KeyboardTarget::enter(w, seat, data, keys, serial)
            }
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => {
                KeyboardTarget::enter(l.wl_surface(), seat, data, keys, serial)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::enter(p.wl_surface(), seat, data, keys, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::enter(l.wl_surface(), seat, data, keys, serial)
            }
            KeyboardFocusTarget::XWaylandGrab(g) => {
                KeyboardTarget::enter(g, seat, data, keys, serial)
            }
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        match self {
            KeyboardFocusTarget::Element(w) => KeyboardTarget::leave(w, seat, data, serial),
            KeyboardFocusTarget::Fullscreen(w) => KeyboardTarget::leave(w, seat, data, serial),
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => {
                KeyboardTarget::leave(l.wl_surface(), seat, data, serial)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::leave(p.wl_surface(), seat, data, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::leave(l.wl_surface(), seat, data, serial)
            }
            KeyboardFocusTarget::XWaylandGrab(g) => KeyboardTarget::leave(g, seat, data, serial),
        }
    }
    fn key(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        time: u32,
    ) {
        match self {
            KeyboardFocusTarget::Element(w) => {
                KeyboardTarget::key(w, seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Fullscreen(w) => {
                KeyboardTarget::key(w, seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => {
                KeyboardTarget::key(l.wl_surface(), seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::key(p.wl_surface(), seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::key(l.wl_surface(), seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::XWaylandGrab(g) => {
                KeyboardTarget::key(g, seat, data, key, state, serial, time)
            }
        }
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        match self {
            KeyboardFocusTarget::Element(w) => {
                KeyboardTarget::modifiers(w, seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Fullscreen(w) => {
                KeyboardTarget::modifiers(w, seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => {
                KeyboardTarget::modifiers(l.wl_surface(), seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::modifiers(p.wl_surface(), seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::modifiers(l.wl_surface(), seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::XWaylandGrab(g) => {
                KeyboardTarget::modifiers(g, seat, data, modifiers, serial)
            }
        }
    }
    fn replace(
        &self,
        replaced: <State as smithay::input::SeatHandler>::KeyboardFocus,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        if !replaced
            .wl_surface()
            .is_some_and(|s| Some(s) == self.wl_surface())
        {
            KeyboardTarget::leave(&replaced, seat, data, serial);
            KeyboardTarget::enter(self, seat, data, keys, serial);
            KeyboardTarget::modifiers(self, seat, data, modifiers, serial);
        }
    }
}

impl WaylandFocus for KeyboardFocusTarget {
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        match self {
            KeyboardFocusTarget::Element(w) => WaylandFocus::wl_surface(w),
            KeyboardFocusTarget::Fullscreen(w) => WaylandFocus::wl_surface(w),
            KeyboardFocusTarget::Group(_) => None,
            KeyboardFocusTarget::LayerSurface(l) => Some(Cow::Borrowed(l.wl_surface())),
            KeyboardFocusTarget::Popup(p) => Some(Cow::Borrowed(p.wl_surface())),
            KeyboardFocusTarget::LockSurface(l) => Some(Cow::Borrowed(l.wl_surface())),
            KeyboardFocusTarget::XWaylandGrab(g) => Some(Cow::Borrowed(g)),
        }
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            KeyboardFocusTarget::Element(w) => WaylandFocus::same_client_as(w, object_id),
            KeyboardFocusTarget::Fullscreen(w) => WaylandFocus::same_client_as(w, object_id),
            KeyboardFocusTarget::Group(_) => false,
            KeyboardFocusTarget::LayerSurface(l) => l.wl_surface().id().same_client_as(object_id),
            KeyboardFocusTarget::Popup(p) => p.wl_surface().id().same_client_as(object_id),
            KeyboardFocusTarget::LockSurface(l) => l.wl_surface().id().same_client_as(object_id),
            KeyboardFocusTarget::XWaylandGrab(g) => g.id().same_client_as(object_id),
        }
    }
}

impl WaylandFocus for PointerFocusTarget {
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        Some(match self {
            PointerFocusTarget::WlSurface { surface, .. } => Cow::Borrowed(surface),
            PointerFocusTarget::ResizeFork(_)
            | PointerFocusTarget::StackUI(_)
            | PointerFocusTarget::WindowUI(_)
            | PointerFocusTarget::ZoomUI(_) => {
                return None;
            }
        })
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => surface.id().same_client_as(object_id),
            PointerFocusTarget::StackUI(stack) => stack
                .active()
                .wl_surface()
                .map(|s| s.id().same_client_as(object_id))
                .unwrap_or(false),
            PointerFocusTarget::WindowUI(window) => window
                .wl_surface()
                .map(|s| s.id().same_client_as(object_id))
                .unwrap_or(false),
            PointerFocusTarget::ResizeFork(_) | PointerFocusTarget::ZoomUI(_) => false,
        }
    }
}

impl From<PopupKind> for PointerFocusTarget {
    fn from(p: PopupKind) -> Self {
        PointerFocusTarget::WlSurface {
            surface: p.wl_surface().clone(),
            toplevel: None,
        }
    }
}

impl From<ResizeForkTarget> for PointerFocusTarget {
    fn from(f: ResizeForkTarget) -> Self {
        PointerFocusTarget::ResizeFork(f)
    }
}

impl From<LockSurface> for PointerFocusTarget {
    fn from(l: LockSurface) -> Self {
        PointerFocusTarget::WlSurface {
            surface: l.wl_surface().clone(),
            toplevel: None,
        }
    }
}

impl From<CosmicMapped> for KeyboardFocusTarget {
    fn from(w: CosmicMapped) -> Self {
        KeyboardFocusTarget::Element(w)
    }
}

impl From<CosmicSurface> for KeyboardFocusTarget {
    fn from(s: CosmicSurface) -> Self {
        KeyboardFocusTarget::Fullscreen(s)
    }
}

impl From<WindowGroup> for KeyboardFocusTarget {
    fn from(w: WindowGroup) -> Self {
        KeyboardFocusTarget::Group(w)
    }
}

impl From<LayerSurface> for KeyboardFocusTarget {
    fn from(l: LayerSurface) -> Self {
        KeyboardFocusTarget::LayerSurface(l)
    }
}

impl From<PopupKind> for KeyboardFocusTarget {
    fn from(p: PopupKind) -> Self {
        KeyboardFocusTarget::Popup(p)
    }
}

impl From<LockSurface> for KeyboardFocusTarget {
    fn from(l: LockSurface) -> Self {
        KeyboardFocusTarget::LockSurface(l)
    }
}
