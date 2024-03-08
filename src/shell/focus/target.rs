use std::sync::Weak;

use crate::{
    shell::{element::CosmicMapped, layout::tiling::ResizeForkTarget, CosmicSurface},
    utils::prelude::*,
    wayland::handlers::xdg_shell::popup::get_surface_toplevel,
};
use id_tree::NodeId;
use smithay::{
    backend::input::KeyState,
    desktop::{LayerSurface, PopupKind, WindowSurface, WindowSurfaceType},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        Seat,
    },
    reexports::wayland_server::{backend::ObjectId, protocol::wl_surface::WlSurface, Resource},
    utils::{IsAlive, Logical, Point, Serial},
    wayland::{seat::WaylandFocus, session_lock::LockSurface},
    xwayland::X11Surface,
};

// discuss: should contain WlSurface, IcedElement
#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusTarget {
    WlSurface(WlSurface),
    Element(CosmicMapped),
    X11(X11Surface),
    ResizeFork(ResizeForkTarget),
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyboardFocusTarget {
    // Unlike pointer target, should not be a subsurface
    WlSurface(WlSurface),
    Element(CosmicMapped),
    Group(WindowGroup),
}

// TODO: This should be TryFrom, but PopupGrab needs to be able to convert. Fix this in smithay
impl From<KeyboardFocusTarget> for PointerFocusTarget {
    fn from(target: KeyboardFocusTarget) -> Self {
        match target {
            KeyboardFocusTarget::WlSurface(surface) => PointerFocusTarget::WlSurface(surface),
            KeyboardFocusTarget::Element(elem) => PointerFocusTarget::Element(elem),
            // KeyboardFocusTarget::Fullscreen(elem) => PointerFocusTarget::Fullscreen(elem),
            _ => unreachable!("A window grab cannot start a popup grab"),
        }
    }
}

// TODO get parent toplevel of subsurface
impl TryFrom<PointerFocusTarget> for KeyboardFocusTarget {
    type Error = ();
    fn try_from(target: PointerFocusTarget) -> Result<Self, Self::Error> {
        match target {
            PointerFocusTarget::WlSurface(surface) => Ok(KeyboardFocusTarget::WlSurface(surface)),
            PointerFocusTarget::Element(mapped) => Ok(KeyboardFocusTarget::Element(mapped)),
            PointerFocusTarget::X11(surface) => {
                if let Some(wl_surface) = surface.wl_surface() {
                    Ok(KeyboardFocusTarget::WlSurface(wl_surface))
                } else {
                    Err(())
                }
            }
            // PointerFocusTarget::Fullscreen(surf) => Ok(KeyboardFocusTarget::Fullscreen(surf)),
            _ => Err(()),
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
                .map(|(wl_surface, point)| (Self::WlSurface(wl_surface), point)),
            WindowSurface::X11(surface) => Some((Self::X11(surface.clone()), Point::default())),
        }
    }
}

impl KeyboardFocusTarget {
    pub fn toplevel(&self) -> Option<WlSurface> {
        match self {
            KeyboardFocusTarget::Element(mapped) => mapped.wl_surface(),
            KeyboardFocusTarget::WlSurface(surface) => get_surface_toplevel(&surface),
            _ => None,
        }
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
            PointerFocusTarget::WlSurface(s) => s.alive(),
            PointerFocusTarget::Element(e) => e.alive(),
            PointerFocusTarget::X11(s) => s.alive(),
            PointerFocusTarget::ResizeFork(f) => f.alive(),
        }
    }
}

impl IsAlive for KeyboardFocusTarget {
    fn alive(&self) -> bool {
        match self {
            KeyboardFocusTarget::WlSurface(s) => s.alive(),
            KeyboardFocusTarget::Element(e) => e.alive(),
            KeyboardFocusTarget::Group(g) => g.alive.upgrade().is_some(),
        }
    }
}

impl PointerTarget<State> for PointerFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::enter(s, seat, data, event),
            PointerFocusTarget::Element(w) => PointerTarget::enter(w, seat, data, event),
            PointerFocusTarget::X11(s) => PointerTarget::enter(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::enter(f, seat, data, event),
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::motion(w, seat, data, event),
            PointerFocusTarget::WlSurface(s) => PointerTarget::motion(s, seat, data, event),
            PointerFocusTarget::X11(s) => PointerTarget::motion(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::motion(f, seat, data, event),
        }
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::relative_motion(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => PointerTarget::relative_motion(w, seat, data, event),
            PointerFocusTarget::X11(s) => PointerTarget::relative_motion(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::relative_motion(f, seat, data, event)
            }
        }
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::button(s, seat, data, event),
            PointerFocusTarget::Element(w) => PointerTarget::button(w, seat, data, event),
            PointerFocusTarget::X11(s) => PointerTarget::button(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::button(f, seat, data, event),
        }
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::axis(s, seat, data, frame),
            PointerFocusTarget::Element(w) => PointerTarget::axis(w, seat, data, frame),
            PointerFocusTarget::X11(s) => PointerTarget::axis(s, seat, data, frame),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::axis(f, seat, data, frame),
        }
    }
    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::frame(s, seat, data),
            PointerFocusTarget::Element(w) => PointerTarget::frame(w, seat, data),
            PointerFocusTarget::X11(s) => PointerTarget::frame(s, seat, data),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::frame(f, seat, data),
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::leave(s, seat, data, serial, time),
            PointerFocusTarget::Element(w) => PointerTarget::leave(w, seat, data, serial, time),
            PointerFocusTarget::X11(s) => PointerTarget::leave(s, seat, data, serial, time),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::leave(f, seat, data, serial, time),
        }
    }
    fn gesture_swipe_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_swipe_begin(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_swipe_begin(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_swipe_begin(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_begin(f, seat, data, event)
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
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_swipe_update(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_swipe_update(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_swipe_update(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_update(f, seat, data, event)
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
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_swipe_end(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_swipe_end(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_swipe_end(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_swipe_end(f, seat, data, event)
            }
        }
    }
    fn gesture_pinch_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_pinch_begin(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_pinch_begin(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_pinch_begin(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_begin(f, seat, data, event)
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
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_pinch_update(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_pinch_update(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_pinch_update(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_update(f, seat, data, event)
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
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_pinch_end(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_pinch_end(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_pinch_end(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_pinch_end(f, seat, data, event)
            }
        }
    }
    fn gesture_hold_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureHoldBeginEvent,
    ) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_hold_begin(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => {
                PointerTarget::gesture_hold_begin(w, seat, data, event)
            }
            PointerFocusTarget::X11(s) => PointerTarget::gesture_hold_begin(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_hold_begin(f, seat, data, event)
            }
        }
    }
    fn gesture_hold_end(&self, seat: &Seat<State>, data: &mut State, event: &GestureHoldEndEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::gesture_hold_end(s, seat, data, event)
            }
            PointerFocusTarget::Element(w) => PointerTarget::gesture_hold_end(w, seat, data, event),
            PointerFocusTarget::X11(s) => PointerTarget::gesture_hold_end(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::gesture_hold_end(f, seat, data, event)
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
            KeyboardFocusTarget::WlSurface(s) => KeyboardTarget::enter(s, seat, data, keys, serial),
            KeyboardFocusTarget::Element(w) => KeyboardTarget::enter(w, seat, data, keys, serial),
            KeyboardFocusTarget::Group(_) => {}
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        match self {
            KeyboardFocusTarget::WlSurface(s) => KeyboardTarget::leave(s, seat, data, serial),
            KeyboardFocusTarget::Element(w) => KeyboardTarget::leave(w, seat, data, serial),
            KeyboardFocusTarget::Group(_) => {}
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
            KeyboardFocusTarget::WlSurface(s) => {
                KeyboardTarget::key(s, seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Element(w) => {
                KeyboardTarget::key(w, seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Group(_) => {}
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
            KeyboardFocusTarget::WlSurface(s) => {
                KeyboardTarget::modifiers(s, seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Element(w) => {
                KeyboardTarget::modifiers(w, seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Group(_) => {}
        }
    }
}

impl WaylandFocus for KeyboardFocusTarget {
    fn wl_surface(&self) -> Option<WlSurface> {
        match self {
            KeyboardFocusTarget::WlSurface(s) => Some(s.clone()),
            KeyboardFocusTarget::Element(w) => WaylandFocus::wl_surface(w),
            KeyboardFocusTarget::Group(_) => None,
        }
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            KeyboardFocusTarget::WlSurface(s) => s.id().same_client_as(object_id),
            KeyboardFocusTarget::Element(w) => WaylandFocus::same_client_as(w, object_id),
            KeyboardFocusTarget::Group(_) => false,
        }
    }
}

impl WaylandFocus for PointerFocusTarget {
    fn wl_surface(&self) -> Option<WlSurface> {
        Some(match self {
            PointerFocusTarget::WlSurface(s) => s.clone(),
            PointerFocusTarget::Element(w) => WaylandFocus::wl_surface(w)?,
            PointerFocusTarget::X11(s) => {
                return s.wl_surface();
            }
            PointerFocusTarget::ResizeFork(_) => {
                return None;
            }
        })
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            PointerFocusTarget::WlSurface(s) => s.id().same_client_as(object_id),
            PointerFocusTarget::Element(w) => WaylandFocus::same_client_as(w, object_id),
            PointerFocusTarget::X11(s) => WaylandFocus::same_client_as(s, object_id),
            PointerFocusTarget::ResizeFork(_) => false,
        }
    }
}

impl From<CosmicMapped> for PointerFocusTarget {
    fn from(w: CosmicMapped) -> Self {
        PointerFocusTarget::Element(w)
    }
}

impl From<WlSurface> for PointerFocusTarget {
    fn from(s: WlSurface) -> Self {
        PointerFocusTarget::WlSurface(s)
    }
}

impl From<PopupKind> for PointerFocusTarget {
    fn from(p: PopupKind) -> Self {
        PointerFocusTarget::WlSurface(p.wl_surface().clone())
    }
}

impl From<X11Surface> for PointerFocusTarget {
    fn from(s: X11Surface) -> Self {
        PointerFocusTarget::X11(s)
    }
}

impl From<ResizeForkTarget> for PointerFocusTarget {
    fn from(f: ResizeForkTarget) -> Self {
        PointerFocusTarget::ResizeFork(f)
    }
}

impl From<LockSurface> for PointerFocusTarget {
    fn from(l: LockSurface) -> Self {
        PointerFocusTarget::WlSurface(l.wl_surface().clone())
    }
}

impl From<CosmicMapped> for KeyboardFocusTarget {
    fn from(w: CosmicMapped) -> Self {
        KeyboardFocusTarget::Element(w)
    }
}

impl TryFrom<CosmicSurface> for KeyboardFocusTarget {
    type Error = ();

    fn try_from(s: CosmicSurface) -> Result<Self, ()> {
        Ok(KeyboardFocusTarget::WlSurface(s.wl_surface().ok_or(())?))
    }
}

impl From<WindowGroup> for KeyboardFocusTarget {
    fn from(w: WindowGroup) -> Self {
        KeyboardFocusTarget::Group(w)
    }
}

impl From<LayerSurface> for KeyboardFocusTarget {
    fn from(l: LayerSurface) -> Self {
        KeyboardFocusTarget::WlSurface(l.wl_surface().clone())
    }
}

impl From<PopupKind> for KeyboardFocusTarget {
    fn from(p: PopupKind) -> Self {
        KeyboardFocusTarget::WlSurface(p.wl_surface().clone())
    }
}

impl From<LockSurface> for KeyboardFocusTarget {
    fn from(l: LockSurface) -> Self {
        KeyboardFocusTarget::WlSurface(l.wl_surface().clone())
    }
}
