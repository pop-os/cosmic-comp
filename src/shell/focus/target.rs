use std::sync::Weak;

use crate::{
    shell::{
        element::{CosmicMapped, CosmicStack, CosmicWindow},
        layout::tiling::ResizeForkTarget,
        CosmicSurface,
    },
    utils::prelude::*,
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
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
};

// discuss: should contain WlSurface, IcedElement
#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusTarget {
    WlSurface(WlSurface),
    StackUI(CosmicStack),
    WindowUI(CosmicWindow),
    ResizeFork(ResizeForkTarget),
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyboardFocusTarget {
    Element(CosmicMapped),
    Fullscreen(CosmicSurface),
    Group(WindowGroup),
    LayerSurface(LayerSurface),
    Popup(PopupKind),
    LockSurface(LockSurface),
}

// TODO: This should be TryFrom, but PopupGrab needs to be able to convert. Fix this in smithay
impl From<KeyboardFocusTarget> for PointerFocusTarget {
    fn from(target: KeyboardFocusTarget) -> Self {
        match target {
            KeyboardFocusTarget::Element(elem) => {
                PointerFocusTarget::WlSurface(elem.active_window().wl_surface().unwrap())
            }
            KeyboardFocusTarget::Fullscreen(elem) => {
                PointerFocusTarget::WlSurface(elem.wl_surface().unwrap())
            }
            KeyboardFocusTarget::LayerSurface(layer) => {
                PointerFocusTarget::WlSurface(layer.wl_surface().clone())
            }
            KeyboardFocusTarget::Popup(popup) => {
                PointerFocusTarget::WlSurface(popup.wl_surface().clone())
            }
            KeyboardFocusTarget::LockSurface(lock) => {
                PointerFocusTarget::WlSurface(lock.wl_surface().clone())
            }
            _ => unreachable!("A group cannot start a popup grab"),
        }
    }
}

/*
impl TryFrom<PointerFocusTarget> for KeyboardFocusTarget {
    type Error = ();
    fn try_from(target: PointerFocusTarget) -> Result<Self, Self::Error> {
        match target {
            PointerFocusTarget::Element(mapped) => Ok(KeyboardFocusTarget::Element(mapped)),
            PointerFocusTarget::Fullscreen(surf) => Ok(KeyboardFocusTarget::Fullscreen(surf)),
            PointerFocusTarget::LayerSurface(layer) => Ok(KeyboardFocusTarget::LayerSurface(layer)),
            PointerFocusTarget::Popup(popup) => Ok(KeyboardFocusTarget::Popup(popup)),
            PointerFocusTarget::LockSurface(lock) => Ok(KeyboardFocusTarget::LockSurface(lock)),
            _ => Err(()),
        }
    }
}
*/

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
            WindowSurface::X11(surface) => {
                Some((Self::WlSurface(surface.wl_surface()?), Point::default()))
            }
        }
    }
}

impl KeyboardFocusTarget {
    pub fn toplevel(&self) -> Option<WlSurface> {
        match self {
            KeyboardFocusTarget::Element(mapped) => mapped.wl_surface(),
            KeyboardFocusTarget::Popup(PopupKind::Xdg(xdg)) => get_popup_toplevel(&xdg),
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
            PointerFocusTarget::StackUI(e) => e.alive(),
            PointerFocusTarget::WindowUI(e) => e.alive(),
            PointerFocusTarget::ResizeFork(f) => f.alive(),
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
        }
    }
}

impl PointerTarget<State> for PointerFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::enter(s, seat, data, event),
            PointerFocusTarget::StackUI(u) => PointerTarget::enter(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::enter(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::enter(f, seat, data, event),
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::motion(s, seat, data, event),
            PointerFocusTarget::StackUI(u) => PointerTarget::motion(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::motion(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::motion(f, seat, data, event),
        }
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => {
                PointerTarget::relative_motion(s, seat, data, event)
            }
            PointerFocusTarget::StackUI(u) => PointerTarget::relative_motion(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::relative_motion(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::relative_motion(f, seat, data, event)
            }
        }
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::button(s, seat, data, event),
            PointerFocusTarget::StackUI(u) => PointerTarget::button(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => PointerTarget::button(u, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::button(f, seat, data, event),
        }
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::axis(s, seat, data, frame),
            PointerFocusTarget::StackUI(u) => PointerTarget::axis(u, seat, data, frame),
            PointerFocusTarget::WindowUI(u) => PointerTarget::axis(u, seat, data, frame),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::axis(f, seat, data, frame),
        }
    }
    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::frame(s, seat, data),
            PointerFocusTarget::StackUI(u) => PointerTarget::frame(u, seat, data),
            PointerFocusTarget::WindowUI(u) => PointerTarget::frame(u, seat, data),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::frame(f, seat, data),
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        match self {
            PointerFocusTarget::WlSurface(s) => PointerTarget::leave(s, seat, data, serial, time),
            PointerFocusTarget::StackUI(u) => PointerTarget::leave(u, seat, data, serial, time),
            PointerFocusTarget::WindowUI(u) => PointerTarget::leave(u, seat, data, serial, time),
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_begin(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_update(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_update(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_swipe_end(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_swipe_end(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_begin(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_update(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_update(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_pinch_end(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_pinch_end(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => {
                PointerTarget::gesture_hold_begin(u, seat, data, event)
            }
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_hold_begin(u, seat, data, event)
            }
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
            PointerFocusTarget::StackUI(u) => PointerTarget::gesture_hold_end(u, seat, data, event),
            PointerFocusTarget::WindowUI(u) => {
                PointerTarget::gesture_hold_end(u, seat, data, event)
            }
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
            KeyboardFocusTarget::Element(w) => KeyboardTarget::enter(w, seat, data, keys, serial),
            KeyboardFocusTarget::Fullscreen(w) => {
                KeyboardTarget::enter(w, seat, data, keys, serial)
            }
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => {
                KeyboardTarget::enter(l, seat, data, keys, serial)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::enter(p.wl_surface(), seat, data, keys, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::enter(l.wl_surface(), seat, data, keys, serial)
            }
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        match self {
            KeyboardFocusTarget::Element(w) => KeyboardTarget::leave(w, seat, data, serial),
            KeyboardFocusTarget::Fullscreen(w) => KeyboardTarget::leave(w, seat, data, serial),
            KeyboardFocusTarget::Group(_) => {}
            KeyboardFocusTarget::LayerSurface(l) => KeyboardTarget::leave(l, seat, data, serial),
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::leave(p.wl_surface(), seat, data, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::leave(l.wl_surface(), seat, data, serial)
            }
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
                KeyboardTarget::key(l, seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::key(p.wl_surface(), seat, data, key, state, serial, time)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::key(l.wl_surface(), seat, data, key, state, serial, time)
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
                KeyboardTarget::modifiers(l, seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::Popup(p) => {
                KeyboardTarget::modifiers(p.wl_surface(), seat, data, modifiers, serial)
            }
            KeyboardFocusTarget::LockSurface(l) => {
                KeyboardTarget::modifiers(l.wl_surface(), seat, data, modifiers, serial)
            }
        }
    }
}

impl WaylandFocus for KeyboardFocusTarget {
    fn wl_surface(&self) -> Option<WlSurface> {
        match self {
            KeyboardFocusTarget::Element(w) => WaylandFocus::wl_surface(w),
            KeyboardFocusTarget::Fullscreen(w) => WaylandFocus::wl_surface(w),
            KeyboardFocusTarget::Group(_) => None,
            KeyboardFocusTarget::LayerSurface(l) => Some(l.wl_surface().clone()),
            KeyboardFocusTarget::Popup(p) => Some(p.wl_surface().clone()),
            KeyboardFocusTarget::LockSurface(l) => Some(l.wl_surface().clone()),
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
        }
    }
}

impl WaylandFocus for PointerFocusTarget {
    fn wl_surface(&self) -> Option<WlSurface> {
        Some(match self {
            PointerFocusTarget::WlSurface(s) => s.clone(),
            PointerFocusTarget::ResizeFork(_)
            | PointerFocusTarget::StackUI(_)
            | PointerFocusTarget::WindowUI(_) => {
                return None;
            }
        })
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            PointerFocusTarget::WlSurface(s) => s.id().same_client_as(object_id),
            PointerFocusTarget::StackUI(stack) => stack
                .active()
                .wl_surface()
                .map(|s| s.id().same_client_as(object_id))
                .unwrap_or(false),
            PointerFocusTarget::WindowUI(window) => window
                .wl_surface()
                .map(|s| s.id().same_client_as(object_id))
                .unwrap_or(false),
            PointerFocusTarget::ResizeFork(_) => false,
        }
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
