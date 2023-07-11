use std::sync::Weak;

use crate::{
    shell::{element::CosmicMapped, layout::tiling::ResizeForkTarget, CosmicSurface},
    utils::prelude::*,
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
};
use id_tree::NodeId;
use smithay::{
    backend::input::KeyState,
    desktop::{LayerSurface, PopupKind},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget, RelativeMotionEvent},
        Seat,
    },
    output::WeakOutput,
    reexports::wayland_server::{backend::ObjectId, protocol::wl_surface::WlSurface, Resource},
    utils::{IsAlive, Serial},
    wayland::seat::WaylandFocus,
    xwayland::X11Surface,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusTarget {
    Element(CosmicMapped),
    Fullscreen(CosmicSurface),
    LayerSurface(LayerSurface),
    Popup(PopupKind),
    OverrideRedirect(X11Surface),
    ResizeFork(ResizeForkTarget),
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyboardFocusTarget {
    Element(CosmicMapped),
    Fullscreen(CosmicSurface),
    Group(WindowGroup),
    LayerSurface(LayerSurface),
    Popup(PopupKind),
}

// TODO: This should be TryFrom, but PopupGrab needs to be able to convert. Fix this in smithay
impl From<KeyboardFocusTarget> for PointerFocusTarget {
    fn from(target: KeyboardFocusTarget) -> Self {
        match target {
            KeyboardFocusTarget::Element(elem) => PointerFocusTarget::Element(elem),
            KeyboardFocusTarget::Fullscreen(elem) => PointerFocusTarget::Fullscreen(elem),
            KeyboardFocusTarget::LayerSurface(layer) => PointerFocusTarget::LayerSurface(layer),
            KeyboardFocusTarget::Popup(popup) => PointerFocusTarget::Popup(popup),
            _ => unreachable!("A window grab cannot start a popup grab"),
        }
    }
}

impl TryFrom<PointerFocusTarget> for KeyboardFocusTarget {
    type Error = ();
    fn try_from(target: PointerFocusTarget) -> Result<Self, Self::Error> {
        match target {
            PointerFocusTarget::Element(mapped) => Ok(KeyboardFocusTarget::Element(mapped)),
            PointerFocusTarget::Fullscreen(surf) => Ok(KeyboardFocusTarget::Fullscreen(surf)),
            PointerFocusTarget::LayerSurface(layer) => Ok(KeyboardFocusTarget::LayerSurface(layer)),
            PointerFocusTarget::Popup(popup) => Ok(KeyboardFocusTarget::Popup(popup)),
            _ => Err(()),
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
    pub output: WeakOutput,
    pub alive: Weak<()>,
    pub focus_stack: Vec<NodeId>,
}

impl PartialEq for WindowGroup {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
            && self.output == other.output
            && Weak::ptr_eq(&self.alive, &other.alive)
    }
}

impl IsAlive for PointerFocusTarget {
    fn alive(&self) -> bool {
        match self {
            PointerFocusTarget::Element(e) => e.alive(),
            PointerFocusTarget::Fullscreen(f) => f.alive(),
            PointerFocusTarget::LayerSurface(l) => l.alive(),
            PointerFocusTarget::Popup(p) => p.alive(),
            PointerFocusTarget::OverrideRedirect(s) => s.alive(),
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
        }
    }
}

impl PointerTarget<State> for PointerFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::enter(w, seat, data, event),
            PointerFocusTarget::Fullscreen(w) => PointerTarget::enter(w, seat, data, event),
            PointerFocusTarget::LayerSurface(l) => PointerTarget::enter(l, seat, data, event),
            PointerFocusTarget::Popup(p) => PointerTarget::enter(p.wl_surface(), seat, data, event),
            PointerFocusTarget::OverrideRedirect(s) => PointerTarget::enter(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::enter(f, seat, data, event),
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::motion(w, seat, data, event),
            PointerFocusTarget::Fullscreen(w) => PointerTarget::motion(w, seat, data, event),
            PointerFocusTarget::LayerSurface(l) => PointerTarget::motion(l, seat, data, event),
            PointerFocusTarget::Popup(p) => {
                PointerTarget::motion(p.wl_surface(), seat, data, event)
            }
            PointerFocusTarget::OverrideRedirect(s) => PointerTarget::motion(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::motion(f, seat, data, event),
        }
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::relative_motion(w, seat, data, event),
            PointerFocusTarget::Fullscreen(w) => {
                PointerTarget::relative_motion(w, seat, data, event)
            }
            PointerFocusTarget::LayerSurface(l) => {
                PointerTarget::relative_motion(l, seat, data, event)
            }
            PointerFocusTarget::Popup(p) => {
                PointerTarget::relative_motion(p.wl_surface(), seat, data, event)
            }
            PointerFocusTarget::OverrideRedirect(s) => {
                PointerTarget::relative_motion(s, seat, data, event)
            }
            PointerFocusTarget::ResizeFork(f) => {
                PointerTarget::relative_motion(f, seat, data, event)
            }
        }
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::button(w, seat, data, event),
            PointerFocusTarget::Fullscreen(w) => PointerTarget::button(w, seat, data, event),
            PointerFocusTarget::LayerSurface(l) => PointerTarget::button(l, seat, data, event),
            PointerFocusTarget::Popup(p) => {
                PointerTarget::button(p.wl_surface(), seat, data, event)
            }
            PointerFocusTarget::OverrideRedirect(s) => PointerTarget::button(s, seat, data, event),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::button(f, seat, data, event),
        }
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::axis(w, seat, data, frame),
            PointerFocusTarget::Fullscreen(w) => PointerTarget::axis(w, seat, data, frame),
            PointerFocusTarget::LayerSurface(l) => PointerTarget::axis(l, seat, data, frame),
            PointerFocusTarget::Popup(p) => PointerTarget::axis(p.wl_surface(), seat, data, frame),
            PointerFocusTarget::OverrideRedirect(s) => PointerTarget::axis(s, seat, data, frame),
            PointerFocusTarget::ResizeFork(f) => PointerTarget::axis(f, seat, data, frame),
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        match self {
            PointerFocusTarget::Element(w) => PointerTarget::leave(w, seat, data, serial, time),
            PointerFocusTarget::Fullscreen(w) => PointerTarget::leave(w, seat, data, serial, time),
            PointerFocusTarget::LayerSurface(l) => {
                PointerTarget::leave(l, seat, data, serial, time)
            }
            PointerFocusTarget::Popup(p) => {
                PointerTarget::leave(p.wl_surface(), seat, data, serial, time)
            }
            PointerFocusTarget::OverrideRedirect(s) => {
                PointerTarget::leave(s, seat, data, serial, time)
            }
            PointerFocusTarget::ResizeFork(f) => PointerTarget::leave(f, seat, data, serial, time),
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
        }
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            KeyboardFocusTarget::Element(w) => WaylandFocus::same_client_as(w, object_id),
            KeyboardFocusTarget::Fullscreen(w) => WaylandFocus::same_client_as(w, object_id),
            KeyboardFocusTarget::Group(_) => false,
            KeyboardFocusTarget::LayerSurface(l) => l.wl_surface().id().same_client_as(object_id),
            KeyboardFocusTarget::Popup(p) => p.wl_surface().id().same_client_as(object_id),
        }
    }
}

impl WaylandFocus for PointerFocusTarget {
    fn wl_surface(&self) -> Option<WlSurface> {
        Some(match self {
            PointerFocusTarget::Element(w) => WaylandFocus::wl_surface(w)?,
            PointerFocusTarget::Fullscreen(w) => WaylandFocus::wl_surface(w)?,
            PointerFocusTarget::LayerSurface(l) => l.wl_surface().clone(),
            PointerFocusTarget::Popup(p) => p.wl_surface().clone(),
            PointerFocusTarget::OverrideRedirect(s) => {
                return s.wl_surface();
            }
            PointerFocusTarget::ResizeFork(_) => {
                return None;
            }
        })
    }
    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match self {
            PointerFocusTarget::Element(w) => WaylandFocus::same_client_as(w, object_id),
            PointerFocusTarget::Fullscreen(w) => WaylandFocus::same_client_as(w, object_id),
            PointerFocusTarget::LayerSurface(l) => l.wl_surface().id().same_client_as(object_id),
            PointerFocusTarget::Popup(p) => p.wl_surface().id().same_client_as(object_id),
            PointerFocusTarget::OverrideRedirect(s) => WaylandFocus::same_client_as(s, object_id),
            PointerFocusTarget::ResizeFork(_) => false,
        }
    }
}

impl From<CosmicMapped> for PointerFocusTarget {
    fn from(w: CosmicMapped) -> Self {
        PointerFocusTarget::Element(w)
    }
}

impl From<CosmicSurface> for PointerFocusTarget {
    fn from(s: CosmicSurface) -> Self {
        PointerFocusTarget::Fullscreen(s)
    }
}

impl From<LayerSurface> for PointerFocusTarget {
    fn from(l: LayerSurface) -> Self {
        PointerFocusTarget::LayerSurface(l)
    }
}

impl From<PopupKind> for PointerFocusTarget {
    fn from(p: PopupKind) -> Self {
        PointerFocusTarget::Popup(p)
    }
}

impl From<X11Surface> for PointerFocusTarget {
    fn from(s: X11Surface) -> Self {
        PointerFocusTarget::OverrideRedirect(s)
    }
}

impl From<ResizeForkTarget> for PointerFocusTarget {
    fn from(f: ResizeForkTarget) -> Self {
        PointerFocusTarget::ResizeFork(f)
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
