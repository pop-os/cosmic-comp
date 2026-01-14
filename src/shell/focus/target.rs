use std::{
    borrow::Cow,
    sync::{Arc, Weak},
    time::Duration,
};

use crate::{
    shell::{
        CosmicSurface, SeatExt,
        element::{CosmicMapped, CosmicStack, CosmicWindow},
        layout::tiling::ResizeForkTarget,
        zoom::ZoomFocusTarget,
    },
    utils::prelude::*,
    wayland::handlers::{image_copy_capture::SessionHolder, xdg_shell::popup::get_popup_toplevel},
};
use id_tree::NodeId;
use smithay::{
    backend::input::KeyState,
    desktop::{LayerSurface, PopupKind, WindowSurface, WindowSurfaceType, space::SpaceElement},
    input::{
        Seat,
        dnd::{DndFocus, OfferData, Source},
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
    },
    reexports::wayland_server::{
        Client, DisplayHandle, Resource, backend::ObjectId, protocol::wl_surface::WlSurface,
    },
    utils::{IsAlive, Logical, Point, Serial, Transform},
    wayland::{seat::WaylandFocus, selection::data_device::WlOfferData, session_lock::LockSurface},
    xwayland::{
        X11Surface,
        xwm::{XwmId, XwmOfferData},
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum PointerFocusTarget {
    WlSurface {
        surface: WlSurface,
        toplevel: Option<PointerFocusToplevel>,
    },
    X11Surface {
        surface: X11Surface,
        toplevel: Option<CosmicSurface>,
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
}

// TODO: This should be TryFrom, but PopupGrab needs to be able to convert. Fix this in smithay
impl From<KeyboardFocusTarget> for PointerFocusTarget {
    fn from(target: KeyboardFocusTarget) -> Self {
        match target {
            KeyboardFocusTarget::Element(elem) => {
                let window = elem.active_window();

                if let Some(xsurface) = window.x11_surface() {
                    PointerFocusTarget::X11Surface {
                        surface: xsurface.clone(),
                        toplevel: Some(window),
                    }
                } else {
                    PointerFocusTarget::WlSurface {
                        surface: window.wl_surface().unwrap().into_owned(),
                        toplevel: Some(window.into()),
                    }
                }
            }
            KeyboardFocusTarget::Fullscreen(elem) => {
                if let Some(xsurface) = elem.x11_surface() {
                    PointerFocusTarget::X11Surface {
                        surface: xsurface.clone(),
                        toplevel: Some(elem),
                    }
                } else {
                    PointerFocusTarget::WlSurface {
                        surface: elem.wl_surface().unwrap().into_owned(),
                        toplevel: Some(elem.into()),
                    }
                }
            }
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
    fn inner_pointer_target(&self) -> &dyn PointerTarget<State> {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => surface,
            PointerFocusTarget::X11Surface { surface, .. } => surface,
            PointerFocusTarget::StackUI(u) => u,
            PointerFocusTarget::WindowUI(u) => u,
            PointerFocusTarget::ResizeFork(f) => f,
            PointerFocusTarget::ZoomUI(e) => e,
        }
    }

    fn inner_touch_target(&self) -> &dyn TouchTarget<State> {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => surface,
            PointerFocusTarget::X11Surface { surface, .. } => surface,
            PointerFocusTarget::StackUI(u) => u,
            PointerFocusTarget::WindowUI(u) => u,
            PointerFocusTarget::ResizeFork(f) => f,
            PointerFocusTarget::ZoomUI(e) => e,
        }
    }

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
                Self::X11Surface {
                    surface: x11_surface.clone(),
                    toplevel: Some(surface.clone()),
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
            PointerFocusTarget::X11Surface {
                toplevel: Some(surface),
                ..
            } => Some(surface.clone()),
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
            PointerFocusTarget::X11Surface { surface, .. } => surface
                .wl_surface()
                .is_some_and(|s| s.client().is_some_and(|c| c == *client)),
            _ => false,
        }
    }
}

impl KeyboardFocusTarget {
    fn inner_keyboard_target(&self) -> Option<&dyn KeyboardTarget<State>> {
        match self {
            KeyboardFocusTarget::Element(w) => Some(w),
            KeyboardFocusTarget::Fullscreen(w) => Some(w),
            KeyboardFocusTarget::Group(_) => None,
            KeyboardFocusTarget::LayerSurface(l) => Some(l.wl_surface()),
            KeyboardFocusTarget::Popup(p) => Some(p.wl_surface()),
            KeyboardFocusTarget::LockSurface(l) => Some(l.wl_surface()),
        }
    }

    pub fn toplevel(&self) -> Option<Cow<'_, WlSurface>> {
        match self {
            KeyboardFocusTarget::Element(mapped) => mapped.wl_surface(),
            KeyboardFocusTarget::Popup(PopupKind::Xdg(xdg)) => {
                get_popup_toplevel(xdg).map(Cow::Owned)
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

    pub fn active_window(&self) -> Option<CosmicSurface> {
        match self {
            KeyboardFocusTarget::Element(mapped) => Some(mapped.active_window()),
            KeyboardFocusTarget::Fullscreen(surface) => Some(surface.clone()),
            _ => None,
        }
    }

    fn x11_surface(&self) -> Option<X11Surface> {
        match self {
            KeyboardFocusTarget::Element(mapped) => mapped.active_window().x11_surface().cloned(),
            KeyboardFocusTarget::Fullscreen(surface) => surface.x11_surface().cloned(),
            _ => None,
        }
    }

    pub fn is_xwm(&self, xwm: XwmId) -> bool {
        if let Some(surface) = self.x11_surface() {
            surface.xwm_id().unwrap() == xwm
        } else {
            false
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
            PointerFocusTarget::WlSurface { surface, .. } => surface.alive(),
            PointerFocusTarget::X11Surface { surface, .. } => surface.alive(),
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
        }
    }
}

impl PointerTarget<State> for PointerFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let toplevel = self.toplevel(&data.common.shell.read());
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

        self.inner_pointer_target().enter(seat, data, event);
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        let toplevel = self.toplevel(&data.common.shell.read());
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

        self.inner_pointer_target().motion(seat, data, event);
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        self.inner_pointer_target()
            .relative_motion(seat, data, event);
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        self.inner_pointer_target().button(seat, data, event);
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        self.inner_pointer_target().axis(seat, data, frame);
    }
    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        self.inner_pointer_target().frame(seat, data);
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        let toplevel = self.toplevel(&data.common.shell.read());
        if let Some(element) = toplevel {
            for session in element.cursor_sessions() {
                session.set_cursor_pos(None);
                session.set_cursor_hotspot((0, 0));
            }
        }

        self.inner_pointer_target().leave(seat, data, serial, time);
    }
    fn gesture_swipe_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeBeginEvent,
    ) {
        self.inner_pointer_target()
            .gesture_swipe_begin(seat, data, event);
    }
    fn gesture_swipe_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeUpdateEvent,
    ) {
        self.inner_pointer_target()
            .gesture_swipe_update(seat, data, event);
    }
    fn gesture_swipe_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeEndEvent,
    ) {
        self.inner_pointer_target()
            .gesture_swipe_end(seat, data, event);
    }
    fn gesture_pinch_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchBeginEvent,
    ) {
        self.inner_pointer_target()
            .gesture_pinch_begin(seat, data, event);
    }
    fn gesture_pinch_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchUpdateEvent,
    ) {
        self.inner_pointer_target()
            .gesture_pinch_update(seat, data, event);
    }
    fn gesture_pinch_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchEndEvent,
    ) {
        self.inner_pointer_target()
            .gesture_pinch_end(seat, data, event);
    }
    fn gesture_hold_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureHoldBeginEvent,
    ) {
        self.inner_pointer_target()
            .gesture_hold_begin(seat, data, event);
    }
    fn gesture_hold_end(&self, seat: &Seat<State>, data: &mut State, event: &GestureHoldEndEvent) {
        self.inner_pointer_target()
            .gesture_hold_end(seat, data, event);
    }
}

impl TouchTarget<State> for PointerFocusTarget {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, seq: Serial) {
        self.inner_touch_target().down(seat, data, event, seq);
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent, seq: Serial) {
        self.inner_touch_target().up(seat, data, event, seq);
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent, seq: Serial) {
        self.inner_touch_target().motion(seat, data, event, seq);
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        self.inner_touch_target().frame(seat, data, seq);
    }

    fn cancel(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        self.inner_touch_target().cancel(seat, data, seq);
    }

    fn shape(&self, seat: &Seat<State>, data: &mut State, event: &ShapeEvent, seq: Serial) {
        self.inner_touch_target().shape(seat, data, event, seq);
    }

    fn orientation(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &OrientationEvent,
        seq: Serial,
    ) {
        self.inner_touch_target()
            .orientation(seat, data, event, seq);
    }
}

pub enum CosmicOfferData<S: Source> {
    Wl(WlOfferData<S>),
    X11(XwmOfferData<S>),
}

impl<S: Source> OfferData for CosmicOfferData<S> {
    fn disable(&self) {
        match self {
            CosmicOfferData::Wl(data) => data.disable(),
            CosmicOfferData::X11(data) => data.disable(),
        }
    }

    fn drop(&self) {
        match self {
            CosmicOfferData::Wl(data) => data.drop(),
            CosmicOfferData::X11(data) => data.drop(),
        }
    }

    fn validated(&self) -> bool {
        match self {
            CosmicOfferData::Wl(data) => data.validated(),
            CosmicOfferData::X11(data) => data.validated(),
        }
    }
}

impl DndFocus<State> for PointerFocusTarget {
    type OfferData<S>
        = CosmicOfferData<S>
    where
        S: Source;

    fn enter<S: Source>(
        &self,
        data: &mut State,
        dh: &DisplayHandle,
        source: Arc<S>,
        seat: &Seat<State>,
        location: Point<f64, Logical>,
        serial: &Serial,
    ) -> Option<CosmicOfferData<S>> {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                DndFocus::enter(surface, data, dh, source, seat, location, serial)
                    .map(CosmicOfferData::Wl)
            }
            PointerFocusTarget::X11Surface { surface, .. } => {
                DndFocus::enter(surface, data, dh, source, seat, location, serial)
                    .map(CosmicOfferData::X11)
            }
            _ => None,
        }
    }

    fn motion<S: Source>(
        &self,
        data: &mut State,
        offer: Option<&mut CosmicOfferData<S>>,
        seat: &Seat<State>,
        location: Point<f64, Logical>,
        time: u32,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::Wl(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::motion(surface, data, offer, seat, location, time)
            }
            PointerFocusTarget::X11Surface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::X11(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::motion(surface, data, offer, seat, location, time)
            }
            _ => {}
        }
    }

    fn leave<S: Source>(
        &self,
        data: &mut State,
        offer: Option<&mut CosmicOfferData<S>>,
        seat: &Seat<State>,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::Wl(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::leave(surface, data, offer, seat)
            }
            PointerFocusTarget::X11Surface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::X11(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::leave(surface, data, offer, seat)
            }
            _ => {}
        }
    }

    fn drop<S: Source>(
        &self,
        data: &mut State,
        offer: Option<&mut CosmicOfferData<S>>,
        seat: &Seat<State>,
    ) {
        match self {
            PointerFocusTarget::WlSurface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::Wl(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::drop(surface, data, offer, seat)
            }
            PointerFocusTarget::X11Surface { surface, .. } => {
                let offer = match offer {
                    Some(CosmicOfferData::X11(offer)) => Some(offer),
                    None => None,
                    _ => return,
                };
                DndFocus::drop(surface, data, offer, seat)
            }
            _ => {}
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
        if let Some(inner) = self.inner_keyboard_target() {
            inner.enter(seat, data, keys, serial);
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        if let Some(inner) = self.inner_keyboard_target() {
            inner.leave(seat, data, serial);
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
        if let Some(inner) = self.inner_keyboard_target() {
            inner.key(seat, data, key, state, serial, time);
        }
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        if let Some(inner) = self.inner_keyboard_target() {
            inner.modifiers(seat, data, modifiers, serial);
        }
    }
    fn replace(
        &self,
        replaced: Self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        if !replaced
            .wl_surface()
            .is_some_and(|s| Some(s) == self.wl_surface())
            && !replaced
                .x11_surface()
                .is_some_and(|s| Some(s) == self.x11_surface())
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
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        Some(match self {
            PointerFocusTarget::WlSurface { surface, .. } => Cow::Borrowed(surface),
            PointerFocusTarget::X11Surface { surface, .. } => Cow::Owned(surface.wl_surface()?),
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
            PointerFocusTarget::X11Surface { surface, .. } => surface
                .wl_surface()
                .is_some_and(|s| s.id().same_client_as(object_id)),
            PointerFocusTarget::StackUI(stack) => stack
                .active()
                .wl_surface()
                .is_some_and(|s| s.id().same_client_as(object_id)),
            PointerFocusTarget::WindowUI(window) => window
                .wl_surface()
                .is_some_and(|s| s.id().same_client_as(object_id)),
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
