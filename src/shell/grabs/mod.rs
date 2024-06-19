use smithay::{
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        touch::{
            DownEvent, GrabStartData as TouchGrabStartData, MotionEvent as TouchMotionEvent,
            OrientationEvent, ShapeEvent, TouchGrab, TouchInnerHandle, UpEvent,
        },
    },
    reexports::wayland_protocols::xdg::shell::server::xdg_toplevel,
    utils::{Logical, Point, Serial},
    xwayland::xwm,
};

use crate::state::State;

use super::{
    focus::target::PointerFocusTarget,
    layout::{floating::ResizeSurfaceGrab, tiling::ResizeForkGrab},
};

#[derive(Debug, Clone)]
pub enum GrabStartData {
    Touch(TouchGrabStartData<State>),
    Pointer(PointerGrabStartData<State>),
}

impl GrabStartData {
    pub fn focus(&self) -> Option<&(PointerFocusTarget, Point<f64, Logical>)> {
        match self {
            Self::Touch(touch) => touch.focus.as_ref(),
            Self::Pointer(pointer) => pointer.focus.as_ref(),
        }
    }

    pub fn set_focus(&mut self, focus: Option<(PointerFocusTarget, Point<f64, Logical>)>) {
        match self {
            Self::Touch(touch) => touch.focus = focus,
            Self::Pointer(pointer) => pointer.focus = focus,
        }
    }

    pub fn location(&self) -> Point<f64, Logical> {
        match self {
            Self::Touch(touch) => touch.location,
            Self::Pointer(pointer) => pointer.location,
        }
    }

    pub fn set_location(&mut self, location: Point<f64, Logical>) {
        match self {
            Self::Touch(touch) => touch.location = location,
            Self::Pointer(pointer) => pointer.location = location,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReleaseMode {
    Click,
    NoMouseButtons,
}

mod menu;
pub use self::menu::*;
mod moving;
pub use self::moving::*;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ResizeEdge: u32 {
        const TOP          = 0b0001;
        const BOTTOM       = 0b0010;
        const LEFT         = 0b0100;
        const RIGHT        = 0b1000;

        const TOP_LEFT     = Self::TOP.bits() | Self::LEFT.bits();
        const BOTTOM_LEFT  = Self::BOTTOM.bits() | Self::LEFT.bits();

        const TOP_RIGHT    = Self::TOP.bits() | Self::RIGHT.bits();
        const BOTTOM_RIGHT = Self::BOTTOM.bits() | Self::RIGHT.bits();
    }
}

impl ResizeEdge {
    pub fn flip_direction(&mut self) {
        let mut new_edge = ResizeEdge::empty();
        if self.contains(ResizeEdge::TOP) {
            new_edge.insert(ResizeEdge::BOTTOM);
        }
        if self.contains(ResizeEdge::BOTTOM) {
            new_edge.insert(ResizeEdge::TOP);
        }
        if self.contains(ResizeEdge::LEFT) {
            new_edge.insert(ResizeEdge::RIGHT);
        }
        if self.contains(ResizeEdge::RIGHT) {
            new_edge.insert(ResizeEdge::LEFT);
        }
        *self = new_edge;
    }
}

impl From<xdg_toplevel::ResizeEdge> for ResizeEdge {
    #[inline]
    fn from(x: xdg_toplevel::ResizeEdge) -> Self {
        Self::from_bits(x.into()).unwrap()
    }
}

impl From<ResizeEdge> for xdg_toplevel::ResizeEdge {
    #[inline]
    fn from(x: ResizeEdge) -> Self {
        Self::try_from(x.bits()).unwrap()
    }
}

impl From<xwm::ResizeEdge> for ResizeEdge {
    #[inline]
    fn from(x: xwm::ResizeEdge) -> Self {
        match x {
            xwm::ResizeEdge::Top => ResizeEdge::TOP,
            xwm::ResizeEdge::Bottom => ResizeEdge::BOTTOM,
            xwm::ResizeEdge::Left => ResizeEdge::LEFT,
            xwm::ResizeEdge::Right => ResizeEdge::RIGHT,
            xwm::ResizeEdge::TopLeft => ResizeEdge::TOP_LEFT,
            xwm::ResizeEdge::BottomLeft => ResizeEdge::BOTTOM_LEFT,
            xwm::ResizeEdge::TopRight => ResizeEdge::TOP_RIGHT,
            xwm::ResizeEdge::BottomRight => ResizeEdge::BOTTOM_RIGHT,
        }
    }
}

pub enum ResizeGrab {
    Floating(ResizeSurfaceGrab),
    Tiling(ResizeForkGrab),
}

impl From<ResizeSurfaceGrab> for ResizeGrab {
    fn from(grab: ResizeSurfaceGrab) -> Self {
        ResizeGrab::Floating(grab)
    }
}

impl From<ResizeForkGrab> for ResizeGrab {
    fn from(grab: ResizeForkGrab) -> Self {
        ResizeGrab::Tiling(grab)
    }
}

impl ResizeGrab {
    pub fn is_touch_grab(&self) -> bool {
        match self {
            ResizeGrab::Floating(grab) => grab.is_touch_grab(),
            ResizeGrab::Tiling(grab) => grab.is_touch_grab(),
        }
    }
}

impl PointerGrab<State> for ResizeGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => PointerGrab::motion(grab, data, handle, focus, event),
            ResizeGrab::Tiling(grab) => PointerGrab::motion(grab, data, handle, focus, event),
        }
    }

    fn relative_motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.relative_motion(data, handle, focus, event),
            ResizeGrab::Tiling(grab) => grab.relative_motion(data, handle, focus, event),
        }
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.button(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.button(data, handle, event),
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.axis(data, handle, details),
            ResizeGrab::Tiling(grab) => grab.axis(data, handle, details),
        }
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        match self {
            ResizeGrab::Floating(grab) => PointerGrab::frame(grab, data, handle),
            ResizeGrab::Tiling(grab) => PointerGrab::frame(grab, data, handle),
        }
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_swipe_begin(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_swipe_begin(data, handle, event),
        }
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_swipe_update(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_swipe_update(data, handle, event),
        }
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_swipe_end(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_swipe_end(data, handle, event),
        }
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_pinch_begin(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_pinch_begin(data, handle, event),
        }
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_pinch_update(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_pinch_update(data, handle, event),
        }
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_pinch_end(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_pinch_end(data, handle, event),
        }
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_hold_begin(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_hold_begin(data, handle, event),
        }
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.gesture_hold_end(data, handle, event),
            ResizeGrab::Tiling(grab) => grab.gesture_hold_end(data, handle, event),
        }
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match self {
            ResizeGrab::Floating(grab) => PointerGrab::start_data(grab),
            ResizeGrab::Tiling(grab) => PointerGrab::start_data(grab),
        }
    }

    fn unset(&mut self, data: &mut State) {
        match self {
            ResizeGrab::Floating(grab) => PointerGrab::unset(grab, data),
            ResizeGrab::Tiling(grab) => PointerGrab::unset(grab, data),
        }
    }
}

impl TouchGrab<State> for ResizeGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &DownEvent,
        seq: Serial,
    ) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::down(grab, data, handle, focus, event, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::down(grab, data, handle, focus, event, seq),
        }
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &UpEvent,
        seq: Serial,
    ) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::up(grab, data, handle, event, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::up(grab, data, handle, event, seq),
        }
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &TouchMotionEvent,
        seq: Serial,
    ) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::motion(grab, data, handle, focus, event, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::motion(grab, data, handle, focus, event, seq),
        }
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::frame(grab, data, handle, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::frame(grab, data, handle, seq),
        }
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::cancel(grab, data, handle, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::cancel(grab, data, handle, seq),
        }
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &ShapeEvent,
        seq: Serial,
    ) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::shape(grab, data, handle, event, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::shape(grab, data, handle, event, seq),
        }
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &OrientationEvent,
        seq: Serial,
    ) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::orientation(grab, data, handle, event, seq),
            ResizeGrab::Tiling(grab) => TouchGrab::orientation(grab, data, handle, event, seq),
        }
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::start_data(grab),
            ResizeGrab::Tiling(grab) => TouchGrab::start_data(grab),
        }
    }

    fn unset(&mut self, data: &mut State) {
        match self {
            ResizeGrab::Floating(grab) => TouchGrab::unset(grab, data),
            ResizeGrab::Tiling(grab) => TouchGrab::unset(grab, data),
        }
    }
}
