use smithay::{
    input::pointer::{
        AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent, GesturePinchBeginEvent,
        GesturePinchEndEvent, GesturePinchUpdateEvent, GestureSwipeBeginEvent,
        GestureSwipeEndEvent, GestureSwipeUpdateEvent, GrabStartData as PointerGrabStartData,
        MotionEvent, PointerGrab, PointerInnerHandle, RelativeMotionEvent,
    },
    reexports::wayland_protocols::xdg::shell::server::xdg_toplevel,
    utils::{Logical, Point},
    xwayland::xwm,
};

use crate::state::State;

use super::{
    focus::target::PointerFocusTarget,
    layout::{floating::ResizeSurfaceGrab, tiling::ResizeForkGrab},
};

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

impl PointerGrab<State> for ResizeGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        match self {
            ResizeGrab::Floating(grab) => grab.motion(data, handle, focus, event),
            ResizeGrab::Tiling(grab) => grab.motion(data, handle, focus, event),
        }
    }

    fn relative_motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
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
            ResizeGrab::Floating(grab) => grab.frame(data, handle),
            ResizeGrab::Tiling(grab) => grab.frame(data, handle),
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
            ResizeGrab::Floating(grab) => grab.start_data(),
            ResizeGrab::Tiling(grab) => grab.start_data(),
        }
    }
}
