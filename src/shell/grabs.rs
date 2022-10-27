use smithay::{
    input::pointer::{
        AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab,
        PointerInnerHandle,
    },
    reexports::wayland_protocols::xdg::shell::server::xdg_toplevel,
    utils::{Logical, Point},
};

use crate::state::State;

use super::{
    focus::target::PointerFocusTarget,
    layout::{floating::ResizeSurfaceGrab, tiling::ResizeForkGrab},
};

bitflags::bitflags! {
    pub struct ResizeEdge: u32 {
        const TOP          = 0b0001;
        const BOTTOM       = 0b0010;
        const LEFT         = 0b0100;
        const RIGHT        = 0b1000;

        const TOP_LEFT     = Self::TOP.bits | Self::LEFT.bits;
        const BOTTOM_LEFT  = Self::BOTTOM.bits | Self::LEFT.bits;

        const TOP_RIGHT    = Self::TOP.bits | Self::RIGHT.bits;
        const BOTTOM_RIGHT = Self::BOTTOM.bits | Self::RIGHT.bits;
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

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match self {
            ResizeGrab::Floating(grab) => grab.start_data(),
            ResizeGrab::Tiling(grab) => grab.start_data(),
        }
    }
}
