// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::layout::Orientation, utils::prelude::*};
use atomic_float::AtomicF64;
use smithay::{
    input::pointer::{
        AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab,
        PointerInnerHandle,
    },
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Point, Size},
};
use std::sync::{atomic::Ordering, Arc};

pub struct ResizeForkGrab {
    pub start_data: PointerGrabStartData<State>,
    pub orientation: Orientation,
    pub initial_size: Size<i32, Logical>,
    pub initial_ratio: f64,
    pub ratio: Arc<AtomicF64>,
}

impl PointerGrab<State> for ResizeForkGrab {
    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(WlSurface, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(data, None, event);

        let delta = event.location - self.start_data.location;
        let delta = match self.orientation {
            Orientation::Vertical => delta.x / self.initial_size.w as f64,
            Orientation::Horizontal => delta.y / self.initial_size.h as f64,
        };
        self.ratio.store(
            0.9f64.min(0.1f64.max(self.initial_ratio + delta)),
            Ordering::SeqCst,
        );
    }

    fn button(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(data, event);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(data, event.serial, event.time);
        }
    }

    fn axis(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(data, details)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }
}
