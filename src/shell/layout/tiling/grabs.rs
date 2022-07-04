// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::layout::Orientation, utils::prelude::*};
use atomic_float::AtomicF64;
use smithay::{
    reexports::wayland_server::DisplayHandle,
    utils::{Logical, Size},
    wayland::seat::{
        AxisFrame, ButtonEvent, MotionEvent, PointerGrab, PointerGrabStartData, PointerInnerHandle,
    },
};
use std::sync::{atomic::Ordering, Arc};

pub struct ResizeForkGrab {
    pub start_data: PointerGrabStartData,
    pub orientation: Orientation,
    pub initial_size: Size<i32, Logical>,
    pub initial_ratio: f64,
    pub ratio: Arc<AtomicF64>,
}

impl PointerGrab<State> for ResizeForkGrab {
    fn motion(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &MotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(event.location, None, event.serial, event.time);

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
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        handle.button(event.button, event.state, event.serial, event.time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(event.serial, event.time);
        }
    }

    fn axis(
        &mut self,
        _data: &mut State,
        _dh: &DisplayHandle,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}
