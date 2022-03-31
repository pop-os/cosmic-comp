// SPDX-License-Identifier: GPL-3.0-only
use super::Orientation;
use atomic_float::AtomicF64;
use smithay::{
    reexports::wayland_server::protocol::{wl_pointer::ButtonState, wl_surface},
    utils::{Logical, Point, Size},
    wayland::{
        seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle},
        Serial,
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

impl PointerGrab for ResizeForkGrab {
    fn motion(
        &mut self,
        handle: &mut PointerInnerHandle<'_>,
        location: Point<f64, Logical>,
        _focus: Option<(wl_surface::WlSurface, Point<i32, Logical>)>,
        serial: Serial,
        time: u32,
    ) {
        // While the grab is active, no client has pointer focus
        handle.motion(location, None, serial, time);

        let delta = location - self.start_data.location;
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
        handle: &mut PointerInnerHandle<'_>,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        handle.button(button, state, serial, time);
        if handle.current_pressed().is_empty() {
            // No more buttons are pressed, release the grab.
            handle.unset_grab(serial, time);
        }
    }

    fn axis(&mut self, handle: &mut PointerInnerHandle<'_>, details: AxisFrame) {
        handle.axis(details)
    }

    fn start_data(&self) -> &PointerGrabStartData {
        &self.start_data
    }
}
