// SPDX-License-Identifier: GPL-3.0-only
use super::{Data, Orientation, OutputInfo};
use id_tree::{NodeId, Tree};
use smithay::{
    desktop::layer_map_for_output,
    reexports::wayland_server::protocol::{wl_pointer::ButtonState, wl_surface},
    utils::{Logical, Point},
    wayland::{
        output::Output,
        seat::{AxisFrame, PointerGrab, PointerGrabStartData, PointerInnerHandle},
        Serial,
    },
};
use std::cell::RefCell;

pub struct ResizeForkGrab {
    pub start_data: PointerGrabStartData,
    pub node_id: NodeId,
    pub initial_ratio: f64,
    pub idx: u8,
    pub output: Output,
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

        let mut output_info = self
            .output
            .user_data()
            .get::<RefCell<OutputInfo>>()
            .unwrap()
            .borrow_mut();
        let tree = &mut output_info.trees.entry(self.idx).or_insert_with(Tree::new);
        if let Some(&mut Data::Fork {
            ref mut ratio,
            ref orientation,
        }) = tree.get_mut(&self.node_id).map(|x| x.data_mut()).ok()
        {
            let size = layer_map_for_output(&self.output).non_exclusive_zone().size;
            let delta = match orientation {
                Orientation::Vertical => delta.x / size.w as f64,
                Orientation::Horizontal => delta.y / size.h as f64,
            };
            *ratio = 0.9f64.min(0.1f64.max(self.initial_ratio + delta));
        } else {
            handle.unset_grab(serial, time);
        }
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
