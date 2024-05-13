// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_pointer_constraints,
    input::pointer::PointerHandle,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::{
        pointer_constraints::{with_pointer_constraint, PointerConstraintsHandler},
        seat::WaylandFocus,
    },
};

impl PointerConstraintsHandler for State {
    fn new_constraint(&mut self, surface: &WlSurface, pointer: &PointerHandle<Self>) {
        // XXX region
        if pointer
            .current_focus()
            .map_or(false, |x| x.wl_surface().as_deref() == Some(surface))
        {
            with_pointer_constraint(surface, pointer, |constraint| {
                constraint.unwrap().activate();
            });
        }
    }
}
delegate_pointer_constraints!(State);
