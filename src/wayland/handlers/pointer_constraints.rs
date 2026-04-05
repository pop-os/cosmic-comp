// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, utils::prelude::*};
use smithay::{
    delegate_pointer_constraints,
    input::pointer::PointerHandle,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Point},
    wayland::{
        pointer_constraints::{PointerConstraintsHandler, with_pointer_constraint},
        seat::WaylandFocus,
    },
};

impl PointerConstraintsHandler for State {
    fn new_constraint(&mut self, surface: &WlSurface, pointer: &PointerHandle<Self>) {
        // Activate immediately if the pointer's tracked focus matches this surface.
        let focus_matches = pointer
            .current_focus()
            .is_some_and(|x| x.wl_surface().as_deref() == Some(surface));

        // Also activate if the pointer is physically over this surface but
        // current_focus() is stale (e.g., pointer hasn't moved since the window
        // received keyboard focus and requested the constraint).
        let physically_over = if !focus_matches {
            let pos = pointer.current_location().as_global();
            let shell = self.common.shell.read();
            shell
                .outputs()
                .find(|o| o.geometry().to_f64().contains(pos))
                .or_else(|| shell.outputs().next())
                .cloned()
                .and_then(|output| State::surface_under(pos, &output, &shell))
                .map(|(t, _)| t.wl_surface().as_deref().is_some_and(|s| s == surface))
                .unwrap_or(false)
        } else {
            false
        };

        if focus_matches || physically_over {
            with_pointer_constraint(surface, pointer, |constraint| {
                constraint.unwrap().activate();
            });
        }
    }

    fn cursor_position_hint(
        &mut self,
        _surface: &WlSurface,
        _pointer: &PointerHandle<Self>,
        _location: Point<f64, Logical>,
    ) {
        // TODO
    }
}
delegate_pointer_constraints!(State);
