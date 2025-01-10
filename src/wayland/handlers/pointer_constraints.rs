// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, utils::prelude::RectGlobalExt};
use smithay::{
    delegate_pointer_constraints,
    desktop::utils::bbox_from_surface_tree,
    input::pointer::PointerHandle,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Point, Rectangle},
    wayland::{
        pointer_constraints::{with_pointer_constraint, PointerConstraintsHandler},
        seat::WaylandFocus,
    }
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

    fn cursor_position_hint(
        &mut self,
        surface: &WlSurface,
        pointer: &PointerHandle<Self>,
        location: Point<f64, Logical>,
    ) {
        if with_pointer_constraint(surface, pointer, |constraint| {
            constraint.is_some_and(|c| c.is_active())
        }) {
            // OR windows

            // popup? TODO
            
            // windows
            if let Some(location) = bbox_from_surface_tree(surface, (0, 0))
                .intersection(Rectangle::new(location.to_i32_round(), (0, 0).into()))
                .map(|rect| rect.loc.to_f64())
            {
                let shell = self.common.shell.read().unwrap();
                if let Some(element) = shell.element_for_surface(surface).cloned() {
                    if let Some(geometry) = shell.element_geometry(&element) {
                        pointer.set_location(geometry.as_logical().loc.to_f64() + location);
                    }
                }
            }

            // layer-shell

            // lock surface

        }
    }
}
delegate_pointer_constraints!(State);
