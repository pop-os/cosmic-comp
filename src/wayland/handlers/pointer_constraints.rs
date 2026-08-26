// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::CosmicSurface, state::State, utils::prelude::*};
use smithay::{
    input::pointer::PointerHandle,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Point},
    wayland::{pointer_constraints::PointerConstraintsHandler, seat::WaylandFocus},
};

pub use smithay::wayland::pointer_constraints::{PointerConstraintRef, with_pointer_constraint};

impl PointerConstraintsHandler for State {
    fn new_constraint(&mut self, surface: &WlSurface, pointer: &PointerHandle<Self>) {
        let seat = self
            .common
            .shell
            .read()
            .seats
            .iter()
            .find(|s| s.get_pointer().as_ref() == Some(pointer))
            .cloned();

        let (is_under, is_focused, surface_location) = if let Some(seat) = seat {
            seat.set_pointer_constraint_hint(None);
            let current_output = seat.active_output();
            let position = seat.get_pointer().unwrap().current_location().as_global();
            let shell = self.common.shell.read();
            let under = State::surface_under(position, &current_output, &shell);
            let mut surface_location = None;

            let is_under = if let Some((target, target_loc)) = under
                && let Some(under_surface) = target.wl_surface()
            {
                if *under_surface == *surface {
                    surface_location = Some(target_loc);
                    true
                } else {
                    CosmicSurface::surface_tree_offset(surface, &under_surface).is_some_and(
                        |offset| {
                            surface_location = Some(target_loc - offset.to_f64().as_global());
                            true
                        },
                    )
                }
            } else {
                false
            };

            let is_focused = seat
                .get_keyboard()
                .and_then(|k| k.current_focus())
                .is_some_and(|f| f.has_surface(&shell, surface));

            (is_under, is_focused, surface_location)
        } else {
            (false, false, None)
        };

        if is_focused && is_under {
            with_pointer_constraint(surface, pointer, |constraint| {
                if let Some(constraint) = constraint {
                    if let Some(region) = constraint.region() {
                        if let Some(surface_location) = surface_location
                            && let position = pointer.current_location()
                            && let point = (position - surface_location.as_logical()).to_i32_floor()
                            && region.contains(point)
                        {
                            constraint.activate();
                        }
                    } else {
                        constraint.activate();
                    }
                }
            });
        }
    }

    fn remove_constraint(&mut self, surface: &WlSurface, pointer: &PointerHandle<Self>) {
        if with_pointer_constraint(surface, pointer, |constraint| constraint.is_none()) {
            let seat = self
                .common
                .shell
                .read()
                .seats
                .iter()
                .find(|s| s.get_pointer().as_ref() == Some(pointer))
                .cloned();

            if let Some(seat) = seat
                && let Some((hint_surface, hint_location)) = seat.pointer_constraint_hint()
                && hint_surface == *surface
            {
                self.apply_cursor_hint(surface, pointer, hint_location);
                seat.set_pointer_constraint_hint(None);
            }
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
            let seat = self
                .common
                .shell
                .read()
                .seats
                .iter()
                .find(|s| s.get_pointer().as_ref() == Some(pointer))
                .cloned();

            if let Some(seat) = seat {
                seat.set_pointer_constraint_hint(Some((surface.clone(), location)));
            }
        }
    }
}
