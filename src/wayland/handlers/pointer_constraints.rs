// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::CosmicSurface, state::State, utils::prelude::*};
use ron::de::Position;
use smithay::{
    input::{Seat, pointer::PointerHandle},
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Point},
    wayland::{
        pointer_constraints::{PointerConstraint, PointerConstraintsHandler},
        seat::WaylandFocus,
    },
};

pub use smithay::wayland::pointer_constraints::{PointerConstraintRef, with_pointer_constraint};

impl State {
    /// Activate the pointer constraint of `surface`, if any, now that the
    /// pointer is over it at `surface_location`.
    ///
    /// Constraints are deactivated whenever the surface loses pointer focus,
    /// which happens for reasons entirely outside the client's control (a
    /// window resize moving the pointer out, for example). Every path that
    /// gives a surface pointer focus back therefore has to offer the
    /// constraint another chance to activate, or a game stays without
    /// relative motion until the user happens to move the mouse.
    pub fn maybe_activate_pointer_constraint(
        &self,
        seat: &Seat<State>,
        surface: &WlSurface,
        surface_location: Point<f64, Logical>,
    ) {
        let Some(pointer) = seat.get_pointer() else {
            return;
        };

        let shell = self.common.shell.read(); // Grabs a read lock on the shell
        let is_focused = seat
            .get_keyboard()
            .and_then(|k| k.current_focus())
            .is_some_and(|f| {
                f.has_surface(&shell, surface)
                    || self.common.xwayland_constraint_focus_override(&f, surface)
            });
        drop(shell); // Drop the shell lock as early as possible

        if !is_focused {
            // No point in activating a constraint for a surface that's not focused.
            return;
        }

        with_pointer_constraint(surface, &pointer, |constraint| {
            let Some(constraint) = constraint else {
                return;
            };
            if constraint.is_active() {
                return;
            }
            let region = match &*constraint {
                PointerConstraint::Locked(locked) => locked.region(),
                PointerConstraint::Confined(confined) => confined.region(),
            };
            let point = (pointer.current_location() - surface_location).to_i32_floor();
            if region.is_none_or(|region| region.contains(point)) {
                constraint.activate();
            }
        });
    }
}

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

        let Some(seat) = seat else {
            // The seat is None, so we can't get the pointer. We can't set the constraint.
            return;
        };
        seat.set_pointer_constraint_hint(None);

        let current_output = seat.active_output();
        let pointer = seat.get_pointer();
        let position;
        if let Some(pointer) = pointer {
            position = pointer.current_location().as_global();
        } else {
            // The pointer is None, so we can't get its location. So we can't set the constraint.
            return;
        }
        let shell = self.common.shell.read(); // Grabs a read lock on the shell
        let under = State::surface_under(position, &current_output, &shell);
        drop(shell); // Drops the read lock on the shell as soon as possible

        // Only the surface actually under the pointer may hold it captive.
        let surface_location = if let Some((target, target_loc)) = under
            && let Some(under_surface) = target.wl_surface()
        {
            if *under_surface == *surface {
                Some(target_loc)
            } else {
                CosmicSurface::surface_tree_offset(surface, &under_surface)
                    .map(|offset| target_loc - offset.to_f64().as_global())
            }
        } else {
            None
        };

        if let Some(surface_location) = surface_location {
            self.maybe_activate_pointer_constraint(&seat, surface, surface_location.as_logical());
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
