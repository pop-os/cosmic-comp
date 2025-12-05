// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, utils::prelude::OutputExt};
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
        // XXX region
        if pointer
            .current_focus()
            .is_some_and(|x| x.wl_surface().as_deref() == Some(surface))
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
            for (out, set) in &self.common.shell.read().workspaces.sets {
                for workspace in &set.workspaces {
                    // get the right window position based on if the window is fullscreen
                    // this isn't the global position, rather the position inside the workspace
                    let origin = if let Some(fullscreen) = workspace.get_fullscreen() {
                        if fullscreen.wl_surface().as_deref() == Some(surface) {
                            workspace
                                .fullscreen_geometry()
                                .and_then(|geometry| {
                                    let pos = geometry
                                        .loc
                                        .to_f64();

                                    Some((pos.x, pos.y))
                                })
                            }
                            else {
                                None
                            }
                        }
                        else {
                            workspace
                                .mapped()
                                .find(|window| {
                                    window.wl_surface().as_deref() == Some(surface)
                                })
                                .and_then(|window| {
                                    workspace
                                        .element_geometry(window)
                                        .and_then(|rect| {
                                            // windows can have variable window decoration height
                                            let header_offset = window
                                                .active_window_offset()
                                                .to_f64();

                                            let position = rect
                                                .loc
                                                .to_f64();

                                            Some((
                                                header_offset.x + position.x,
                                                header_offset.y + position.y
                                            ))
                                        })
                                })
                    };
                    
                    // the window isn't on this workspace, don't set the position
                    if origin.is_none() {
                        continue;
                    }

                    // the offset from the output (monitor position)
                    let workspace_origin = out
                        .geometry()
                        .loc
                        .to_f64();

                    pointer.set_location(Point::new(
                        workspace_origin.x + origin.unwrap().0 + location.x,
                        workspace_origin.y + origin.unwrap().1 + location.y
                    ));
                }
            }
        }
    }
}
delegate_pointer_constraints!(State);
