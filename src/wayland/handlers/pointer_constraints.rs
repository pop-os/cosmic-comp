// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, utils::prelude::{Local, OutputExt}};
use smithay::{
    delegate_pointer_constraints, input::pointer::PointerHandle, reexports::wayland_server::protocol::wl_surface::WlSurface, utils::{Rectangle, Logical, Point}, wayland::{
        pointer_constraints::{PointerConstraintsHandler, with_pointer_constraint},
        seat::WaylandFocus,
    }
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
            // loop over all workspace sets
            for (out, set) in &self.common.shell.read().workspaces.sets {
                let mut geometry: Option<Rectangle<i32, Local>> = None;
                let mut header: Option<Point<i32, Logical>> = None;
                
                // scan sticky windows
                if geometry == None {
                    geometry = set
                        .sticky_layer
                        .mapped()
                        .find(|w| {
                            w.wl_surface().as_deref() == Some(surface)
                        })
                        .and_then(|w| {
                            header = Some(w.active_window_offset());
                            set.sticky_layer.element_geometry(w)
                        });
                }

                for workspace in &set.workspaces {
                    // check the fullscreen window
                    if geometry == None {
                        if let Some(fullscreen) = workspace.get_fullscreen() {
                            if fullscreen.wl_surface().as_deref() == Some(surface) {
                                geometry = workspace.fullscreen_geometry();
                            }
                        }
                    }
    
                    // scan 'normal' windows
                    if geometry == None {
                        geometry = workspace
                            .mapped()
                            .find(|w| {
                                w.wl_surface().as_deref() == Some(surface)
                            })
                            .and_then(|w| {
                                header = Some(w.active_window_offset());
                                workspace.element_geometry(w)
                            });
                    }
                }

                // the window wasn't found, it's not on this set
                if geometry.is_none() {
                    continue;
                }
    
                let window_size = geometry
                    .unwrap()
                    .size
                    .to_f64();
    
                // prevent locations outside the window boundaries
                if
                    location.x < 0.0 ||
                    location.y < 0.0 ||
                    location.x > window_size.w ||
                    location.y > window_size.h
                {
                    continue;
                }

                let header_offset = header
                    .and_then(|h| {
                        Some(h.to_f64())
                    })
                    .unwrap_or_default();

                let origin = geometry
                    .unwrap()
                    .loc
                    .to_f64();
    
                // the offset from the output (monitor position)
                let workspace_origin = out
                    .geometry()
                    .loc
                    .to_f64();
    
                pointer.set_location(Point::new(
                    workspace_origin.x + origin.x + header_offset.x + location.x,
                    workspace_origin.y + origin.y + header_offset.y + location.y
                ));
    
                break;
            }
        }
    }
}
delegate_pointer_constraints!(State);
