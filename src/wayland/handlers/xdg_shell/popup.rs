// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::Shell, utils::prelude::*};
use smithay::{
    desktop::{
        LayerSurface, PopupKind, PopupManager, WindowSurfaceType, get_popup_toplevel_coords,
        layer_map_for_output, space::SpaceElement, utils,
    },
    output::Output,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_positioner::ConstraintAdjustment,
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Point, Rectangle},
    wayland::{
        compositor::{get_role, with_states},
        seat::WaylandFocus,
        shell::xdg::{PopupCachedState, ToplevelSurface, XDG_POPUP_ROLE, XdgPopupSurfaceData},
    },
};
use tracing::warn;

impl Shell {
    pub fn unconstrain_popup(&self, surface: &PopupKind) {
        if let Some(parent) = get_popup_toplevel(surface) {
            if let Some(elem) = self.element_for_surface(&parent) {
                let (mut element_geo, output, is_tiled) =
                    if let Some(workspace) = self.space_for(elem) {
                        let Some(elem_geo) = workspace.element_geometry(elem) else {
                            return;
                        };
                        (
                            elem_geo.to_global(workspace.output()),
                            workspace.output.clone(),
                            workspace.is_tiled(&elem.active_window()),
                        )
                    } else if let Some((output, set)) = self
                        .workspaces
                        .sets
                        .iter()
                        .find(|(_, set)| set.sticky_layer.mapped().any(|m| m == elem))
                    {
                        (
                            set.sticky_layer
                                .element_geometry(elem)
                                .unwrap()
                                .to_global(output),
                            output.clone(),
                            false,
                        )
                    } else {
                        return;
                    };

                let (window, offset) = elem
                    .windows()
                    .find(|(w, _)| w.wl_surface().as_deref() == Some(&parent))
                    .unwrap();
                let window_geo_offset = window.geometry().loc;
                let window_loc: Point<i32, Global> =
                    element_geo.loc + offset.as_global() + window_geo_offset.as_global();
                if is_tiled {
                    element_geo.loc = (0, 0).into();
                    if !unconstrain_xdg_popup_tile(surface, element_geo.as_logical()) {
                        unconstrain_xdg_popup(surface, window_loc, output.geometry());
                    }
                } else {
                    unconstrain_xdg_popup(surface, window_loc, output.geometry());
                }
            } else if let Some(output) = self.workspaces.spaces().find_map(|w| {
                w.fullscreen_surfaces.iter().find_map(|f| {
                    (f.surface.wl_surface().as_deref() == Some(&parent)).then_some(w.output())
                })
            }) {
                let window_loc = output.geometry().loc;
                unconstrain_xdg_popup(surface, window_loc, output.geometry());
            } else if let Some((output, layer_surface)) = self.outputs().find_map(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(&parent, WindowSurfaceType::ALL)
                    .map(|l| (o, l.clone()))
            }) {
                unconstrain_layer_popup(surface, output, &layer_surface)
            }
        }
    }
}

pub fn update_reactive_popups<'a>(
    toplevel: &ToplevelSurface,
    loc: Point<i32, Global>,
    outputs: impl Iterator<Item = &'a Output>,
) {
    let output_geo = outputs.map(|o| o.geometry()).collect::<Vec<_>>();
    for (popup, _) in PopupManager::popups_for_surface(toplevel.wl_surface()) {
        match popup {
            PopupKind::Xdg(surface) => {
                let positioner = with_states(surface.wl_surface(), |states| {
                    let mut guard = states.cached_state.get::<PopupCachedState>();
                    guard
                        .current()
                        .last_acked
                        .map_or_else(Default::default, |configure| configure.state.positioner)
                });
                if positioner.reactive {
                    let anchor_point = loc + positioner.get_anchor_point().as_global();
                    if let Some(rect) = output_geo
                        .iter()
                        .find(|geo| geo.contains(anchor_point))
                        .copied()
                    {
                        unconstrain_xdg_popup(&PopupKind::from(surface.clone()), loc, rect);
                        if let Err(err) = surface.send_configure() {
                            warn!(
                                ?err,
                                "Compositor bug: Unable to re-configure reactive popup",
                            );
                        }
                    }
                }
            }
            PopupKind::InputMethod(_) => {}
        }
    }
}

// Attempt to constraint to tile, without resize. Return `true` if it fits.
fn unconstrain_xdg_popup_tile(surface: &PopupKind, mut rect: Rectangle<i32, Logical>) -> bool {
    rect.loc -= get_popup_toplevel_coords(surface);
    position_popup_within_rect(surface, rect.as_global(), true)
}

fn unconstrain_xdg_popup(
    surface: &PopupKind,
    window_loc: Point<i32, Global>,
    mut rect: Rectangle<i32, Global>,
) {
    rect.loc -= window_loc;
    rect.loc -= get_popup_toplevel_coords(surface).as_global();
    position_popup_within_rect(surface, rect, false);
}

fn unconstrain_layer_popup(surface: &PopupKind, output: &Output, layer_surface: &LayerSurface) {
    let map = layer_map_for_output(output);
    let layer_geo = map.layer_geometry(layer_surface).unwrap();

    // the output_rect represented relative to the parents coordinate system
    let mut relative = Rectangle::from_size(output.geometry().size).as_logical();
    relative.loc -= layer_geo.loc;
    relative.loc -= get_popup_toplevel_coords(surface);
    position_popup_within_rect(surface, relative.as_global(), false);
}

fn position_popup_within_rect(
    surface: &PopupKind,
    rect: Rectangle<i32, Global>,
    is_tiled: bool,
) -> bool {
    match surface {
        PopupKind::Xdg(surface) => {
            let rect = rect.as_logical();
            let geometry = surface.with_pending_state(|state| {
                let positioner = if is_tiled {
                    let mut positioner_no_resize = state.positioner;
                    positioner_no_resize
                        .constraint_adjustment
                        .remove(ConstraintAdjustment::ResizeX | ConstraintAdjustment::ResizeY);
                    positioner_no_resize
                } else {
                    state.positioner
                };
                positioner.get_unconstrained_geometry(rect)
            });
            surface.with_pending_state(|state| {
                state.geometry = geometry;
            });
            rect.contains_rect(geometry)
        }
        PopupKind::InputMethod(popup) => {
            let input_rect = popup.text_input_rectangle();

            // We basically place the IME popup below the input rect.
            let mut popup_bbox = utils::bbox_from_surface_tree(popup.wl_surface(), input_rect.loc);
            popup_bbox.loc.y += input_rect.size.h;
            // tracing::debug!(
            //     "IME input_rect: {:?}, popup_bbox: {:?}",
            //     input_rect,
            //     popup_bbox
            // );

            // Handle the right edge overflow
            let popup_right = popup_bbox.loc.x + popup_bbox.size.w;
            let rect_right = rect.loc.x + rect.size.w;
            popup_bbox.loc.x -= (popup_right - rect_right).max(0);

            // Flip vertically if the bottom edge overflows
            let popup_bottom = popup_bbox.loc.y + popup_bbox.size.h;
            let rect_bottom = rect.loc.y + rect.size.h;
            if popup_bottom > rect_bottom {
                popup_bbox.loc.y = input_rect.loc.y - popup_bbox.size.h;
            }

            popup.set_location(popup_bbox.loc);
            rect.as_logical().contains_rect(popup_bbox)
        }
    }
}

pub fn get_popup_toplevel(popup: &PopupKind) -> Option<WlSurface> {
    let mut parent = match popup {
        PopupKind::Xdg(popup) => popup.get_parent_surface()?,
        PopupKind::InputMethod(popup) => popup.get_parent()?.surface.clone(),
    };
    while get_role(&parent) == Some(XDG_POPUP_ROLE) {
        parent = with_states(&parent, |states| {
            states
                .data_map
                .get::<XdgPopupSurfaceData>()
                .unwrap()
                .lock()
                .unwrap()
                .parent
                .as_ref()
                .cloned()
        })?;
    }
    Some(parent)
}
