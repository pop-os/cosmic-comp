// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::Shell, utils::prelude::*};
use smithay::{
    desktop::{
        layer_map_for_output, space::SpaceElement, LayerSurface, PopupKind, PopupManager,
        WindowSurfaceType,
    },
    output::Output,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_positioner::{
            Anchor, ConstraintAdjustment, Gravity,
        },
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Point, Rectangle},
    wayland::{
        compositor::{get_role, with_states},
        seat::WaylandFocus,
        shell::xdg::{
            PopupSurface, PositionerState, SurfaceCachedState, ToplevelSurface,
            XdgPopupSurfaceData, XDG_POPUP_ROLE,
        },
    },
};
use tracing::{trace, warn};

impl Shell {
    pub fn unconstrain_popup(&self, surface: &PopupSurface) {
        if let Some(parent) = get_popup_toplevel(&surface) {
            if let Some(elem) = self.element_for_surface(&parent) {
                let (mut element_geo, output, is_tiled) =
                    if let Some(workspace) = self.space_for(elem) {
                        let Some(elem_geo) = workspace.element_geometry(elem) else {
                            return;
                        };
                        (
                            elem_geo.to_global(workspace.output()),
                            workspace.output.clone(),
                            workspace.is_tiled(elem),
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
                    .find(|(w, _)| w.wl_surface().as_ref() == Some(&parent))
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
                let positioner = with_states(&surface.wl_surface(), |states| {
                    let attributes = states
                        .data_map
                        .get::<XdgPopupSurfaceData>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attributes.current.positioner.clone()
                });
                if positioner.reactive {
                    let anchor_point = loc + get_anchor_point(&positioner).as_global();
                    if let Some(rect) = output_geo
                        .iter()
                        .find(|geo| geo.contains(anchor_point))
                        .copied()
                    {
                        unconstrain_xdg_popup(&surface, loc, rect);
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

fn unconstrain_xdg_popup_tile(surface: &PopupSurface, rect: Rectangle<i32, Logical>) -> bool {
    let toplevel_offset = get_popup_toplevel_coords(surface);
    let mut geometry = surface.with_pending_state(|state| state.positioner.get_geometry());
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, rect);

    if offset.x != 0 || offset.y != 0 {
        trace!(?surface, "Unconstraining popup to tile.");
        if !unconstrain_flip(&surface, rect) {
            return unconstrain_slide(&surface, rect);
            // don't try to resize for fitting to a tile
        }
    }
    true
}

fn unconstrain_xdg_popup(
    surface: &PopupSurface,
    window_loc: Point<i32, Global>,
    mut rect: Rectangle<i32, Global>,
) {
    rect.loc -= window_loc;
    let relative = rect.as_logical();
    let toplevel_offset = get_popup_toplevel_coords(surface);
    let mut geometry = surface.with_pending_state(|state| state.positioner.get_geometry());
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, relative);

    if offset.x != 0 || offset.y != 0 {
        trace!(?surface, "Unconstraining popup.");
        if !unconstrain_flip(&surface, relative) {
            if !unconstrain_slide(&surface, relative) {
                unconstrain_resize(&surface, relative);
            }
        }
    }
}

fn unconstrain_layer_popup(surface: &PopupSurface, output: &Output, layer_surface: &LayerSurface) {
    let map = layer_map_for_output(output);
    let layer_geo = map.layer_geometry(layer_surface).unwrap();

    // the output_rect represented relative to the parents coordinate system
    let mut relative = Rectangle::from_loc_and_size((0, 0), output.geometry().size).as_logical();
    relative.loc -= layer_geo.loc;
    let toplevel_offset = get_popup_toplevel_coords(surface);
    let mut geometry = surface.with_pending_state(|state| state.positioner.get_geometry());
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, relative);

    if offset.x != 0 || offset.y != 0 {
        trace!(?surface, "Unconstraining popup.");
        if !unconstrain_flip(&surface, relative) {
            if !unconstrain_slide(&surface, relative) {
                unconstrain_resize(&surface, relative);
            }
        }
    }
}

fn unconstrain_flip(popup: &PopupSurface, toplevel_box: Rectangle<i32, Logical>) -> bool {
    let toplevel_offset = get_popup_toplevel_coords(popup);
    let positioner = popup.with_pending_state(|state| state.positioner.clone());
    let mut geometry = positioner.get_geometry();
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, toplevel_box);
    if offset.x == 0 && offset.y == 0 {
        return true;
    }

    let mut positioner = positioner.clone();

    let flip_x = offset.x != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::FlipX);
    let flip_y = offset.y != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::FlipY);

    if flip_x {
        let old_positioner = positioner.clone();
        positioner.anchor_edges = invert_anchor_x(positioner.anchor_edges);
        positioner.gravity = invert_gravity_x(positioner.gravity);
        geometry = positioner.get_geometry();
        geometry.loc += toplevel_offset;
        let new_offset = check_constrained(geometry, toplevel_box);
        if !(new_offset.x.abs() < offset.x.abs()) {
            positioner = old_positioner;
        }
    }
    if flip_y {
        let old_positioner = positioner.clone();
        positioner.anchor_edges = invert_anchor_y(positioner.anchor_edges);
        positioner.gravity = invert_gravity_y(positioner.gravity);
        geometry = positioner.get_geometry();
        geometry.loc += toplevel_offset;
        let new_offset = check_constrained(geometry, toplevel_box);
        if !(new_offset.y.abs() < offset.y.abs()) {
            positioner = old_positioner;
        }
    }

    geometry = positioner.get_geometry();
    geometry.loc += toplevel_offset;
    let new_offset = check_constrained(geometry, toplevel_box);
    if new_offset.x.abs() < offset.x.abs() || new_offset.y.abs() < offset.y.abs() {
        popup.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });
    }

    new_offset.x == 0 && new_offset.y == 0
}

fn unconstrain_slide(popup: &PopupSurface, toplevel_box: Rectangle<i32, Logical>) -> bool {
    let toplevel_offset = get_popup_toplevel_coords(popup);
    let positioner = popup.with_pending_state(|state| state.positioner.clone());
    let mut geometry = positioner.get_geometry();
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, toplevel_box);
    if offset.x == 0 && offset.y == 0 {
        return true;
    }

    let slide_x = offset.x != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::SlideX);
    let slide_y = offset.y != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::SlideY);

    let mut geometry = positioner.get_geometry();
    if slide_x {
        geometry.loc.x += offset.x.abs().min(geometry.size.w) * offset.x.signum();
    }
    if slide_y {
        geometry.loc.y += offset.y.abs().min(geometry.size.h) * offset.y.signum();
    }

    let toplevel = get_popup_toplevel_coords(popup);
    if slide_x && toplevel.x < toplevel_box.loc.x {
        geometry.loc.x += toplevel_box.loc.x - toplevel.x;
    }
    if slide_y && toplevel.y < toplevel_box.loc.y {
        geometry.loc.y += toplevel_box.loc.y - toplevel.y;
    }

    let mut check_geometry = geometry.clone();
    check_geometry.loc += toplevel;
    let new_offset = check_constrained(check_geometry, toplevel_box);
    if new_offset.x.abs() < offset.x.abs() || new_offset.y.abs() < offset.y.abs() {
        popup.with_pending_state(|state| {
            state.geometry = geometry;
        });
    }

    new_offset.x == 0 && new_offset.y == 0
}

fn unconstrain_resize(popup: &PopupSurface, toplevel_box: Rectangle<i32, Logical>) -> bool {
    let toplevel_offset = get_popup_toplevel_coords(popup);
    let positioner = popup.with_pending_state(|state| state.positioner.clone());
    let mut geometry = positioner.get_geometry();
    geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, toplevel_box);
    if offset.x == 0 && offset.y == 0 {
        return true;
    }

    let resize_x = offset.x != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::ResizeX);
    let resize_y = offset.y != 0
        && positioner
            .constraint_adjustment
            .contains(ConstraintAdjustment::ResizeY);

    let mut geometry = positioner.get_geometry();
    if resize_x {
        geometry.size.w -= offset.x;
    }
    if resize_y {
        geometry.size.h -= offset.y;
    }

    let mut check_geometry = geometry.clone();
    check_geometry.loc += toplevel_offset;
    let offset = check_constrained(geometry, toplevel_box);
    if offset.x == 0 && offset.y == 0 {
        // no longer constrained
        popup.with_pending_state(|state| {
            state.geometry = geometry;
        });
        true
    } else {
        false
    }
}

fn check_constrained(
    geometry: Rectangle<i32, Logical>,
    toplevel_box: Rectangle<i32, Logical>,
) -> Point<i32, Logical> {
    let mut offset = (0, 0).into();
    if toplevel_box.contains_rect(geometry) {
        return offset;
    }

    if geometry.loc.x < toplevel_box.loc.x {
        offset.x = toplevel_box.loc.x - geometry.loc.x;
    } else if geometry.loc.x + geometry.size.w > toplevel_box.loc.x + toplevel_box.size.w {
        offset.x = toplevel_box.loc.x + toplevel_box.size.w - (geometry.loc.x + geometry.size.w);
    }

    if geometry.loc.y < toplevel_box.loc.y {
        offset.y = toplevel_box.loc.y - geometry.loc.y;
    } else if geometry.loc.y + geometry.size.h > toplevel_box.loc.y + toplevel_box.size.h {
        offset.y = toplevel_box.loc.y + toplevel_box.size.h - (geometry.loc.y + geometry.size.h);
    }

    offset
}

fn get_anchor_point(positioner: &PositionerState) -> Point<i32, Logical> {
    let rect = positioner.anchor_rect;
    match positioner.anchor_edges {
        Anchor::Top => (rect.loc.x + (rect.size.w / 2), rect.loc.y),
        Anchor::Bottom => (rect.loc.x + (rect.size.w / 2), rect.loc.y + rect.size.h),
        Anchor::Left => (rect.loc.x, rect.loc.y + (rect.size.h / 2)),
        Anchor::Right => (rect.loc.x + rect.size.w, rect.loc.y + (rect.size.h / 2)),
        Anchor::TopLeft => (rect.loc.x, rect.loc.y),
        Anchor::TopRight => (rect.loc.x + rect.size.w, rect.loc.y),
        Anchor::BottomLeft => (rect.loc.x, rect.loc.y + rect.size.h),
        Anchor::BottomRight => (rect.loc.x + rect.size.w, rect.loc.y + rect.size.h),
        Anchor::None | _ => (
            rect.loc.x + (rect.size.w / 2),
            rect.loc.y + (rect.size.h / 2),
        ),
    }
    .into()
}

pub fn get_popup_toplevel(popup: &PopupSurface) -> Option<WlSurface> {
    let mut parent = popup.get_parent_surface()?;
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
                .unwrap()
        });
    }
    Some(parent)
}

fn get_popup_toplevel_coords(popup: &PopupSurface) -> Point<i32, Logical> {
    let mut parent = match popup.get_parent_surface() {
        Some(parent) => parent,
        None => return (0, 0).into(),
    };

    let mut offset = (0, 0).into();
    while get_role(&parent) == Some(XDG_POPUP_ROLE) {
        offset += with_states(&parent, |states| {
            states
                .data_map
                .get::<XdgPopupSurfaceData>()
                .unwrap()
                .lock()
                .unwrap()
                .current
                .geometry
                .loc
        });
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
                .unwrap()
        });
    }
    offset += with_states(&parent, |states| {
        states
            .cached_state
            .current::<SurfaceCachedState>()
            .geometry
            .map(|x| x.loc)
            .unwrap_or_else(|| (0, 0).into())
    });

    offset
}

fn invert_anchor_x(anchor: Anchor) -> Anchor {
    match anchor {
        Anchor::Left => Anchor::Right,
        Anchor::Right => Anchor::Left,
        Anchor::TopLeft => Anchor::TopRight,
        Anchor::TopRight => Anchor::TopLeft,
        Anchor::BottomLeft => Anchor::BottomRight,
        Anchor::BottomRight => Anchor::BottomLeft,
        x => x,
    }
}
fn invert_anchor_y(anchor: Anchor) -> Anchor {
    match anchor {
        Anchor::Top => Anchor::Bottom,
        Anchor::Bottom => Anchor::Top,
        Anchor::TopLeft => Anchor::BottomLeft,
        Anchor::TopRight => Anchor::BottomRight,
        Anchor::BottomLeft => Anchor::TopLeft,
        Anchor::BottomRight => Anchor::TopRight,
        x => x,
    }
}
fn invert_gravity_x(gravity: Gravity) -> Gravity {
    match gravity {
        Gravity::Left => Gravity::Right,
        Gravity::Right => Gravity::Left,
        Gravity::TopLeft => Gravity::TopRight,
        Gravity::TopRight => Gravity::TopLeft,
        Gravity::BottomLeft => Gravity::BottomRight,
        Gravity::BottomRight => Gravity::BottomLeft,
        x => x,
    }
}
fn invert_gravity_y(gravity: Gravity) -> Gravity {
    match gravity {
        Gravity::Top => Gravity::Bottom,
        Gravity::Bottom => Gravity::Top,
        Gravity::TopLeft => Gravity::BottomLeft,
        Gravity::TopRight => Gravity::BottomRight,
        Gravity::BottomLeft => Gravity::TopLeft,
        Gravity::BottomRight => Gravity::TopRight,
        x => x,
    }
}
