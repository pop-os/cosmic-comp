// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::Shell, utils::prelude::*};
use smithay::{
    desktop::{
        layer_map_for_output, LayerSurface, PopupKind, PopupManager, Window, WindowSurfaceType,
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
        shell::xdg::{
            PopupSurface, PositionerState, SurfaceCachedState, XdgPopupSurfaceRoleAttributes,
            XDG_POPUP_ROLE,
        },
    },
};
use std::sync::Mutex;

impl Shell {
    pub fn unconstrain_popup(&self, surface: &PopupSurface, positioner: &PositionerState) {
        if let Some(parent) = get_popup_toplevel(&surface) {
            if let Some(elem) = self.element_for_surface(&parent) {
                let workspace = self.space_for(elem).unwrap();
                let element_loc = workspace.element_geometry(elem).unwrap().loc;
                let (window, offset) = elem
                    .windows()
                    .find(|(w, _)| w.toplevel().wl_surface() == &parent)
                    .unwrap();
                let window_geo_offset = window.geometry().loc;
                let window_loc = element_loc + offset + window_geo_offset;
                let anchor_point = get_anchor_point(&positioner) + window_loc;
                if let Some(output) = workspace.output_under(anchor_point) {
                    unconstrain_xdg_popup(surface, positioner, window_loc, output.geometry());
                }
            } else if let Some((output, layer_surface)) = self.outputs().find_map(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(&parent, WindowSurfaceType::ALL)
                    .map(|l| (o, l.clone()))
            }) {
                unconstrain_layer_popup(surface, positioner, output, &layer_surface)
            }
        }
    }
}

pub fn update_reactive_popups<'a>(
    window: &Window,
    loc: Point<i32, Logical>,
    outputs: impl Iterator<Item = &'a Output>,
) {
    let output_geo = outputs.map(|o| o.geometry()).collect::<Vec<_>>();
    for (popup, _) in PopupManager::popups_for_surface(window.toplevel().wl_surface()) {
        match popup {
            PopupKind::Xdg(surface) => {
                let positioner = with_states(&surface.wl_surface(), |states| {
                    let attributes = states
                        .data_map
                        .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attributes.current.positioner.clone()
                });
                if positioner.reactive {
                    let anchor_point = get_anchor_point(&positioner) + loc;
                    if let Some(rect) = output_geo
                        .iter()
                        .find(|geo| geo.contains(anchor_point))
                        .copied()
                    {
                        unconstrain_xdg_popup(&surface, &positioner, loc, rect);
                        if let Err(err) = surface.send_configure() {
                            slog_scope::warn!(
                                "Compositor bug: Unable to re-configure reactive popup: {}",
                                err
                            );
                        }
                    }
                }
            }
        }
    }
}

fn unconstrain_xdg_popup(
    surface: &PopupSurface,
    positioner: &PositionerState,
    window_loc: Point<i32, Logical>,
    rect: Rectangle<i32, Logical>,
) {
    let mut relative = rect;
    relative.loc -= window_loc;
    let offset = check_constrained(&surface, positioner.get_geometry(), relative);

    if offset.x != 0 || offset.y != 0 {
        slog_scope::debug!("Unconstraining popup: {:?}", surface);
        if !unconstrain_flip(&surface, &positioner, relative) {
            if !unconstrain_slide(&surface, &positioner, relative) {
                unconstrain_resize(&surface, &positioner, relative);
            }
        }
    }
}

fn unconstrain_layer_popup(
    surface: &PopupSurface,
    positioner: &PositionerState,
    output: &Output,
    layer_surface: &LayerSurface,
) {
    let map = layer_map_for_output(output);
    let layer_geo = map.layer_geometry(layer_surface).unwrap();

    // the output_rect represented relative to the parents coordinate system
    let mut relative = Rectangle::from_loc_and_size((0, 0), output.geometry().size);
    relative.loc -= layer_geo.loc;
    let offset = check_constrained(&surface, positioner.get_geometry(), relative);

    if offset.x != 0 || offset.y != 0 {
        slog_scope::debug!("Unconstraining popup: {:?}", surface);
        if !unconstrain_flip(&surface, &positioner, relative) {
            if !unconstrain_slide(&surface, &positioner, relative) {
                unconstrain_resize(&surface, &positioner, relative);
            }
        }
    }
}

fn unconstrain_flip(
    popup: &PopupSurface,
    positioner: &PositionerState,
    toplevel_box: Rectangle<i32, Logical>,
) -> bool {
    let offset = check_constrained(popup, positioner.get_geometry(), toplevel_box);
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
        positioner.anchor_edges = invert_anchor_x(positioner.anchor_edges);
        positioner.gravity = invert_gravity_x(positioner.gravity);
    }
    if flip_y {
        positioner.anchor_edges = invert_anchor_y(positioner.anchor_edges);
        positioner.gravity = invert_gravity_y(positioner.gravity);
    }

    let offset = check_constrained(popup, positioner.get_geometry(), toplevel_box);
    if offset.x == 0 && offset.y == 0 {
        // no longer constrained
        popup.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });
        true
    } else {
        false
    }
}

fn unconstrain_slide(
    popup: &PopupSurface,
    positioner: &PositionerState,
    toplevel_box: Rectangle<i32, Logical>,
) -> bool {
    let offset = check_constrained(popup, positioner.get_geometry(), toplevel_box);
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
        geometry.loc.x += offset.x;
    }
    if slide_y {
        geometry.loc.y += offset.y;
    }

    let toplevel = get_popup_toplevel_coords(popup);
    if slide_x && toplevel.x < toplevel_box.loc.x {
        geometry.loc.x += toplevel_box.loc.x - toplevel.x;
    }
    if slide_y && toplevel.y < toplevel_box.loc.y {
        geometry.loc.y += toplevel_box.loc.y - toplevel.y;
    }

    let offset = check_constrained(popup, geometry, toplevel_box);
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

fn unconstrain_resize(
    popup: &PopupSurface,
    positioner: &PositionerState,
    toplevel_box: Rectangle<i32, Logical>,
) -> bool {
    let offset = check_constrained(popup, positioner.get_geometry(), toplevel_box);
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

    let offset = check_constrained(popup, geometry, toplevel_box);
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
    popup: &PopupSurface,
    geometry: Rectangle<i32, Logical>,
    toplevel_box: Rectangle<i32, Logical>,
) -> Point<i32, Logical> {
    let relative_coords = Rectangle::from_loc_and_size(
        geometry.loc + get_popup_toplevel_coords(popup),
        geometry.size,
    );

    let mut offset = (0, 0).into();
    if toplevel_box.contains_rect(relative_coords) {
        return offset;
    }

    if relative_coords.loc.x < toplevel_box.loc.x {
        offset.x = toplevel_box.loc.x - relative_coords.loc.x;
    } else if relative_coords.loc.x + relative_coords.size.w
        > toplevel_box.loc.x + toplevel_box.size.w
    {
        offset.x = toplevel_box.loc.x + toplevel_box.size.w
            - (relative_coords.loc.x + relative_coords.size.w);
    }

    if relative_coords.loc.y < toplevel_box.loc.y {
        offset.y = toplevel_box.loc.y - relative_coords.loc.y;
    } else if relative_coords.loc.y + relative_coords.size.h
        > toplevel_box.loc.y + toplevel_box.size.h
    {
        offset.y = toplevel_box.loc.y + toplevel_box.size.h
            - (relative_coords.loc.y + relative_coords.size.h);
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
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
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
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
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
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
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
