use std::{ops::ControlFlow, time::Instant};

use cosmic_comp_config::workspace::WorkspaceLayout;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    desktop::{layer_map_for_output, LayerSurface, PopupKind, PopupManager},
    output::{Output, OutputNoMode},
    utils::{Logical, Point},
    wayland::{session_lock::LockSurface, shell::wlr_layer::Layer},
    xwayland::X11Surface,
};

use crate::{
    backend::render::ElementFilter,
    shell::{
        focus::target::KeyboardFocusTarget,
        layout::{floating::FloatingLayout, tiling::ANIMATION_DURATION},
        SeatExt, Shell, Workspace, WorkspaceDelta,
    },
    utils::{
        geometry::*,
        prelude::OutputExt,
        quirks::{workspace_overview_is_open, WORKSPACE_OVERVIEW_NAMESPACE},
    },
    wayland::protocols::workspace::WorkspaceHandle,
};

pub enum Stage<'a> {
    ZoomUI,
    SessionLock(Option<&'a LockSurface>),
    LayerPopup {
        layer: LayerSurface,
        popup: &'a PopupKind,
        location: Point<i32, Global>,
    },
    LayerSurface {
        layer: LayerSurface,
        location: Point<i32, Global>,
    },
    OverrideRedirect {
        surface: &'a X11Surface,
        location: Point<i32, Global>,
    },
    StickyPopups(&'a FloatingLayout),
    Sticky(&'a FloatingLayout),
    WorkspacePopups {
        workspace: &'a Workspace,
        offset: Point<i32, Logical>,
    },
    Workspace {
        workspace: &'a Workspace,
        offset: Point<i32, Logical>,
    },
}

pub fn render_input_order<R: Default + 'static>(
    shell: &Shell,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    element_filter: ElementFilter,
    callback: impl FnMut(Stage) -> ControlFlow<Result<R, OutputNoMode>, ()>,
) -> Result<R, OutputNoMode> {
    match render_input_order_internal(shell, output, previous, current, element_filter, callback) {
        ControlFlow::Break(result) => result,
        ControlFlow::Continue(_) => Ok(R::default()),
    }
}

fn render_input_order_internal<R: 'static>(
    shell: &Shell,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    element_filter: ElementFilter,
    mut callback: impl FnMut(Stage) -> ControlFlow<Result<R, OutputNoMode>, ()>,
) -> ControlFlow<Result<R, OutputNoMode>, ()> {
    if shell
        .zoom_state
        .as_ref()
        .is_some_and(|state| state.show_overlay && state.current_level(output) != 1.0)
    {
        callback(Stage::ZoomUI)?;
    }

    // Session Lock
    if let Some(session_lock) = &shell.session_lock {
        return callback(Stage::SessionLock(session_lock.surfaces.get(output)));
    }

    // Overlay-level layer shell
    // overlay is above everything
    for (layer, popup, location) in layer_popups(output, Layer::Overlay, element_filter) {
        callback(Stage::LayerPopup {
            layer,
            popup: &popup,
            location,
        })?;
    }
    for (layer, location) in layer_surfaces(output, Layer::Overlay, element_filter) {
        callback(Stage::LayerSurface { layer, location })?;
    }

    // calculate a bunch of stuff for workspace transitions

    let Some(set) = shell.workspaces.sets.get(output) else {
        return ControlFlow::Break(Err(OutputNoMode));
    };
    let Some(workspace) = set.workspaces.iter().find(|w| w.handle == current.0) else {
        return ControlFlow::Break(Err(OutputNoMode));
    };
    let output_size = output.geometry().size;

    // this is more hacky than I would like..
    let fullscreen = workspace.fullscreen.as_ref().filter(|f| !f.is_animating());
    let seat = shell.seats.last_active();
    let is_active_workspace = seat.focused_output().is_some_and(|output| {
        shell
            .active_space(&output)
            .is_some_and(|w| w.handle == workspace.handle)
    });
    let focus_stack_is_valid_fullscreen = workspace
        .focus_stack
        .get(seat)
        .last()
        .zip(fullscreen)
        .is_some_and(|(target, fullscreen)| target == &fullscreen.surface);
    let overview_is_open = workspace_overview_is_open(&output);
    let has_focused_fullscreen = if is_active_workspace {
        let current_focus = seat.get_keyboard().unwrap().current_focus();
        matches!(current_focus, Some(KeyboardFocusTarget::Fullscreen(_)))
            || (current_focus.is_none()
                && focus_stack_is_valid_fullscreen
                && !workspace_overview_is_open(&output))
    } else {
        focus_stack_is_valid_fullscreen && !overview_is_open
    };
    let has_fullscreen = fullscreen.is_some() && !overview_is_open;

    let (previous, current_offset) = match previous.as_ref() {
        Some((previous, previous_idx, start)) => {
            let layout = shell.workspaces.layout;

            let Some(workspace) = shell.workspaces.space_for_handle(&previous) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            let has_fullscreen = workspace.fullscreen.is_some();

            let percentage = match start {
                WorkspaceDelta::Shortcut(st) => ease(
                    EaseInOutCubic,
                    0.0,
                    1.0,
                    Instant::now().duration_since(*st).as_millis() as f32
                        / ANIMATION_DURATION.as_millis() as f32,
                ),
                WorkspaceDelta::Gesture(prog) => *prog as f32,
                WorkspaceDelta::GestureEnd(st, spring) => {
                    (spring.value_at(Instant::now().duration_since(*st)) as f32).clamp(0.0, 1.0)
                }
            };

            let offset = Point::<i32, Logical>::from(match (layout, *previous_idx < current.1) {
                (WorkspaceLayout::Vertical, true) => {
                    (0, (-output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Vertical, false) => {
                    (0, (output_size.h as f32 * percentage).round() as i32)
                }
                (WorkspaceLayout::Horizontal, true) => {
                    ((-output_size.w as f32 * percentage).round() as i32, 0)
                }
                (WorkspaceLayout::Horizontal, false) => {
                    ((output_size.w as f32 * percentage).round() as i32, 0)
                }
            });

            (
                Some((previous, has_fullscreen, offset)),
                Point::<i32, Logical>::from(match (layout, *previous_idx < current.1) {
                    (WorkspaceLayout::Vertical, true) => (0, output_size.h + offset.y),
                    (WorkspaceLayout::Vertical, false) => (0, -(output_size.h - offset.y)),
                    (WorkspaceLayout::Horizontal, true) => (output_size.w + offset.x, 0),
                    (WorkspaceLayout::Horizontal, false) => (-(output_size.w - offset.x), 0),
                }),
            )
        }
        None => (None, Point::default()),
    };

    // Top-level layer shell popups
    if !has_focused_fullscreen {
        for (layer, popup, location) in layer_popups(output, Layer::Top, element_filter) {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if element_filter != ElementFilter::LayerShellOnly {
        // overlay redirect windows
        // they need to be over sticky windows, because they could be popups of sticky windows,
        // and we can't differenciate that.
        for (surface, location) in shell
            .override_redirect_windows
            .iter()
            .rev()
            .filter(|or| {
                (*or)
                    .geometry()
                    .as_global()
                    .intersection(output.geometry())
                    .is_some()
            })
            .map(|or| (or, or.geometry().loc.as_global()))
        {
            callback(Stage::OverrideRedirect { surface, location })?;
        }

        // sticky window popups
        if !has_focused_fullscreen {
            callback(Stage::StickyPopups(&set.sticky_layer))?;
        }
    }

    if element_filter != ElementFilter::LayerShellOnly {
        // previous workspace popups
        if let Some((previous_handle, _, offset)) = previous.as_ref() {
            let Some(workspace) = shell.workspaces.space_for_handle(previous_handle) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };

            callback(Stage::WorkspacePopups {
                workspace,
                offset: *offset,
            })?;
        }

        // current workspace popups
        let Some(workspace) = shell.workspaces.space_for_handle(&current.0) else {
            return ControlFlow::Break(Err(OutputNoMode));
        };

        callback(Stage::WorkspacePopups {
            workspace,
            offset: current_offset,
        })?;
    }

    if !has_focused_fullscreen {
        // bottom layer popups
        for (layer, popup, location) in layer_popups(output, Layer::Bottom, element_filter) {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous bottom layer popups
            for (layer, popup, location) in layer_popups(output, Layer::Bottom, element_filter) {
                callback(Stage::LayerPopup {
                    layer,
                    popup: &popup,
                    location: location + offset.as_global(),
                })?;
            }
        }
    }

    if !has_fullscreen {
        // background layer popups
        for (layer, popup, location) in layer_popups(output, Layer::Background, element_filter) {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous background layer popups
            for (layer, popup, location) in layer_popups(output, Layer::Background, element_filter)
            {
                callback(Stage::LayerPopup {
                    layer,
                    popup: &popup,
                    location: location + offset.as_global(),
                })?;
            }
        }
    }

    if !has_focused_fullscreen {
        // top-layer shell
        for (layer, location) in layer_surfaces(output, Layer::Top, element_filter) {
            callback(Stage::LayerSurface { layer, location })?;
        }

        // sticky windows
        if element_filter != ElementFilter::LayerShellOnly {
            callback(Stage::Sticky(&set.sticky_layer))?;
        }
    }

    if element_filter != ElementFilter::LayerShellOnly {
        // workspace windows
        callback(Stage::Workspace {
            workspace,
            offset: current_offset,
        })?;

        // previous workspace windows
        if let Some((previous_handle, _, offset)) = previous.as_ref() {
            let Some(workspace) = shell.workspaces.space_for_handle(previous_handle) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            callback(Stage::Workspace {
                workspace,
                offset: *offset,
            })?;
        }
    }

    if !has_focused_fullscreen {
        // bottom layer
        for (layer, mut location) in layer_surfaces(output, Layer::Bottom, element_filter) {
            location += current_offset.as_global();
            callback(Stage::LayerSurface { layer, location })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous bottom layer
            for (layer, mut location) in layer_surfaces(output, Layer::Bottom, element_filter) {
                location += offset.as_global();
                callback(Stage::LayerSurface { layer, location })?;
            }
        }
    }

    if !has_fullscreen {
        // background layer
        for (layer, mut location) in layer_surfaces(output, Layer::Background, element_filter) {
            location += current_offset.as_global();
            callback(Stage::LayerSurface { layer, location })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref() {
        if !has_fullscreen {
            // previous background layer
            for (layer, mut location) in layer_surfaces(output, Layer::Background, element_filter) {
                location += offset.as_global();
                callback(Stage::LayerSurface { layer, location })?;
            }
        }
    }

    ControlFlow::Continue(())
}

fn layer_popups<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: ElementFilter,
) -> impl Iterator<Item = (LayerSurface, PopupKind, Point<i32, Global>)> + 'a {
    layer_surfaces(output, layer, element_filter).flat_map(move |(surface, location)| {
        let location_clone = location.clone();
        let surface_clone = surface.clone();
        PopupManager::popups_for_surface(surface.wl_surface()).map(move |(popup, popup_offset)| {
            let offset = (popup_offset - popup.geometry().loc).as_global();
            (surface_clone.clone(), popup, (location_clone + offset))
        })
    })
}

fn layer_surfaces<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: ElementFilter,
) -> impl Iterator<Item = (LayerSurface, Point<i32, Global>)> + 'a {
    // we want to avoid deadlocks on the layer-map in callbacks, so we need to clone the layer surfaces
    let layers = {
        let layer_map = layer_map_for_output(output);
        layer_map
            .layers_on(layer)
            .rev()
            .map(|s| (s.clone(), layer_map.layer_geometry(s).unwrap()))
            .collect::<Vec<_>>()
    };

    layers
        .into_iter()
        .filter(move |(s, _)| {
            !(element_filter == ElementFilter::ExcludeWorkspaceOverview
                && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE)
        })
        .map(|(surface, geometry)| (surface, geometry.loc.as_local().to_global(output)))
}
