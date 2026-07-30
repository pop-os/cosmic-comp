use std::{ops::ControlFlow, time::Instant};

use cosmic_comp_config::workspace::WorkspaceLayout;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    desktop::{
        LayerSurface, PopupKind, PopupManager, layer_map_for_output, utils::bbox_from_surface_tree,
    },
    output::{Output, OutputNoMode},
    reexports::wayland_server::Resource,
    utils::{Logical, Point},
    wayland::{session_lock::LockSurface, shell::wlr_layer::Layer},
    xwayland::X11Surface,
};

use tracing::trace;

use crate::{
    backend::render::{ElementFilter, HomeVisibilityContext},
    logger::GAMING_TARGET,
    shell::{
        CosmicSurface, SeatExt, Shell, Workspace, WorkspaceDelta,
        focus::target::KeyboardFocusTarget, layout::floating::FloatingLayout,
    },
    utils::{
        geometry::*,
        prelude::OutputExt,
        quirks::{WORKSPACE_OVERVIEW_NAMESPACE, workspace_overview_is_open},
    },
    wayland::protocols::workspace::WorkspaceHandle,
};

/// Check if windows/workspaces should be included for the given element filter.
/// Returns false for LayerShellOnly and for LayerBlurCapture when capturing for
/// layers below windows (Bottom, Background).
fn should_include_windows(element_filter: &ElementFilter) -> bool {
    match element_filter {
        ElementFilter::LayerShellOnly => false,
        ElementFilter::LayerBlurCapture(layer, _) => {
            // Only include windows for layers above windows (Top, Overlay)
            // Bottom and Background are below windows, so skip windows when capturing for them
            matches!(layer, Layer::Top | Layer::Overlay)
        }
        _ => true,
    }
}

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
        /// Alpha/opacity for the surface (0.0-1.0), used for home visibility animation
        alpha: f32,
        /// Alpha for blur backdrop (may differ from surface alpha during fade-in)
        blur_alpha: f32,
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
        /// Uniform opacity for the whole workspace (1.0 except during a
        /// `WorkspaceDelta::Crossfade`, where the incoming workspace fades in).
        alpha: f32,
        /// Strict game-mode fullscreen control: when `Some`, this workspace renders
        /// ONLY the controlled set (see [`GameModeView`]). Any other window — e.g.
        /// a Proton game that raw-fullscreens itself before playserve tags + game
        /// mode adopts it — stays completely invisible. `None` renders the
        /// workspace normally.
        game_mode_only: Option<GameModeView<'a>>,
    },
    /// A game-mode overlay (the launcher / a client overlay) composited above
    /// the game at the output origin, honoring the surface's own per-pixel alpha
    /// so the game shows through the transparent regions.
    OverlaySurface {
        surface: &'a CosmicSurface,
    },
}

/// The set of windows strict game-mode control allows on the game's workspace:
/// the adopted `base` plus its own `children` (dialogs, EULA/launcher windows,
/// in-prefix login/browser windows — see `resolve_game_children`). Anything not
/// in this set renders nothing and receives no input.
#[derive(Debug, Clone, Copy)]
pub struct GameModeView<'a> {
    /// The adopted game/launcher surface, presented fullscreen.
    pub base: &'a CosmicSurface,
    /// Windows belonging with the game, stacked above it, FRONT-TO-BACK
    /// (topmost first) — the same order `Workspace::mapped()` yields.
    pub children: &'a [CosmicSurface],
}
pub fn render_input_order<R: Default + 'static>(
    shell: &Shell,
    output: &Output,
    previous: Option<(WorkspaceHandle, usize, WorkspaceDelta)>,
    current: (WorkspaceHandle, usize),
    element_filter: &ElementFilter,
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
    element_filter: &ElementFilter,
    mut callback: impl FnMut(Stage) -> ControlFlow<Result<R, OutputNoMode>, ()>,
) -> ControlFlow<Result<R, OutputNoMode>, ()> {
    // Create home visibility context once for all layer surface filtering
    let home_visibility = HomeVisibilityContext::from_shell(shell);

    // In game mode the fullscreen game is exclusive on its output: suppress all
    // desktop layer-shell surfaces (overlay/top/bottom/background) so the game is
    // the sole render element and can take the primary plane (direct scanout).
    // This is independent of the keyboard-focus heuristic below, which an
    // Xwayland game may not satisfy.
    let game_mode_exclusive = shell.game_mode.active
        && shell
            .workspaces
            .sets
            .get(output)
            .and_then(|set| set.workspaces.iter().find(|w| w.handle == current.0))
            .is_some_and(|w| w.fullscreen_surfaces.iter().any(|f| !f.is_animating()));
    if shell.game_mode.active {
        // Bug 1: when a game workspace's fullscreen is still is_animating() (the
        // entrance), exclusive is false, desktop layers/background still render,
        // and a bufferless game workspace clears to grey — the grey slide.
        trace!(target: GAMING_TARGET, output = %output.name(), game_mode_exclusive, "gm: exclusivity resolved");
    }

    // Strict game-mode control: the game's own workspace renders ONLY the surface
    // game mode controls (its `game_surface`). A game that raw-fullscreens itself
    // lands in that workspace's fullscreen list and would otherwise show; gate it
    // off until playserve tags it and game mode adopts it as the controlled surface
    // (then it renders + fades in like any game).
    //
    // Scoped to the workspace actually holding the controlled surface, NOT to the
    // whole output: the other workspaces on the game's output are an ordinary
    // desktop, so switching to one and launching an app there must keep working.
    let game_mode_controlled: Option<GameModeView<'_>> = (shell.game_mode.active
        && shell.game_mode.output.as_ref() == Some(output))
    .then_some(shell.game_mode.game_surface.as_ref())
    .flatten()
    .filter(|controlled| {
        shell
            .workspaces
            .sets
            .get(output)
            .and_then(|set| set.workspaces.iter().find(|w| w.handle == current.0))
            .is_some_and(|w| {
                w.fullscreen_surfaces
                    .iter()
                    .any(|f| &f.surface == *controlled)
            })
    })
    .map(|base| GameModeView {
        base,
        children: &shell.game_mode.children,
    });

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
    // overlay is above everything (suppressed for an exclusive game).
    if !game_mode_exclusive {
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Overlay, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
        for (layer, location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Overlay, element_filter, &home_visibility)
        {
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
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
    let seat = shell.seats.last_active();
    let fullscreen = workspace.get_fullscreen(seat);
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
    let overview_is_open = workspace_overview_is_open(output);
    let has_focused_fullscreen = game_mode_exclusive
        || if is_active_workspace {
            let current_focus = seat.get_keyboard().unwrap().current_focus();
            matches!(current_focus, Some(KeyboardFocusTarget::Fullscreen(_)))
                || (current_focus.is_none()
                    && focus_stack_is_valid_fullscreen
                    && !workspace_overview_is_open(output))
        } else {
            focus_stack_is_valid_fullscreen && !overview_is_open
        };
    let has_fullscreen = game_mode_exclusive || (fullscreen.is_some() && !overview_is_open);

    // For a transition: `previous` = (outgoing workspace, has_fullscreen, its
    // offset); plus the incoming `current_offset` and the two opacities.
    // A slide keeps both opaque and moves them apart; a Crossfade keeps both at
    // offset 0 (stacked) and fades the incoming one in over the opaque outgoing.
    let (previous, current_offset, previous_alpha, current_alpha) = match previous.as_ref() {
        Some((previous, previous_idx, start)) => {
            let layout = shell.workspaces.layout;

            let Some(workspace) = shell.workspaces.space_for_handle(previous) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            let has_fullscreen = workspace.get_fullscreen(seat).is_some();

            if let WorkspaceDelta::Crossfade(st) = start {
                let t: f32 = ease(
                    EaseInOutCubic,
                    0.0f32,
                    1.0f32,
                    Instant::now().duration_since(*st).as_millis() as f32
                        / shell.theme().motion.slide_crossfade.as_millis() as f32,
                )
                .clamp(0.0, 1.0);
                // Both stacked at offset 0; outgoing opaque (drawn under),
                // incoming fades in (drawn over, emitted first == topmost).
                (
                    Some((previous, has_fullscreen, Point::default())),
                    Point::default(),
                    1.0f32,
                    t,
                )
            } else {
                let (forward, percentage) = match start {
                    WorkspaceDelta::Shortcut(st) => (
                        *previous_idx < current.1,
                        ease(
                            EaseInOutCubic,
                            0.0,
                            1.0,
                            Instant::now().duration_since(*st).as_millis() as f32
                                / shell.theme().motion.animation.as_millis() as f32,
                        ),
                    ),
                    WorkspaceDelta::Gesture {
                        percentage: prog,
                        forward,
                    } => (*forward, *prog as f32),
                    WorkspaceDelta::GestureEnd {
                        start,
                        spring,
                        forward,
                    } => (
                        *forward,
                        (spring.value_at(Instant::now().duration_since(*start)) as f32)
                            .clamp(0.0, 1.0),
                    ),
                    WorkspaceDelta::Crossfade(_) => unreachable!("handled above"),
                };

                let offset = Point::<i32, Logical>::from(match (layout, forward) {
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
                    Point::<i32, Logical>::from(match (layout, forward) {
                        (WorkspaceLayout::Vertical, true) => (0, output_size.h + offset.y),
                        (WorkspaceLayout::Vertical, false) => (0, -(output_size.h - offset.y)),
                        (WorkspaceLayout::Horizontal, true) => (output_size.w + offset.x, 0),
                        (WorkspaceLayout::Horizontal, false) => (-(output_size.w - offset.x), 0),
                    }),
                    1.0f32,
                    1.0f32,
                )
            }
        }
        None => (None, Point::default(), 1.0f32, 1.0f32),
    };

    // Top-level layer shell popups
    if !has_focused_fullscreen {
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Top, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if should_include_windows(element_filter) {
        // overlay redirect windows
        // they need to be over sticky windows, because they could be popups of sticky windows,
        // and we can't differenciate that.
        for (surface, location) in shell
            .override_redirect_windows
            .iter()
            .rev()
            .filter(|or| {
                (*or)
                    .last_configure()
                    .as_global()
                    .intersection(output.geometry())
                    .is_some()
            })
            .map(|or| (or, or.last_configure().loc.as_global()))
        {
            callback(Stage::OverrideRedirect { surface, location })?;
        }

        // sticky window popups — suppressed under an exclusive game so nothing
        // desktop renders over the fullscreen game (real overlays are
        // override-redirect, handled above).
        if !game_mode_exclusive {
            callback(Stage::StickyPopups(&set.sticky_layer))?;
        }
    }

    if should_include_windows(element_filter) {
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
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Bottom, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref()
        && !has_fullscreen
    {
        // previous bottom layer popups
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Bottom, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location: location + offset.as_global(),
            })?;
        }
    }

    if !has_fullscreen {
        // background layer popups
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Background, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref()
        && !has_fullscreen
    {
        // previous background layer popups
        for (layer, popup, location, _alpha) in
            layer_popups(output, Layer::Background, element_filter, &home_visibility)
        {
            callback(Stage::LayerPopup {
                layer,
                popup: &popup,
                location: location + offset.as_global(),
            })?;
        }
    }

    if !has_focused_fullscreen {
        // top-layer shell
        for (layer, location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Top, element_filter, &home_visibility)
        {
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
    }

    // sticky windows — suppressed under an exclusive game (see sticky popups above)
    if should_include_windows(element_filter) && !game_mode_exclusive {
        callback(Stage::Sticky(&set.sticky_layer))?;
    }

    if should_include_windows(element_filter) {
        // Game-mode overlay: the launcher (or a client overlay) composited ABOVE
        // the game. Emitted first (topmost) so it stacks over the game workspace;
        // its own per-pixel alpha lets the game show through the transparent part.
        // Only on the game's own output (game mode is single-output).
        if shell.game_mode.active
            && shell.game_mode.overlay_active
            && shell.game_mode.output.as_ref() == Some(output)
            && let Some(surface) = shell.game_mode.overlay_surface.as_ref()
        {
            trace!(target: GAMING_TARGET, output = %output.name(), overlay_app_id = %surface.app_id(), "overlay stage: EMITTED topmost over game");
            callback(Stage::OverlaySurface { surface })?;
        } else if shell.game_mode.active {
            // BUG 2: the QAM was asserted but the 4-way gate dropped it — surface
            // exactly which sub-condition failed (output mismatch, overlay_active
            // false, or overlay_surface None). Only in game mode so it's not spam.
            trace!(
                target: GAMING_TARGET,
                output = %output.name(),
                overlay_active = shell.game_mode.overlay_active,
                output_match = shell.game_mode.output.as_ref() == Some(output),
                has_surface = shell.game_mode.overlay_surface.is_some(),
                "overlay stage: GATED OUT"
            );
        }

        // workspace windows (the incoming workspace — fades in during a crossfade)
        callback(Stage::Workspace {
            workspace,
            offset: current_offset,
            alpha: current_alpha,
            game_mode_only: game_mode_controlled,
        })?;

        // previous workspace windows (the outgoing workspace)
        if let Some((previous_handle, _, offset)) = previous.as_ref() {
            let Some(workspace) = shell.workspaces.space_for_handle(previous_handle) else {
                return ControlFlow::Break(Err(OutputNoMode));
            };
            callback(Stage::Workspace {
                workspace,
                offset: *offset,
                alpha: previous_alpha,
                game_mode_only: None,
            })?;
        }
    }

    if !has_focused_fullscreen {
        // bottom layer
        for (layer, mut location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Bottom, element_filter, &home_visibility)
        {
            location += current_offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref()
        && !has_fullscreen
    {
        // previous bottom layer
        for (layer, mut location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Bottom, element_filter, &home_visibility)
        {
            location += offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
    }

    if !has_fullscreen {
        // background layer
        for (layer, mut location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Background, element_filter, &home_visibility)
        {
            location += current_offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
    }

    if let Some((_, has_fullscreen, offset)) = previous.as_ref()
        && !has_fullscreen
    {
        // previous background layer
        for (layer, mut location, alpha, blur_alpha) in
            layer_surfaces(output, Layer::Background, element_filter, &home_visibility)
        {
            location += offset.as_global();
            callback(Stage::LayerSurface {
                layer,
                location,
                alpha,
                blur_alpha,
            })?;
        }
    }

    ControlFlow::Continue(())
}

fn layer_popups<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: &'a ElementFilter,
    home_visibility: &'a HomeVisibilityContext,
) -> impl Iterator<Item = (LayerSurface, PopupKind, Point<i32, Global>, f32)> + 'a {
    let output_geo = output.geometry();
    layer_surfaces(output, layer, element_filter, home_visibility).flat_map(
        move |(surface, location, alpha, _home_only)| {
            let location_clone = location;
            let surface_clone = surface.clone();
            PopupManager::popups_for_surface(surface.wl_surface()).map(
                move |(popup, popup_offset)| {
                    let standard_offset = (popup_offset - popup.geometry().loc).as_global();
                    let standard_location = location_clone + standard_offset;

                    // Check for compositor-driven tooltip position override
                    let tooltip_override = crate::wayland::protocols::tooltip::get_tooltip_position(
                        popup.wl_surface(),
                    );

                    tracing::trace!(
                        "[tooltip-layer-render] popup has_override={} standard_loc=({},{})",
                        tooltip_override.is_some(),
                        standard_location.x,
                        standard_location.y,
                    );

                    let final_location = if let Some(override_data) = tooltip_override {
                        let popup_geo = popup.geometry();
                        // popup.geometry() returns (0,0,0,0) when the client
                        // hasn't called xdg_surface.set_window_geometry.
                        // Fall back to the surface tree bounding box.
                        let popup_size = if popup_geo.size.w > 0 && popup_geo.size.h > 0 {
                            popup_geo.size
                        } else {
                            let bbox = bbox_from_surface_tree(popup.wl_surface(), (0, 0));
                            bbox.size
                        };
                        let mut pos = override_data.position;

                        // Adjust for popup geometry offset (content may not start at surface origin)
                        pos.x -= popup_geo.loc.x;
                        pos.y -= popup_geo.loc.y;

                        // Apply anchor-based offset so the correct corner aligns.
                        override_data.anchor.adjust_position(
                            &mut pos.x,
                            &mut pos.y,
                            popup_size.w,
                            popup_size.h,
                        );

                        // Clamp to output bounds so the tooltip stays on-screen
                        let max_x = output_geo.loc.x + output_geo.size.w - popup_size.w;
                        let max_y = output_geo.loc.y + output_geo.size.h - popup_size.h;
                        pos.x = pos.x.max(output_geo.loc.x).min(max_x);
                        pos.y = pos.y.max(output_geo.loc.y).min(max_y);

                        pos
                    } else {
                        standard_location
                    };

                    (surface_clone.clone(), popup, final_location, alpha)
                },
            )
        },
    )
}

/// Get the z-order level of a layer (higher = closer to viewer)
/// Background=0, Bottom=1, Top=2, Overlay=3
fn layer_z_level(layer: Layer) -> u8 {
    match layer {
        Layer::Background => 0,
        Layer::Bottom => 1,
        Layer::Top => 2,
        Layer::Overlay => 3,
    }
}

fn layer_surfaces<'a>(
    output: &'a Output,
    layer: Layer,
    element_filter: &'a ElementFilter,
    home_visibility: &'a HomeVisibilityContext,
) -> impl Iterator<Item = (LayerSurface, Point<i32, Global>, f32, f32)> + 'a {
    // For BlurCapture and LayerBlurCapture modes, use cached layer surfaces to prevent deadlocks
    let use_cache = matches!(
        element_filter,
        ElementFilter::BlurCapture(_) | ElementFilter::LayerBlurCapture(_, _)
    );

    // Skip layers based on blur capture mode:
    // - BlurCapture (window blur): Always skip Top and Overlay layers (they render above windows)
    // - LayerBlurCapture: Skip layers at or above the requesting layer's z-level
    let skip_layer = match element_filter {
        ElementFilter::BlurCapture(_) => {
            // Window blur should only capture Background and Bottom layers
            // Top and Overlay are above windows and should never appear in window blur
            matches!(layer, Layer::Top | Layer::Overlay)
        }
        ElementFilter::LayerBlurCapture(requesting_layer, _) => {
            layer_z_level(layer) >= layer_z_level(*requesting_layer)
        }
        _ => false,
    };

    // we want to avoid deadlocks on the layer-map in callbacks, so we need to clone the layer surfaces
    let layers = if skip_layer {
        // Skip this entire layer for layer blur capture
        Vec::new()
    } else if use_cache {
        // Use cached layer surfaces (populated by main thread)
        crate::backend::render::get_cached_layer_surfaces(&output.name(), layer)
            .into_iter()
            .map(|cached| (cached.surface, cached.location))
            .collect::<Vec<_>>()
    } else {
        let layer_map = layer_map_for_output(output);
        layer_map
            .layers_on(layer)
            .rev()
            .map(|s| (s.clone(), layer_map.layer_geometry(s).unwrap().loc))
            .collect::<Vec<_>>()
    };

    layers.into_iter().filter_map(move |(s, loc)| {
        // Filter out workspace overview namespace
        if *element_filter == ElementFilter::ExcludeWorkspaceOverview
            && s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE
        {
            return None;
        }

        // Get visibility and alpha for this surface using home visibility context
        let surface_id = s.wl_surface().id();
        let namespace = s.namespace();
        let (mut visible, mut alpha) =
            home_visibility.surface_visibility(&surface_id, Some(layer), Some(namespace));

        // Apply layer fade-in alpha if this surface is still fading in
        let is_fading_in =
            if let Some(&fade_alpha) = home_visibility.layer_fade_in_alphas.get(&surface_id) {
                tracing::trace!(
                    surface_protocol_id = s.wl_surface().id().protocol_id(),
                    fade_alpha = format!("{:.3}", fade_alpha),
                    original_alpha = format!("{:.3}", alpha),
                    "layer_surfaces: applying fade-IN alpha"
                );
                alpha *= fade_alpha;
                true
            } else {
                false
            };

        // Apply layer fade-out alpha if this surface is fading out.
        // During fade-out the surface is still visible (not yet in hidden_surfaces)
        // but with decreasing alpha.
        let is_fading_out =
            if let Some(&fade_alpha) = home_visibility.layer_fade_out_alphas.get(&surface_id) {
                tracing::trace!(
                    surface_protocol_id = s.wl_surface().id().protocol_id(),
                    fade_alpha = format!("{:.3}", fade_alpha),
                    "layer_surfaces: applying fade-OUT alpha"
                );
                visible = true;
                alpha = fade_alpha;
                true
            } else {
                false
            };

        // Compute blur alpha: same as surface alpha for fade-in/out (both fade together),
        // but squared during home visibility animation for a softer blur transition
        let blur_alpha = if is_fading_in || is_fading_out {
            let result = alpha;
            tracing::trace!(
                surface_protocol_id = s.wl_surface().id().protocol_id(),
                blur_alpha = format!("{:.3}", result),
                is_fading_in,
                is_fading_out,
                "layer_surfaces: blur_alpha during fade"
            );
            result
        } else {
            let is_animating = alpha < 1.0;
            let has_home_visibility = home_visibility.home_only_surfaces.contains(&surface_id)
                || home_visibility.hide_on_home_surfaces.contains(&surface_id);
            if has_home_visibility && is_animating {
                alpha * alpha
            } else {
                alpha
            }
        };

        // Filter out completely invisible surfaces
        if !visible {
            return None;
        }

        // Use the surface-specific alpha (which considers home_alpha for home-only surfaces)
        // blur_alpha may be more aggressively reduced during home visibility animations
        Some((s, loc.as_local().to_global(output), alpha, blur_alpha))
    })
}
