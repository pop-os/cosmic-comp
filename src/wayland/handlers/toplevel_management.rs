// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{WindowSurfaceType, layer_map_for_output},
    input::{Seat, pointer::MotionEvent},
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Logical, Point, Rectangle, SERIAL_COUNTER, Size},
    wayland::seat::WaylandFocus,
};
use tracing::trace;

use crate::{
    shell::{CosmicSurface, Shell, WorkspaceDelta, focus::target::KeyboardFocusTarget},
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoHandler,
        toplevel_management::{
            ManagementWindow, ToplevelManagementHandler, ToplevelManagementState,
            delegate_toplevel_management, toplevel_rectangle_for,
        },
        workspace::WorkspaceHandle,
    },
};

impl ToplevelManagementHandler for State {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState {
        &mut self.common.toplevel_management_state
    }

    fn activate(
        &mut self,
        dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        seat: Option<Seat<Self>>,
    ) {
        self.unminimize(dh, window);

        let mut shell = self.common.shell.write();
        for output in shell.outputs().cloned().collect::<Vec<_>>().iter() {
            let maybe = shell
                .workspaces
                .spaces_for_output(output)
                .enumerate()
                .find(|(_, w)| {
                    w.get_fullscreen_surfaces().any(|f| &f.surface == window)
                        || w.mapped()
                            .flat_map(|m| m.windows().map(|(s, _)| s))
                            .any(|w| &w == window)
                });

            let seat = seat.clone().unwrap_or(shell.seats.last_active().clone());
            let (target, new_pos) = if let Some((idx, workspace)) = maybe {
                let handle = workspace.handle;
                let new_pos = shell.activate(
                    output,
                    idx,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );

                let workspace = shell.workspaces.space_for_handle_mut(&handle).unwrap();
                if seat
                    .get_keyboard()
                    .unwrap()
                    .current_focus()
                    .is_some_and(|focus| !focus.windows().any(|w| w == *window))
                    && workspace.is_tiled(window)
                {
                    for mapped in workspace
                        .mapped()
                        .filter(|m| {
                            m.maximized_state.lock().unwrap().is_some()
                                && !m.windows().any(|(ref w, _)| w == window)
                        })
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        workspace.unmaximize_request(&mapped);
                    }
                }

                let target = if let Some(mapped) = workspace.element_for_surface(window) {
                    mapped.focus_window(window);
                    KeyboardFocusTarget::Element(mapped.clone())
                } else {
                    KeyboardFocusTarget::Fullscreen(window.clone())
                };

                (target, new_pos.ok())
            // sticky window?
            } else if let Some(mapped) = shell
                .workspaces
                .sets
                .get(output)
                .unwrap()
                .sticky_layer
                .mapped()
                .find(|m| m.windows().any(|(w, _)| &w == window))
            {
                mapped.focus_window(window);

                let output_geo = output.geometry();
                let new_pos =
                    output_geo.loc + Point::from((output_geo.size.w / 2, output_geo.size.h / 2));
                (KeyboardFocusTarget::Element(mapped.clone()), Some(new_pos))
            } else {
                continue;
            };

            std::mem::drop(shell);

            // move pointer to window if it’s on a different monitor/output
            if seat.active_output() != *output
                && self.common.config.cosmic_conf.cursor_follows_focus
                && let Some(new_pos) = new_pos
            {
                seat.set_active_output(output);
                if let Some(ptr) = seat.get_pointer() {
                    let serial = SERIAL_COUNTER.next_serial();
                    ptr.motion(
                        self,
                        None,
                        &MotionEvent {
                            location: new_pos.to_f64().as_logical(),
                            serial,
                            time: self.common.clock.now().as_millis(),
                        },
                    );
                    ptr.frame(self);
                }
            }

            Shell::set_focus(self, Some(&target), &seat, None, false);
            return;
        }
    }

    fn close(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        window.close();
    }

    fn move_to_workspace(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        to_handle: WorkspaceHandle,
        _output: Output,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let Some(surface) = window.wl_surface() else {
            return;
        };
        let Some((from_workspace, _)) = shell.workspace_for_surface(&surface) else {
            return;
        };

        let res = shell.move_window(
            Some(&seat),
            window,
            &from_workspace,
            &to_handle,
            false,
            None,
            &mut self.common.workspace_state.update(),
            &self.common.event_loop_handle,
        );
        if let Some((target, _)) = res {
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    fn fullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        output: Option<Output>,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = output
            .or_else(|| {
                window
                    .wl_surface()
                    .and_then(|surface| shell.visible_output_for_surface(&surface).cloned())
            })
            .unwrap_or_else(|| seat.focused_or_active_output());
        if let Some(target) =
            shell.fullscreen_request(window, output, &self.common.event_loop_handle)
        {
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    fn unfullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        let mut shell = self.common.shell.write();
        let _ = shell.unfullscreen_request(window, &self.common.event_loop_handle);
        // don't switch focus because of a programmatic action.
        // If the toplevel-management client intends to focus the now unfullscreened toplevel, it can send an `activate`-request.
    }

    fn maximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat, true, &self.common.event_loop_handle);
        }
    }

    fn unmaximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            shell.unmaximize_request(&mapped);
        }
    }

    fn minimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        shell.minimize_request(window);
    }

    fn unminimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        shell.unminimize_request(window, &seat, &self.common.event_loop_handle);
    }

    fn set_sticky(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if window.is_sticky() {
            return;
        }

        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.toggle_sticky(&seat, &mapped);
        }
    }

    fn unset_sticky(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        if !window.is_sticky() {
            return;
        }

        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.toggle_sticky(&seat, &mapped);
        }
    }
}

impl ManagementWindow for CosmicSurface {
    fn close(&self) {
        CosmicSurface::close(self)
    }
}

/// Side length in logical pixels of the fallback minimize target.
///
/// Roughly the size of a panel applet, so that the fallback animation reads the same as an
/// animation into an actual applet target.
const FALLBACK_MINIMIZE_SIZE: i32 = 64;

/// Gap in logical pixels between the fallback minimize target and the bottom edge of the
/// non-exclusive zone.
const FALLBACK_MINIMIZE_MARGIN: i32 = 8;

/// Why a client-provided minimize rectangle was discarded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RejectedCandidate {
    /// The rectangle has no area, so there is nothing to animate into.
    ZeroSized,
    /// The rectangle lies completely outside of the output the window is minimized on.
    OutsideOutput,
}

impl RejectedCandidate {
    fn reason(&self) -> &'static str {
        match self {
            RejectedCandidate::ZeroSized => "zero-sized",
            RejectedCandidate::OutsideOutput => "outside of the output",
        }
    }
}

fn validate_minimize_candidate(
    candidate: Rectangle<i32, Logical>,
    output_bounds: Rectangle<i32, Logical>,
) -> Result<(), RejectedCandidate> {
    if candidate.size.w <= 0 || candidate.size.h <= 0 {
        return Err(RejectedCandidate::ZeroSized);
    }
    if !output_bounds.overlaps(candidate) {
        return Err(RejectedCandidate::OutsideOutput);
    }
    Ok(())
}

/// Pick the minimize target a client set for this toplevel.
///
/// Rectangles are stored per-surface in the order clients first set them and the first usable
/// one wins. That is the same selection this function has always made; the only change is that
/// unusable rectangles are now skipped instead of being taken and animated into.
///
/// This means the choice between several clients still depends on their start-up order. In
/// practice cosmic-panel sets at most one rectangle per output, so there is normally only one
/// candidate to begin with. Making the choice order-independent is a separate policy question
/// and deliberately not part of this fix.
///
/// Note that candidates after the selected one are never examined, so only rejections up to the
/// winner are traced. The caller logs the total and resolved counts.
fn select_minimize_candidate(
    candidates: impl IntoIterator<Item = Rectangle<i32, Logical>>,
    output_bounds: Rectangle<i32, Logical>,
) -> Option<Rectangle<i32, Logical>> {
    candidates.into_iter().find(|candidate| {
        match validate_minimize_candidate(*candidate, output_bounds) {
            Ok(()) => true,
            Err(rejected) => {
                trace!(
                    ?candidate,
                    reason = rejected.reason(),
                    "Discarding minimize rectangle"
                );
                false
            }
        }
    })
}

/// Fallback minimize target, used whenever no client provided a usable rectangle.
///
/// A square just inside the bottom edge of the non-exclusive zone, horizontally centered, so
/// the animation ends where a dock would be and keeps the same shape as an applet target. Size
/// and position are clamped into the output, so portrait, landscape and very small outputs all
/// produce a fully visible rectangle.
fn fallback_minimize_rectangle(
    output_bounds: Rectangle<i32, Logical>,
    non_exclusive_zone: Rectangle<i32, Logical>,
) -> Rectangle<i32, Logical> {
    let side = FALLBACK_MINIMIZE_SIZE
        .min(non_exclusive_zone.size.w)
        .min(non_exclusive_zone.size.h)
        .min(output_bounds.size.w)
        .min(output_bounds.size.h)
        .max(1);
    let margin = FALLBACK_MINIMIZE_MARGIN.min((non_exclusive_zone.size.h - side).max(0));

    let x = non_exclusive_zone.loc.x + (non_exclusive_zone.size.w - side) / 2;
    let y = non_exclusive_zone.loc.y + non_exclusive_zone.size.h - side - margin;

    let max_x = (output_bounds.loc.x + output_bounds.size.w - side).max(output_bounds.loc.x);
    let max_y = (output_bounds.loc.y + output_bounds.size.h - side).max(output_bounds.loc.y);

    Rectangle::new(
        Point::from((
            x.clamp(output_bounds.loc.x, max_x),
            y.clamp(output_bounds.loc.y, max_y),
        )),
        Size::from((side, side)),
    )
}

pub fn minimize_rectangle(output: &Output, window: &CosmicSurface) -> Rectangle<i32, Local> {
    let output_bounds = Rectangle::from_size(output.geometry().size.as_logical());
    let map = layer_map_for_output(output);
    let non_exclusive_zone = map.non_exclusive_zone();

    let requested = toplevel_rectangle_for(window).collect::<Vec<_>>();
    let candidates = requested
        .iter()
        .filter_map(|(surface, relative)| {
            let Some(layer_geometry) = map
                .layer_for_surface(surface, WindowSurfaceType::ALL)
                .and_then(|layer| map.layer_geometry(layer))
            else {
                trace!(
                    candidate = ?relative,
                    reason = "surface is not a layer surface mapped on this output",
                    "Discarding minimize rectangle"
                );
                return None;
            };
            Some(Rectangle::new(
                layer_geometry.loc + relative.loc,
                relative.size,
            ))
        })
        .collect::<Vec<_>>();

    let requested_count = requested.len();
    let resolved_count = candidates.len();
    let target = select_minimize_candidate(candidates, output_bounds);
    std::mem::drop(map);

    match target {
        Some(rectangle) => {
            trace!(
                requested = requested_count,
                resolved = resolved_count,
                ?rectangle,
                "Minimizing into applet-provided target"
            );
            rectangle.as_local()
        }
        None => {
            let rectangle = fallback_minimize_rectangle(output_bounds, non_exclusive_zone);
            trace!(
                requested = requested_count,
                resolved = resolved_count,
                ?rectangle,
                "Minimizing into fallback target"
            );
            rectangle.as_local()
        }
    }
}

delegate_toplevel_management!(State);

#[cfg(test)]
mod tests {
    use super::*;

    fn rect(x: i32, y: i32, w: i32, h: i32) -> Rectangle<i32, Logical> {
        Rectangle::new(Point::from((x, y)), Size::from((w, h)))
    }

    fn output(w: i32, h: i32) -> Rectangle<i32, Logical> {
        Rectangle::from_size(Size::from((w, h)))
    }

    #[test]
    fn no_candidate_selects_nothing() {
        assert_eq!(select_minimize_candidate([], output(1920, 1080)), None);
    }

    #[test]
    fn single_valid_candidate_is_selected() {
        let dock = rect(900, 1020, 48, 48);
        assert_eq!(
            select_minimize_candidate([dock], output(1920, 1080)),
            Some(dock)
        );
    }

    #[test]
    fn zero_sized_candidates_are_ignored() {
        let bounds = output(1920, 1080);
        let valid = rect(900, 1020, 48, 48);

        // note: `Size` refuses to be constructed from negative values, so a rectangle with a
        // negative size cannot reach us in the first place.
        for degenerate in [
            rect(900, 1020, 0, 48),
            rect(900, 1020, 48, 0),
            rect(900, 1020, 0, 0),
        ] {
            assert_eq!(
                validate_minimize_candidate(degenerate, bounds),
                Err(RejectedCandidate::ZeroSized)
            );
            assert_eq!(select_minimize_candidate([degenerate], bounds), None);
            // a degenerate candidate never beats a valid one, whichever way around they come
            assert_eq!(
                select_minimize_candidate([degenerate, valid], bounds),
                Some(valid)
            );
            assert_eq!(
                select_minimize_candidate([valid, degenerate], bounds),
                Some(valid)
            );
        }
    }

    #[test]
    fn candidates_outside_the_output_are_ignored() {
        let bounds = output(1920, 1080);

        // fully past the bottom edge, e.g. a dock rectangle for a different output
        let offscreen = rect(900, 1080, 48, 48);
        assert_eq!(
            validate_minimize_candidate(offscreen, bounds),
            Err(RejectedCandidate::OutsideOutput)
        );
        assert_eq!(select_minimize_candidate([offscreen], bounds), None);

        // negative coordinates are equally out of reach
        assert_eq!(
            select_minimize_candidate([rect(-100, -100, 48, 48)], bounds),
            None
        );

        // partially overlapping is still usable
        let partial = rect(900, 1060, 48, 48);
        assert_eq!(validate_minimize_candidate(partial, bounds), Ok(()));
        assert_eq!(select_minimize_candidate([partial], bounds), Some(partial));
    }

    #[test]
    fn the_first_valid_candidate_wins() {
        let bounds = output(1920, 1080);
        let top_panel = rect(60, 4, 32, 32);
        let dock = rect(900, 1020, 48, 48);
        let offscreen = rect(900, 1080, 48, 48);

        // invalid candidates are skipped over rather than ending the search
        assert_eq!(
            select_minimize_candidate([offscreen, dock], bounds),
            Some(dock)
        );
        assert_eq!(
            select_minimize_candidate([offscreen, rect(0, 0, 0, 0), top_panel, dock], bounds),
            Some(top_panel)
        );

        // among several valid candidates the first one still wins, so the result depends on the
        // order clients set their rectangles in. Documented here so a change of policy is a
        // deliberate change of this test.
        assert_eq!(
            select_minimize_candidate([top_panel, dock], bounds),
            Some(top_panel)
        );
        assert_eq!(
            select_minimize_candidate([dock, top_panel], bounds),
            Some(dock)
        );
    }

    #[test]
    fn fallback_is_centered_near_the_bottom_of_the_non_exclusive_zone() {
        let bounds = output(1920, 1080);
        let zone = output(1920, 1080);
        let fallback = fallback_minimize_rectangle(bounds, zone);

        assert_eq!(fallback.size, Size::from((64, 64)));
        // horizontally centered
        assert_eq!(fallback.loc.x, (1920 - 64) / 2);
        assert_eq!(
            fallback.loc.x + fallback.size.w / 2,
            zone.loc.x + zone.size.w / 2
        );
        // just inside the bottom edge
        assert_eq!(fallback.loc.y + fallback.size.h, 1080 - 8);
    }

    #[test]
    fn fallback_respects_an_exclusive_dock_zone() {
        let bounds = output(1920, 1080);
        // a 60px dock at the bottom shrinks the non-exclusive zone
        let zone = rect(0, 0, 1920, 1020);
        let fallback = fallback_minimize_rectangle(bounds, zone);

        assert_eq!(fallback.loc.y + fallback.size.h, 1020 - 8);
        assert!(bounds.contains_rect(fallback));
    }

    #[test]
    fn fallback_stays_inside_portrait_landscape_and_tiny_outputs() {
        for (w, h) in [
            (1920, 1080),
            (1080, 1920),
            (3840, 1080),
            (640, 480),
            (100, 100),
            (64, 64),
            (32, 48),
            (1, 1),
        ] {
            let bounds = output(w, h);
            let fallback = fallback_minimize_rectangle(bounds, bounds);

            assert!(
                fallback.size.w > 0 && fallback.size.h > 0,
                "{w}x{h} produced an empty fallback: {fallback:?}"
            );
            assert_eq!(
                fallback.size.w, fallback.size.h,
                "{w}x{h} produced a non-square fallback: {fallback:?}"
            );
            assert!(
                bounds.contains_rect(fallback),
                "{w}x{h} produced an out-of-bounds fallback: {fallback:?}"
            );
        }
    }

    #[test]
    fn fallback_stays_inside_a_zone_smaller_than_the_target() {
        let bounds = output(1920, 1080);
        // a panel and a dock leaving only a sliver of non-exclusive space
        let zone = rect(0, 500, 1920, 40);
        let fallback = fallback_minimize_rectangle(bounds, zone);

        assert_eq!(fallback.size, Size::from((40, 40)));
        assert!(bounds.contains_rect(fallback));
    }
}
