// SPDX-License-Identifier: GPL-3.0-only

use crate::delegate_layer_surface_visibility;
use crate::shell::SeatExt;
use crate::shell::Shell;
use crate::shell::focus::target::KeyboardFocusTarget;
use crate::state::State;
use crate::wayland::protocols::layer_surface_visibility::{
    LayerSurfaceVisibilityHandler, LayerSurfaceVisibilityState, LayerTransition,
};
use smithay::desktop::layer_map_for_output;
use smithay::reexports::wayland_server::Resource;
use smithay::wayland::compositor::with_states;
use smithay::wayland::shell::wlr_layer::{KeyboardInteractivity, Layer, LayerSurfaceCachedState};
use wayland_backend::server::ObjectId;

impl LayerSurfaceVisibilityHandler for State {
    fn layer_surface_visibility_state(&self) -> &LayerSurfaceVisibilityState {
        &self.common.layer_surface_visibility_state
    }

    fn set_surface_hidden(&mut self, surface_id: ObjectId, hidden: bool) {
        let mut shell = self.common.shell.write();
        shell.set_surface_hidden(surface_id.clone(), hidden);

        // Clear exclusive focus tracking so the commit handler will
        // re-grant focus on the next show→Exclusive transition.
        if hidden {
            shell.exclusive_focus_granted.remove(&surface_id);
        }

        let is_agnostic = shell.is_output_agnostic_layer(&surface_id);
        tracing::debug!(
            surface_id = surface_id.protocol_id(),
            hidden,
            is_agnostic,
            "set_surface_hidden"
        );

        // For output-agnostic layer surfaces becoming visible, move them
        // to the output where the cursor currently is.
        if !hidden && is_agnostic {
            let cursor_output = shell.seats.last_active().active_output();
            let current_output = shell
                .outputs()
                .find(|o| {
                    let map = layer_map_for_output(o);
                    map.layers().any(|l| l.wl_surface().id() == surface_id)
                })
                .cloned();

            tracing::debug!(
                cursor_output = cursor_output.name(),
                current_output = current_output.as_ref().map(|o| o.name()),
                "Output-agnostic surface show: checking if move needed"
            );

            if let Some(ref old_output) = current_output
                && old_output != &cursor_output
            {
                // Find the layer surface and move it
                let layer = {
                    let map = layer_map_for_output(old_output);
                    map.layers()
                        .find(|l| l.wl_surface().id() == surface_id)
                        .cloned()
                };
                if let Some(layer) = layer {
                    tracing::debug!(
                        from = old_output.name(),
                        to = cursor_output.name(),
                        "Moving layer surface between outputs"
                    );
                    {
                        let mut old_map = layer_map_for_output(old_output);
                        old_map.unmap_layer(&layer);
                    }
                    {
                        let mut new_map = layer_map_for_output(&cursor_output);
                        let _ = new_map.map_layer(&layer);
                    }
                    // Update old output's blur cache (surface is gone from its map,
                    // so the general block below won't find it there).
                    shell.workspaces.recalculate();
                    self.backend.schedule_render(old_output);
                    self.backend.schedule_render(&cursor_output);
                }
            }
        }

        // Update layer blur cache for all outputs that have this surface.
        // When hiding, this removes the surface from blur processing.
        // When showing, it re-adds it.
        {
            let outputs_with_surface: Vec<_> = shell
                .outputs()
                .filter(|o| {
                    let map = layer_map_for_output(o);
                    map.layers().any(|l| l.wl_surface().id() == surface_id)
                })
                .cloned()
                .collect();
            for _output in &outputs_with_surface {}
        }

        if hidden {
            // When hiding a surface, clear keyboard focus from it so it
            // doesn't steal interactivity while invisible.
            let seats_to_clear: Vec<_> = shell
                .seats
                .iter()
                .filter_map(|seat| {
                    let keyboard = seat.get_keyboard()?;
                    if let Some(KeyboardFocusTarget::LayerSurface(ref layer)) =
                        keyboard.current_focus()
                        && layer.wl_surface().id() == surface_id
                    {
                        return Some(seat.clone());
                    }
                    None
                })
                .collect();
            std::mem::drop(shell);

            for seat in seats_to_clear {
                // Restore focus to the normal target instead of clearing to None: a
                // None focus is never repaired (refresh_focus only replaces INVALID
                // targets, and None is not invalid), so after a start-hidden overlay
                // (e.g. the launcher) mapped, took focus and was hidden, typing went
                // to nobody for the rest of the session.
                let target = {
                    use crate::shell::SeatExt as _;
                    let shell = self.common.shell.read();
                    let output = seat.active_output();
                    crate::shell::focus::update_focus_target(&shell, &seat, &output)
                };
                Shell::set_focus(self, target.as_ref(), &seat, None, false);
            }
        } else {
            // Surface becoming visible — grant keyboard focus if it has
            // exclusive/on-demand interactivity on a Top or Overlay layer
            // (e.g. the launcher overlay).
            let focus_target = 'target: {
                for output in shell.outputs().cloned().collect::<Vec<_>>() {
                    let map = layer_map_for_output(&output);
                    for layer in map.layers() {
                        if layer.wl_surface().id() == surface_id {
                            let wants_focus = with_states(layer.wl_surface(), |states| {
                                let mut cached =
                                    states.cached_state.get::<LayerSurfaceCachedState>();
                                let current = cached.current();
                                matches!(current.layer, Layer::Top | Layer::Overlay)
                                    && current.keyboard_interactivity != KeyboardInteractivity::None
                            });
                            if wants_focus {
                                let seat = shell.seats.last_active().clone();
                                let target: KeyboardFocusTarget = layer.clone().into();
                                break 'target Some((target, seat));
                            }
                        }
                    }
                }
                None
            };
            std::mem::drop(shell);

            if let Some((target, seat)) = focus_target {
                Shell::set_focus(self, Some(&target), &seat, None, false);
            }
        }
    }

    fn set_surface_transition(&mut self, surface_id: ObjectId, transition: LayerTransition) {
        let mut shell = self.common.shell.write();
        shell.set_surface_transition(surface_id, transition);
    }

    fn is_surface_hidden(&self, surface_id: &ObjectId) -> bool {
        let shell = self.common.shell.read();
        shell.is_surface_hidden(surface_id)
    }
}

delegate_layer_surface_visibility!(State);
