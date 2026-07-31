// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::PendingLayer, utils::prelude::*};
use smithay::{
    desktop::{LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    output::Output,
    reexports::wayland_server::{Resource, protocol::wl_output::WlOutput},
    wayland::shell::{
        wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
        xdg::PopupSurface,
    },
};

/// All layer types that need to be cached for render thread access
const CACHED_LAYERS: [Layer; 4] = [Layer::Background, Layer::Bottom, Layer::Top, Layer::Overlay];

impl WlrLayerShellHandler for State {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.common.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        surface: WlrLayerSurface,
        wl_output: Option<WlOutput>,
        _layer: Layer,
        namespace: String,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let no_output = wl_output.is_none();
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| seat.active_output());
        let layer_surface = LayerSurface::new(surface, namespace);
        if no_output {
            shell
                .output_agnostic_layers
                .insert(layer_surface.wl_surface().id());
        }
        shell.pending_layers.push(PendingLayer {
            surface: layer_surface,
            output,
            seat,
        });
    }

    fn new_popup(&mut self, _parent: WlrLayerSurface, popup: PopupSurface) {
        self.common
            .shell
            .read()
            .unconstrain_popup(&PopupKind::from(popup.clone()));

        if let Err(err) = popup.send_configure() {
            tracing::warn!("Unable to configure popup. {err:?}",);
        } else {
            self.common
                .popups
                .track_popup(PopupKind::from(popup))
                .unwrap();
        }
    }

    fn layer_destroyed(&mut self, surface: WlrLayerSurface) {
        let surface_id = surface.wl_surface().id();
        let mut shell = self.common.shell.write();

        // Clean up visibility tracking for this surface
        shell.remove_surface_visibility(surface_id.clone());
        shell.remove_hidden_surface(&surface_id);
        shell.remove_client_exclusive_zone(&surface_id);
        shell.remove_layer_fade_in(&surface_id);
        shell.remove_layer_fade_out(&surface_id);
        shell.remove_layer_open(&surface_id);
        shell.remove_layer_close(&surface_id);
        shell.layer_slides.retain(|s| s.surface_id != surface_id);
        shell.output_agnostic_layers.remove(&surface_id);
        shell.exclusive_focus_granted.remove(&surface_id);

        // Release this surface's blurred backdrop. It is a full-output-sized GPU
        // texture, and the cache is otherwise only pruned when a whole output
        // goes away, so skipping this leaks one texture per destroyed layer.

        // Clean up any edge-resize state for this surface: a panel destroyed mid
        // drag/animation must not leave a stuck ghost, grab target, spring or settle.
        // (A stuck settle in particular would keep the dispatch loop re-evaluating
        // edge hover every iteration forever.)
        if shell
            .edge_drag_ghost
            .as_ref()
            .is_some_and(|g| g.surface_id == surface_id)
        {
            shell.edge_drag_ghost = None;
        }
        if shell
            .edge_hover
            .as_ref()
            .is_some_and(|h| h.surface_id == surface_id)
        {
            shell.edge_hover = None;
        }
        if shell
            .active_layer_resize
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.active_layer_resize = None;
        }
        if shell
            .layer_resize_settle
            .as_ref()
            .is_some_and(|r| r.surface_id == surface_id)
        {
            shell.layer_resize_settle = None;
        }
        if shell
            .active_layer_resize_anim
            .as_ref()
            .is_some_and(|a| a.surface_id == surface_id)
        {
            shell.active_layer_resize_anim = None;
        }
        if shell
            .layer_maximize
            .as_ref()
            .is_some_and(|m| m.surface_id == surface_id)
        {
            shell.layer_maximize = None;
        }

        let maybe_output = shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .is_some()
            })
            .cloned();

        if let Some(output) = maybe_output {
            {
                let mut map = layer_map_for_output(&output);
                let layer = map
                    .layer_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                    .unwrap()
                    .clone();
                map.unmap_layer(&layer);
            }

            // Update layer blur cache after unmapping

            shell.workspaces.recalculate();

            self.backend.schedule_render(&output);
        }
    }
}
