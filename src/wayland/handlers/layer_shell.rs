// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    delegate_layer_shell,
    desktop::{layer_map_for_output, LayerSurface, PopupKind, WindowSurfaceType},
    output::Output,
    reexports::wayland_server::protocol::wl_output::WlOutput,
    wayland::shell::{
        wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
        xdg::PopupSurface,
    },
};

use super::screencopy::PendingScreencopyBuffers;

impl WlrLayerShellHandler for State {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.common.shell.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        surface: WlrLayerSurface,
        wl_output: Option<WlOutput>,
        _layer: Layer,
        namespace: String,
    ) {
        let seat = self.common.last_active_seat().clone();
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| seat.active_output());
        self.common.shell.pending_layers.push((
            LayerSurface::new(surface, namespace),
            output,
            seat,
        ));
    }

    fn new_popup(&mut self, _parent: WlrLayerSurface, popup: PopupSurface) {
        let positioner = popup.with_pending_state(|state| state.positioner);
        self.common.shell.unconstrain_popup(&popup, &positioner);

        if popup.send_configure().is_ok() {
            self.common
                .shell
                .popups
                .track_popup(PopupKind::from(popup))
                .unwrap();
        }
    }

    fn layer_destroyed(&mut self, surface: WlrLayerSurface) {
        let maybe_output = self
            .common
            .shell
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

            for workspace in self.common.shell.workspaces.spaces_mut() {
                workspace.recalculate(&output);
            }

            // collect screencopy sessions needing an update
            let mut scheduled_sessions = self.schedule_workspace_sessions(surface.wl_surface());
            if let Some(sessions) = output.user_data().get::<PendingScreencopyBuffers>() {
                scheduled_sessions
                    .get_or_insert_with(Vec::new)
                    .extend(sessions.borrow_mut().drain(..));
            }

            self.backend.schedule_render(
                &self.common.event_loop_handle,
                &output,
                scheduled_sessions,
            );
        }
    }
}

delegate_layer_shell!(State);
