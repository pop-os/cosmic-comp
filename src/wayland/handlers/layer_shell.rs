// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    delegate_layer_shell,
    desktop::LayerSurface,
    reexports::wayland_server::{protocol::wl_output::WlOutput, DisplayHandle},
    wayland::{
        output::Output,
        shell::wlr_layer::{
            Layer, LayerSurface as WlrLayerSurface, WlrLayerShellHandler, WlrLayerShellState,
        },
    },
};

impl WlrLayerShellHandler for State {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.common.shell.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        _dh: &DisplayHandle,
        surface: WlrLayerSurface,
        wl_output: Option<WlOutput>,
        _layer: Layer,
        namespace: String,
    ) {
        super::mark_dirty_on_drop(&self.common, surface.wl_surface());
        let seat = self.common.last_active_seat.clone();
        let output = wl_output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| active_output(&seat, &self.common));
        self.common.shell.pending_layers.push((
            LayerSurface::new(surface, namespace),
            output,
            seat,
        ));
    }
}

delegate_layer_shell!(State);
