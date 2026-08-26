// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    backend::renderer::utils::with_renderer_surface_state, desktop::layer_map_for_output,
    output::Output,
};

/// Layer shell namespace used by `cosmic-workspaces`
// TODO: Avoid special case, or add protocol to expose required behavior
pub const WORKSPACE_OVERVIEW_NAMESPACE: &str = "cosmic-workspace-overview";

/// Check if a workspace overview shell surface is open on the output
pub fn workspace_overview_is_open(output: &Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .filter(|s| s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE)
        // Only consider the overview open once it has committed a buffer. The
        // surface is inserted into the layer map on its initial (bufferless)
        // commit, so checking for the namespace alone hides all toplevels for a
        // frame before the overview has anything to draw, briefly flashing the
        // bare wallpaper.
        .any(|s| {
            with_renderer_surface_state(s.wl_surface(), |state| state.buffer().is_some())
                .unwrap_or(false)
        })
}
