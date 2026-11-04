// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    backend::renderer::utils::with_renderer_surface_state,
    desktop::layer_map_for_output,
    output::Output,
};

/// Layer shell namespace used by `cosmic-workspaces`
pub const WORKSPACE_OVERVIEW_NAMESPACE: &str = "cosmic-workspace-overview";

/// Layer shell namespace used by the COSMIC App Library
pub const APP_LIBRARY_NAMESPACE: &str = "cosmic-app-library";

/// Check if a workspace overview shell surface is open on the output
pub fn workspace_overview_is_open(output: &Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .filter(|s| s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE)
        .any(|s| {
            with_renderer_surface_state(s.wl_surface(), |state| state.buffer().is_some())
                .unwrap_or(false)
        })
}

/// Check if the App Library shell surface is open on the output
pub fn app_library_is_open(output: &Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .filter(|s| s.namespace() == APP_LIBRARY_NAMESPACE)
        .any(|s| {
            with_renderer_surface_state(s.wl_surface(), |state| state.buffer().is_some())
                .unwrap_or(false)
        })
}