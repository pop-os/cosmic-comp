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

/// Layer shell namespaces holding a screen recorder's own chrome, which is never
/// included in captures.
///
/// Note this is not where the workspace overview belongs, even though it is also
/// kept out of some captures. The overview is excluded only from the previews it
/// renders itself, and should appear in a screen recording like anything else the
/// user can see. See [`crate::backend::render::ElementFilter`].
// TODO: Add protocol to set surfaces that are transparent to capture?
pub const CAPTURE_EXCLUDED_NAMESPACES: &[&str] = &["cosmic-screen-recorder-toolbar"];

/// Whether a layer shell surface with this namespace should be omitted from captures
pub fn namespace_is_capture_excluded(namespace: &str) -> bool {
    CAPTURE_EXCLUDED_NAMESPACES.contains(&namespace)
}

/// Check if any recorder chrome is currently drawing on the output.
///
/// Captures of an output normally hand out the frame the backend already
/// composited for scan-out. That frame has the chrome baked in, so while one is
/// up the capture has to be composited separately with it filtered out.
pub fn output_has_capture_excluded_surface(output: &Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .any(|s| namespace_is_capture_excluded(s.namespace()))
}
