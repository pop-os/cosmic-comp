// SPDX-License-Identifier: GPL-3.0-only

use smithay::{desktop::layer_map_for_output, output::Output};

/// Layer shell namespace used by `cosmic-workspaces`
// TODO: Avoid special case, or add protocol to expose required behavior
pub const WORKSPACE_OVERVIEW_NAMESPACE: &str = "cosmic-workspace-overview";

/// Check if a workspace overview shell surface is open on the output
pub fn workspace_overview_is_open(output: &Output) -> bool {
    layer_map_for_output(output)
        .layers()
        .any(|s| s.namespace() == WORKSPACE_OVERVIEW_NAMESPACE)
}
