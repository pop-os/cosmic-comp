// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

fn default_workspace_layout() -> WorkspaceLayout {
    WorkspaceLayout::Vertical
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub workspace_mode: WorkspaceMode,
    #[serde(default = "default_workspace_layout")]
    pub workspace_layout: WorkspaceLayout,
    #[serde(default)]
    pub remove_empty: RemoveEmpty,
}

/// Setting of which empty workspaces the compositor should automatically remove.
#[derive(Clone, Copy, Debug, Default, PartialEq, Serialize, Deserialize)]
pub enum RemoveEmpty {
    /// Remove all empty workspaces, except the last one.
    #[default]
    All,

    /// Remove only trailing empty workspaces, after the last one.
    Trailing,
}

impl Default for WorkspaceConfig {
    fn default() -> Self {
        Self {
            workspace_mode: WorkspaceMode::OutputBound,
            workspace_layout: WorkspaceLayout::Vertical,
            remove_empty: RemoveEmpty::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceMode {
    OutputBound,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceLayout {
    Vertical,
    Horizontal,
}
