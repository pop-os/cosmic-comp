// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

fn default_workspace_layout() -> WorkspaceLayout {
    WorkspaceLayout::Vertical
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub workspace_mode: WorkspaceMode,
    pub workspace_amount: WorkspaceAmount,
    #[serde(default = "default_workspace_layout")]
    pub workspace_layout: WorkspaceLayout,
}

impl Default for WorkspaceConfig {
    fn default() -> Self {
        Self {
            workspace_mode: WorkspaceMode::OutputBound,
            workspace_amount: WorkspaceAmount::Dynamic,
            workspace_layout: WorkspaceLayout::Vertical,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceAmount {
    Dynamic,
    Static(u8),
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
