// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

use crate::EdidProduct;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub workspace_mode: WorkspaceMode,
    #[serde(default)]
    pub workspace_layout: WorkspaceLayout,
    #[serde(default)]
    pub action_on_typing: Action,
    #[serde(default = "default_wraparound")]
    pub workspace_wraparound: bool,
    #[serde(default = "default_grid_columns")]
    pub workspace_grid_columns: u32,
    #[serde(default = "default_grid_rows")]
    pub workspace_grid_rows: u32,
}

fn default_wraparound() -> bool {
    true
}

fn default_grid_columns() -> u32 {
    3
}

fn default_grid_rows() -> u32 {
    2
}

impl Default for WorkspaceConfig {
    fn default() -> Self {
        Self {
            workspace_mode: WorkspaceMode::default(),
            workspace_layout: WorkspaceLayout::default(),
            action_on_typing: Action::default(),
            workspace_wraparound: default_wraparound(),
            workspace_grid_columns: default_grid_columns(),
            workspace_grid_rows: default_grid_rows(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceMode {
    #[default]
    OutputBound,
    Global,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceLayout {
    #[default]
    Vertical,
    Horizontal,
    Grid,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    #[default]
    None,
    OpenLauncher,
    OpenApplications,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputMatch {
    pub name: String,
    pub edid: Option<EdidProduct>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PinnedWorkspace {
    pub output: OutputMatch,
    pub tiling_enabled: bool,
    pub id: Option<String>,
    pub name: Option<String>,
}
