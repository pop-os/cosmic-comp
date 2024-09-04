// SPDX-License-Identifier: GPL-3.0-only

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct WorkspaceConfig {
    pub workspace_mode: WorkspaceMode,
    #[serde(default)]
    pub workspace_layout: WorkspaceLayout,
    pub workspace_thumbnail_placement: WorkspaceThumbnailPlacement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum WorkspaceMode {
    #[default]
    OutputBound,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[repr(usize)]
pub enum WorkspaceThumbnailPlacement {
    Top = 0,
    Bottom = 1,
    #[default]
    Left = 2,
    Right = 3,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum WorkspaceLayout {
    #[default]
    Vertical,
    Horizontal,
}
