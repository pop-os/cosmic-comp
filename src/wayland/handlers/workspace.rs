// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::WorkspaceDelta,
    utils::prelude::*,
    wayland::protocols::workspace::{
        delegate_workspace, Request, State as WState, WorkspaceHandler, WorkspaceState,
    },
};
use cosmic_protocols::workspace::v2::server::zcosmic_workspace_handle_v2::TilingState;
use smithay::reexports::wayland_server::DisplayHandle;

impl WorkspaceHandler for State {
    fn workspace_state(&self) -> &WorkspaceState<Self> {
        &self.common.workspace_state
    }
    fn workspace_state_mut(&mut self) -> &mut WorkspaceState<Self> {
        &mut self.common.workspace_state
    }

    fn commit_requests(&mut self, _dh: &DisplayHandle, requests: Vec<Request>) {
        for request in requests.into_iter() {
            match request {
                Request::Activate(handle) => {
                    let mut shell = self.common.shell.write();
                    let maybe = shell.workspaces.iter().find_map(|(o, set)| {
                        set.workspaces
                            .iter()
                            .position(|w| w.handle == handle)
                            .map(|i| (o.clone(), i))
                    });

                    if let Some((output, idx)) = maybe {
                        let _ = shell.activate(
                            &output,
                            idx,
                            WorkspaceDelta::new_shortcut(),
                            &mut self.common.workspace_state.update(),
                        );
                        // TODO: move cursor?
                    }
                }
                Request::SetTilingState { workspace, state } => {
                    let mut shell = self.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    if let Some(workspace) = shell.workspaces.space_for_handle_mut(&workspace) {
                        let mut guard = self.common.workspace_state.update();
                        workspace.set_tiling(
                            match state.into_result() {
                                Ok(TilingState::FloatingOnly) => false,
                                _ => true,
                            },
                            &seat,
                            &mut guard,
                        );
                    }
                }
                Request::SetPin { workspace, pinned } => {
                    let mut shell = self.common.shell.write();
                    if let Some(workspace) = shell.workspaces.space_for_handle_mut(&workspace) {
                        workspace.pinned = pinned;
                        let mut update = self.common.workspace_state.update();
                        if pinned {
                            if workspace.id.is_none() {
                                let id = crate::shell::random_workspace_id();
                                update
                                    .set_id(&workspace.handle, &id)
                                    .expect("workspace already has id");
                                workspace.id = Some(id);
                            }
                            update.add_workspace_state(&workspace.handle, WState::Pinned);
                            // TODO: Also need to update on changing other properties that are saved
                            shell.workspaces.persist(&self.common.config);
                        } else {
                            update.remove_workspace_state(&workspace.handle, WState::Pinned);
                            shell.workspaces.persist(&self.common.config);
                        }
                    }
                }
                Request::MoveBefore {
                    workspace,
                    other_workspace,
                    axis,
                } => {
                    if axis != 0 {
                        continue;
                    }
                    let mut shell = self.common.shell.write();
                    let mut update = self.common.workspace_state.update();
                    shell.workspaces.move_workspace(
                        &workspace,
                        &other_workspace,
                        &mut update,
                        false,
                    );
                }
                Request::MoveAfter {
                    workspace,
                    other_workspace,
                    axis,
                } => {
                    if axis != 0 {
                        continue;
                    }
                    let mut shell = self.common.shell.write();
                    let mut update = self.common.workspace_state.update();
                    shell.workspaces.move_workspace(
                        &workspace,
                        &other_workspace,
                        &mut update,
                        true,
                    );
                }
                _ => {}
            }
        }
    }
}

delegate_workspace!(State);
