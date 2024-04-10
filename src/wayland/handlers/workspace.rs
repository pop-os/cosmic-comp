// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::WorkspaceDelta,
    state::ClientState,
    utils::prelude::*,
    wayland::protocols::workspace::{
        delegate_workspace, Request, WorkspaceClientHandler, WorkspaceClientState,
        WorkspaceHandler, WorkspaceState,
    },
};
use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::TilingState;
use smithay::reexports::wayland_server::DisplayHandle;

impl WorkspaceClientHandler for ClientState {
    fn workspace_state(&self) -> &WorkspaceClientState {
        &self.workspace_client_state
    }
}

impl WorkspaceHandler for State {
    type Client = ClientState;
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
                    let mut shell = self.common.shell.write().unwrap();
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
                    let mut shell = self.common.shell.write().unwrap();
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
                _ => {}
            }
        }
    }
}

delegate_workspace!(State);
