// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::WorkspaceMode,
    state::ClientState,
    utils::prelude::*,
    wayland::protocols::workspace::{
        delegate_workspace, Request, WorkspaceClientHandler, WorkspaceClientState,
        WorkspaceHandler, WorkspaceState,
    },
};
use smithay::reexports::wayland_server::DisplayHandle;

impl WorkspaceClientHandler for ClientState {
    fn workspace_state(&self) -> &WorkspaceClientState {
        &self.workspace_client_state
    }
}

impl WorkspaceHandler for State {
    type Client = ClientState;
    fn workspace_state(&self) -> &WorkspaceState<Self> {
        &self.common.shell.workspace_state
    }
    fn workspace_state_mut(&mut self) -> &mut WorkspaceState<Self> {
        &mut self.common.shell.workspace_state
    }

    fn commit_requests(&mut self, _dh: &DisplayHandle, requests: Vec<Request>) {
        for request in requests.into_iter() {
            match request {
                Request::Activate(handle) => {
                    let maybe = match &self.common.shell.workspaces {
                        WorkspaceMode::Global(set) => set
                            .workspaces
                            .iter()
                            .position(|w| w.handle == handle)
                            .map(|i| (self.common.last_active_seat().active_output(), i)),
                        WorkspaceMode::OutputBound(sets, _) => sets.iter().find_map(|(o, set)| {
                            set.workspaces
                                .iter()
                                .position(|w| w.handle == handle)
                                .map(|i| (o.clone(), i))
                        }),
                    };

                    if let Some((output, idx)) = maybe {
                        let _ = self.common.shell.activate(&output, idx); // TODO: move cursor?
                    }
                }
                _ => {}
            }
        }
    }
}

delegate_workspace!(State);
