// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::wayland_server::DisplayHandle;
use crate::{
    state::ClientState,
    wayland::protocols::workspace::{
        Request,
        WorkspaceHandler,
        WorkspaceState,
        WorkspaceClientHandler,
        WorkspaceClientState,
        delegate_workspace,
    },
    utils::prelude::*,
};

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
                    if let Some(idx) = self.common.shell.spaces.iter().position(|w| w.handle == handle) {
                        let seat = &self.common.last_active_seat;
                        let output = active_output(seat, &self.common);
                        self.common.shell.activate(seat, &output, idx);
                    }
                },
                _ => {},
            }
        }
    }
}

delegate_workspace!(State);
