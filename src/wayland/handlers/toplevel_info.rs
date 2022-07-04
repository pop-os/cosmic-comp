// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::toplevel_info::{
        ToplevelInfoHandler,
        ToplevelInfoState,
        delegate_toplevel_info,
    },
};

impl ToplevelInfoHandler for State {
    fn toplevel_info_state(&self) -> &ToplevelInfoState<State> {
        &self.common.shell.toplevel_info_state
    }
    fn toplevel_info_state_mut(&mut self) -> &mut ToplevelInfoState<State> {
        &mut self.common.shell.toplevel_info_state
    }
}

delegate_toplevel_info!(State);
