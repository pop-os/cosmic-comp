// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_xwayland_shell,
    wayland::xwayland_shell::{XWaylandShellHandler, XWaylandShellState},
};

impl XWaylandShellHandler for State {
    fn xwayland_shell_state(&mut self) -> &mut XWaylandShellState {
        &mut self.common.xwayland_shell_state
    }
}

delegate_xwayland_shell!(State);
