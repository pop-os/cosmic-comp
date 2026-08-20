// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::State,
    wayland::protocols::session_hold::{
        SessionHoldHandler, SessionHoldState, delegate_session_hold,
    },
};

impl SessionHoldHandler for State {
    fn session_hold_state(&mut self) -> &mut SessionHoldState {
        &mut self.common.session_hold_state
    }

    fn session_hold_taken(&mut self, timeout_ms: u32) {
        let mut shell = self.common.shell.write();
        shell.take_handoff_hold(timeout_ms);
    }

    fn session_hold_released(&mut self) {
        let mut shell = self.common.shell.write();
        shell.release_handoff_hold();
    }
}

delegate_session_hold!(State);
