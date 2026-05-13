// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::wayland::shm::{ShmHandler, ShmState};

impl ShmHandler for State {
    fn shm_state(&self) -> &ShmState {
        &self.common.shm_state
    }
}
