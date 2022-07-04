// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_shm,
    wayland::shm::{ShmHandler, ShmState},
};

impl ShmHandler for State {
    fn shm_state(&self) -> &ShmState {
        &self.common.shm_state
    }
}

delegate_shm!(State);
