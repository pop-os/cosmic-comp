// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    wayland::shm::{ShmHandler, ShmState},
    delegate_shm,
};
use crate::state::State;

impl ShmHandler for State {
    fn shm_state(&self) -> &ShmState {
        &self.common.shm_state
    }
}

delegate_shm!(State);
