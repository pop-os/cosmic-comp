// SPDX-License-Identifier: GPL-3.0-only

use smithay::{delegate_xdg_foreign, wayland::xdg_foreign::{XdgForeignHandler, XdgForeignState}};
use crate::state::State;

impl XdgForeignHandler for State {
    fn xdg_foreign_state(&mut self) -> &mut XdgForeignState {
        &mut self.common.xdg_foreign_state
    }
}

delegate_xdg_foreign!(State);
