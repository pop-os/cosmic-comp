// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, wayland::protocols::toplevel_info::ToplevelInfoHandler};
use smithay::wayland::foreign_toplevel_list::{
    ForeignToplevelListHandler, ForeignToplevelListState,
};

impl ForeignToplevelListHandler for State {
    fn foreign_toplevel_list_state(&mut self) -> &mut ForeignToplevelListState {
        &mut self.toplevel_info_state_mut().foreign_toplevel_list
    }
}

smithay::delegate_foreign_toplevel_list!(State);
