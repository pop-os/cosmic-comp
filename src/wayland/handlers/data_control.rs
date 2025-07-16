// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_data_control,
    wayland::selection::wlr_data_control::{DataControlHandler, DataControlState},
};

impl DataControlHandler for State {
    fn data_control_state(&mut self) -> &mut DataControlState {
        self.common.data_control_state.as_mut().unwrap()
    }
}

delegate_data_control!(State);
