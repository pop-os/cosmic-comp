// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_seat,
    wayland::seat::{SeatHandler, SeatState},
};

impl SeatHandler for State {
    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }
}

delegate_seat!(State);
