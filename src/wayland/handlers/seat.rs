// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    wayland::seat::{SeatHandler, SeatState},
    delegate_seat,
};
use crate::state::State;

impl SeatHandler for State {
    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }
}

delegate_seat!(State);
