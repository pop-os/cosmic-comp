// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    shell::Devices,
    state::State,
};
use smithay::{
    delegate_seat,
    input::{keyboard::LedState, pointer::CursorImageStatus, SeatHandler, SeatState},
};
use std::cell::RefCell;

impl SeatHandler for State {
    type KeyboardFocus = KeyboardFocusTarget;
    type PointerFocus = PointerFocusTarget;
    type TouchFocus = PointerFocusTarget;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }

    fn cursor_image(
        &mut self,
        seat: &smithay::input::Seat<Self>,
        image: smithay::input::pointer::CursorImageStatus,
    ) {
        *seat
            .user_data()
            .get::<RefCell<CursorImageStatus>>()
            .unwrap()
            .borrow_mut() = image;
    }

    fn focus_changed(
        &mut self,
        _seat: &smithay::input::Seat<Self>,
        _focused: Option<&Self::KeyboardFocus>,
    ) {
    }

    fn led_state_changed(&mut self, seat: &smithay::input::Seat<Self>, led_state: LedState) {
        let userdata = seat.user_data();
        let devices = userdata.get::<Devices>().unwrap();
        devices.update_led_state(led_state);
    }
}

delegate_seat!(State);
