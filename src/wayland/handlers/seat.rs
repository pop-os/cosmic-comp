// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    shell::Devices,
    state::State,
    utils::prelude::SeatExt,
};
use smithay::{
    delegate_cursor_shape, delegate_seat,
    input::{keyboard::LedState, pointer::CursorImageStatus, SeatHandler, SeatState},
};

impl SeatHandler for State {
    type KeyboardFocus = KeyboardFocusTarget;
    type PointerFocus = PointerFocusTarget;
    type TouchFocus = PointerFocusTarget;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.common.seat_state
    }

    fn cursor_image(&mut self, seat: &smithay::input::Seat<Self>, image: CursorImageStatus) {
        seat.set_cursor_image_status(image);
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
delegate_cursor_shape!(State);
