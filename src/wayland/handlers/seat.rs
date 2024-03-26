// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    input::Devices,
    shell::focus::target::{KeyboardFocusTarget, PointerFocusTarget},
    state::State,
};
use smithay::{
    delegate_seat,
    input::{keyboard::LedState, pointer::CursorImageStatus, SeatHandler, SeatState},
    reexports::wayland_server::Resource,
    wayland::{
        seat::WaylandFocus, selection::data_device::set_data_device_focus,
        selection::primary_selection::set_primary_focus,
    },
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
        seat: &smithay::input::Seat<Self>,
        focused: Option<&Self::KeyboardFocus>,
    ) {
        let dh = &self.common.display_handle;
        if let Some(client) = focused
            .and_then(|t| t.wl_surface())
            .and_then(|s| dh.get_client(s.id()).ok())
        {
            set_data_device_focus(dh, seat, Some(client.clone()));
            set_primary_focus(dh, seat, Some(client))
        }
    }

    fn led_state_changed(&mut self, seat: &smithay::input::Seat<Self>, led_state: LedState) {
        let userdata = seat.user_data();
        let devices = userdata.get::<Devices>().unwrap();
        devices.update_led_state(led_state);
    }
}

delegate_seat!(State);
