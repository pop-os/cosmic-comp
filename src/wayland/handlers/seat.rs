// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_seat,
    input::{pointer::CursorImageStatus, SeatHandler, SeatState},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Resource},
    wayland::{data_device::set_data_device_focus, primary_selection::set_primary_focus},
};
use std::cell::RefCell;

impl SeatHandler for State {
    type KeyboardFocus = WlSurface;
    type PointerFocus = WlSurface;

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
        if let Some(client) = focused.and_then(|s| dh.get_client(s.id()).ok()) {
            set_data_device_focus(dh, seat, Some(client));
            let client2 = focused.and_then(|s| dh.get_client(s.id()).ok()).unwrap();
            set_primary_focus(dh, seat, Some(client2))
        }
    }
}

delegate_seat!(State);
