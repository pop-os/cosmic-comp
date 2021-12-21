// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    backend::input::{Device, DeviceCapability, InputBackend, InputEvent},
    reexports::wayland_server::Display,
    wayland::{
        data_device::set_data_device_focus,
        output::Output,
        seat::{CursorImageStatus, Keysym, Seat, XkbConfig},
    },
};
use std::{cell::RefCell, collections::HashMap};

pub struct ActiveOutput(pub RefCell<Output>);

pub fn add_seat(display: &mut Display, name: String) -> Seat {
    let (seat, _) = Seat::new(display, name, None);
    seat
}

pub fn active_output(seat: &Seat, state: &State) -> Output {
    seat.user_data()
        .get::<ActiveOutput>()
        .map(|x| x.0.borrow().clone())
        .unwrap_or_else(|| {
            state
                .spaces
                .outputs()
                .next()
                .cloned()
                .expect("Backend has no outputs?")
        })
}
