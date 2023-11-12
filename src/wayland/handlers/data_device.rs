// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_data_device,
    input::Seat,
    reexports::wayland_server::protocol::{wl_data_source::WlDataSource, wl_surface::WlSurface},
    utils::IsAlive,
    wayland::selection::data_device::{
        ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
    },
};
use std::cell::RefCell;

pub struct DnDIcon {
    surface: RefCell<Option<WlSurface>>,
}

pub fn get_dnd_icon(seat: &Seat<State>) -> Option<WlSurface> {
    let userdata = seat.user_data();
    userdata
        .get::<DnDIcon>()
        .and_then(|x| x.surface.borrow().clone())
        .filter(IsAlive::alive)
}

impl ClientDndGrabHandler for State {
    fn started(
        &mut self,
        _source: Option<WlDataSource>,
        icon: Option<WlSurface>,
        seat: Seat<Self>,
    ) {
        let user_data = seat.user_data();
        user_data.insert_if_missing(|| DnDIcon {
            surface: RefCell::new(None),
        });
        *user_data.get::<DnDIcon>().unwrap().surface.borrow_mut() = icon;
    }
    fn dropped(&mut self, seat: Seat<Self>) {
        seat.user_data()
            .get::<DnDIcon>()
            .unwrap()
            .surface
            .borrow_mut()
            .take();
    }
}
impl ServerDndGrabHandler for State {}
impl DataDeviceHandler for State {
    fn data_device_state(&self) -> &DataDeviceState {
        &self.common.data_device_state
    }
}

delegate_data_device!(State);
