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
use std::sync::Mutex;

pub struct DnDIcon {
    surface: Mutex<Option<WlSurface>>,
}

pub fn get_dnd_icon(seat: &Seat<State>) -> Option<WlSurface> {
    let userdata = seat.user_data();
    userdata
        .get::<DnDIcon>()
        .and_then(|x| x.surface.lock().unwrap().clone())
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
        user_data.insert_if_missing_threadsafe(|| DnDIcon {
            surface: Mutex::new(None),
        });
        *user_data.get::<DnDIcon>().unwrap().surface.lock().unwrap() = icon;
    }
    fn dropped(&mut self, seat: Seat<Self>) {
        seat.user_data()
            .get::<DnDIcon>()
            .unwrap()
            .surface
            .lock()
            .unwrap()
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
