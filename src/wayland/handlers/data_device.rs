// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_data_device,
    input::{
        pointer::{CursorImageStatus, CursorImageSurfaceData},
        Seat,
    },
    reexports::wayland_server::protocol::{wl_data_source::WlDataSource, wl_surface::WlSurface},
    utils::{IsAlive, Logical, Point},
    wayland::{
        compositor::{self, SurfaceAttributes},
        selection::data_device::{
            ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
        },
    },
};
use std::sync::Mutex;

#[derive(Debug, Clone)]
pub struct DnDIcon {
    pub surface: WlSurface,
    pub offset: Point<i32, Logical>,
}

pub fn get_dnd_icon(seat: &Seat<State>) -> Option<DnDIcon> {
    let userdata = seat.user_data();
    userdata
        .get::<Mutex<Option<DnDIcon>>>()
        .and_then(|x| x.lock().unwrap().clone())
        .filter(|icon| icon.surface.alive())
}

pub fn on_commit(surface: &WlSurface, seat: &Seat<State>) {
    let userdata = seat.user_data();

    let Some(mut guard) = userdata
        .get::<Mutex<Option<DnDIcon>>>()
        .map(|guard| guard.lock().unwrap())
    else {
        return;
    };

    let Some(icon) = guard.as_mut() else {
        return;
    };

    if &icon.surface == surface {
        compositor::with_states(surface, |states| {
            let buffer_delta = states
                .cached_state
                .get::<SurfaceAttributes>()
                .current()
                .buffer_delta
                .take()
                .unwrap_or_default();
            icon.offset += buffer_delta;
        });
    }
}

impl ClientDndGrabHandler for State {
    fn started(
        &mut self,
        _source: Option<WlDataSource>,
        icon: Option<WlSurface>,
        seat: Seat<Self>,
    ) {
        let user_data = seat.user_data();
        user_data.insert_if_missing_threadsafe::<Mutex<Option<DnDIcon>>, _>(Default::default);

        let offset = seat
            .user_data()
            .get::<Mutex<CursorImageStatus>>()
            .map(|guard| {
                if let CursorImageStatus::Surface(ref surface) = *guard.lock().unwrap() {
                    compositor::with_states(surface, |states| {
                        let hotspot = states
                            .data_map
                            .get::<CursorImageSurfaceData>()
                            .unwrap()
                            .lock()
                            .unwrap()
                            .hotspot;
                        Point::from((-hotspot.x, -hotspot.y))
                    })
                } else {
                    (0, 0).into()
                }
            })
            .unwrap_or_default();

        *user_data
            .get::<Mutex<Option<DnDIcon>>>()
            .unwrap()
            .lock()
            .unwrap() = icon.map(|surface| DnDIcon { surface, offset })
    }

    fn dropped(&mut self, _target: Option<WlSurface>, _validated: bool, seat: Seat<Self>) {
        seat.user_data()
            .get::<Mutex<Option<DnDIcon>>>()
            .unwrap()
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
