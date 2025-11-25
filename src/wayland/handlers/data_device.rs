// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, utils::prelude::SeatExt};
use smithay::{
    delegate_data_device,
    input::{
        Seat,
        dnd::{DnDGrab, DndGrabHandler, DndTarget, GrabType},
        pointer::{CursorImageStatus, CursorImageSurfaceData, Focus},
    },
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Logical, Point},
    wayland::{
        compositor::{self, SurfaceAttributes},
        selection::data_device::{DataDeviceHandler, DataDeviceState, WaylandDndGrabHandler},
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

impl WaylandDndGrabHandler for State {
    fn dnd_requested<S: smithay::input::dnd::Source>(
        &mut self,
        source: S,
        icon: Option<WlSurface>,
        seat: Seat<Self>,
        serial: smithay::utils::Serial,
        type_: GrabType,
    ) {
        let user_data = seat.user_data();
        user_data.insert_if_missing_threadsafe::<Mutex<Option<DnDIcon>>, _>(Default::default);

        let offset = if let CursorImageStatus::Surface(ref surface) = seat.cursor_image_status() {
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
        };

        *user_data
            .get::<Mutex<Option<DnDIcon>>>()
            .unwrap()
            .lock()
            .unwrap() = icon.map(|surface| DnDIcon { surface, offset });

        match type_ {
            GrabType::Pointer => {
                let pointer = seat.get_pointer().unwrap();
                let start_data = pointer.grab_start_data().unwrap();
                pointer.set_grab(
                    self,
                    DnDGrab::new_pointer(&self.common.display_handle, start_data, source, seat),
                    serial,
                    Focus::Keep,
                );
            }
            GrabType::Touch => {
                let touch = seat.get_touch().unwrap();
                let start_data = touch.grab_start_data().unwrap();
                touch.set_grab(
                    self,
                    DnDGrab::new_touch(&self.common.display_handle, start_data, source, seat),
                    serial,
                );
            }
        }
    }
}

impl DndGrabHandler for State {
    fn dropped(
        &mut self,
        _target: Option<DndTarget<'_, Self>>,
        _validated: bool,
        seat: Seat<Self>,
        _location: Point<f64, Logical>,
    ) {
        if let Some(icon) = seat.user_data().get::<Mutex<Option<DnDIcon>>>() {
            icon.lock().unwrap().take();
        }
    }
}

impl DataDeviceHandler for State {
    fn data_device_state(&mut self) -> &mut DataDeviceState {
        &mut self.common.data_device_state
    }
}

delegate_data_device!(State);
