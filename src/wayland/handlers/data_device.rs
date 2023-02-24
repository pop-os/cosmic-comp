// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_data_device,
    input::Seat,
    reexports::wayland_server::protocol::{wl_data_source::WlDataSource, wl_surface::WlSurface},
    utils::IsAlive,
    wayland::data_device::{
        with_source_metadata, ClientDndGrabHandler, DataDeviceHandler, DataDeviceState,
        ServerDndGrabHandler,
    },
    xwayland::xwm::{SelectionType, XwmId},
};
use std::{cell::RefCell, os::unix::io::OwnedFd};
use tracing::warn;

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
    type SelectionUserData = XwmId;

    fn data_device_state(&self) -> &DataDeviceState {
        &self.common.data_device_state
    }

    fn new_selection(&mut self, source: Option<WlDataSource>) {
        for xstate in self.common.xwayland_state.values_mut() {
            if let Some(xwm) = xstate.xwm.as_mut() {
                if let Some(source) = &source {
                    if let Ok(Err(err)) = with_source_metadata(source, |metadata| {
                        xwm.new_selection(
                            SelectionType::Clipboard,
                            Some(metadata.mime_types.clone()),
                        )
                    }) {
                        warn!(?err, "Failed to set Xwayland clipboard selection.");
                    }
                } else if let Err(err) = xwm.new_selection(SelectionType::Clipboard, None) {
                    warn!(?err, "Failed to clear Xwayland clipboard selection.");
                }
            }
        }
    }

    fn send_selection(
        &mut self,
        mime_type: String,
        fd: OwnedFd,
        user_data: &Self::SelectionUserData,
    ) {
        if let Some(xwm) = self
            .common
            .xwayland_state
            .values_mut()
            .flat_map(|xstate| xstate.xwm.as_mut())
            .find(|xwm| &xwm.id() == user_data)
        {
            if let Err(err) = xwm.send_selection(
                SelectionType::Clipboard,
                mime_type,
                fd,
                self.common.event_loop_handle.clone(),
            ) {
                warn!(?err, "Failed to send clipboard (X11 -> Wayland).");
            }
        }
    }
}

delegate_data_device!(State);
