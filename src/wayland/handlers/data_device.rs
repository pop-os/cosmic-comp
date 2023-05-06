// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{Common, SelectionSource, State},
    wayland::protocols::data_control,
};
use smithay::{
    delegate_data_device,
    input::Seat,
    reexports::wayland_server::protocol::{wl_data_source::WlDataSource, wl_surface::WlSurface},
    utils::IsAlive,
    wayland::data_device::{
        with_source_metadata, ClientDndGrabHandler, DataDeviceHandler, DataDeviceState,
        ServerDndGrabHandler,
    },
    xwayland::xwm::SelectionType,
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
    type SelectionUserData = ();

    fn data_device_state(&self) -> &DataDeviceState {
        &self.common.data_device_state
    }

    fn new_selection(&mut self, source: Option<WlDataSource>) {
        let Ok(mime_types) = source
            .map(|s| with_source_metadata(&s, |metadata| metadata.mime_types.clone()))
            .transpose() else { return };

        if let Some(state) = self.common.xwayland_state.as_mut() {
            if let Some(xwm) = state.xwm.as_mut() {
                if let Some(mime_types) = &mime_types {
                    if let Err(err) =
                        xwm.new_selection(SelectionType::Clipboard, Some(mime_types.clone()))
                    {
                        warn!(?err, "Failed to set Xwayland clipboard selection.");
                    }
                } else if let Err(err) = xwm.new_selection(SelectionType::Clipboard, None) {
                    warn!(?err, "Failed to clear Xwayland clipboard selection.");
                }
            }
        }

        let seat = self.common.last_active_seat().clone();
        if self.common.data_control_state.is_some() {
            let dh = self.common.display_handle.clone();
            data_control::set_selection(
                self,
                &seat,
                &dh,
                data_control::SelectionType::Clipboard,
                mime_types,
            );
        }

        Common::update_selection_sources(&seat, |mut sources| {
            sources.clipboard = SelectionSource::Native
        });
    }

    fn send_selection(
        &mut self,
        mime_type: String,
        fd: OwnedFd,
        _user_data: &Self::SelectionUserData,
    ) {
        let seat = self.common.last_active_seat();
        match Common::selection_sources(seat).clipboard {
            SelectionSource::XWayland(_) => {
                if let Some(xwm) = self
                    .common
                    .xwayland_state
                    .as_mut()
                    .and_then(|xstate| xstate.xwm.as_mut())
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
            SelectionSource::DataControl => {
                if self.common.data_control_state.is_some() {
                    let seat = seat.clone();
                    if let Err(err) = data_control::request_selection(
                        self,
                        &seat,
                        data_control::SelectionType::Clipboard,
                        mime_type,
                        fd,
                    ) {
                        warn!(?err, "Failed to send clipboard (X11 -> DataControl).");
                    };
                }
            }
            SelectionSource::Native => {}
        }
    }
}

delegate_data_device!(State);
