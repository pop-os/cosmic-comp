// SPDX-License-Identifier: GPL-3.0-only

use crate::state::{Common, SelectionSource, State};
use crate::wayland::protocols::data_control::{self, SelectionType};
use smithay::wayland::data_device::{
    clear_data_device_selection, request_data_device_client_selection, set_data_device_selection,
};
use smithay::wayland::primary_selection::{
    clear_primary_selection, request_primary_client_selection, set_primary_selection,
};
use smithay::xwayland::xwm;
use tracing::{error, warn};

use std::os::unix::io::OwnedFd;

impl data_control::Handler for State {
    fn state(&mut self) -> &mut data_control::State<Self> {
        self.common.data_control_state.as_mut().unwrap()
    }

    fn new_selection(&mut self, ty: SelectionType, mime_types: Option<Vec<String>>) {
        if let Some(state) = self.common.xwayland_state.as_mut() {
            if let Some(xwm) = state.xwm.as_mut() {
                if let Err(err) = xwm.new_selection(ty.into(), mime_types.clone()) {
                    warn!(?err, "Failed to set Xwayland DataControl selection");
                }
            }
        }

        let dh = &self.common.display_handle;
        let seat = self.common.last_active_seat();
        match (ty, mime_types) {
            (SelectionType::Clipboard, Some(mt)) => set_data_device_selection(dh, seat, mt, ()),
            (SelectionType::Clipboard, None) => clear_data_device_selection(dh, seat),
            (SelectionType::Primary, Some(mt)) => set_primary_selection(dh, seat, mt, ()),
            (SelectionType::Primary, None) => clear_primary_selection(dh, seat),
        }

        Common::update_selection_sources(&seat, |mut sources| match ty {
            SelectionType::Clipboard => sources.clipboard = SelectionSource::DataControl,
            SelectionType::Primary => sources.primary_selection = SelectionSource::DataControl,
        });
    }

    fn send_selection(&mut self, ty: SelectionType, mime_type: String, fd: OwnedFd) {
        let seat = self.common.last_active_seat();
        match (ty, Common::selection_sources(seat).tuple()) {
            (SelectionType::Clipboard, (SelectionSource::Native, _)) => {
                if let Err(err) = request_data_device_client_selection(seat, mime_type, fd) {
                    error!(
                        ?err,
                        "Failed to request current wayland clipboard for DataControl.",
                    );
                }
            }
            (SelectionType::Primary, (_, SelectionSource::Native)) => {
                if let Err(err) = request_primary_client_selection(seat, mime_type, fd) {
                    error!(
                        ?err,
                        "Failed to request current wayland primary selection for DataControl.",
                    );
                }
            }
            (SelectionType::Clipboard, (SelectionSource::XWayland(_), _))
            | (SelectionType::Primary, (_, SelectionSource::XWayland(_))) => {
                if let Some(xwm) = self
                    .common
                    .xwayland_state
                    .as_mut()
                    .and_then(|xstate| xstate.xwm.as_mut())
                {
                    if let Err(err) = xwm.send_selection(
                        ty.into(),
                        mime_type,
                        fd,
                        self.common.event_loop_handle.clone(),
                    ) {
                        warn!(
                            ?err,
                            "Failed to send primary selection (X11 -> DataControl)."
                        );
                    }
                }
            }
            (SelectionType::Clipboard, (SelectionSource::DataControl, _))
            | (SelectionType::Primary, (_, SelectionSource::DataControl)) => {}
        }
    }
}

impl From<SelectionType> for xwm::SelectionType {
    fn from(ty: SelectionType) -> Self {
        match ty {
            SelectionType::Clipboard => xwm::SelectionType::Clipboard,
            SelectionType::Primary => xwm::SelectionType::Clipboard,
        }
    }
}

impl From<xwm::SelectionType> for SelectionType {
    fn from(ty: xwm::SelectionType) -> Self {
        match ty {
            xwm::SelectionType::Clipboard => SelectionType::Clipboard,
            xwm::SelectionType::Primary => SelectionType::Clipboard,
        }
    }
}

data_control::delegate!(State);
