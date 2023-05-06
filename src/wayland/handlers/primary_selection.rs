// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{Common, SelectionSource, State},
    wayland::protocols::data_control,
};
use smithay::{
    delegate_primary_selection,
    wayland::primary_selection::{
        with_source_metadata, PrimarySelectionHandler, PrimarySelectionState,
    },
    xwayland::xwm::SelectionType, input::Seat,
};
use smithay::
    reexports::wayland_protocols::wp::primary_selection::zv1::server::zwp_primary_selection_source_v1::ZwpPrimarySelectionSourceV1;
use tracing::warn;

use std::os::unix::io::OwnedFd;

impl PrimarySelectionHandler for State {
    type SelectionUserData = ();

    fn primary_selection_state(&self) -> &PrimarySelectionState {
        &self.common.primary_selection_state
    }

    fn new_selection(&mut self, source: Option<ZwpPrimarySelectionSourceV1>, _seat: Seat<State>) {
        let Ok(mime_types) = source
            .map(|s| with_source_metadata(&s, |metadata| metadata.mime_types.clone()))
            .transpose() else { return };

        if let Some(state) = self.common.xwayland_state.as_mut() {
            if let Some(xwm) = state.xwm.as_mut() {
                if let Some(mime_types) = &mime_types {
                    if let Err(err) =
                        xwm.new_selection(SelectionType::Primary, Some(mime_types.clone()))
                    {
                        warn!(?err, "Failed to set Xwayland primary selection.");
                    }
                } else if let Err(err) = xwm.new_selection(SelectionType::Primary, None) {
                    warn!(?err, "Failed to clear Xwayland primary selection.");
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
                data_control::SelectionType::Primary,
                mime_types,
            );
        }

        Common::update_selection_sources(&seat, |mut sources| {
            sources.primary_selection = SelectionSource::Native
        });
    }

    fn send_selection(
        &mut self,
        mime_type: String,
        fd: OwnedFd,
        _seat: Seat<State>,
        _user_data: &Self::SelectionUserData,
    ) {
        let seat = self.common.last_active_seat();
        match Common::selection_sources(seat).primary_selection {
            SelectionSource::XWayland(_) => {
                if let Some(xwm) = self
                    .common
                    .xwayland_state
                    .as_mut()
                    .and_then(|xstate| xstate.xwm.as_mut())
                {
                    if let Err(err) = xwm.send_selection(
                        SelectionType::Primary,
                        mime_type,
                        fd,
                        self.common.event_loop_handle.clone(),
                    ) {
                        warn!(?err, "Failed to send primary selection (X11 -> Wayland).");
                    }
                }
            }
            SelectionSource::DataControl => {
                if self.common.data_control_state.is_some() {
                    let seat = seat.clone();
                    if let Err(err) = data_control::request_selection(
                        self,
                        &seat,
                        data_control::SelectionType::Primary,
                        mime_type,
                        fd,
                    ) {
                        warn!(
                            ?err,
                            "Failed to send primary selection (DataControl -> PrimarySelection)."
                        );
                    };
                }
            }
            SelectionSource::Native => {}
        }
    }
}

delegate_primary_selection!(State);
