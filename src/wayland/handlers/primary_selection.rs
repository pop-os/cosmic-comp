// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    delegate_primary_selection,
    wayland::primary_selection::{PrimarySelectionHandler, PrimarySelectionState, with_source_metadata}, xwayland::xwm::{XwmId, SelectionType}, reexports::wayland_protocols::wp::primary_selection::zv1::server::zwp_primary_selection_source_v1::ZwpPrimarySelectionSourceV1,
};
use tracing::warn;

use std::os::unix::io::OwnedFd;

impl PrimarySelectionHandler for State {
    type SelectionUserData = XwmId;

    fn primary_selection_state(&self) -> &PrimarySelectionState {
        &self.common.primary_selection_state
    }

    fn new_selection(&mut self, source: Option<ZwpPrimarySelectionSourceV1>) {
        for xstate in self.common.xwayland_state.values_mut() {
            if let Some(xwm) = xstate.xwm.as_mut() {
                if let Some(source) = &source {
                    if let Ok(Err(err)) = with_source_metadata(source, |metadata| {
                        xwm.new_selection(SelectionType::Primary, Some(metadata.mime_types.clone()))
                    }) {
                        warn!(?err, "Failed to set Xwayland primary selection");
                    }
                } else if let Err(err) = xwm.new_selection(SelectionType::Primary, None) {
                    warn!(?err, "Failed to clear Xwayland primary selection");
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
                SelectionType::Primary,
                mime_type,
                fd,
                self.common.event_loop_handle.clone(),
            ) {
                warn!(?err, "Failed to send primary selection (X11 -> Wayland).");
            }
        }
    }
}

delegate_primary_selection!(State);
