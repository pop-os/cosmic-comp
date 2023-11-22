// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    input::Seat,
    wayland::selection::{SelectionHandler, SelectionSource, SelectionTarget},
    xwayland::xwm::XwmId,
};
use std::os::unix::io::OwnedFd;
use tracing::warn;

impl SelectionHandler for State {
    type SelectionUserData = XwmId;

    fn new_selection(
        &mut self,
        target: SelectionTarget,
        source: Option<SelectionSource>,
        _seat: Seat<State>,
    ) {
        if let Some(xwm) = self
            .common
            .shell
            .xwayland_state
            .as_mut()
            .and_then(|xstate| xstate.xwm.as_mut())
        {
            if let Some(source) = &source {
                if let Err(err) = xwm.new_selection(target, Some(source.mime_types())) {
                    warn!(?err, "Failed to set Xwayland clipboard selection.");
                }
            } else if let Err(err) = xwm.new_selection(target, None) {
                warn!(?err, "Failed to clear Xwayland selection.");
            }
        }
    }

    fn send_selection(
        &mut self,
        target: SelectionTarget,
        mime_type: String,
        fd: OwnedFd,
        _seat: Seat<State>,
        _user_data: &Self::SelectionUserData,
    ) {
        if let Some(xwm) = self
            .common
            .shell
            .xwayland_state
            .as_mut()
            .and_then(|xstate| xstate.xwm.as_mut())
        {
            if let Err(err) =
                xwm.send_selection(target, mime_type, fd, self.common.event_loop_handle.clone())
            {
                warn!(?err, "Failed to send selection (X11 -> Wayland).");
            }
        }
    }
}
