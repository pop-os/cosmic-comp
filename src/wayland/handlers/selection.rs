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
        let Some(xwm_id) = self
            .common
            .xwayland_state
            .as_ref()
            .and_then(|xstate| xstate.xwm.as_ref())
            .map(|xwm| xwm.id())
        else {
            return;
        };

        let x_has_focus = self.common.has_x_keyboard_focus(xwm_id);

        let xstate = self.common.xwayland_state.as_mut().unwrap();
        let xwm = xstate.xwm.as_mut().unwrap();

        if let Some(source) = &source {
            if x_has_focus {
                if let Err(err) = xwm.new_selection(target, Some(source.mime_types())) {
                    warn!(?err, "Failed to set Xwayland clipboard selection.");
                }
            } else {
                match target {
                    SelectionTarget::Clipboard => {
                        xstate.clipboard_selection_dirty = Some(source.mime_types())
                    }
                    SelectionTarget::Primary => {
                        xstate.primary_selection_dirty = Some(source.mime_types())
                    }
                };
            }
        } else {
            if let Err(err) = xwm.new_selection(target, None) {
                warn!(?err, "Failed to clear Xwayland selection.");
            }
            xstate.clipboard_selection_dirty = None;
            xstate.primary_selection_dirty = None;
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
