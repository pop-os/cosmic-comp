// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{Kind, Window},
    input::Seat,
    reexports::wayland_server::DisplayHandle,
};

use crate::{
    utils::prelude::*,
    wayland::protocols::toplevel_management::{
        delegate_toplevel_management, ToplevelManagementHandler, ToplevelManagementState,
    },
};

impl ToplevelManagementHandler for State {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState {
        &mut self.common.shell.toplevel_management_state
    }

    fn activate(&mut self, _dh: &DisplayHandle, window: &Window, seat: Option<Seat<Self>>) {
        if let Some(idx) = self
            .common
            .shell
            .space_for_window(window.toplevel().wl_surface())
            .map(|w| w.idx)
        {
            let seat = seat.unwrap_or(self.common.last_active_seat.clone());
            let output = active_output(&seat, &self.common);
            if self.common.shell.active_space(&output).idx != idx {
                self.common.shell.activate(&seat, &output, idx as usize);
            }
            Common::set_focus(self, Some(window.toplevel().wl_surface()), &seat, None);
        }
    }

    fn close(&mut self, _dh: &DisplayHandle, window: &Window) {
        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.send_close();
        }
    }
}

delegate_toplevel_management!(State);
