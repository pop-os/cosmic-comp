// SPDX-License-Identifier: GPL-3.0-only

use smithay::{input::Seat, reexports::wayland_server::DisplayHandle};

use crate::{
    shell::CosmicSurface,
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoHandler,
        toplevel_management::{
            delegate_toplevel_management, ManagementWindow, ToplevelManagementHandler,
            ToplevelManagementState,
        },
    },
};

impl ToplevelManagementHandler for State {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState {
        &mut self.common.shell.toplevel_management_state
    }

    fn activate(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        seat: Option<Seat<Self>>,
    ) {
        for output in self
            .common
            .shell
            .outputs()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
        {
            let maybe = self
                .common
                .shell
                .workspaces
                .spaces_for_output(output)
                .enumerate()
                .find(|(_, w)| w.windows().any(|w| &w == window));
            if let Some((idx, workspace)) = maybe {
                let seat = seat.unwrap_or(self.common.last_active_seat().clone());
                let mapped = workspace
                    .mapped()
                    .find(|m| m.windows().any(|(w, _)| &w == window))
                    .unwrap()
                    .clone();

                let _ = self.common.shell.activate(&output, idx as usize); // TODO: Move pointer?
                mapped.focus_window(window);
                Common::set_focus(self, Some(&mapped.clone().into()), &seat, None);
                return;
            }
        }
    }

    fn close(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        window.close();
    }
}

impl ManagementWindow for CosmicSurface {
    fn close(&self) {
        CosmicSurface::close(self)
    }
}

delegate_toplevel_management!(State);
