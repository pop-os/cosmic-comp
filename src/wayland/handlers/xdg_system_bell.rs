// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::State, wayland::protocols::workspace::State as WState};
use smithay::{
    delegate_xdg_system_bell, reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xdg_system_bell::XdgSystemBellHandler,
};

impl XdgSystemBellHandler for State {
    fn ring(&mut self, surface: Option<WlSurface>) {
        let shell = self.common.shell.read();

        if let Some(surface) = surface {
            if let Some((workspace, _output)) = shell.workspace_for_surface(&surface) {
                let mut workspace_guard = self.common.workspace_state.update();
                workspace_guard.add_workspace_state(&workspace, WState::Urgent);
            }
        }
    }
}

delegate_xdg_system_bell!(State);
