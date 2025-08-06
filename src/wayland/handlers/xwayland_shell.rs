// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::Shell, state::State, wayland::handlers::xdg_activation::ActivationContext};
use smithay::{
    delegate_xwayland_shell,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::{
        xdg_activation::XdgActivationToken,
        xwayland_shell::{XWaylandShellHandler, XWaylandShellState},
    },
    xwayland::{xwm::XwmId, X11Surface},
};

impl XWaylandShellHandler for State {
    fn xwayland_shell_state(&mut self) -> &mut XWaylandShellState {
        &mut self.common.xwayland_shell_state
    }

    fn surface_associated(&mut self, _xwm: XwmId, _wl_surface: WlSurface, surface: X11Surface) {
        if !surface.is_override_redirect() {
            let mut shell = self.common.shell.write();
            if let Some(window) = shell
                .pending_windows
                .iter()
                .find(|pending| pending.surface.x11_surface() == Some(&surface))
                .map(|pending| pending.surface.clone())
            {
                if !shell
                    .pending_activations
                    .contains_key(&crate::shell::ActivationKey::X11(surface.window_id()))
                {
                    if let Some(startup_id) = window.x11_surface().and_then(|x| x.startup_id()) {
                        if let Some(context) = self
                            .common
                            .xdg_activation_state
                            .data_for_token(&XdgActivationToken::from(startup_id))
                            .and_then(|data| data.user_data.get::<ActivationContext>())
                        {
                            shell.pending_activations.insert(
                                crate::shell::ActivationKey::X11(surface.window_id()),
                                context.clone(),
                            );
                        }
                    }
                }
                let res = shell.map_window(
                    &window,
                    &mut self.common.toplevel_info_state,
                    &mut self.common.workspace_state,
                    &self.common.event_loop_handle,
                );
                if let Some(target) = res {
                    let seat = shell.seats.last_active().clone();
                    std::mem::drop(shell);
                    Shell::set_focus(self, Some(&target), &seat, None, false);
                }
            }
        }
    }
}

delegate_xwayland_shell!(State);
