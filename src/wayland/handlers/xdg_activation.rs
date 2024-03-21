use smithay::{
    delegate_xdg_activation,
    input::Seat,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xdg_activation::{
        XdgActivationHandler, XdgActivationState, XdgActivationToken, XdgActivationTokenData,
    },
};
use tracing::debug;

use crate::{shell::ActivationKey, state::ClientState, utils::prelude::*};
use crate::{state::State, wayland::protocols::workspace::WorkspaceHandle};

#[derive(Debug, Clone, Copy)]
pub enum ActivationContext {
    UrgentOnly,
    Workspace(WorkspaceHandle),
}

impl XdgActivationHandler for State {
    fn activation_state(&mut self) -> &mut XdgActivationState {
        &mut self.common.shell.xdg_activation_state
    }

    fn token_created(&mut self, token: XdgActivationToken, data: XdgActivationTokenData) -> bool {
        // Privileged clients always get valid tokens
        if data
            .client_id
            .and_then(|client_id| {
                self.common
                    .display_handle
                    .backend_handle()
                    .get_client_data(client_id)
                    .ok()
            })
            .and_then(|data| {
                data.downcast_ref::<ClientState>()
                    .map(|data| data.privileged)
            })
            .unwrap_or(false)
        {
            if let Some(seat) = data.serial.and_then(|(_, seat)| Seat::from_resource(&seat)) {
                let output = seat.active_output();
                let workspace = self.common.shell.active_space_mut(&output);
                workspace.pending_tokens.insert(token.clone());
                let handle = workspace.handle;
                data.user_data
                    .insert_if_missing(move || ActivationContext::Workspace(handle));
                debug!(?token, "created workspace token for privileged client");
            } else {
                data.user_data
                    .insert_if_missing(|| ActivationContext::UrgentOnly);
                debug!(
                    ?token,
                    "created urgent-only token for privileged client without seat"
                );
            }

            return true;
        };

        // Tokens without validation aren't allowed to steal focus
        let Some((serial, seat)) = data.serial else {
            data.user_data
                .insert_if_missing(|| ActivationContext::UrgentOnly);
            debug!(?token, "created urgent-only token for missing seat/serial");
            return true;
        };
        let Some(seat) = Seat::from_resource(&seat) else {
            data.user_data
                .insert_if_missing(|| ActivationContext::UrgentOnly);
            debug!(?token, "created urgent-only token for unknown seat");
            return true;
        };

        // At this point we don't bother with urgent-only tokens.
        // If the client provides a bad serial, it should be fixed.

        let keyboard = seat.get_keyboard().unwrap();
        let valid = keyboard
            .last_enter()
            .map(|last_enter| serial.is_no_older_than(&last_enter))
            .unwrap_or(false);

        if valid {
            let output = seat.active_output();
            let workspace = self.common.shell.active_space_mut(&output);
            workspace.pending_tokens.insert(token.clone());
            let handle = workspace.handle;
            data.user_data
                .insert_if_missing(move || ActivationContext::Workspace(handle));

            debug!(?token, "created workspace token");
        } else {
            debug!(?token, "created urgent-only token for invalid serial");
        }

        valid
    }

    fn request_activation(
        &mut self,
        _token: XdgActivationToken,
        token_data: XdgActivationTokenData,
        surface: WlSurface,
    ) {
        if let Some(context) = token_data.user_data.get::<ActivationContext>() {
            if let Some(element) = self.common.shell.element_for_surface(&surface).cloned() {
                match context {
                    ActivationContext::UrgentOnly => {
                        if let Some((workspace, _output)) =
                            self.common.shell.workspace_for_surface(&surface)
                        {
                            self.common.shell.set_urgent(&workspace);
                        }
                    }
                    ActivationContext::Workspace(workspace) => {
                        let seat = self.common.last_active_seat().clone();
                        let current_output = seat.active_output();

                        if element.is_minimized() {
                            self.common.shell.unminimize_request(&element, &seat);
                        }

                        let current_workspace = self.common.shell.active_space_mut(&current_output);

                        let in_current_workspace = current_workspace
                            .floating_layer
                            .mapped()
                            .any(|m| m == &element);

                        if in_current_workspace {
                            current_workspace
                                .floating_layer
                                .space
                                .raise_element(&element, true);
                        }

                        if element.is_stack() {
                            if let Some((window, _)) = element.windows().find(|(window, _)| {
                                let mut found = false;
                                window.with_surfaces(|wl_surface, _| {
                                    if wl_surface == &surface {
                                        found = true;
                                    }
                                });
                                found
                            }) {
                                element.set_active(&window);
                            }
                        }

                        if workspace == &current_workspace.handle || in_current_workspace {
                            let target = element.into();
                            Shell::set_focus(self, Some(&target), &seat, None);
                        } else if let Some(w) = self
                            .common
                            .shell
                            .space_for(&element)
                            .map(|w| w.handle.clone())
                        {
                            Shell::append_focus_stack(self, &element, &seat);
                            self.common.shell.set_urgent(&w);
                        }
                    }
                }
            } else {
                self.common
                    .shell
                    .pending_activations
                    .insert(ActivationKey::Wayland(surface), context.clone());
            }
        }
    }
}

delegate_xdg_activation!(State);
