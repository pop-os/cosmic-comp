use crate::shell::focus::target::KeyboardFocusTarget;
use crate::shell::WorkspaceDelta;
use crate::{shell::ActivationKey, state::ClientState, utils::prelude::*};
use crate::{
    state::State,
    wayland::protocols::workspace::{State as WState, WorkspaceHandle},
};
use smithay::{
    delegate_xdg_activation,
    input::Seat,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::xdg_activation::{
        XdgActivationHandler, XdgActivationState, XdgActivationToken, XdgActivationTokenData,
    },
};
use tracing::{debug, warn};

#[derive(Debug, Clone, Copy)]
pub enum ActivationContext {
    UrgentOnly,
    Workspace(WorkspaceHandle),
}

impl XdgActivationHandler for State {
    fn activation_state(&mut self) -> &mut XdgActivationState {
        &mut self.common.xdg_activation_state
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
            let seat = data
                .serial
                .and_then(|(_, seat)| Seat::from_resource(&seat))
                .unwrap_or_else(|| {
                    let shell = self.common.shell.read();
                    shell.seats.last_active().clone()
                });
            let output = seat.active_output();
            let mut shell = self.common.shell.write();
            let workspace = shell.active_space_mut(&output).unwrap();
            let handle = workspace.handle;
            data.user_data
                .insert_if_missing(move || ActivationContext::Workspace(handle));
            debug!(?token, "created workspace token for privileged client");
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
            let mut shell = self.common.shell.write();
            let workspace = shell.active_space_mut(&output).unwrap();
            let handle = workspace.handle;
            data.user_data
                .insert_if_missing(move || ActivationContext::Workspace(handle));

            debug!(?token, "created workspace token");
        }

        valid
    }

    fn request_activation(
        &mut self,
        _token: XdgActivationToken,
        token_data: XdgActivationTokenData,
        surface: WlSurface,
    ) {
        let Some(context) = token_data.user_data.get::<ActivationContext>() else {
            return;
        };
        let mut shell = self.common.shell.write();

        match context {
            ActivationContext::UrgentOnly => {
                if let Some((workspace, _output)) = shell.workspace_for_surface(&surface) {
                    let mut workspace_guard = self.common.workspace_state.update();
                    workspace_guard.add_workspace_state(&workspace, WState::Urgent);
                }
            }
            ActivationContext::Workspace(_) => {
                let seat = shell.seats.last_active().clone();
                let current_output = seat.active_output();

                if let Some(element) = shell.element_for_surface(&surface).cloned() {
                    if element.is_minimized() {
                        shell.unminimize_request(&surface, &seat, &self.common.event_loop_handle);
                    }

                    let Some((element_output, element_workspace)) = shell
                        .space_for(&element)
                        .map(|w| (w.output.clone(), w.handle.clone()))
                    else {
                        return;
                    };
                    let in_current_workspace =
                        element_workspace == shell.active_space(&current_output).unwrap().handle;

                    if !in_current_workspace {
                        let Some(idx) = shell
                            .workspaces
                            .idx_for_handle(&element_output, &element_workspace)
                        else {
                            warn!("Couldn't determine idx for elements workspace?");
                            return;
                        };

                        if let Err(err) = shell.activate(
                            &element_output,
                            idx,
                            WorkspaceDelta::new_shortcut(),
                            &mut self.common.workspace_state.update(),
                        ) {
                            warn!("Failed to activate the workspace: {err:?}");
                        }
                    }

                    let current_workspace = shell.active_space_mut(&current_output).unwrap();
                    current_workspace
                        .floating_layer
                        .space
                        .raise_element(&element, true);
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
                        } else {
                            warn!("Failed to find activated window in the stack");
                            return;
                        }
                    }

                    if seat.get_keyboard().unwrap().current_focus() != Some(element.clone().into())
                        && current_workspace.is_tiled(&surface)
                    {
                        for mapped in current_workspace
                            .mapped()
                            .filter(|m| m.maximized_state.lock().unwrap().is_some())
                            .cloned()
                            .collect::<Vec<_>>()
                            .into_iter()
                        {
                            current_workspace.unmaximize_request(&mapped);
                        }
                    }

                    std::mem::drop(shell);
                    Shell::set_focus(
                        self,
                        Some(&KeyboardFocusTarget::Element(element.clone())),
                        &seat,
                        None,
                        false,
                    );
                } else if let Some((workspace, _)) = shell.workspace_for_surface(&surface) {
                    let current_workspace = shell.active_space(&current_output).unwrap();
                    if workspace == current_workspace.handle {
                        let Some(target) = shell
                            .workspaces
                            .space_for_handle(&workspace)
                            .unwrap()
                            .get_fullscreen()
                            .cloned()
                            .map(KeyboardFocusTarget::Fullscreen)
                        else {
                            return;
                        };

                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), &seat, None, false);
                    } else {
                        if let Some(surface) = shell
                            .workspaces
                            .space_for_handle(&workspace)
                            .and_then(|w| w.get_fullscreen())
                            .cloned()
                        {
                            shell.append_focus_stack(surface, &seat)
                        }
                        let mut workspace_guard = self.common.workspace_state.update();
                        workspace_guard.add_workspace_state(&workspace, WState::Urgent);
                    }
                } else {
                    shell
                        .pending_activations
                        .insert(ActivationKey::Wayland(surface), context.clone());
                };
            }
        }
    }
}

delegate_xdg_activation!(State);
