use std::sync::OnceLock;

use crate::shell::WorkspaceDelta;
use crate::shell::focus::target::KeyboardFocusTarget;
use crate::{shell::ActivationKey, state::ClientState, utils::prelude::*};
use crate::{
    state::State,
    wayland::protocols::workspace::{State as WState, WorkspaceHandle},
};
use cosmic_comp_config::ActivationPolicy;
use smithay::{
    desktop::{WindowSurfaceType, layer_map_for_output},
    input::Seat,
    reexports::wayland_server::{Resource, backend::ClientId, protocol::wl_surface::WlSurface},
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

/// Executables that are allowed full activation even with stale serials.
/// Configurable via `COSMIC_ACTIVATION_TRUSTED_APPS` (comma-separated).
/// Empty by default — no apps are trusted unless explicitly configured.
fn trusted_activators() -> &'static [String] {
    static APPS: OnceLock<Vec<String>> = OnceLock::new();
    APPS.get_or_init(|| {
        std::env::var("COSMIC_ACTIVATION_TRUSTED_APPS")
            .unwrap_or_default()
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    })
}

fn is_trusted_activator(
    display_handle: &smithay::reexports::wayland_server::DisplayHandle,
    client_id: Option<&ClientId>,
) -> bool {
    let apps = trusted_activators();
    if apps.is_empty() {
        return false;
    }
    let Some(client_id) = client_id else {
        return false;
    };
    let exe = display_handle
        .backend_handle()
        .get_client_credentials(client_id.clone())
        .ok()
        .and_then(|creds| std::fs::read_link(format!("/proc/{}/exe", creds.pid)).ok())
        .and_then(|p| p.file_name().and_then(|n| n.to_str()).map(String::from));
    let Some(exe) = exe else { return false };
    apps.iter().any(|app| app == &exe)
}

// It may happen that we get activation-requests, while all outputs are disabled.
// In these cases we won't be able to determine workspaces for windows and thus
// need to handle the corresponding code paths defensively.

impl XdgActivationHandler for State {
    fn activation_state(&mut self) -> &mut XdgActivationState {
        &mut self.common.xdg_activation_state
    }

    fn token_created(&mut self, token: XdgActivationToken, data: XdgActivationTokenData) -> bool {
        // Privileged clients always get valid tokens
        if data
            .client_id
            .as_ref()
            .and_then(|client_id| {
                self.common
                    .display_handle
                    .backend_handle()
                    .get_client_data(client_id.clone())
                    .ok()
            })
            .and_then(|data| {
                data.downcast_ref::<ClientState>()
                    .map(|data| data.not_sandboxed())
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
            let Some(workspace) = shell.active_space_mut(&output) else {
                debug!(?token, "created urgent token for privileged client");
                data.user_data
                    .insert_if_missing(move || ActivationContext::UrgentOnly);
                return true;
            };
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

        let keyboard = seat.get_keyboard().unwrap();
        let valid = keyboard
            .last_enter()
            .map(|last_enter| serial.is_no_older_than(&last_enter))
            .unwrap_or(false);

        if valid {
            let output = seat.active_output();
            let mut shell = self.common.shell.write();
            let Some(workspace) = shell.active_space_mut(&output) else {
                data.user_data
                    .insert_if_missing(|| ActivationContext::UrgentOnly);
                return true;
            };
            let handle = workspace.handle;
            data.user_data
                .insert_if_missing(move || ActivationContext::Workspace(handle));

            debug!(?token, "created workspace token");
        } else if is_trusted_activator(&self.common.display_handle, data.client_id.as_ref()) {
            // Trusted activator with a stale serial — grant full workspace
            // token so the target window can be focused immediately.
            let output = seat.active_output();
            let mut shell = self.common.shell.write();
            let workspace = shell.active_space_mut(&output).unwrap();
            let handle = workspace.handle;
            data.user_data
                .insert_if_missing(move || ActivationContext::Workspace(handle));
            debug!(
                ?token,
                "created workspace token for trusted activator with stale serial"
            );
        } else {
            // Stale serial from untrusted client — grant UrgentOnly so the
            // window can still be focused via activate_surface.
            data.user_data
                .insert_if_missing(|| ActivationContext::UrgentOnly);
            debug!(?token, "created urgent-only token for stale serial");
        }

        true
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

        // Layer surfaces are shell chrome, not workspace contents: there is no
        // workspace to switch to and no element to raise, so none of the
        // machinery below applies to them. `FocusIfActiveWorkspace` would in fact
        // drop the request outright, since `workspace_for_surface` never matches
        // a layer surface. Focus it directly instead — `set_focus` still refuses
        // when a lock screen or an exclusive layer surface owns input.
        if let Some((target, seat)) = self.layer_activation_target(&surface) {
            debug!("activating layer surface");
            Shell::set_focus(self, Some(&target), &seat, None, false);
            return;
        }

        match context {
            ActivationContext::UrgentOnly => {
                // UrgentOnly tokens still attempt to focus the window.
                // activate_surface will raise, switch workspace, and focus if
                // the surface is already mapped.  For unmapped surfaces on a
                // different workspace it falls back to marking the workspace
                // as urgent internally.
                self.activate_surface(&surface, None);
            }
            ActivationContext::Workspace(_) => {
                match self.common.config.cosmic_conf.activation_policy {
                    ActivationPolicy::Focus => {
                        self.activate_surface(
                            &surface,
                            Some((ActivationKey::Wayland(surface.clone()), *context)),
                        );
                    }
                    ActivationPolicy::FocusIfActiveWorkspace => {
                        let shell = self.common.shell.write();

                        let Some((target_workspace, _)) = shell.workspace_for_surface(&surface)
                        else {
                            return;
                        };

                        let seat = shell.seats.last_active().clone();
                        let current_output = seat.active_output();
                        let current_workspace = shell.active_space(&current_output).unwrap().handle;

                        if target_workspace == current_workspace {
                            std::mem::drop(shell);
                            self.activate_surface(
                                &surface,
                                Some((ActivationKey::Wayland(surface.clone()), *context)),
                            );
                        } else {
                            let mut workspace_guard = self.common.workspace_state.update();
                            workspace_guard.add_workspace_state(&target_workspace, WState::Urgent);
                        }
                    }
                    ActivationPolicy::Urgent => {
                        let shell = self.common.shell.write();
                        if let Some((workspace, _output)) = shell.workspace_for_surface(&surface) {
                            let mut workspace_guard = self.common.workspace_state.update();
                            workspace_guard.add_workspace_state(&workspace, WState::Urgent);
                        }
                    }
                }
            }
        }
    }
}

impl State {
    /// The keyboard-focus target for `surface`, if it is a mapped layer surface
    /// that can take focus.
    ///
    /// Any layer works, not just `Top`/`Overlay`: `focus_target_is_valid` accepts
    /// a layer surface purely on it being mapped on the output, which is how
    /// click-to-focus already reaches a `Bottom` surface. What layer surfaces have
    /// lacked is a way for the client to *ask*, which is what routing
    /// xdg-activation here provides.
    fn layer_activation_target(
        &self,
        surface: &WlSurface,
    ) -> Option<(KeyboardFocusTarget, Seat<State>)> {
        let shell = self.common.shell.read();

        let layer = shell.outputs().find_map(|output| {
            layer_map_for_output(output)
                .layer_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .cloned()
        })?;

        // A surface the client asked to hide must not pull focus back, and one
        // that opted out of keyboard input must never take it — the same test the
        // pointer path applies before focusing a layer surface on click.
        if shell.is_surface_hidden(&layer.wl_surface().id()) || !layer.can_receive_keyboard_focus()
        {
            return None;
        }

        let seat = shell.seats.last_active().clone();
        Some((layer.into(), seat))
    }

    pub fn activate_surface(
        &mut self,
        surface: &WlSurface,
        pending_activation: Option<(ActivationKey, ActivationContext)>,
    ) {
        let mut shell = self.common.shell.write();

        let seat = shell.seats.last_active().clone();
        let current_output = seat.active_output();

        if let Some(element) = shell.element_for_surface(surface).cloned() {
            if element.is_minimized() {
                shell.unminimize_request(surface, &seat, &self.common.event_loop_handle);
            }

            let Some((element_output, element_workspace)) = shell
                .space_for(&element)
                .map(|w| (w.output.clone(), w.handle))
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
                        if wl_surface == surface {
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
                && current_workspace.is_tiled(surface)
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
        } else if let Some((workspace, _)) = shell.workspace_for_surface(surface) {
            let current_workspace = shell.active_space(&current_output).unwrap();
            if workspace == current_workspace.handle {
                let Some(target) = shell
                    .workspaces
                    .space_for_handle(&workspace)
                    .unwrap()
                    .get_fullscreen(&seat)
                    .map(|f| f.surface.clone())
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
                    .and_then(|w| w.get_fullscreen(&seat))
                    .map(|f| f.surface.clone())
                {
                    shell.append_focus_stack(surface, &seat)
                }
                let mut workspace_guard = self.common.workspace_state.update();
                workspace_guard.add_workspace_state(&workspace, WState::Urgent);
            }
        } else if let Some((activation_key, context)) = pending_activation {
            shell.pending_activations.insert(activation_key, context);
        };
    }
}
