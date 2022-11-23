// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::BackendData, utils::prelude::*, wayland::protocols::screencopy::SessionType};
use smithay::{
    backend::renderer::utils::{on_commit_buffer_handler, with_renderer_surface_state},
    delegate_compositor,
    desktop::{layer_map_for_output, Kind, LayerSurface, PopupKind, WindowSurfaceType},
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::{
        compositor::{with_states, CompositorHandler, CompositorState},
        shell::{
            wlr_layer::LayerSurfaceAttributes,
            xdg::{
                ToplevelSurface, XdgPopupSurfaceRoleAttributes, XdgToplevelSurfaceRoleAttributes,
            },
        },
    },
};
use std::sync::Mutex;

use super::screencopy::PendingScreencopyBuffers;

impl State {
    fn early_import_surface(&mut self, surface: &WlSurface) {
        let mut import_nodes = std::collections::HashSet::new();
        let dh = &self.common.display_handle;
        for output in self.common.shell.visible_outputs_for_surface(&surface) {
            if let BackendData::Kms(ref mut kms_state) = &mut self.backend {
                if let Some(target) = kms_state.target_node_for_output(&output) {
                    if import_nodes.insert(target) {
                        kms_state.try_early_import(
                            dh,
                            surface,
                            &output,
                            target,
                            &self.common.shell,
                        );
                    }
                }
            }
        }
    }

    fn toplevel_ensure_initial_configure(&mut self, toplevel: &ToplevelSurface) -> bool {
        // send the initial configure if relevant
        let initial_configure_sent = with_states(toplevel.wl_surface(), |states| {
            states
                .data_map
                .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        });
        if !initial_configure_sent {
            // TODO: query expected size from shell (without inserting and mapping)
            toplevel.with_pending_state(|states| states.size = None);
            toplevel.send_configure();
        }
        initial_configure_sent
    }

    fn xdg_popup_ensure_initial_configure(&mut self, popup: &PopupKind) {
        let PopupKind::Xdg(ref popup) = popup;
        let initial_configure_sent = with_states(popup.wl_surface(), |states| {
            states
                .data_map
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        });
        if !initial_configure_sent {
            // NOTE: This should never fail as the initial configure is always
            // allowed.
            popup.send_configure().expect("initial configure failed");
        }
    }

    fn layer_surface_ensure_inital_configure(&mut self, surface: &LayerSurface) -> bool {
        // send the initial configure if relevant
        let initial_configure_sent = with_states(surface.wl_surface(), |states| {
            states
                .data_map
                .get::<Mutex<LayerSurfaceAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        });
        if !initial_configure_sent {
            // compute initial dimensions by mapping
            Shell::map_layer(self, &surface);
            surface.layer_surface().send_configure();
        }
        initial_configure_sent
    }
}

impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.common.compositor_state
    }

    fn commit(&mut self, surface: &WlSurface) {
        // first load the buffer for various smithay helper functions
        on_commit_buffer_handler(surface);

        // then handle initial configure events and map windows if necessary
        if let Some((window, seat)) = self
            .common
            .shell
            .pending_windows
            .iter()
            .find(|(window, _)| window.toplevel().wl_surface() == surface)
            .cloned()
        {
            match window.toplevel() {
                Kind::Xdg(toplevel) => {
                    if self.toplevel_ensure_initial_configure(&toplevel)
                        && with_renderer_surface_state(&surface, |state| {
                            state.wl_buffer().is_some()
                        })
                    {
                        let output = seat.active_output();
                        Shell::map_window(self, &window, &output);
                    } else {
                        return;
                    }
                }
            }
        }

        if let Some((layer_surface, _, _)) = self
            .common
            .shell
            .pending_layers
            .iter()
            .find(|(layer_surface, _, _)| layer_surface.wl_surface() == surface)
            .cloned()
        {
            if !self.layer_surface_ensure_inital_configure(&layer_surface) {
                return;
            }
        };

        if let Some(popup) = self.common.shell.popups.find_popup(surface) {
            self.xdg_popup_ensure_initial_configure(&popup);
        }

        // at last handle some special cases, like grabs and changing layer surfaces

        // If we would re-position the window inside the grab we would get a weird jittery animation.
        // We only want to resize once the client has acknoledged & commited the new size,
        // so we need to carefully track the state through different handlers.
        if let Some(element) = self.common.shell.element_for_surface(surface).cloned() {
            if let Some(workspace) = self.common.shell.space_for_mut(&element) {
                crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_to_location(
                    element.clone(),
                    workspace,
                );
                workspace.commit(surface);
            }
        }

        //handle window screencopy sessions
        self.schedule_window_session(surface);

        // We need to know every potential output for importing to the right gpu and scheduling a render,
        // so call this only after every potential surface map operation has been done.
        self.early_import_surface(surface);

        // and refresh smithays internal state
        self.common.shell.popups.commit(surface);

        // re-arrange layer-surfaces (commits may change size and positioning)
        if let Some(output) = self.common.shell.outputs().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL)
                .is_some()
        }) {
            layer_map_for_output(output).arrange();
        }

        let mut scheduled_sessions = self.schedule_workspace_sessions(surface);

        // schedule a new render
        for output in self.common.shell.visible_outputs_for_surface(surface) {
            if let Some(sessions) = output.user_data().get::<PendingScreencopyBuffers>() {
                scheduled_sessions
                    .get_or_insert_with(Vec::new)
                    .extend(sessions.borrow_mut().drain(..));
            }

            self.backend.schedule_render(
                &self.common.event_loop_handle,
                &output,
                scheduled_sessions.as_ref().map(|sessions| {
                    sessions
                        .iter()
                        .filter(|(s, _)| match s.session_type() {
                            SessionType::Output(o) | SessionType::Workspace(o, _)
                                if o == output =>
                            {
                                true
                            }
                            _ => false,
                        })
                        .cloned()
                        .collect::<Vec<_>>()
                }),
            );
        }
    }
}

delegate_compositor!(State);
