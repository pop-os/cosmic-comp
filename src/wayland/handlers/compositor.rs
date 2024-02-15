// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::CosmicSurface, state::ClientState, utils::prelude::*,
    wayland::protocols::screencopy::SessionType,
};
use calloop::Interest;
use smithay::{
    backend::renderer::utils::{on_commit_buffer_handler, with_renderer_surface_state},
    delegate_compositor,
    desktop::{layer_map_for_output, LayerSurface, PopupKind, WindowSurfaceType},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client, Resource},
    wayland::{
        compositor::{
            add_blocker, add_pre_commit_hook, with_states, BufferAssignment, CompositorClientState,
            CompositorHandler, CompositorState, SurfaceAttributes,
        },
        dmabuf::get_dmabuf,
        seat::WaylandFocus,
        shell::{
            wlr_layer::LayerSurfaceAttributes,
            xdg::{
                ToplevelSurface, XdgPopupSurfaceRoleAttributes, XdgToplevelSurfaceRoleAttributes,
            },
        },
    },
    xwayland::{X11Wm, XWaylandClientData},
};
use std::sync::Mutex;

use super::screencopy::PendingScreencopyBuffers;

impl State {
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
        if let PopupKind::Xdg(ref popup) = popup {
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

pub fn client_compositor_state<'a>(client: &'a Client) -> &'a CompositorClientState {
    if let Some(state) = client.get_data::<XWaylandClientData>() {
        return &state.compositor_state;
    }
    if let Some(state) = client.get_data::<ClientState>() {
        return &state.compositor_client_state;
    }
    panic!("Unknown client data type")
}

impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.common.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        client_compositor_state(client)
    }

    fn new_surface(&mut self, surface: &WlSurface) {
        add_pre_commit_hook::<Self, _>(surface, move |state, _dh, surface| {
            let maybe_dmabuf = with_states(surface, |surface_data| {
                surface_data
                    .cached_state
                    .pending::<SurfaceAttributes>()
                    .buffer
                    .as_ref()
                    .and_then(|assignment| match assignment {
                        BufferAssignment::NewBuffer(buffer) => get_dmabuf(buffer).ok(),
                        _ => None,
                    })
            });
            if let Some(dmabuf) = maybe_dmabuf {
                if let Ok((blocker, source)) = dmabuf.generate_blocker(Interest::READ) {
                    let client = surface.client().unwrap();
                    let res =
                        state
                            .common
                            .event_loop_handle
                            .insert_source(source, move |_, _, state| {
                                let dh = state.common.display_handle.clone();
                                state
                                    .client_compositor_state(&client)
                                    .blocker_cleared(state, &dh);
                                Ok(())
                            });
                    if res.is_ok() {
                        add_blocker(surface, blocker);
                    }
                }
            }
        })
    }

    fn commit(&mut self, surface: &WlSurface) {
        X11Wm::commit_hook::<State>(surface);
        // first load the buffer for various smithay helper functions
        on_commit_buffer_handler::<Self>(surface);

        // then handle initial configure events and map windows if necessary
        if let Some((window, _, _)) = self
            .common
            .shell
            .pending_windows
            .iter()
            .find(|(window, _, _)| window.wl_surface().as_ref() == Some(surface))
            .cloned()
        {
            match window {
                CosmicSurface::Wayland(ref wl_window) => {
                    let toplevel = wl_window.toplevel();
                    if self.toplevel_ensure_initial_configure(&toplevel)
                        && with_renderer_surface_state(&surface, |state| state.buffer().is_some())
                            .unwrap_or(false)
                    {
                        window.on_commit();
                        Shell::map_window(self, &window);
                    } else {
                        return;
                    }
                }
                CosmicSurface::X11(_) => {}
                _ => unreachable!(),
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
        if let Some(element) = self.common.shell.element_for_wl_surface(surface).cloned() {
            crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_to_location(
                element.clone(),
                &mut self.common.shell,
            );
            if let Some(workspace) = self.common.shell.space_for_mut(&element) {
                workspace.commit(surface);
            }
        }

        //handle window screencopy sessions
        self.schedule_window_session(surface);

        // and refresh smithays internal state
        self.common.shell.popups.commit(surface);

        // re-arrange layer-surfaces (commits may change size and positioning)
        let layer_output = self
            .common
            .shell
            .outputs()
            .find(|o| {
                let map = layer_map_for_output(o);
                map.layer_for_surface(surface, WindowSurfaceType::ALL)
                    .is_some()
            })
            .cloned();
        if let Some(output) = layer_output {
            let changed = layer_map_for_output(&output).arrange();
            if changed {
                self.common.shell.workspaces.recalculate();
            }
        }

        let mut scheduled_sessions = self.schedule_workspace_sessions(surface);

        // schedule a new render
        if let Some(output) = self.common.shell.visible_output_for_surface(surface) {
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
                                if &o == output =>
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
