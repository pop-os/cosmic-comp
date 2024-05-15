// SPDX-License-Identifier: GPL-3.0-only

use crate::{shell::grabs::SeatMoveGrabState, state::ClientState, utils::prelude::*};
use calloop::Interest;
use smithay::{
    backend::renderer::utils::{on_commit_buffer_handler, with_renderer_surface_state},
    delegate_compositor,
    desktop::{layer_map_for_output, LayerSurface, PopupKind, WindowSurfaceType},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client, Resource},
    utils::SERIAL_COUNTER,
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

fn toplevel_ensure_initial_configure(toplevel: &ToplevelSurface) -> bool {
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

fn xdg_popup_ensure_initial_configure(popup: &PopupKind) {
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

fn layer_surface_check_inital_configure(surface: &LayerSurface) -> bool {
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

    initial_configure_sent
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
        });
    }

    fn commit(&mut self, surface: &WlSurface) {
        X11Wm::commit_hook::<State>(self, surface);
        // first load the buffer for various smithay helper functions (which also initializes the RendererSurfaceState)
        on_commit_buffer_handler::<Self>(surface);

        // and refresh smithays internal state
        self.common.on_commit(surface);

        let mut shell = self.common.shell.write().unwrap();

        // schedule a new render
        if let Some(output) = shell.visible_output_for_surface(surface) {
            self.backend
                .schedule_render(&self.common.event_loop_handle, &output);
        }

        // handle initial configure events and map windows if necessary
        if let Some((window, _, _)) = shell
            .pending_windows
            .iter()
            .find(|(window, _, _)| window.wl_surface().as_ref() == Some(surface))
            .cloned()
        {
            if let Some(toplevel) = window.0.toplevel() {
                if toplevel_ensure_initial_configure(&toplevel)
                    && with_renderer_surface_state(&surface, |state| state.buffer().is_some())
                        .unwrap_or(false)
                {
                    window.on_commit();
                    let res = shell.map_window(
                        &window,
                        &mut self.common.toplevel_info_state,
                        &mut self.common.workspace_state,
                        &self.common.event_loop_handle,
                    );
                    if let Some(target) = res {
                        let seat = shell.seats.last_active().clone();
                        std::mem::drop(shell);
                        Shell::set_focus(self, Some(&target), &seat, None);
                        return;
                    }
                }
            }
        }

        if let Some((layer_surface, _, _)) = shell
            .pending_layers
            .iter()
            .find(|(layer_surface, _, _)| layer_surface.wl_surface() == surface)
            .cloned()
        {
            if !layer_surface_check_inital_configure(&layer_surface) {
                // compute initial dimensions by mapping
                if let Some(target) = shell.map_layer(&layer_surface) {
                    let seat = shell.seats.last_active().clone();
                    std::mem::drop(shell);
                    Shell::set_focus(self, Some(&target), &seat, None);
                }
                layer_surface.layer_surface().send_configure();
                return;
            }
        };

        if let Some(popup) = self.common.popups.find_popup(surface) {
            xdg_popup_ensure_initial_configure(&popup);
            return;
        }

        if with_renderer_surface_state(surface, |state| state.buffer().is_none()).unwrap_or(false) {
            // handle null-commits causing weird conflicts:

            // session-lock disallows null commits

            // if it was a move-grab?
            let seat = shell.seats.last_active().clone();
            let moved_window = seat
                .user_data()
                .get::<SeatMoveGrabState>()
                .unwrap()
                .borrow()
                .as_ref()
                .and_then(|state| {
                    state
                        .element()
                        .windows()
                        .any(|(s, _)| {
                            s.wl_surface()
                                .as_ref()
                                .map(|s| s == surface)
                                .unwrap_or(false)
                        })
                        .then(|| state.element())
                });
            if let Some(mut window) = moved_window {
                if window.is_stack() {
                    let stack = window.stack_ref_mut().unwrap();
                    if let Some(i) = stack.surfaces().position(|s| {
                        s.wl_surface()
                            .as_ref()
                            .map(|s| s == surface)
                            .unwrap_or(false)
                    }) {
                        stack.remove_idx(i);
                    }
                } else {
                    std::mem::drop(shell);
                    seat.get_pointer()
                        .unwrap()
                        .unset_grab(self, SERIAL_COUNTER.next_serial(), 0);
                    return;
                }
            }

            // if it was a layer-shell surface?
            // ignore, that will affect recompute normally

            // if it was a sticky / floating / tiled window
            // we could unmap, I guess?

            // if it was an x11 surface => do nothing, we will get a separate UnmapNotify anyway
        } else {
            // handle some special cases, like grabs and changing layer surfaces

            // If we would re-position the window inside the grab we would get a weird jittery animation.
            // We only want to resize once the client has acknoledged & commited the new size,
            // so we need to carefully track the state through different handlers.
            if let Some(element) = shell.element_for_surface(surface).cloned() {
                crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_to_location(
                    element.clone(),
                    &mut *shell,
                );
            }
        }

        // re-arrange layer-surfaces (commits may change size and positioning)
        let layer_output = shell
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
                shell.workspaces.recalculate();
            }
        }
    }
}

delegate_compositor!(State);
