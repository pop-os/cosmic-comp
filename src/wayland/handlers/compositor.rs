// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::BackendData, utils::prelude::*};
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    delegate_compositor,
    desktop::{Kind, LayerSurface, PopupKind, WindowSurfaceType, layer_map_for_output},
    reexports::wayland_server::{protocol::wl_surface::WlSurface, DisplayHandle},
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

impl State {
    fn early_import_surface(&mut self, dh: &DisplayHandle, surface: &WlSurface) {
        let mut import_nodes = std::collections::HashSet::new();
        for output in self.common.shell.outputs_for_surface(&surface) {
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
            self.backend
                .schedule_render(&self.common.event_loop_handle, &output);
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

    fn layer_surface_ensure_inital_configure(&mut self, surface: &LayerSurface, dh: &DisplayHandle) -> bool {
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
            self.common.shell.map_layer(&surface, dh);
            // this will also send a configure
        }
        initial_configure_sent
    }
}

impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.common.compositor_state
    }

    fn commit(&mut self, dh: &DisplayHandle, surface: &WlSurface) {
        // initial configure
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
                    if self.toplevel_ensure_initial_configure(&toplevel) {
                        let output = active_output(&seat, &self.common);
                        self.common.shell.map_window(&window, &output);
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
            if !self.layer_surface_ensure_inital_configure(&layer_surface, dh) {
                return;
            }
        };

        if let Some(popup) = self.common.shell.popups.find_popup(surface) {
            self.xdg_popup_ensure_initial_configure(&popup);
        }

        // If we would re-position the window inside the grab we would get a weird jittery animation.
        // We only want to resize once the client has acknoledged & commited the new size,
        // so we need to carefully track the state through different handlers.
        if let Some((space, window)) =
            self.common
                .shell
                .space_for_surface_mut(surface)
                .and_then(|workspace| {
                    workspace
                        .space
                        .window_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                        .cloned()
                        .map(|window| (&mut workspace.space, window))
                })
        {
            let new_location =
                crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_state(
                    &window,
                    space.window_location(&window).unwrap(),
                    window.geometry().size,
                );
            if let Some(location) = new_location {
                space.map_window(&window, location, true);
            }
        }

        if let Some(output) = self.common.shell.outputs().find(|o| {
            let map = layer_map_for_output(o);
            map.layer_for_surface(surface, WindowSurfaceType::ALL).is_some()
        }) {
            layer_map_for_output(output).arrange(dh);
        }

        on_commit_buffer_handler(surface);
        self.early_import_surface(dh, surface);
        self.common.shell.popups.commit(surface);
        for workspace in &self.common.shell.spaces {
            workspace.space.commit(surface);
        }
    }
}

delegate_compositor!(State);
