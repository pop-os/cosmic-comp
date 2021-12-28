// SPDX-License-Identifier: GPL-3.0-only

use crate::{input::active_output, state::State};
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    desktop::{layer_map_for_output, Kind, LayerSurface, PopupKind, PopupManager, Window},
    reexports::{
        wayland_protocols::xdg_shell::server::xdg_toplevel,
        wayland_server::{protocol::wl_surface::WlSurface, Display},
    },
    wayland::{
        compositor::{compositor_init, with_states},
        output::Output,
        shell::{
            wlr_layer::{
                wlr_layer_shell_init, LayerShellRequest, LayerShellState, LayerSurfaceAttributes,
            },
            xdg::{
                xdg_shell_init, Configure, ShellState as XdgShellState,
                XdgPopupSurfaceRoleAttributes, XdgRequest, XdgToplevelSurfaceRoleAttributes,
            },
        },
    },
};
use std::sync::{Arc, Mutex};

pub mod workspaces;

pub struct ShellStates {
    popups: PopupManager,
    xdg: Arc<Mutex<XdgShellState>>,
    layer: Arc<Mutex<LayerShellState>>,
}

pub fn init_shell(display: &mut Display) -> ShellStates {
    compositor_init(
        display,
        move |surface, mut ddata| {
            on_commit_buffer_handler(&surface);
            let state = ddata.get::<State>().unwrap();
            state.spaces.commit(&surface);
            state.shell.popups.commit(&surface);
            commit(&surface, state)
        },
        None,
    );

    let (xdg_shell_state, _xdg_global) = xdg_shell_init(
        display,
        |event, mut ddata| {
            let state = ddata.get::<State>().unwrap();

            match event {
                XdgRequest::NewToplevel { surface } => {
                    state.pending_toplevels.push(surface.clone());

                    let seat = &state.last_active_seat;
                    let output = active_output(seat, &state);
                    let space = state.spaces.active_space_mut(&output);
                    let window = Window::new(Kind::Xdg(surface));
                    space.map_window(&window, (0, 0));
                    // We will position the window after the first commit, when we know its size
                }
                XdgRequest::NewPopup { surface, .. } => {
                    state
                        .shell
                        .popups
                        .track_popup(PopupKind::from(surface))
                        .unwrap();
                }
                XdgRequest::RePosition {
                    surface,
                    positioner,
                    token,
                } => {
                    let result = surface.with_pending_state(|state| {
                        // TODO: This is a simplification, a proper compositor would
                        // calculate the geometry of the popup here.
                        // For now we just use the default implementation here that does not take the
                        // window position and output constraints into account.
                        let geometry = positioner.get_geometry();
                        state.geometry = geometry;
                        state.positioner = positioner;
                    });

                    if result.is_ok() {
                        surface.send_repositioned(token);
                    }
                }
                /*
                XdgRequest::AckConfigure { surface, configure: Configure::Toplevel(configure) } => {

                },
                */
                XdgRequest::Maximize { surface } => {
                    let seat = &state.last_active_seat;
                    let output = active_output(seat, &state);
                    let space = state.spaces.active_space_mut(&output);
                    let window = space
                        .window_for_surface(surface.get_surface().unwrap())
                        .unwrap()
                        .clone();
                    let layers = layer_map_for_output(&output);
                    let geometry = layers.non_exclusive_zone();

                    space.map_window(&window, geometry.loc);
                    let ret = surface.with_pending_state(|state| {
                        state.states.set(xdg_toplevel::State::Maximized);
                        state.size = Some(geometry.size);
                    });

                    if ret.is_ok() {
                        window.configure();
                    }
                }
                XdgRequest::UnMaximize { surface } => {
                    let ret = surface.with_pending_state(|state| {
                        state.states.unset(xdg_toplevel::State::Maximized);
                        state.size = None;
                    });

                    if ret.is_ok() {
                        surface.send_configure();
                    }
                }
                _ => { /*TODO*/ }
            }
        },
        None,
    );

    let (layer_shell_state, _layer_global) = wlr_layer_shell_init(
        display,
        |event, mut ddata| match event {
            LayerShellRequest::NewLayerSurface {
                surface,
                output: wl_output,
                namespace,
                ..
            } => {
                let state = ddata.get::<State>().unwrap();
                let seat = &state.last_active_seat;
                let output = wl_output
                    .as_ref()
                    .and_then(Output::from_resource)
                    .unwrap_or_else(|| active_output(seat, &state));

                let mut map = layer_map_for_output(&output);
                map.map_layer(&LayerSurface::new(surface, namespace))
                    .unwrap();
            }
            _ => {}
        },
        None,
    );

    ShellStates {
        popups: PopupManager::new(None),
        xdg: xdg_shell_state,
        layer: layer_shell_state,
    }
}

fn commit(surface: &WlSurface, state: &mut State) {
    if let Some(toplevel) = state.pending_toplevels.iter().find(|toplevel| {
        toplevel
            .get_surface()
            .map(|s| s == surface)
            .unwrap_or(false)
    }) {
        // send the initial configure if relevant
        let initial_configure_sent = with_states(surface, |states| {
            states
                .data_map
                .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        })
        .unwrap();
        if !initial_configure_sent {
            toplevel.send_configure();
        }

        // position our new window
        if let Some(space) = toplevel
            .get_surface()
            .and_then(|surface| state.spaces.space_for_surface_mut(surface))
        {
            let window = space
                .window_for_surface(toplevel.get_surface().unwrap())
                .unwrap()
                .clone();
            if let Some(output) = space.outputs_for_window(&window).iter().next() {
                let win_geo = window.bbox();
                if win_geo.size.w > 0 && win_geo.size.h > 0 {
                    let layers = layer_map_for_output(&output);
                    let geometry = layers.non_exclusive_zone();

                    let position = (
                        geometry.loc.x + (geometry.size.w / 2) - (win_geo.size.w / 2),
                        geometry.loc.y + (geometry.size.h / 2) - (win_geo.size.h / 2),
                    );
                    space.map_window(&window, position);
                    state.pending_toplevels.retain(|toplevel| {
                        toplevel
                            .get_surface()
                            .map(|s| s != surface)
                            .unwrap_or(false)
                    });
                }
            }
        }

        return;
    }

    if let Some(popup) = state.shell.popups.find_popup(surface) {
        let PopupKind::Xdg(ref popup) = popup;
        let initial_configure_sent = with_states(surface, |states| {
            states
                .data_map
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        })
        .unwrap();
        if !initial_configure_sent {
            // NOTE: This should never fail as the initial configure is always
            // allowed.
            popup.send_configure().expect("initial configure failed");
        }

        return;
    }

    if let Some(output) = state.spaces.outputs().find(|o| {
        let map = layer_map_for_output(o);
        map.layer_for_surface(surface).is_some()
    }) {
        let mut map = layer_map_for_output(output);
        let layer = map.layer_for_surface(surface).unwrap();

        // send the initial configure if relevant
        let initial_configure_sent = with_states(surface, |states| {
            states
                .data_map
                .get::<Mutex<LayerSurfaceAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .initial_configure_sent
        })
        .unwrap();
        if !initial_configure_sent {
            layer.layer_surface().send_configure();
        }

        map.arrange();

        return;
    };
}
