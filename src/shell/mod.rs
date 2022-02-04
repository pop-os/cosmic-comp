// SPDX-License-Identifier: GPL-3.0-only

use crate::{input::active_output, state::State, utils::SurfaceDropNotifier};
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    desktop::{
        layer_map_for_output, Kind, LayerSurface, PopupKeyboardGrab, PopupKind, PopupManager,
        PopupPointerGrab, PopupUngrabStrategy, Window,
    },
    reexports::{
        wayland_protocols::xdg_shell::server::xdg_toplevel,
        wayland_server::{protocol::wl_surface::WlSurface, Display},
    },
    wayland::{
        compositor::{compositor_init, with_states},
        output::Output,
        seat::{PointerGrabStartData, PointerHandle, Seat},
        shell::{
            wlr_layer::{
                wlr_layer_shell_init, LayerShellRequest, LayerShellState, LayerSurfaceAttributes,
            },
            xdg::{
                xdg_shell_init, Configure, ShellState as XdgShellState,
                XdgPopupSurfaceRoleAttributes, XdgRequest, XdgToplevelSurfaceRoleAttributes,
            },
        },
        Serial,
    },
};
use std::sync::{Arc, Mutex};

pub mod grabs;
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
            state.common.spaces.commit(&surface);
            state.common.shell.popups.commit(&surface);
            commit(&surface, state)
        },
        None,
    );

    let (xdg_shell_state, _xdg_global) = xdg_shell_init(
        display,
        |event, mut ddata| {
            let state = &mut ddata.get::<State>().unwrap().common;

            match event {
                XdgRequest::NewToplevel { surface } => {
                    state.pending_toplevels.push(surface.clone());

                    let seat = &state.last_active_seat;
                    let output = active_output(seat, &*state);
                    let space = state.spaces.active_space_mut(&output);
                    let window = Window::new(Kind::Xdg(surface));
                    space.map_window(&window, (0, 0), true);
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
                XdgRequest::Move {
                    surface,
                    seat,
                    serial,
                } => {
                    let seat = Seat::from_resource(&seat).unwrap();
                    if let Some((pointer, start_data)) =
                        check_grab_preconditions(&seat, surface.get_surface(), serial)
                    {
                        let space = state
                            .spaces
                            .space_for_surface(surface.get_surface().unwrap())
                            .unwrap();
                        let window = space
                            .window_for_surface(surface.get_surface().unwrap())
                            .unwrap()
                            .clone();
                        let mut initial_window_location =
                            space.window_geometry(&window).unwrap().loc;

                        // If surface is maximized then unmaximize it
                        if let Some(current_state) = surface.current_state() {
                            if current_state
                                .states
                                .contains(xdg_toplevel::State::Maximized)
                            {
                                let fs_changed = surface.with_pending_state(|state| {
                                    state.states.unset(xdg_toplevel::State::Maximized);
                                    state.size = None;
                                });

                                if fs_changed.is_ok() {
                                    surface.send_configure();

                                    // NOTE: In real compositor mouse location should be mapped to a new window size
                                    // For example, you could:
                                    // 1) transform mouse pointer position from compositor space to window space (location relative)
                                    // 2) divide the x coordinate by width of the window to get the percentage
                                    //   - 0.0 would be on the far left of the window
                                    //   - 0.5 would be in middle of the window
                                    //   - 1.0 would be on the far right of the window
                                    // 3) multiply the percentage by new window width
                                    // 4) by doing that, drag will look a lot more natural
                                    //
                                    // but for anvil needs setting location to pointer location is fine
                                    let pos = pointer.current_location();
                                    initial_window_location = (pos.x as i32, pos.y as i32).into();
                                }
                            }
                        }

                        let grab = grabs::MoveSurfaceGrab::new(
                            start_data,
                            window,
                            initial_window_location,
                        );

                        pointer.set_grab(grab, serial);
                    }
                }

                XdgRequest::Resize {
                    surface,
                    seat,
                    serial,
                    edges,
                } => {
                    let seat = Seat::from_resource(&seat).unwrap();
                    if let Some((pointer, start_data)) =
                        check_grab_preconditions(&seat, surface.get_surface(), serial)
                    {
                        let space = state
                            .spaces
                            .space_for_surface(surface.get_surface().unwrap())
                            .unwrap();
                        let window = space
                            .window_for_surface(surface.get_surface().unwrap())
                            .unwrap()
                            .clone();
                        let geometry = space.window_geometry(&window).unwrap();

                        let grab =
                            grabs::ResizeSurfaceGrab::new(start_data, window, edges, geometry);

                        pointer.set_grab(grab, serial);
                    }
                }
                XdgRequest::AckConfigure {
                    surface,
                    configure: Configure::Toplevel(configure),
                } => {
                    if let Some(window) = state
                        .spaces
                        .space_for_surface(&surface)
                        .and_then(|space| space.window_for_surface(&surface))
                    {
                        grabs::ResizeSurfaceGrab::ack_configure(window, configure)
                    }
                }
                XdgRequest::Maximize { surface } => {
                    let seat = &state.last_active_seat;
                    let output = active_output(seat, &*state);
                    let space = state.spaces.active_space_mut(&output);
                    let window = space
                        .window_for_surface(surface.get_surface().unwrap())
                        .unwrap()
                        .clone();
                    let layers = layer_map_for_output(&output);
                    let geometry = layers.non_exclusive_zone();

                    space.map_window(&window, geometry.loc, true);
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
                XdgRequest::Grab {
                    serial,
                    surface,
                    seat,
                } => {
                    let seat = Seat::from_resource(&seat).unwrap();
                    let ret = state.shell.popups.grab_popup(surface.into(), &seat, serial);

                    if let Ok(mut grab) = ret {
                        if let Some(keyboard) = seat.get_keyboard() {
                            if keyboard.is_grabbed()
                                && !(keyboard.has_grab(serial)
                                    || keyboard.has_grab(grab.previous_serial().unwrap_or(serial)))
                            {
                                grab.ungrab(PopupUngrabStrategy::All);
                                return;
                            }
                            keyboard.set_focus(grab.current_grab().as_ref(), serial);
                            keyboard.set_grab(PopupKeyboardGrab::new(&grab), serial);
                        }

                        if let Some(pointer) = seat.get_pointer() {
                            if pointer.is_grabbed()
                                && !(pointer.has_grab(serial)
                                    || pointer.has_grab(
                                        grab.previous_serial().unwrap_or_else(|| grab.serial()),
                                    ))
                            {
                                grab.ungrab(PopupUngrabStrategy::All);
                                return;
                            }
                            pointer.set_grab(PopupPointerGrab::new(&grab), serial);
                        }
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
                let state = &mut ddata.get::<State>().unwrap().common;
                let seat = &state.last_active_seat;
                let output = wl_output
                    .as_ref()
                    .and_then(Output::from_resource)
                    .unwrap_or_else(|| active_output(seat, &*state));

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

fn check_grab_preconditions(
    seat: &Seat,
    surface: Option<&WlSurface>,
    serial: Serial,
) -> Option<(PointerHandle, PointerGrabStartData)> {
    let surface = if let Some(surface) = surface {
        surface
    } else {
        return None;
    };

    // TODO: touch resize.
    let pointer = seat.get_pointer().unwrap();

    // Check that this surface has a click grab.
    if !pointer.has_grab(serial) {
        return None;
    }

    let start_data = pointer.grab_start_data().unwrap();

    // If the focus was for a different surface, ignore the request.
    if start_data.focus.is_none()
        || !start_data
            .focus
            .as_ref()
            .unwrap()
            .0
            .as_ref()
            .same_client_as(surface.as_ref())
    {
        return None;
    }

    Some((pointer, start_data))
}

fn commit(surface: &WlSurface, state: &mut State) {
    // TODO figure out which output the surface is on.
    for output in state.common.spaces.outputs() {//.cloned().collect::<Vec<_>>().into_iter() {
        state.backend.schedule_render(output);
        // let space = state.common.spaces.active_space(output);
        // get output for surface
    }

    let state = &mut state.common;
    let _ = with_states(surface, |states| {
        states
            .data_map
            .insert_if_missing(|| SurfaceDropNotifier::from(&*state));
    });

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
                    space.map_window(&window, position, true);
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

    if let Some((space, window)) = state
        .spaces
        .space_for_surface_mut(surface)
        .and_then(|space| {
            space
                .window_for_surface(surface)
                .cloned()
                .map(|window| (space, window))
        })
    {
        let new_location = grabs::ResizeSurfaceGrab::apply_resize_state(
            &window,
            space.window_geometry(&window).unwrap(),
        );
        if let Some(location) = new_location {
            space.map_window(&window, location, true);
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
