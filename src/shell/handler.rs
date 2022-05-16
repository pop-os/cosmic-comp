// SPDX-License-Identifier: GPL-3.0-only

use crate::{config::Config, input::active_output, state::State, utils::SurfaceDropNotifier};
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    desktop::{
        layer_map_for_output, Kind, LayerSurface, PopupGrab, PopupKeyboardGrab, PopupKind,
        PopupPointerGrab, PopupUngrabStrategy, Window, WindowSurfaceType,
    },
    reexports::{
        wayland_protocols::xdg_shell::server::xdg_toplevel,
        wayland_server::{protocol::wl_surface::WlSurface, Display},
    },
    wayland::{
        compositor::{compositor_init, with_states},
        output::Output,
        seat::{PointerGrabStartData, Seat},
        shell::{
            wlr_layer::{wlr_layer_shell_init, LayerShellRequest, LayerSurfaceAttributes},
            xdg::{
                xdg_shell_init, Configure, XdgPopupSurfaceRoleAttributes, XdgRequest,
                XdgToplevelSurfaceRoleAttributes,
            },
        },
        Serial,
    },
};
use std::{cell::Cell, sync::Mutex};

pub type PopupGrabData = Cell<Option<PopupGrab>>;

pub fn init_shell(config: &Config, display: &mut Display) -> super::Shell {
    compositor_init(
        display,
        move |surface, mut ddata| {
            on_commit_buffer_handler(&surface);
            let state = ddata.get::<State>().unwrap();
            state
                .common
                .shell
                .commit(&surface, state.common.seats.iter());
            commit(&surface, state)
        },
        None,
    );

    let (_xdg_shell_state, _xdg_global) = xdg_shell_init(
        display,
        move |event, mut ddata| {
            let state = &mut ddata.get::<State>().unwrap().common;

            match event {
                XdgRequest::NewToplevel { surface } => {
                    state.pending_toplevels.push(surface.clone());

                    let seat = &state.last_active_seat;
                    let output = active_output(seat, &*state);
                    let space = state.shell.active_space_mut(&output);
                    let window = Window::new(Kind::Xdg(surface));
                    space.pending_window(window, seat);
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
                    if let Some(start_data) =
                        check_grab_preconditions(&seat, surface.get_surface(), serial)
                    {
                        let workspace = state
                            .shell
                            .space_for_surface_mut(surface.get_surface().unwrap())
                            .unwrap();
                        let window = workspace
                            .space
                            .window_for_surface(surface.get_surface().unwrap(), WindowSurfaceType::TOPLEVEL)
                            .unwrap()
                            .clone();

                        workspace.move_request(&window, &seat, serial, start_data);
                    }
                }

                XdgRequest::Resize {
                    surface,
                    seat,
                    serial,
                    edges,
                } => {
                    let seat = Seat::from_resource(&seat).unwrap();
                    if let Some(start_data) =
                        check_grab_preconditions(&seat, surface.get_surface(), serial)
                    {
                        let workspace = state
                            .shell
                            .space_for_surface_mut(surface.get_surface().unwrap())
                            .unwrap();
                        let window = workspace
                            .space
                            .window_for_surface(surface.get_surface().unwrap(), WindowSurfaceType::TOPLEVEL)
                            .unwrap()
                            .clone();

                        workspace.resize_request(&window, &seat, serial, start_data, edges);
                    }
                }
                XdgRequest::AckConfigure {
                    surface,
                    configure: Configure::Toplevel(configure),
                } => {
                    // TODO: This is way to hardcoded and hacky,  but wayland-rs 0.30 will make this unnecessary so don't bother.
                    if let Some(window) = state
                        .shell
                        .space_for_surface(&surface)
                        .and_then(|workspace| workspace.space.window_for_surface(&surface, WindowSurfaceType::TOPLEVEL))
                    {
                        crate::shell::layout::floating::ResizeSurfaceGrab::ack_configure(
                            window, configure,
                        )
                    }
                }
                XdgRequest::Maximize { surface } => {
                    if let Some(surface) = surface.get_surface() {
                        let seat = &state.last_active_seat;
                        let output = active_output(seat, &*state);

                        if let Some(workspace) = state.shell.space_for_surface_mut(surface) {
                            let window =
                                workspace.space.window_for_surface(surface, WindowSurfaceType::TOPLEVEL).unwrap().clone();
                            workspace.maximize_request(&window, &output)
                        }
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
                            state.set_focus(grab.current_grab().as_ref(), &seat, Some(serial));
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
                            pointer.set_grab(PopupPointerGrab::new(&grab), serial, 0);
                        }

                        seat.user_data()
                            .insert_if_missing(|| PopupGrabData::new(None));
                        seat.user_data()
                            .get::<PopupGrabData>()
                            .unwrap()
                            .set(Some(grab));
                    }
                }
                XdgRequest::Fullscreen { surface, output } => {
                    let output = output
                        .as_ref()
                        .and_then(Output::from_resource)
                        .unwrap_or_else(|| {
                            let seat = &state.last_active_seat;
                            active_output(seat, &*state)
                        });
                    if let Some(surface) = surface.get_surface() {
                        if let Some(workspace) = state.shell.space_for_surface_mut(surface) {
                            let window =
                                workspace.space.window_for_surface(surface, WindowSurfaceType::TOPLEVEL).unwrap().clone();
                            workspace.fullscreen_request(&window, &output)
                        }
                    }
                }
                XdgRequest::UnFullscreen { surface } => {
                    if let Some(surface) = surface.get_surface() {
                        if let Some(workspace) = state.shell.space_for_surface_mut(surface) {
                            let window =
                                workspace.space.window_for_surface(surface, WindowSurfaceType::TOPLEVEL).unwrap().clone();
                            workspace.unfullscreen_request(&window)
                        }
                    }
                }
                _ => { /*TODO*/ }
            }
        },
        None,
    );

    let (_layer_shell_state, _layer_global) = wlr_layer_shell_init(
        display,
        |event, mut ddata| match event {
            LayerShellRequest::NewLayerSurface {
                surface,
                output: wl_output,
                namespace,
                ..
            } => {
                let state = &mut ddata.get::<State>().unwrap().common;
                let seat = state.last_active_seat.clone();
                let output = wl_output
                    .as_ref()
                    .and_then(Output::from_resource)
                    .unwrap_or_else(|| active_output(&seat, &*state));
                state.shell.active_space_mut(&output).pending_layer(
                    LayerSurface::new(surface, namespace),
                    &output,
                    &seat,
                );
            }
            _ => {}
        },
        None,
    );

    super::Shell::new(config, display)
}

fn check_grab_preconditions(
    seat: &Seat,
    surface: Option<&WlSurface>,
    serial: Serial,
) -> Option<PointerGrabStartData> {
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

    Some(start_data)
}

fn commit(surface: &WlSurface, state: &mut State) {
    // TODO figure out which output the surface is on.
    for output in state.common.shell.outputs() {
        //.cloned().collect::<Vec<_>>().into_iter() {
        state
            .backend
            .schedule_render(&state.common.event_loop_handle, output);
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

        return;
    }

    // TODO: This is way to hardcoded and hacky,  but wayland-rs 0.30 will make this unnecessary so don't bother.
    if let Some((space, window)) =
        state
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
        let new_location = crate::shell::layout::floating::ResizeSurfaceGrab::apply_resize_state(
            &window,
            space.window_location(&window).unwrap(),
            window.geometry().size,
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

    if let Some(output) = state.shell.outputs().find(|o| {
        let map = layer_map_for_output(o);
        map.layer_for_surface(surface, WindowSurfaceType::TOPLEVEL).is_some()
    }) {
        let mut map = layer_map_for_output(output);
        let layer = map.layer_for_surface(surface, WindowSurfaceType::TOPLEVEL).unwrap();

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
