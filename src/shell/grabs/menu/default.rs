use cosmic_settings_config::shortcuts::Action;
use smithay::{
    input::pointer::MotionEvent, reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::SERIAL_COUNTER, wayland::seat::WaylandFocus,
};

use crate::{
    config::Config,
    fl,
    shell::{
        CosmicSurface, PointGlobalExt, Shell,
        element::{CosmicMapped, CosmicWindow},
        grabs::ReleaseMode,
    },
    state::State,
    utils::{prelude::SeatExt, screenshot::screenshot_window},
    wayland::protocols::workspace::WorkspaceHandle,
};

use super::{Item, ResizeEdge};

fn toggle_stacking(state: &mut State, mapped: &CosmicMapped) {
    let mut shell = state.common.shell.write();
    let seat = shell.seats.last_active().clone();
    if let Some(new_focus) = shell.toggle_stacking(&seat, mapped) {
        std::mem::drop(shell);
        Shell::set_focus(state, Some(&new_focus), &seat, None, false);
    }
}

fn prev_workspace(
    shell: &Shell,
    surface: &WlSurface,
) -> Option<(WorkspaceHandle, WorkspaceHandle)> {
    let (current_handle, output) = shell.workspace_for_surface(surface)?;
    shell
        .workspaces
        .spaces_for_output(&output)
        .enumerate()
        .find_map(|(i, space)| (space.handle == current_handle).then_some(i))
        .and_then(|i| i.checked_sub(1))
        .and_then(|i| shell.workspaces.get(i, &output))
        .map(|space| (current_handle, space.handle))
}

fn next_workspace(
    shell: &Shell,
    surface: &WlSurface,
) -> Option<(WorkspaceHandle, WorkspaceHandle)> {
    let (current_handle, output) = shell.workspace_for_surface(surface)?;
    shell
        .workspaces
        .spaces_for_output(&output)
        .skip_while(|space| space.handle != current_handle)
        .nth(1)
        .map(|space| (current_handle, space.handle))
}

fn move_fullscreen_prev_workspace(state: &mut State, surface: &CosmicSurface) {
    let mut shell = state.common.shell.write();
    let Some(wl_surface) = surface.wl_surface() else {
        return;
    };
    let Some((from, to)) = prev_workspace(&shell, &wl_surface) else {
        return;
    };

    let seat = shell.seats.last_active().clone();
    let res = shell.move_window(
        Some(&seat),
        surface,
        &from,
        &to,
        true,
        None,
        &mut state.common.workspace_state.update(),
        &state.common.event_loop_handle,
    );
    if let Some((target, _)) = res {
        std::mem::drop(shell);
        Shell::set_focus(state, Some(&target), &seat, None, true);
    }
}

fn move_fullscreen_next_workspace(state: &mut State, surface: &CosmicSurface) {
    let mut shell = state.common.shell.write();
    let Some(wl_surface) = surface.wl_surface() else {
        return;
    };
    let Some((from, to)) = next_workspace(&shell, &wl_surface) else {
        return;
    };

    let seat = shell.seats.last_active().clone();
    let res = shell.move_window(
        Some(&seat),
        surface,
        &from,
        &to,
        true,
        None,
        &mut state.common.workspace_state.update(),
        &state.common.event_loop_handle,
    );
    if let Some((target, _)) = res {
        std::mem::drop(shell);
        Shell::set_focus(state, Some(&target), &seat, None, true);
    }
}

fn move_element_prev_workspace(state: &mut State, mapped: &CosmicMapped) {
    let mut shell = state.common.shell.write();
    let window = mapped.active_window();
    let Some(wl_surface) = window.wl_surface() else {
        return;
    };
    let Some((from, to)) = prev_workspace(&shell, &wl_surface) else {
        return;
    };

    let seat = shell.seats.last_active().clone();
    let res = shell.move_element(
        Some(&seat),
        mapped,
        &from,
        &to,
        true,
        None,
        &mut state.common.workspace_state.update(),
    );
    if let Some((target, _)) = res {
        std::mem::drop(shell);
        Shell::set_focus(state, Some(&target), &seat, None, true);
    }
}

fn move_element_next_workspace(state: &mut State, mapped: &CosmicMapped) {
    let mut shell = state.common.shell.write();
    let window = mapped.active_window();
    let Some(wl_surface) = window.wl_surface() else {
        return;
    };
    let Some((from, to)) = next_workspace(&shell, &wl_surface) else {
        return;
    };

    let seat = shell.seats.last_active().clone();
    let res = shell.move_element(
        Some(&seat),
        mapped,
        &from,
        &to,
        true,
        None,
        &mut state.common.workspace_state.update(),
    );
    if let Some((target, _point)) = res {
        std::mem::drop(shell);
        Shell::set_focus(state, Some(&target), &seat, None, true)
    }
}

pub fn tab_items(
    stack: &CosmicMapped,
    tab: &CosmicSurface,
    is_tiled: bool,
    config: &Config,
) -> impl Iterator<Item = Item> {
    let unstack_clone_stack = stack.clone();
    let unstack_clone_tab = tab.clone();
    let screenshot_clone = tab.clone();
    let close_clone = tab.clone();

    vec![
        Item::new(fl!("window-menu-unstack"), move |handle| {
            let mapped = unstack_clone_stack.clone();
            let surface = unstack_clone_tab.clone();
            let _ = handle.insert_idle(move |state| {
                mapped.stack_ref().unwrap().remove_window(&surface);
                let mapped: CosmicMapped = CosmicWindow::new(
                    surface,
                    state.common.event_loop_handle.clone(),
                    state.common.theme.clone(),
                    state.common.config.cosmic_conf.appearance_settings,
                )
                .into();

                let mut shell = state.common.shell.write();
                let seat = shell.seats.last_active().clone();
                let output = seat.active_output();
                let workspace = shell.workspaces.active_mut(&output).unwrap();
                if is_tiled {
                    for mapped in workspace
                        .mapped()
                        .filter(|m| m.maximized_state.lock().unwrap().is_some())
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        workspace.unmaximize_request(&mapped);
                    }
                    let focus_stack = workspace.focus_stack.get(&seat);
                    workspace
                        .tiling_layer
                        .map(mapped, Some(focus_stack.iter()), None);
                } else {
                    workspace.floating_layer.map(mapped, None)
                }
            });
        }),
        Item::Separator,
        Item::new(fl!("window-menu-screenshot"), move |handle| {
            let tab = screenshot_clone.clone();
            let _ = handle.insert_idle(move |state| screenshot_window(state, &tab));
        }),
        Item::Separator,
        Item::new(fl!("window-menu-close"), move |_handle| {
            close_clone.close();
        })
        .shortcut(config.shortcut_for_action(&Action::Close)),
    ]
    .into_iter()
}

pub fn window_items(
    window: &CosmicMapped,
    is_tiled: bool,
    is_stacked: bool,
    is_sticky: bool,
    tiling_enabled: bool,
    possible_resizes: ResizeEdge,
    config: &Config,
) -> impl Iterator<Item = Item> {
    let minimize_clone = window.clone();
    let maximize_clone = window.clone();
    let fullscreen_clone = window.clone();
    let tile_clone = window.clone();
    let move_prev_clone = window.clone();
    let move_next_clone = window.clone();
    let move_clone = window.clone();
    let resize_top_clone = window.clone();
    let resize_left_clone = window.clone();
    let resize_right_clone = window.clone();
    let resize_bottom_clone = window.clone();
    let unstack_clone = window.clone();
    let screenshot_clone = window.clone();
    let stack_clone = window.clone();
    let sticky_clone = window.clone();
    let close_clone = window.clone();

    vec![
        (!is_stacked).then_some(
            Item::new(fl!("window-menu-stack"), move |handle| {
                let mapped = stack_clone.clone();
                let _ = handle.insert_idle(move |state| toggle_stacking(state, &mapped));
            })
            .shortcut(config.shortcut_for_action(&Action::ToggleStacking)),
        ),
        is_stacked.then_some(
            Item::new(fl!("window-menu-unstack-all"), move |handle| {
                let mapped = unstack_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    toggle_stacking(state, &mapped);
                });
            })
            .shortcut(config.shortcut_for_action(&Action::ToggleStacking)),
        ),
        Some(Item::Separator),
        Some(
            Item::new(fl!("window-menu-minimize"), move |handle| {
                let mapped = minimize_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    state
                        .common
                        .shell
                        .write()
                        .minimize_request(&mapped.active_window());
                });
            })
            .shortcut(config.shortcut_for_action(&Action::Minimize)),
        ),
        Some(
            Item::new(fl!("window-menu-maximize"), move |handle| {
                let mapped = maximize_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    shell.maximize_toggle(&mapped, &seat, &state.common.event_loop_handle);
                });
            })
            .shortcut(config.shortcut_for_action(&Action::Maximize))
            .toggled(window.is_maximized(false)),
        ),
        Some(
            Item::new(fl!("window-menu-fullscreen"), move |handle| {
                let mapped = fullscreen_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    let output = seat.active_output();
                    if let Some(target) = shell.fullscreen_request(
                        &mapped.active_window(),
                        output,
                        &state.common.event_loop_handle,
                    ) {
                        std::mem::drop(shell);
                        Shell::set_focus(state, Some(&target), &seat, None, false);
                    }
                });
            })
            .shortcut(config.shortcut_for_action(&Action::Fullscreen))
            .toggled(false),
        ),
        (tiling_enabled && !is_sticky).then_some(
            Item::new(fl!("window-menu-tiled"), move |handle| {
                let tile_clone = tile_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    if let Some(ws) = shell.space_for_mut(&tile_clone) {
                        ws.toggle_floating_window(&seat, &tile_clone);
                    }
                });
            })
            .shortcut(config.shortcut_for_action(&Action::ToggleWindowFloating))
            .toggled(!is_tiled),
        ),
        Some(Item::Separator),
        // TODO: Where to save?
        Some(Item::new(fl!("window-menu-screenshot"), move |handle| {
            let mapped = screenshot_clone.clone();
            let _ =
                handle.insert_idle(move |state| screenshot_window(state, &mapped.active_window()));
        })),
        Some(Item::Separator),
        Some(Item::new(fl!("window-menu-move"), move |handle| {
            let move_clone = move_clone.clone();
            let _ = handle.insert_idle(move |state| {
                if let Some(surface) = move_clone.wl_surface() {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    let res = shell.move_request(
                        &surface,
                        &seat,
                        None,
                        ReleaseMode::Click,
                        false,
                        &state.common.config,
                        &state.common.event_loop_handle,
                        false,
                    );

                    std::mem::drop(shell);
                    if let Some((grab, focus)) = res {
                        if grab.is_touch_grab() {
                            seat.get_touch().unwrap().set_grab(
                                state,
                                grab,
                                SERIAL_COUNTER.next_serial(),
                            )
                        } else {
                            seat.get_pointer().unwrap().set_grab(
                                state,
                                grab,
                                SERIAL_COUNTER.next_serial(),
                                focus,
                            );
                        }
                    }
                }
            });
        })),
        Some(Item::new_submenu(
            fl!("window-menu-resize"),
            vec![
                Item::new(fl!("window-menu-resize-edge-top"), move |handle| {
                    let resize_clone = resize_top_clone.clone();
                    let _ = handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        let seat = shell.seats.last_active().clone();
                        let res = shell.menu_resize_request(
                            &resize_clone,
                            &seat,
                            ResizeEdge::TOP,
                            state.common.config.cosmic_conf.edge_snap_threshold,
                        );

                        std::mem::drop(shell);
                        if let Some(((target, loc), (grab, focus))) = res {
                            let serial = SERIAL_COUNTER.next_serial();
                            if grab.is_touch_grab() {
                                seat.get_touch().unwrap().set_grab(state, grab, serial);
                            } else {
                                let pointer = seat.get_pointer().unwrap();
                                pointer.motion(
                                    state,
                                    target,
                                    &MotionEvent {
                                        location: loc.as_logical().to_f64(),
                                        serial,
                                        time: state.common.clock.now().as_millis(),
                                    },
                                );
                                pointer.frame(state);
                                pointer.set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                })
                .disabled(!possible_resizes.contains(ResizeEdge::TOP)),
                Item::new(fl!("window-menu-resize-edge-left"), move |handle| {
                    let resize_clone = resize_left_clone.clone();
                    let _ = handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        let seat = shell.seats.last_active().clone();
                        let res = shell.menu_resize_request(
                            &resize_clone,
                            &seat,
                            ResizeEdge::LEFT,
                            state.common.config.cosmic_conf.edge_snap_threshold,
                        );

                        std::mem::drop(shell);
                        if let Some(((target, loc), (grab, focus))) = res {
                            let serial = SERIAL_COUNTER.next_serial();
                            if grab.is_touch_grab() {
                                seat.get_touch().unwrap().set_grab(state, grab, serial);
                            } else {
                                let pointer = seat.get_pointer().unwrap();
                                pointer.motion(
                                    state,
                                    target,
                                    &MotionEvent {
                                        location: loc.as_logical().to_f64(),
                                        serial,
                                        time: state.common.clock.now().as_millis(),
                                    },
                                );
                                pointer.frame(state);
                                pointer.set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                })
                .disabled(!possible_resizes.contains(ResizeEdge::LEFT)),
                Item::new(fl!("window-menu-resize-edge-right"), move |handle| {
                    let resize_clone = resize_right_clone.clone();
                    let _ = handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        let seat = shell.seats.last_active().clone();
                        let res = shell.menu_resize_request(
                            &resize_clone,
                            &seat,
                            ResizeEdge::RIGHT,
                            state.common.config.cosmic_conf.edge_snap_threshold,
                        );

                        std::mem::drop(shell);
                        if let Some(((target, loc), (grab, focus))) = res {
                            let serial = SERIAL_COUNTER.next_serial();
                            if grab.is_touch_grab() {
                                seat.get_touch().unwrap().set_grab(state, grab, serial);
                            } else {
                                let pointer = seat.get_pointer().unwrap();
                                pointer.motion(
                                    state,
                                    target,
                                    &MotionEvent {
                                        location: loc.as_logical().to_f64(),
                                        serial,
                                        time: state.common.clock.now().as_millis(),
                                    },
                                );
                                pointer.frame(state);
                                pointer.set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                })
                .disabled(!possible_resizes.contains(ResizeEdge::RIGHT)),
                Item::new(fl!("window-menu-resize-edge-bottom"), move |handle| {
                    let resize_clone = resize_bottom_clone.clone();
                    let _ = handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        let seat = shell.seats.last_active().clone();
                        let res = shell.menu_resize_request(
                            &resize_clone,
                            &seat,
                            ResizeEdge::BOTTOM,
                            state.common.config.cosmic_conf.edge_snap_threshold,
                        );

                        std::mem::drop(shell);
                        if let Some(((target, loc), (grab, focus))) = res {
                            let serial = SERIAL_COUNTER.next_serial();
                            if grab.is_touch_grab() {
                                seat.get_touch().unwrap().set_grab(state, grab, serial);
                            } else {
                                let pointer = seat.get_pointer().unwrap();
                                pointer.motion(
                                    state,
                                    target,
                                    &MotionEvent {
                                        location: loc.as_logical().to_f64(),
                                        serial,
                                        time: state.common.clock.now().as_millis(),
                                    },
                                );
                                pointer.frame(state);
                                pointer.set_grab(state, grab, serial, focus);
                            }
                        }
                    });
                })
                .disabled(!possible_resizes.contains(ResizeEdge::BOTTOM)),
            ],
        )),
        Some(
            Item::new(fl!("window-menu-move-prev-workspace"), move |handle| {
                let mapped = move_prev_clone.clone();
                let _ =
                    handle.insert_idle(move |state| move_element_prev_workspace(state, &mapped));
            })
            .shortcut(config.shortcut_for_action(&Action::MoveToPreviousWorkspace))
            .disabled(is_sticky),
        ),
        Some(
            Item::new(fl!("window-menu-move-next-workspace"), move |handle| {
                let mapped = move_next_clone.clone();
                let _ =
                    handle.insert_idle(move |state| move_element_next_workspace(state, &mapped));
            })
            .shortcut(config.shortcut_for_action(&Action::MoveToNextWorkspace))
            .disabled(is_sticky),
        ),
        Some(Item::Separator),
        Some(
            Item::new(fl!("window-menu-sticky"), move |handle| {
                let mapped = sticky_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    shell.toggle_sticky(&seat, &mapped);
                });
            })
            .toggled(is_sticky),
        ),
        Some(Item::Separator),
        if is_stacked {
            Some(Item::new(fl!("window-menu-close-all"), move |_handle| {
                for (window, _) in close_clone.windows() {
                    window.close();
                }
            }))
        } else {
            Some(
                Item::new(fl!("window-menu-close"), move |_handle| {
                    close_clone.send_close();
                })
                .shortcut(config.shortcut_for_action(&Action::Close)),
            )
        },
    ]
    .into_iter()
    .flatten()
}

pub fn fullscreen_items(window: &CosmicSurface, config: &Config) -> impl Iterator<Item = Item> {
    let minimize_clone = window.clone();
    let fullscreen_clone = window.clone();
    let move_prev_clone = window.clone();
    let move_next_clone = window.clone();
    let move_clone = window.clone();
    let screenshot_clone = window.clone();
    let close_clone = window.clone();

    vec![
        Some(
            Item::new(fl!("window-menu-minimize"), move |handle| {
                let window = minimize_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    state.common.shell.write().minimize_request(&window);
                });
            })
            .shortcut(config.shortcut_for_action(&Action::Minimize)),
        ),
        Some(
            Item::new(fl!("window-menu-fullscreen"), move |handle| {
                let window = fullscreen_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let mut shell = state.common.shell.write();
                    if let Some(target) =
                        shell.unfullscreen_request(&window, &state.common.event_loop_handle)
                    {
                        let seat = shell.seats.last_active().clone();
                        std::mem::drop(shell);
                        Shell::set_focus(state, Some(&target), &seat, None, true);
                    }
                });
            })
            .shortcut(config.shortcut_for_action(&Action::Fullscreen))
            .toggled(true),
        ),
        Some(Item::Separator),
        // TODO: Where to save?
        Some(Item::new(fl!("window-menu-screenshot"), move |handle| {
            let window = screenshot_clone.clone();
            let _ = handle.insert_idle(move |state| screenshot_window(state, &window));
        })),
        Some(Item::Separator),
        Some(Item::new(fl!("window-menu-move"), move |handle| {
            let move_clone = move_clone.clone();
            let _ = handle.insert_idle(move |state| {
                if let Some(surface) = move_clone.wl_surface() {
                    let mut shell = state.common.shell.write();
                    let seat = shell.seats.last_active().clone();
                    let res = shell.move_request(
                        &surface,
                        &seat,
                        None,
                        ReleaseMode::Click,
                        false,
                        &state.common.config,
                        &state.common.event_loop_handle,
                        false,
                    );

                    std::mem::drop(shell);
                    if let Some((grab, focus)) = res {
                        if grab.is_touch_grab() {
                            seat.get_touch().unwrap().set_grab(
                                state,
                                grab,
                                SERIAL_COUNTER.next_serial(),
                            )
                        } else {
                            seat.get_pointer().unwrap().set_grab(
                                state,
                                grab,
                                SERIAL_COUNTER.next_serial(),
                                focus,
                            );
                        }
                    }
                }
            });
        })),
        Some(
            Item::new(fl!("window-menu-move-prev-workspace"), move |handle| {
                let window = move_prev_clone.clone();
                let _ =
                    handle.insert_idle(move |state| move_fullscreen_prev_workspace(state, &window));
            })
            .shortcut(config.shortcut_for_action(&Action::MoveToPreviousWorkspace)),
        ),
        Some(
            Item::new(fl!("window-menu-move-next-workspace"), move |handle| {
                let window = move_next_clone.clone();
                let _ =
                    handle.insert_idle(move |state| move_fullscreen_next_workspace(state, &window));
            })
            .shortcut(config.shortcut_for_action(&Action::MoveToNextWorkspace)),
        ),
        Some(Item::Separator),
        Some(
            Item::new(fl!("window-menu-close"), move |_handle| {
                close_clone.close();
            })
            .shortcut(config.shortcut_for_action(&Action::Close)),
        ),
    ]
    .into_iter()
    .flatten()
}
