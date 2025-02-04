// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::ZcosmicWorkspaceHandleV1;
use smithay::{
    desktop::{layer_map_for_output, WindowSurfaceType},
    input::{pointer::MotionEvent, Seat},
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Point, Rectangle, Size, SERIAL_COUNTER},
};

use crate::{
    shell::{element::CosmicWindow, CosmicSurface, Shell, WorkspaceDelta},
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoHandler,
        toplevel_management::{
            delegate_toplevel_management, toplevel_rectangle_for, ManagementWindow,
            ToplevelManagementHandler, ToplevelManagementState,
        },
    },
};

impl ToplevelManagementHandler for State {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState {
        &mut self.common.toplevel_management_state
    }

    fn activate(
        &mut self,
        dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        seat: Option<Seat<Self>>,
    ) {
        self.unminimize(dh, window);

        let mut shell = self.common.shell.write().unwrap();
        for output in shell.outputs().cloned().collect::<Vec<_>>().iter() {
            let maybe = shell
                .workspaces
                .spaces_for_output(output)
                .enumerate()
                .find(|(_, w)| {
                    w.mapped()
                        .flat_map(|m| m.windows().map(|(s, _)| s))
                        .any(|w| &w == window)
                });
            if let Some((idx, workspace)) = maybe {
                let seat = seat.unwrap_or(shell.seats.last_active().clone());
                let mapped = workspace
                    .mapped()
                    .find(|m| m.windows().any(|(w, _)| &w == window))
                    .unwrap()
                    .clone();

                let res = shell.activate(
                    &output,
                    idx,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );
                std::mem::drop(shell);

                if seat.active_output() != *output {
                    match res {
                        Ok(Some(new_pos)) => {
                            seat.set_active_output(&output);
                            if let Some(ptr) = seat.get_pointer() {
                                let serial = SERIAL_COUNTER.next_serial();
                                ptr.motion(
                                    self,
                                    None,
                                    &MotionEvent {
                                        location: new_pos.to_f64().as_logical(),
                                        serial,
                                        time: self.common.clock.now().as_millis(),
                                    },
                                );
                                ptr.frame(self);
                            }
                        }
                        Ok(None) => {
                            seat.set_active_output(&output);
                        }
                        _ => {}
                    }
                }

                mapped.focus_window(window);
                Shell::set_focus(self, Some(&mapped.clone().into()), &seat, None, false);
                return;
            }
        }
    }

    fn close(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        window.close();
    }

    fn move_to_workspace(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        workspace: ZcosmicWorkspaceHandleV1,
        _output: Output,
    ) {
        let Some(to_handle) = self.common.workspace_state.get_workspace_handle(&workspace) else {
            return;
        };

        let mut shell = self.common.shell.write().unwrap();
        if let Some(mut mapped) = shell.element_for_surface(window).cloned() {
            if let Some(from_workspace) = shell.space_for_mut(&mapped) {
                // If window is part of a stack, remove it and map it outside the stack
                if let Some(stack) = mapped.stack_ref() {
                    stack.remove_window(&window);
                    mapped = CosmicWindow::new(
                        window.clone(),
                        self.common.event_loop_handle.clone(),
                        self.common.theme.clone(),
                    )
                    .into();
                    if from_workspace.tiling_enabled {
                        from_workspace.tiling_layer.map(
                            mapped.clone(),
                            None::<std::iter::Empty<_>>,
                            None,
                        );
                    } else {
                        from_workspace.floating_layer.map(mapped.clone(), None);
                    }
                }

                let from_handle = from_workspace.handle;
                let seat = shell.seats.last_active().clone();
                let res = shell.move_window(
                    Some(&seat),
                    &mapped,
                    &from_handle,
                    &to_handle,
                    false,
                    None,
                    &mut self.common.workspace_state.update(),
                );
                if let Some((target, _)) = res {
                    std::mem::drop(shell);
                    Shell::set_focus(self, Some(&target), &seat, None, true);
                }
            }
        }
    }

    fn fullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        output: Option<Output>,
    ) {
        let mut shell = self.common.shell.write().unwrap();
        let seat = shell.seats.last_active().clone();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            if let Some((output, workspace)) =
                output.and_then(|output| shell.workspaces.active_mut(&output).map(|w| (output, w)))
            {
                let from = minimize_rectangle(&output, window);
                workspace.fullscreen_request(window, None, from, &seat);
            } else if let Some((output, handle)) = shell
                .space_for(&mapped)
                .map(|workspace| (workspace.output.clone(), workspace.handle.clone()))
            {
                let from = minimize_rectangle(&output, window);
                shell
                    .workspaces
                    .space_for_handle_mut(&handle)
                    .unwrap()
                    .fullscreen_request(window, None, from, &seat);
            }
        }
    }

    fn unfullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            if let Some(workspace) = shell.space_for_mut(&mapped) {
                if let Some((layer, previous_workspace)) = workspace.unfullscreen_request(window) {
                    let old_handle = workspace.handle.clone();
                    let new_workspace_handle = shell
                        .workspaces
                        .space_for_handle(&previous_workspace)
                        .is_some()
                        .then_some(previous_workspace)
                        .unwrap_or(old_handle); // if the workspace doesn't exist anymore, we can still remap on the right layer

                    shell.remap_unfullscreened_window(
                        mapped,
                        &old_handle,
                        &new_workspace_handle,
                        layer,
                    );
                }
            }
        }
    }

    fn maximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat, true);
        }
    }

    fn unmaximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            shell.unmaximize_request(&mapped);
        }
    }

    fn minimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            if !mapped.is_stack() || &mapped.active_window() == window {
                shell.minimize_request(&mapped);
            }
        }
    }

    fn unminimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.unminimize_request(&mapped, &seat);
            if mapped.is_stack() {
                mapped.stack_ref().unwrap().set_active(window);
            }
        }
    }

    fn set_sticky(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if window.is_sticky() {
            return;
        }

        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.toggle_sticky(&seat, &mapped);
        }
    }

    fn unset_sticky(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        if !window.is_sticky() {
            return;
        }

        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.toggle_sticky(&seat, &mapped);
        }
    }
}

impl ManagementWindow for CosmicSurface {
    fn close(&self) {
        CosmicSurface::close(self)
    }
}

pub fn minimize_rectangle(output: &Output, window: &CosmicSurface) -> Rectangle<i32, Local> {
    toplevel_rectangle_for(window)
        .find_map(|(surface, relative)| {
            let map = layer_map_for_output(output);
            let layer = map.layer_for_surface(&surface, WindowSurfaceType::ALL);
            layer.and_then(|s| map.layer_geometry(s)).map(|local| {
                Rectangle::new(
                    Point::from((local.loc.x + relative.loc.x, local.loc.y + relative.loc.y)),
                    relative.size,
                )
            })
        })
        .unwrap_or_else(|| {
            let output_size = output.geometry().size;
            Rectangle::new(
                Point::from((
                    (output_size.w / 2) - 100,
                    output_size.h - (output_size.h / 3) - 50,
                )),
                Size::from((200, 100)),
            )
        })
        .as_local()
}

delegate_toplevel_management!(State);
