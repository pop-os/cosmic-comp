// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    desktop::{layer_map_for_output, WindowSurfaceType},
    input::{pointer::MotionEvent, Seat},
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Point, Rectangle, Size, SERIAL_COUNTER},
    wayland::seat::WaylandFocus,
};

use crate::{
    shell::{focus::target::KeyboardFocusTarget, CosmicSurface, Shell, WorkspaceDelta},
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoHandler,
        toplevel_management::{
            delegate_toplevel_management, toplevel_rectangle_for, ManagementWindow,
            ToplevelManagementHandler, ToplevelManagementState,
        },
        workspace::WorkspaceHandle,
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

        let mut shell = self.common.shell.write();
        for output in shell.outputs().cloned().collect::<Vec<_>>().iter() {
            let maybe = shell
                .workspaces
                .spaces_for_output(output)
                .enumerate()
                .find(|(_, w)| {
                    w.get_fullscreen().is_some_and(|f| f == window)
                        || w.mapped()
                            .flat_map(|m| m.windows().map(|(s, _)| s))
                            .any(|w| &w == window)
                });
            if let Some((idx, workspace)) = maybe {
                let seat = seat.unwrap_or(shell.seats.last_active().clone());

                let handle = workspace.handle;
                let res = shell.activate(
                    &output,
                    idx,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                );

                let workspace = shell.workspaces.space_for_handle_mut(&handle).unwrap();
                if seat
                    .get_keyboard()
                    .unwrap()
                    .current_focus()
                    .is_some_and(|focus| !focus.windows().any(|w| w == *window))
                    && workspace.is_tiled(window)
                {
                    for mapped in workspace
                        .mapped()
                        .filter(|m| {
                            m.maximized_state.lock().unwrap().is_some()
                                && !m.windows().any(|(ref w, _)| w == window)
                        })
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                    {
                        workspace.unmaximize_request(&mapped);
                    }
                }

                let target = if let Some(mapped) = workspace.element_for_surface(window) {
                    mapped.focus_window(window);
                    KeyboardFocusTarget::Element(mapped.clone())
                } else {
                    KeyboardFocusTarget::Fullscreen(window.clone())
                };
                std::mem::drop(shell);

                if seat.active_output() != *output {
                    match res {
                        Ok(new_pos) => {
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
                        _ => {}
                    }
                }

                Shell::set_focus(self, Some(&target), &seat, None, false);
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
        to_handle: WorkspaceHandle,
        _output: Output,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let Some(surface) = window.wl_surface() else {
            return;
        };
        let Some((from_workspace, _)) = shell.workspace_for_surface(&*surface) else {
            return;
        };

        let res = shell.move_window(
            Some(&seat),
            window,
            &from_workspace,
            &to_handle,
            false,
            None,
            &mut self.common.workspace_state.update(),
            &self.common.event_loop_handle,
        );
        if let Some((target, _)) = res {
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    fn fullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        output: Option<Output>,
    ) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = output
            .or_else(|| {
                window
                    .wl_surface()
                    .and_then(|surface| shell.visible_output_for_surface(&*surface).cloned())
            })
            .unwrap_or_else(|| seat.focused_or_active_output());
        if let Some(target) =
            shell.fullscreen_request(window, output, &self.common.event_loop_handle)
        {
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    fn unfullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        let mut shell = self.common.shell.write();
        if let Some(target) = shell.unfullscreen_request(window, &self.common.event_loop_handle) {
            let seat = shell.seats.last_active().clone();
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        }
    }

    fn maximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat, true, &self.common.event_loop_handle);
        }
    }

    fn unmaximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(window).cloned() {
            shell.unmaximize_request(&mapped);
        }
    }

    fn minimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        shell.minimize_request(window);
    }

    fn unminimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        shell.unminimize_request(window, &seat, &self.common.event_loop_handle);
    }

    fn set_sticky(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if window.is_sticky() {
            return;
        }

        let mut shell = self.common.shell.write();
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

        let mut shell = self.common.shell.write();
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
