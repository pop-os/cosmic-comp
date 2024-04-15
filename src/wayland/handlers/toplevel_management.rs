// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::ZcosmicWorkspaceHandleV1;
use smithay::{
    desktop::{layer_map_for_output, WindowSurfaceType},
    input::Seat,
    output::Output,
    reexports::wayland_server::DisplayHandle,
    utils::{Point, Rectangle, Size},
};

use crate::{
    shell::{CosmicSurface, Shell, WorkspaceDelta},
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

                let _ = shell.activate(
                    &output,
                    idx as usize,
                    WorkspaceDelta::new_shortcut(),
                    &mut self.common.workspace_state.update(),
                ); // TODO: Move pointer?
                mapped.focus_window(window);

                std::mem::drop(shell);
                Shell::set_focus(self, Some(&mapped.clone().into()), &seat, None);
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
        let from_workspace = shell.workspaces.spaces().find(|w| {
            w.mapped()
                .flat_map(|m| m.windows().map(|(s, _)| s))
                .any(|w| &w == window)
        });
        if let Some(from_workspace) = from_workspace {
            let mapped = from_workspace
                .mapped()
                .find(|m| m.windows().any(|(w, _)| &w == window))
                .unwrap()
                .clone();
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
                Shell::set_focus(self, Some(&target), &seat, None);
            }
            return;
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
            if let Some(output) = output {
                let from = minimize_rectangle(&output, window);
                let workspace = shell.workspaces.active_mut(&output);
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
            shell.maximize_request(&mapped, &seat);
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
        if let Some(mut mapped) = shell.element_for_surface(window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.unminimize_request(&mapped, &seat);
            if mapped.is_stack() {
                mapped.stack_ref_mut().unwrap().set_active(window);
            }
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
                Rectangle::from_loc_and_size(
                    Point::from((local.loc.x + relative.loc.x, local.loc.y + relative.loc.y)),
                    relative.size,
                )
            })
        })
        .unwrap_or_else(|| {
            let output_size = output.geometry().size;
            Rectangle::from_loc_and_size(
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
