// SPDX-License-Identifier: GPL-3.0-only

use cosmic_protocols::workspace::v1::server::zcosmic_workspace_handle_v1::ZcosmicWorkspaceHandleV1;
use smithay::{input::Seat, output::Output, reexports::wayland_server::DisplayHandle};

use crate::{
    shell::{CosmicSurface, Shell},
    utils::prelude::*,
    wayland::protocols::{
        toplevel_info::ToplevelInfoHandler,
        toplevel_management::{
            delegate_toplevel_management, ManagementWindow, ToplevelManagementHandler,
            ToplevelManagementState,
        },
    },
};

impl ToplevelManagementHandler for State {
    fn toplevel_management_state(&mut self) -> &mut ToplevelManagementState {
        &mut self.common.shell.toplevel_management_state
    }

    fn activate(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        seat: Option<Seat<Self>>,
    ) {
        for output in self
            .common
            .shell
            .outputs()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
        {
            let maybe = self
                .common
                .shell
                .workspaces
                .spaces_for_output(output)
                .enumerate()
                .find(|(_, w)| w.windows().any(|w| &w == window));
            if let Some((idx, workspace)) = maybe {
                let seat = seat.unwrap_or(self.common.last_active_seat().clone());
                let mapped = workspace
                    .mapped()
                    .find(|m| m.windows().any(|(w, _)| &w == window))
                    .unwrap()
                    .clone();

                let _ = self.common.shell.activate(&output, idx as usize); // TODO: Move pointer?
                mapped.focus_window(window);
                Common::set_focus(self, Some(&mapped.clone().into()), &seat, None);
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
        let Some(to_handle) = self
            .common
            .shell
            .workspace_state
            .get_workspace_handle(&workspace)
        else {
            return;
        };

        let from_workspace = self
            .common
            .shell
            .workspaces
            .spaces()
            .find(|w| w.windows().any(|w| &w == window));
        if let Some(from_workspace) = from_workspace {
            let mapped = from_workspace
                .mapped()
                .find(|m| m.windows().any(|(w, _)| &w == window))
                .unwrap()
                .clone();
            let from_handle = from_workspace.handle;
            let _ = Shell::move_window(self, None, &mapped, &from_handle, &to_handle, false, None);
            return;
        }
    }

    fn fullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
        output: Option<Output>,
    ) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            if let Some(output) = output {
                let workspace = self.common.shell.workspaces.active_mut(&output);
                workspace.fullscreen_request(window, None);
            } else if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                workspace.fullscreen_request(window, None);
            }
        }
    }

    fn unfullscreen(
        &mut self,
        _dh: &DisplayHandle,
        window: &<Self as ToplevelInfoHandler>::Window,
    ) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                let previous = workspace.unfullscreen_request(window);
                assert!(previous.is_none());
            }
        }
    }

    fn maximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            self.common.shell.maximize_request(&mapped);
        }
    }

    fn unmaximize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            self.common.shell.unmaximize_request(&mapped);
        }
    }

    fn minimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            self.common.shell.minimize_request(&mapped);
        }
    }

    fn unminimize(&mut self, _dh: &DisplayHandle, window: &<Self as ToplevelInfoHandler>::Window) {
        if let Some(mapped) = self.common.shell.element_for_surface(window).cloned() {
            self.common.shell.unminimize_request(&mapped);
        }
    }
}

impl ManagementWindow for CosmicSurface {
    fn close(&self) {
        CosmicSurface::close(self)
    }
}

delegate_toplevel_management!(State);
