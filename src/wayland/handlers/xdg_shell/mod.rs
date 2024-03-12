// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{element::CosmicWindow, grabs::ReleaseMode, CosmicMapped, CosmicSurface, ManagedLayer},
    utils::prelude::*,
};
use smithay::{
    delegate_xdg_shell,
    desktop::{
        find_popup_root_surface, PopupGrab, PopupKeyboardGrab, PopupKind, PopupPointerGrab,
        PopupUngrabStrategy,
    },
    input::{pointer::Focus, Seat},
    output::Output,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::protocol::{wl_output::WlOutput, wl_seat::WlSeat},
    },
    utils::{Logical, Point, Serial},
    wayland::{
        compositor::with_states,
        seat::WaylandFocus,
        shell::xdg::{
            PopupSurface, PositionerState, SurfaceCachedState, ToplevelSurface, XdgShellHandler,
            XdgShellState,
        },
    },
};
use std::cell::Cell;
use tracing::warn;

use super::{compositor::client_compositor_state, toplevel_management::ToplevelManagementExt};

pub mod popup;

pub type PopupGrabData = Cell<Option<PopupGrab<State>>>;

impl XdgShellHandler for State {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.common.shell.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        let seat = self.common.last_active_seat().clone();
        let window = CosmicSurface::from(surface);
        self.common.shell.pending_windows.push((window, seat, None));
        // We will position the window after the first commit, when we know its size hints
    }

    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState) {
        surface.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });

        if surface.get_parent_surface().is_some() {
            // let other shells deal with their popups
            self.common.shell.unconstrain_popup(&surface);

            if surface.send_configure().is_ok() {
                self.common
                    .shell
                    .popups
                    .track_popup(PopupKind::from(surface))
                    .unwrap();
            }
        }
    }

    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        let seat = Seat::from_resource(&seat).unwrap();
        let kind = PopupKind::Xdg(surface);
        if let Some(root) = find_popup_root_surface(&kind)
            .ok()
            .and_then(|root| self.common.shell.element_for_surface(&root))
        {
            let target = root.clone().into();
            let ret = self
                .common
                .shell
                .popups
                .grab_popup(target, kind, &seat, serial);

            if let Ok(mut grab) = ret {
                if let Some(keyboard) = seat.get_keyboard() {
                    if keyboard.is_grabbed()
                        && !(keyboard.has_grab(serial)
                            || keyboard.has_grab(grab.previous_serial().unwrap_or(serial)))
                    {
                        grab.ungrab(PopupUngrabStrategy::All);
                        return;
                    }
                    Common::set_focus(self, grab.current_grab().as_ref(), &seat, Some(serial));
                    keyboard.set_grab(PopupKeyboardGrab::new(&grab), serial);
                }

                if let Some(pointer) = seat.get_pointer() {
                    if pointer.is_grabbed()
                        && !(pointer.has_grab(serial)
                            || pointer
                                .has_grab(grab.previous_serial().unwrap_or_else(|| grab.serial())))
                    {
                        grab.ungrab(PopupUngrabStrategy::All);
                        return;
                    }
                    pointer.set_grab(self, PopupPointerGrab::new(&grab), serial, Focus::Keep);
                }

                seat.user_data()
                    .insert_if_missing(|| PopupGrabData::new(None));
                seat.user_data()
                    .get::<PopupGrabData>()
                    .unwrap()
                    .set(Some(grab));
            }
        }
    }

    fn reposition_request(
        &mut self,
        surface: PopupSurface,
        positioner: PositionerState,
        token: u32,
    ) {
        surface.with_pending_state(|state| {
            let geometry = positioner.get_geometry();
            state.geometry = geometry;
            state.positioner = positioner;
        });

        self.common.shell.unconstrain_popup(&surface);
        surface.send_repositioned(token);
        if let Err(err) = surface.send_configure() {
            warn!(
                ?err,
                "Client bug: Unable to re-configure repositioned popup.",
            );
        }
    }

    fn move_request(&mut self, surface: ToplevelSurface, seat: WlSeat, serial: Serial) {
        let seat = Seat::from_resource(&seat).unwrap();
        Shell::move_request(
            self,
            surface.wl_surface(),
            &seat,
            serial,
            ReleaseMode::NoMouseButtons,
            false,
        )
    }

    fn resize_request(
        &mut self,
        surface: ToplevelSurface,
        seat: WlSeat,
        serial: Serial,
        edges: xdg_toplevel::ResizeEdge,
    ) {
        let seat = Seat::from_resource(&seat).unwrap();
        Shell::resize_request(self, surface.wl_surface(), &seat, serial, edges.into())
    }

    fn minimize_request(&mut self, surface: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            if !mapped.is_stack()
                || mapped.active_window().wl_surface().as_ref() == Some(surface.wl_surface())
            {
                self.common.shell.minimize_request(&mapped)
            }
        }
    }

    fn maximize_request(&mut self, surface: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            let seat = self.common.last_active_seat().clone();
            self.common.shell.maximize_request(&mapped, &seat)
        }
    }

    fn unmaximize_request(&mut self, surface: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            self.common.shell.unmaximize_request(&mapped);
        }
    }

    fn fullscreen_request(&mut self, surface: ToplevelSurface, output: Option<WlOutput>) {
        let seat = self.common.last_active_seat().clone();
        let active_output = seat.active_output();
        let output = output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| active_output.clone());

        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            let from = self
                .common
                .shell
                .toplevel_management_state
                .minimize_rectangle(&output, &mapped.active_window());

            if let Some(set) = self
                .common
                .shell
                .workspaces
                .sets
                .values_mut()
                .find(|set| set.sticky_layer.mapped().any(|m| m == &mapped))
            {
                let mapped = if mapped
                    .stack_ref()
                    .map(|stack| stack.len() > 1)
                    .unwrap_or(false)
                {
                    let stack = mapped.stack_ref().unwrap();
                    let surface = stack
                        .surfaces()
                        .find(|s| s.wl_surface().as_ref() == Some(surface.wl_surface()))
                        .unwrap();
                    stack.remove_window(&surface);
                    CosmicMapped::from(CosmicWindow::new(
                        surface,
                        self.common.event_loop_handle.clone(),
                        self.common.theme.clone(),
                    ))
                } else {
                    set.sticky_layer.unmap(&mapped);
                    mapped
                };

                let workspace_handle = self.common.shell.active_space(&output).handle.clone();
                for (window, _) in mapped.windows() {
                    self.common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_output(&window, &output);
                    self.common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_workspace(&window, &workspace_handle);
                }

                let workspace = self.common.shell.active_space_mut(&output);
                workspace.floating_layer.map(mapped.clone(), None);

                workspace.fullscreen_request(
                    &mapped.active_window(),
                    Some((ManagedLayer::Sticky, workspace_handle)),
                    from,
                    &seat,
                );
            } else if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                if workspace.output != output {
                    let (mapped, layer) = if mapped
                        .stack_ref()
                        .map(|stack| stack.len() > 1)
                        .unwrap_or(false)
                    {
                        let stack = mapped.stack_ref().unwrap();
                        let surface = stack
                            .surfaces()
                            .find(|s| s.wl_surface().as_ref() == Some(surface.wl_surface()))
                            .unwrap();
                        stack.remove_window(&surface);
                        (
                            CosmicMapped::from(CosmicWindow::new(
                                surface,
                                self.common.event_loop_handle.clone(),
                                self.common.theme.clone(),
                            )),
                            if workspace.is_tiled(&mapped) {
                                ManagedLayer::Tiling
                            } else {
                                ManagedLayer::Floating
                            },
                        )
                    } else {
                        let layer = workspace.unmap(&mapped).unwrap().layer;
                        (mapped, layer)
                    };
                    let handle = workspace.handle.clone();

                    let workspace_handle = self.common.shell.active_space(&output).handle.clone();
                    for (window, _) in mapped.windows() {
                        self.common
                            .shell
                            .toplevel_info_state
                            .toplevel_enter_output(&window, &output);
                        self.common
                            .shell
                            .toplevel_info_state
                            .toplevel_enter_workspace(&window, &workspace_handle);
                    }

                    let workspace = self.common.shell.active_space_mut(&output);
                    workspace.floating_layer.map(mapped.clone(), None);

                    workspace.fullscreen_request(
                        &mapped.active_window(),
                        Some((layer, handle)),
                        from,
                        &seat,
                    );
                } else {
                    let (window, _) = mapped
                        .windows()
                        .find(|(w, _)| w.wl_surface().as_ref() == Some(surface.wl_surface()))
                        .unwrap();
                    workspace.fullscreen_request(&window, None, from, &seat)
                }
            }
        } else {
            if let Some(o) = self
                .common
                .shell
                .pending_windows
                .iter_mut()
                .find(|(s, _, _)| s.wl_surface().as_ref() == Some(surface.wl_surface()))
                .map(|(_, _, o)| o)
            {
                *o = Some(output);
            }
        }
    }

    fn unfullscreen_request(&mut self, surface: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped
                    .windows()
                    .find(|(w, _)| w.wl_surface().as_ref() == Some(surface.wl_surface()))
                    .unwrap();
                if let Some((layer, previous_workspace)) = workspace.unfullscreen_request(&window) {
                    let old_handle = workspace.handle.clone();
                    let new_workspace_handle = self
                        .common
                        .shell
                        .workspaces
                        .space_for_handle(&previous_workspace)
                        .is_some()
                        .then_some(previous_workspace)
                        .unwrap_or(old_handle); // if the workspace doesn't exist anymore, we can still remap on the right layer

                    self.common.shell.remap_unfullscreened_window(
                        mapped,
                        &old_handle,
                        &new_workspace_handle,
                        layer,
                    );
                }
            }
        }
    }

    fn toplevel_destroyed(&mut self, surface: ToplevelSurface) {
        let seat = self.common.last_active_seat().clone();
        self.common.shell.unmap_surface(surface.wl_surface(), &seat);

        let output = self
            .common
            .shell
            .visible_output_for_surface(surface.wl_surface())
            .cloned();
        if let Some(output) = output.as_ref() {
            self.common.shell.refresh_active_space(output);
        }

        // animations might be unblocked now
        let clients = self.common.shell.update_animations();
        {
            let dh = self.common.display_handle.clone();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(self, &dh);
            }
        }

        if let Some(output) = output.as_ref() {
            self.backend
                .schedule_render(&self.common.event_loop_handle, &output);
        }
    }

    fn show_window_menu(
        &mut self,
        surface: ToplevelSurface,
        seat: WlSeat,
        serial: Serial,
        mut location: Point<i32, Logical>,
    ) {
        let seat = Seat::from_resource(&seat).unwrap();
        location -= with_states(surface.wl_surface(), |states| {
            states.cached_state.current::<SurfaceCachedState>().geometry
        })
        .unwrap_or_default()
        .loc;
        Shell::menu_request(self, surface.wl_surface(), &seat, serial, location, false)
    }
}

delegate_xdg_shell!(State);
