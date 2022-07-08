// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    delegate_xdg_shell,
    desktop::{
        Kind, PopupGrab, PopupKeyboardGrab, PopupKind, PopupPointerGrab, PopupUngrabStrategy,
        Window, WindowSurfaceType,
    },
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::{
            protocol::{wl_output::WlOutput, wl_seat::WlSeat, wl_surface::WlSurface},
            DisplayHandle,
        },
    },
    wayland::{
        output::Output,
        seat::{Focus, PointerGrabStartData, Seat},
        shell::xdg::{
            Configure, PopupSurface, PositionerState, ToplevelSurface, XdgShellHandler,
            XdgShellState,
        },
        Serial,
    },
};
use std::cell::Cell;

pub mod popup;

pub type PopupGrabData = Cell<Option<PopupGrab>>;

impl XdgShellHandler for State {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.common.shell.xdg_shell_state
    }

    fn new_toplevel(&mut self, _dh: &DisplayHandle, surface: ToplevelSurface) {
        super::mark_dirty_on_drop(&self.common, surface.wl_surface());

        let seat = &self.common.last_active_seat;
        let window = Window::new(Kind::Xdg(surface));
        self.common.shell.toplevel_info_state.new_toplevel(&window);
        self.common
            .shell
            .pending_windows
            .push((window, seat.clone()));
        // We will position the window after the first commit, when we know its size hints
    }

    fn new_popup(
        &mut self,
        _dh: &DisplayHandle,
        surface: PopupSurface,
        positioner: PositionerState,
    ) {
        super::mark_dirty_on_drop(&self.common, surface.wl_surface());

        surface.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });

        if surface.get_parent_surface().is_some() {
            // let other shells deal with their popups
            self.common.shell.unconstrain_popup(&surface, &positioner);

            if surface.send_configure().is_ok() {
                self.common
                    .shell
                    .popups
                    .track_popup(PopupKind::from(surface))
                    .unwrap();
            }
        }
    }

    fn ack_configure(&mut self, _dh: &DisplayHandle, surface: WlSurface, configure: Configure) {
        if let Configure::Toplevel(configure) = configure {
            // If we would re-position the window inside the grab we would get a weird jittery animation.
            // We only want to resize once the client has acknoledged & commited the new size,
            // so we need to carefully track the state through different handlers.
            if let Some(window) =
                self.common
                    .shell
                    .space_for_surface(&surface)
                    .and_then(|workspace| {
                        workspace
                            .space
                            .window_for_surface(&surface, WindowSurfaceType::TOPLEVEL)
                    })
            {
                crate::shell::layout::floating::ResizeSurfaceGrab::ack_configure(window, configure)
            }
        }
    }

    fn grab(&mut self, dh: &DisplayHandle, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        let seat = Seat::from_resource(&seat).unwrap();
        let ret = self
            .common
            .shell
            .popups
            .grab_popup(dh, surface.into(), &seat, serial);

        if let Ok(mut grab) = ret {
            if let Some(keyboard) = seat.get_keyboard() {
                if keyboard.is_grabbed()
                    && !(keyboard.has_grab(serial)
                        || keyboard.has_grab(grab.previous_serial().unwrap_or(serial)))
                {
                    grab.ungrab(dh, PopupUngrabStrategy::All);
                    return;
                }
                self.common
                    .set_focus(dh, grab.current_grab().as_ref(), &seat, Some(serial));
                keyboard.set_grab(PopupKeyboardGrab::new(&grab), serial);
            }

            if let Some(pointer) = seat.get_pointer() {
                if pointer.is_grabbed()
                    && !(pointer.has_grab(serial)
                        || pointer
                            .has_grab(grab.previous_serial().unwrap_or_else(|| grab.serial())))
                {
                    grab.ungrab(dh, PopupUngrabStrategy::All);
                    return;
                }
                pointer.set_grab(PopupPointerGrab::new(&grab), serial, Focus::Keep);
            }

            seat.user_data()
                .insert_if_missing(|| PopupGrabData::new(None));
            seat.user_data()
                .get::<PopupGrabData>()
                .unwrap()
                .set(Some(grab));
        }
    }

    fn reposition_request(
        &mut self,
        _dh: &DisplayHandle,
        surface: PopupSurface,
        positioner: PositionerState,
        token: u32,
    ) {
        surface.with_pending_state(|state| {
            let geometry = positioner.get_geometry();
            state.geometry = geometry;
            state.positioner = positioner;
        });

        self.common.shell.unconstrain_popup(&surface, &positioner);
        surface.send_repositioned(token);
        if let Err(err) = surface.send_configure() {
            slog_scope::warn!(
                "Compositor bug: Unable to re configure repositioned popup: {}",
                err
            );
        }
    }

    fn move_request(
        &mut self,
        _dh: &DisplayHandle,
        surface: ToplevelSurface,
        seat: WlSeat,
        serial: Serial,
    ) {
        let seat = Seat::from_resource(&seat).unwrap();
        if let Some(start_data) = check_grab_preconditions(&seat, surface.wl_surface(), serial) {
            let workspace = self
                .common
                .shell
                .space_for_surface_mut(surface.wl_surface())
                .unwrap();
            let window = workspace
                .space
                .window_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();

            self.common.shell.move_request(&window, &seat, serial, start_data);
        }
    }

    fn resize_request(
        &mut self,
        _dh: &DisplayHandle,
        surface: ToplevelSurface,
        seat: WlSeat,
        serial: Serial,
        edges: xdg_toplevel::ResizeEdge,
    ) {
        let seat = Seat::from_resource(&seat).unwrap();
        if let Some(start_data) = check_grab_preconditions(&seat, surface.wl_surface(), serial) {
            let workspace = self
                .common
                .shell
                .space_for_surface_mut(surface.wl_surface())
                .unwrap();
            let window = workspace
                .space
                .window_for_surface(surface.wl_surface(), WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();

            workspace.resize_request(&window, &seat, serial, start_data, edges);
        }
    }

    fn maximize_request(&mut self, _dh: &DisplayHandle, surface: ToplevelSurface) {
        let surface = surface.wl_surface();
        let seat = &self.common.last_active_seat;
        let output = active_output(seat, &self.common);

        if let Some(workspace) = self.common.shell.space_for_surface_mut(surface) {
            let window = workspace
                .space
                .window_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();
            workspace.maximize_request(&window, &output)
        }
    }

    fn unmaximize_request(&mut self, _dh: &DisplayHandle, surface: ToplevelSurface) {
        let surface = surface.wl_surface();

        if let Some(workspace) = self.common.shell.space_for_surface_mut(surface) {
            let window = workspace
                .space
                .window_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();
            workspace.unmaximize_request(&window)
        }
    }

    fn fullscreen_request(
        &mut self,
        _dh: &DisplayHandle,
        surface: ToplevelSurface,
        output: Option<WlOutput>,
    ) {
        let output = output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| {
                let seat = &self.common.last_active_seat;
                active_output(seat, &self.common)
            });

        let surface = surface.wl_surface();
        if let Some(workspace) = self.common.shell.space_for_surface_mut(surface) {
            let window = workspace
                .space
                .window_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();
            workspace.fullscreen_request(&window, &output)
        }
    }

    fn unfullscreen_request(&mut self, _dh: &DisplayHandle, surface: ToplevelSurface) {
        let surface = surface.wl_surface();
        if let Some(workspace) = self.common.shell.space_for_surface_mut(surface) {
            let window = workspace
                .space
                .window_for_surface(surface, WindowSurfaceType::TOPLEVEL)
                .unwrap()
                .clone();
            workspace.unfullscreen_request(&window)
        }
    }
}

fn check_grab_preconditions(
    seat: &Seat<State>,
    surface: &WlSurface,
    serial: Serial,
) -> Option<PointerGrabStartData> {
    use smithay::reexports::wayland_server::Resource;

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
            .id()
            .same_client_as(&surface.id())
    {
        return None;
    }

    Some(start_data)
}

delegate_xdg_shell!(State);
