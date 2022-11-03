// SPDX-License-Identifier: GPL-3.0-only

use crate::utils::prelude::*;
use smithay::{
    delegate_xdg_shell,
    desktop::{
        find_popup_root_surface, Kind, PopupGrab, PopupKeyboardGrab, PopupKind, PopupPointerGrab,
        PopupUngrabStrategy, Window,
    },
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    output::Output,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel,
        wayland_server::protocol::{wl_output::WlOutput, wl_seat::WlSeat, wl_surface::WlSurface},
    },
    utils::Serial,
    wayland::{
        seat::WaylandFocus,
        shell::xdg::{
            PopupSurface, PositionerState, ToplevelSurface, XdgShellHandler, XdgShellState,
        },
    },
};
use std::cell::Cell;

pub mod popup;

pub type PopupGrabData = Cell<Option<PopupGrab<State>>>;

impl XdgShellHandler for State {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.common.shell.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        super::mark_dirty_on_drop(&self.common, surface.wl_surface());

        let seat = self.common.last_active_seat().clone();
        let window = Window::new(Kind::Xdg(surface));
        self.common.shell.toplevel_info_state.new_toplevel(&window);
        self.common.shell.pending_windows.push((window, seat));
        // We will position the window after the first commit, when we know its size hints
    }

    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState) {
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

        self.common.shell.unconstrain_popup(&surface, &positioner);
        surface.send_repositioned(token);
        if let Err(err) = surface.send_configure() {
            slog_scope::warn!(
                "Compositor bug: Unable to re configure repositioned popup: {}",
                err
            );
        }
    }

    fn move_request(&mut self, surface: ToplevelSurface, seat: WlSeat, serial: Serial) {
        let seat = Seat::from_resource(&seat).unwrap();
        if let Some(start_data) = check_grab_preconditions(&seat, surface.wl_surface(), serial) {
            if let Some(mapped) = self
                .common
                .shell
                .element_for_surface(surface.wl_surface())
                .cloned()
            {
                if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                    let output = seat.active_output();
                    let (window, _) = mapped
                        .windows()
                        .find(|(w, _)| matches!(w.toplevel(), Kind::Xdg(s) if s == &surface))
                        .unwrap();
                    if let Some(grab) =
                        workspace.move_request(&window, &seat, &output, serial, start_data)
                    {
                        let handle = workspace.handle;
                        self.common
                            .shell
                            .toplevel_info_state
                            .toplevel_leave_workspace(&window, &handle);
                        self.common
                            .shell
                            .toplevel_info_state
                            .toplevel_leave_output(&window, &output);
                        seat.get_pointer()
                            .unwrap()
                            .set_grab(self, grab, serial, Focus::Clear);
                    }
                }
            }
        }
    }

    fn resize_request(
        &mut self,
        surface: ToplevelSurface,
        seat: WlSeat,
        serial: Serial,
        edges: xdg_toplevel::ResizeEdge,
    ) {
        let seat = Seat::from_resource(&seat).unwrap();
        if let Some(start_data) = check_grab_preconditions(&seat, surface.wl_surface(), serial) {
            if let Some(mapped) = self
                .common
                .shell
                .element_for_surface(surface.wl_surface())
                .cloned()
            {
                if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                    if let Some(grab) =
                        workspace.resize_request(&mapped, &seat, serial, start_data, edges)
                    {
                        seat.get_pointer()
                            .unwrap()
                            .set_grab(self, grab, serial, Focus::Clear);
                    }
                }
            }
        }
    }

    fn maximize_request(&mut self, surface: ToplevelSurface) {
        let seat = self.common.last_active_seat();
        let output = seat.active_output();

        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped
                    .windows()
                    .find(|(w, _)| matches!(w.toplevel(), Kind::Xdg(s) if s == &surface))
                    .unwrap();
                workspace.maximize_request(&window, &output)
            }
        }
    }

    fn unmaximize_request(&mut self, surface: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped
                    .windows()
                    .find(|(w, _)| matches!(w.toplevel(), Kind::Xdg(s) if s == &surface))
                    .unwrap();
                workspace.unmaximize_request(&window)
            }
        }
    }

    fn fullscreen_request(&mut self, surface: ToplevelSurface, output: Option<WlOutput>) {
        let output = output
            .as_ref()
            .and_then(Output::from_resource)
            .unwrap_or_else(|| {
                let seat = self.common.last_active_seat();
                seat.active_output()
            });

        if let Some(mapped) = self
            .common
            .shell
            .element_for_surface(surface.wl_surface())
            .cloned()
        {
            if let Some(workspace) = self.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped
                    .windows()
                    .find(|(w, _)| matches!(w.toplevel(), Kind::Xdg(s) if s == &surface))
                    .unwrap();
                workspace.fullscreen_request(&window, &output)
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
                    .find(|(w, _)| matches!(w.toplevel(), Kind::Xdg(s) if s == &surface))
                    .unwrap();
                workspace.unfullscreen_request(&window)
            }
        }
    }
}

fn check_grab_preconditions(
    seat: &Seat<State>,
    surface: &WlSurface,
    serial: Serial,
) -> Option<PointerGrabStartData<State>> {
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
            .same_client_as(&surface.id())
    {
        return None;
    }

    Some(start_data)
}

delegate_xdg_shell!(State);
