// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    shell::{grabs::ReleaseMode, CosmicSurface, PendingWindow},
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

use super::compositor::client_compositor_state;

pub mod popup;

pub type PopupGrabData = Cell<Option<PopupGrab<State>>>;

impl XdgShellHandler for State {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.common.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let window = CosmicSurface::from(surface);
        shell.pending_windows.push(PendingWindow {
            surface: window,
            seat,
            fullscreen: None,
            maximized: false,
        });
        // We will position the window after the first commit, when we know its size hints
    }

    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState) {
        surface.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });

        if surface.get_parent_surface().is_some() {
            // let other shells deal with their popups
            self.common.shell.read().unconstrain_popup(&surface);

            if surface.send_configure().is_ok() {
                self.common
                    .popups
                    .track_popup(PopupKind::from(surface))
                    .unwrap();
            }
        }
    }

    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        let seat = Seat::from_resource(&seat).unwrap();
        let kind = PopupKind::Xdg(surface);
        let maybe_root = find_popup_root_surface(&kind)
            .ok()
            .and_then(|root| self.common.shell.read().element_for_surface(&root).cloned());

        if let Some(root) = maybe_root {
            let target = root.into();
            let ret = self.common.popups.grab_popup(target, kind, &seat, serial);

            if let Ok(mut grab) = ret {
                if let Some(keyboard) = seat.get_keyboard() {
                    if keyboard.is_grabbed()
                        && !(keyboard.has_grab(serial)
                            || keyboard.has_grab(grab.previous_serial().unwrap_or(serial)))
                    {
                        grab.ungrab(PopupUngrabStrategy::All);
                        return;
                    }
                    Shell::set_focus(
                        self,
                        grab.current_grab().as_ref(),
                        &seat,
                        Some(serial),
                        false,
                    );
                    keyboard.set_grab(self, PopupKeyboardGrab::new(&grab), serial);
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

        self.common.shell.read().unconstrain_popup(&surface);
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
        let mut shell = self.common.shell.write();
        if let Some((grab, focus)) = shell.move_request(
            surface.wl_surface(),
            &seat,
            serial,
            ReleaseMode::NoMouseButtons,
            false,
            &self.common.config,
            &self.common.event_loop_handle,
            true,
        ) {
            std::mem::drop(shell);
            if grab.is_touch_grab() {
                seat.get_touch().unwrap().set_grab(self, grab, serial);
            } else {
                seat.get_pointer()
                    .unwrap()
                    .set_grab(self, grab, serial, focus)
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
        let mut shell = self.common.shell.write();
        if let Some((grab, focus)) = shell.resize_request(
            surface.wl_surface(),
            &seat,
            serial,
            edges.into(),
            self.common.config.cosmic_conf.edge_snap_threshold,
            true,
        ) {
            std::mem::drop(shell);
            if grab.is_touch_grab() {
                seat.get_touch().unwrap().set_grab(self, grab, serial)
            } else {
                seat.get_pointer()
                    .unwrap()
                    .set_grab(self, grab, serial, focus)
            }
        }
    }

    fn minimize_request(&mut self, surface: ToplevelSurface) {
        let mut shell = self.common.shell.write();
        shell.minimize_request(surface.wl_surface())
    }

    fn maximize_request(&mut self, surface: ToplevelSurface) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(surface.wl_surface()).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat, true, &self.common.event_loop_handle)
        } else if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.wl_surface().as_deref() == Some(surface.wl_surface()))
        {
            pending.maximized = true;
        }
    }

    fn unmaximize_request(&mut self, surface: ToplevelSurface) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(surface.wl_surface()).cloned() {
            shell.unmaximize_request(&mapped);
        } else if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.wl_surface().as_deref() == Some(surface.wl_surface()))
        {
            pending.maximized = false;
        }
    }

    fn fullscreen_request(&mut self, surface: ToplevelSurface, output: Option<WlOutput>) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = output
            .as_ref()
            .and_then(Output::from_resource)
            .or_else(|| {
                shell
                    .visible_output_for_surface(surface.wl_surface())
                    .cloned()
            })
            .unwrap_or_else(|| seat.focused_or_active_output());

        match shell.fullscreen_request(&surface, output.clone(), &self.common.event_loop_handle) {
            Some(target) => {
                std::mem::drop(shell);
                Shell::set_focus(self, Some(&target), &seat, None, true);
            }
            None => {
                if let Some(pending) = shell.pending_windows.iter_mut().find(|pending| {
                    pending.surface.wl_surface().as_deref() == Some(surface.wl_surface())
                }) {
                    pending.fullscreen = Some(output);
                }
            }
        }
    }

    fn unfullscreen_request(&mut self, surface: ToplevelSurface) {
        let mut shell = self.common.shell.write();

        if let Some(target) = shell.unfullscreen_request(&surface, &self.common.event_loop_handle) {
            let seat = shell.seats.last_active().clone();
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, true);
        } else {
            if let Some(pending) = shell.pending_windows.iter_mut().find(|pending| {
                pending.surface.wl_surface().as_deref() == Some(surface.wl_surface())
            }) {
                pending.fullscreen.take();
            }
        }
    }

    fn toplevel_destroyed(&mut self, surface: ToplevelSurface) {
        let (output, clients) = {
            let mut shell = self.common.shell.write();
            let seat = shell.seats.last_active().clone();

            let output = shell
                .visible_output_for_surface(surface.wl_surface())
                .cloned();
            shell.unmap_surface(
                surface.wl_surface(),
                &seat,
                &mut self.common.toplevel_info_state,
            );
            if let Some(output) = output.as_ref() {
                shell.refresh_active_space(output);
            }

            // animations might be unblocked now
            (output, shell.update_animations())
        };

        {
            let dh = self.common.display_handle.clone();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(self, &dh);
            }
        }

        if let Some(output) = output.as_ref() {
            self.backend.schedule_render(&output);
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
            states
                .cached_state
                .get::<SurfaceCachedState>()
                .current()
                .geometry
        })
        .unwrap_or_default()
        .loc;

        let shell = self.common.shell.read();
        let res = shell.menu_request(
            surface.wl_surface(),
            &seat,
            serial,
            location,
            false,
            &self.common.config,
            &self.common.event_loop_handle,
            true,
        );
        if let Some((grab, focus)) = res {
            std::mem::drop(shell);
            seat.get_pointer()
                .unwrap()
                .set_grab(self, grab, serial, focus)
        }
    }
}

delegate_xdg_shell!(State);
