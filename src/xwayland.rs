use std::{ffi::OsString, os::unix::io::OwnedFd, process::Stdio};

use crate::{
    backend::render::cursor::{load_cursor_theme, Cursor, CursorShape},
    shell::{focus::target::KeyboardFocusTarget, grabs::ReleaseMode, CosmicSurface, Shell},
    state::State,
    utils::prelude::*,
    wayland::handlers::{
        toplevel_management::minimize_rectangle, xdg_activation::ActivationContext,
    },
};
use smithay::{
    backend::drm::DrmNode,
    desktop::space::SpaceElement,
    reexports::x11rb::protocol::xproto::Window as X11Window,
    utils::{Logical, Point, Rectangle, Size, SERIAL_COUNTER},
    wayland::{
        selection::{
            data_device::{
                clear_data_device_selection, current_data_device_selection_userdata,
                request_data_device_client_selection, set_data_device_selection,
            },
            primary_selection::{
                clear_primary_selection, current_primary_selection_userdata,
                request_primary_client_selection, set_primary_selection,
            },
            SelectionTarget,
        },
        xdg_activation::XdgActivationToken,
    },
    xwayland::{
        xwm::{Reorder, X11Relatable, XwmId},
        X11Surface, X11Wm, XWayland, XWaylandEvent, XwmHandler,
    },
};
use tracing::{error, trace, warn};

#[derive(Debug)]
pub struct XWaylandState {
    pub xwm: Option<X11Wm>,
    pub display: u32,
}

impl State {
    pub fn launch_xwayland(&mut self, render_node: Option<DrmNode>) {
        if self.common.xwayland_state.is_some() {
            return;
        }

        let (xwayland, client) = match XWayland::spawn(
            &self.common.display_handle,
            None,
            std::iter::empty::<(OsString, OsString)>(),
            true,
            Stdio::null(),
            Stdio::null(),
            |user_data| {
                if let Some(node) = render_node {
                    user_data.insert_if_missing(|| node);
                }
            },
        ) {
            Ok((xwayland, client)) => (xwayland, client),
            Err(err) => {
                error!(?err, "Failed to start Xwayland.");
                self.notify_ready();
                return;
            }
        };

        match self
            .common
            .event_loop_handle
            .insert_source(xwayland, move |event, _, data| match event {
                XWaylandEvent::Ready {
                    x11_socket,
                    display_number,
                } => {
                    data.common.xwayland_state = Some(XWaylandState {
                        xwm: None,
                        display: display_number,
                    });

                    let mut wm = match X11Wm::start_wm(
                        data.common.event_loop_handle.clone(),
                        x11_socket,
                        client.clone(),
                    ) {
                        Ok(wm) => wm,
                        Err(err) => {
                            error!(?err, "Failed to start Xwayland WM");
                            return;
                        }
                    };

                    let (theme, size) = load_cursor_theme();
                    let cursor = Cursor::load(&theme, CursorShape::Default, size);
                    let image = cursor.get_image(1, 0);
                    if let Err(err) = wm.set_cursor(
                        &image.pixels_rgba,
                        Size::from((image.width as u16, image.height as u16)),
                        Point::from((image.xhot as u16, image.yhot as u16)),
                    ) {
                        warn!(
                            id = ?wm.id(),
                            ?err,
                            "Failed to set default cursor for Xwayland WM",
                        );
                    }

                    let xwayland_state = data.common.xwayland_state.as_mut().unwrap();
                    xwayland_state.xwm = Some(wm);
                    data.notify_ready();
                }
                XWaylandEvent::Error => {
                    if let Some(mut xwayland_state) = data.common.xwayland_state.take() {
                        xwayland_state.xwm = None;
                    }
                    data.notify_ready();
                }
            }) {
            Ok(_token) => {}
            Err(err) => {
                error!(?err, "Failed to listen for Xwayland");
                self.notify_ready();
                return;
            }
        }
    }
}

impl Common {
    fn is_x_focused(&self, xwm: XwmId) -> bool {
        if let Some(keyboard) = self
            .shell
            .read()
            .unwrap()
            .seats
            .last_active()
            .get_keyboard()
        {
            if let Some(KeyboardFocusTarget::Element(mapped)) = keyboard.current_focus() {
                if let Some(surface) = mapped.active_window().x11_surface() {
                    return surface.xwm_id().unwrap() == xwm;
                }
            }
        }

        false
    }

    pub fn update_x11_stacking_order(&mut self) {
        let shell = self.shell.read().unwrap();
        let active_output = shell.seats.last_active().active_output();
        if let Some(xwm) = self
            .xwayland_state
            .as_mut()
            .and_then(|state| state.xwm.as_mut())
        {
            // front to back, given that is how the workspace enumerates
            let order = shell
                .workspaces
                .sets
                .iter()
                .filter(|(output, _)| *output == &active_output)
                .chain(
                    shell
                        .workspaces
                        .sets
                        .iter()
                        .filter(|(output, _)| *output != &active_output),
                )
                .flat_map(|(_, set)| {
                    set.sticky_layer
                        .mapped()
                        .flat_map(|mapped| {
                            let active = mapped.active_window();
                            std::iter::once(active.clone()).chain(
                                mapped
                                    .is_stack()
                                    .then(move || {
                                        mapped
                                            .windows()
                                            .map(|(s, _)| s)
                                            .filter(move |s| s != &active)
                                    })
                                    .into_iter()
                                    .flatten(),
                            )
                        })
                        .chain(
                            set.workspaces
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| *i == set.active)
                                .chain(
                                    set.workspaces
                                        .iter()
                                        .enumerate()
                                        .filter(|(i, _)| *i != set.active),
                                )
                                .flat_map(|(_, workspace)| {
                                    workspace.get_fullscreen().cloned().into_iter().chain(
                                        workspace.mapped().flat_map(|mapped| {
                                            let active = mapped.active_window();
                                            std::iter::once(active.clone()).chain(
                                                mapped
                                                    .is_stack()
                                                    .then(move || {
                                                        mapped
                                                            .windows()
                                                            .map(|(s, _)| s)
                                                            .filter(move |s| s != &active)
                                                    })
                                                    .into_iter()
                                                    .flatten(),
                                            )
                                        }),
                                    )
                                }),
                        )
                })
                .collect::<Vec<_>>();

            // we don't include the popup elements, which contain the OR windows, because we are not supposed to restack them.
            // Which is also why we match upwards, to not disturb elements at the top.
            //
            // But this also means we need to match across all outputs and workspaces at once, to be sure nothing that shouldn't be on top of us is.
            if let Err(err) = xwm.update_stacking_order_upwards(order.iter().rev()) {
                warn!(wm_id = ?xwm.id(), ?err, "Failed to update Xwm stacking order.");
            }
        }
    }
}

impl XwmHandler for State {
    fn xwm_state(&mut self, _xwm: XwmId) -> &mut X11Wm {
        self.common
            .xwayland_state
            .as_mut()
            .and_then(|state| state.xwm.as_mut())
            .unwrap()
    }

    fn new_window(&mut self, _xwm: XwmId, _window: X11Surface) {}
    fn new_override_redirect_window(&mut self, _xwm: XwmId, _window: X11Surface) {}
    fn destroyed_window(&mut self, _xwm: XwmId, _window: X11Surface) {}

    fn map_window_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(err) = window.set_mapped(true) {
            warn!(?window, ?err, "Failed to send Xwayland Mapped-Event",);
        }

        let mut shell = self.common.shell.write().unwrap();
        let startup_id = window.startup_id();
        if shell.element_for_surface(&window).is_some() {
            return;
        }

        let seat = shell.seats.last_active().clone();
        if let Some(context) = startup_id
            .map(XdgActivationToken::from)
            .and_then(|token| self.common.xdg_activation_state.data_for_token(&token))
            .and_then(|data| data.user_data.get::<ActivationContext>())
        {
            shell.pending_activations.insert(
                crate::shell::ActivationKey::X11(window.window_id()),
                context.clone(),
            );
        }

        let surface = CosmicSurface::from(window);
        shell.pending_windows.push((surface, seat, None));
    }

    fn map_window_notify(&mut self, _xwm: XwmId, surface: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some((window, _, _)) = shell
            .pending_windows
            .iter()
            .find(|(window, _, _)| window.x11_surface() == Some(&surface))
            .cloned()
        {
            if !shell
                .pending_activations
                .contains_key(&crate::shell::ActivationKey::X11(surface.window_id()))
            {
                if let Some(startup_id) = window.x11_surface().and_then(|x| x.startup_id()) {
                    if let Some(context) = self
                        .common
                        .xdg_activation_state
                        .data_for_token(&XdgActivationToken::from(startup_id))
                        .and_then(|data| data.user_data.get::<ActivationContext>())
                    {
                        shell.pending_activations.insert(
                            crate::shell::ActivationKey::X11(surface.window_id()),
                            context.clone(),
                        );
                    }
                }
            }
            let res = shell.map_window(
                &window,
                &mut self.common.toplevel_info_state,
                &mut self.common.workspace_state,
                &self.common.event_loop_handle,
            );
            if let Some(target) = res {
                let seat = shell.seats.last_active().clone();
                std::mem::drop(shell);
                Shell::set_focus(self, Some(&target), &seat, None);
            }
        }
    }

    fn mapped_override_redirect_window(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if shell
            .override_redirect_windows
            .iter()
            .any(|or| or == &window)
        {
            return;
        }
        shell.map_override_redirect(window)
    }

    fn unmapped_window(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if window.is_override_redirect() {
            shell.override_redirect_windows.retain(|or| or != &window);
        } else {
            let seat = shell.seats.last_active().clone();
            shell.unmap_surface(&window, &seat, &mut self.common.toplevel_info_state);
        }

        let outputs = if let Some(wl_surface) = window.wl_surface() {
            shell
                .visible_output_for_surface(&wl_surface)
                .into_iter()
                .cloned()
                .collect::<Vec<_>>()
        } else {
            shell.outputs().cloned().collect::<Vec<_>>()
        };
        for output in outputs.iter() {
            shell.refresh_active_space(output, &self.common.xdg_activation_state);
        }

        for output in outputs.into_iter() {
            self.backend
                .schedule_render(&self.common.event_loop_handle, &output);
        }
    }

    fn configure_request(
        &mut self,
        _xwm: XwmId,
        window: X11Surface,
        x: Option<i32>,
        y: Option<i32>,
        w: Option<u32>,
        h: Option<u32>,
        _reorder: Option<Reorder>,
    ) {
        // We only allow floating X11 windows to resize themselves. Nothing else
        let mut current_geo = window.geometry();
        let shell = self.common.shell.read().unwrap();

        if let Some(mapped) = shell.element_for_surface(&window) {
            let is_floating = shell
                .workspaces
                .sets
                .values()
                .any(|set| set.sticky_layer.mapped().any(|m| m == mapped))
                || shell
                    .space_for(&mapped)
                    .filter(|space| space.is_floating(mapped))
                    .is_some();

            if is_floating {
                mapped.set_geometry(
                    Rectangle::from_loc_and_size(
                        current_geo.loc,
                        (
                            w.map(|w| w as i32).unwrap_or(current_geo.size.w),
                            h.map(|h| h as i32).unwrap_or(current_geo.size.h),
                        ),
                    )
                    .as_global(),
                )
            } else {
                let _ = window.configure(None); // ack and force old state
            }
        } else {
            if let Some(x) = x {
                current_geo.loc.x = x;
            }
            if let Some(y) = y {
                current_geo.loc.y = y;
            }
            if let Some(w) = w {
                current_geo.size.w = w as i32;
            }
            if let Some(h) = h {
                current_geo.size.h = h as i32;
            }
            // the window is not yet mapped. Lets give it what it wants
            let _ = window.configure(current_geo);
        }
    }

    fn configure_notify(
        &mut self,
        _xwm: XwmId,
        window: X11Surface,
        _geometry: Rectangle<i32, Logical>,
        above: Option<X11Window>,
    ) {
        if window.is_override_redirect() {
            let mut shell = self.common.shell.write().unwrap();
            if let Some(id) = above {
                let or_windows = &mut shell.override_redirect_windows;
                if let Some(own_pos) = or_windows.iter().position(|or| or == &window) {
                    let compare_pos = or_windows
                        .iter()
                        .position(|or| or.window_id() == id)
                        .unwrap_or(0);
                    if compare_pos > own_pos {
                        let this = or_windows.remove(own_pos);
                        or_windows.insert(compare_pos, this);
                    }
                }
            }

            let geo = window.geometry().as_global();
            for (output, overlap) in shell.outputs().cloned().map(|o| {
                let intersection = o.geometry().intersection(geo);
                (o, intersection)
            }) {
                if let Some(overlap) = overlap {
                    window.output_enter(&output, overlap.as_logical());
                } else {
                    window.output_leave(&output);
                }
            }
        }
    }

    fn resize_request(
        &mut self,
        _xwm: XwmId,
        window: X11Surface,
        _button: u32,
        resize_edge: smithay::xwayland::xwm::ResizeEdge,
    ) {
        if let Some(wl_surface) = window.wl_surface() {
            let mut shell = self.common.shell.write().unwrap();
            let seat = shell.seats.last_active().clone();
            if let Some((grab, focus)) =
                shell.resize_request(&wl_surface, &seat, None, resize_edge.into())
            {
                std::mem::drop(shell);
                if grab.is_touch_grab() {
                    seat.get_touch()
                        .unwrap()
                        .set_grab(self, grab, SERIAL_COUNTER.next_serial())
                } else {
                    seat.get_pointer().unwrap().set_grab(
                        self,
                        grab,
                        SERIAL_COUNTER.next_serial(),
                        focus,
                    )
                }
            }
        }
    }

    fn move_request(&mut self, _xwm: XwmId, window: X11Surface, _button: u32) {
        if let Some(wl_surface) = window.wl_surface() {
            let mut shell = self.common.shell.write().unwrap();
            let seat = shell.seats.last_active().clone();
            if let Some((grab, focus)) = shell.move_request(
                &wl_surface,
                &seat,
                None,
                ReleaseMode::NoMouseButtons,
                false,
                &self.common.config,
                &self.common.event_loop_handle,
                &self.common.xdg_activation_state,
            ) {
                std::mem::drop(shell);
                if grab.is_touch_grab() {
                    seat.get_touch()
                        .unwrap()
                        .set_grab(self, grab, SERIAL_COUNTER.next_serial())
                } else {
                    seat.get_pointer().unwrap().set_grab(
                        self,
                        grab,
                        SERIAL_COUNTER.next_serial(),
                        focus,
                    )
                }
            }
        }
    }

    fn maximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat);
        }
    }

    fn unmaximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            shell.unmaximize_request(&mapped);
        }
    }

    fn minimize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            if !mapped.is_stack() || mapped.active_window().is_window(&window) {
                shell.minimize_request(&mapped);
            }
        }
    }

    fn unminimize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mut mapped) = shell.element_for_surface(&window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.unminimize_request(&mapped, &seat);
            if mapped.is_stack() {
                let maybe_surface = mapped.windows().find(|(w, _)| w.is_window(&window));
                if let Some((surface, _)) = maybe_surface {
                    mapped.stack_ref_mut().unwrap().set_active(&surface);
                }
            }
        }
    }

    fn fullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        let seat = shell.seats.last_active().clone();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            if let Some((output, handle)) = shell
                .space_for(&mapped)
                .map(|workspace| (workspace.output.clone(), workspace.handle.clone()))
            {
                if let Some((surface, _)) = mapped
                    .windows()
                    .find(|(w, _)| w.x11_surface() == Some(&window))
                {
                    let from = minimize_rectangle(&output, &surface);
                    shell
                        .workspaces
                        .space_for_handle_mut(&handle)
                        .unwrap()
                        .fullscreen_request(&surface, None, from, &seat);
                }
            }
        } else {
            let output = seat.active_output();
            if let Some(o) = shell
                .pending_windows
                .iter_mut()
                .find(|(s, _, _)| s.x11_surface() == Some(&window))
                .map(|(_, _, o)| o)
            {
                *o = Some(output);
            }
        }
    }

    fn unfullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write().unwrap();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            if let Some(workspace) = shell.space_for_mut(&mapped) {
                let (window, _) = mapped
                    .windows()
                    .find(|(w, _)| w.x11_surface() == Some(&window))
                    .unwrap();
                let previous = workspace.unfullscreen_request(&window);
                assert!(previous.is_none());
            }
        }
    }

    fn send_selection(
        &mut self,
        _xwm: XwmId,
        selection: SelectionTarget,
        mime_type: String,
        fd: OwnedFd,
    ) {
        let seat = self
            .common
            .shell
            .read()
            .unwrap()
            .seats
            .last_active()
            .clone();
        match selection {
            SelectionTarget::Clipboard => {
                if let Err(err) = request_data_device_client_selection(&seat, mime_type, fd) {
                    error!(
                        ?err,
                        "Failed to request current wayland clipboard for Xwayland.",
                    );
                }
            }
            SelectionTarget::Primary => {
                if let Err(err) = request_primary_client_selection(&seat, mime_type, fd) {
                    error!(
                        ?err,
                        "Failed to request current wayland primary selection for Xwayland.",
                    );
                }
            }
        }
    }

    fn allow_selection_access(&mut self, xwm: XwmId, _selection: SelectionTarget) -> bool {
        self.common.is_x_focused(xwm)
    }

    fn new_selection(&mut self, xwm: XwmId, selection: SelectionTarget, mime_types: Vec<String>) {
        trace!(?selection, ?mime_types, "Got Selection from Xwayland",);

        if self.common.is_x_focused(xwm) {
            let seat = self
                .common
                .shell
                .read()
                .unwrap()
                .seats
                .last_active()
                .clone();
            match selection {
                SelectionTarget::Clipboard => {
                    set_data_device_selection(&self.common.display_handle, &seat, mime_types, xwm)
                }
                SelectionTarget::Primary => {
                    set_primary_selection(&self.common.display_handle, &seat, mime_types, xwm)
                }
            }
        }
    }

    fn cleared_selection(&mut self, xwm: XwmId, selection: SelectionTarget) {
        let shell = self.common.shell.read().unwrap();
        for seat in shell.seats.iter() {
            match selection {
                SelectionTarget::Clipboard => {
                    if current_data_device_selection_userdata(seat).as_deref() == Some(&xwm) {
                        clear_data_device_selection(&self.common.display_handle, seat)
                    }
                }
                SelectionTarget::Primary => {
                    if current_primary_selection_userdata(seat).as_deref() == Some(&xwm) {
                        clear_primary_selection(&self.common.display_handle, seat)
                    }
                }
            }
        }
    }
}
