use std::ffi::OsString;

use crate::{
    backend::render::cursor::Cursor,
    shell::{CosmicSurface, Shell},
    state::{Data, State},
    utils::prelude::*,
    wayland::{handlers::screencopy::PendingScreencopyBuffers, protocols::screencopy::SessionType},
};
use smithay::{
    backend::drm::DrmNode,
    desktop::space::SpaceElement,
    reexports::x11rb::protocol::xproto::Window as X11Window,
    utils::{Logical, Point, Rectangle, Size},
    xwayland::{
        xwm::{Reorder, XwmId},
        X11Surface, X11Wm, XWayland, XWaylandEvent, XwmHandler,
    },
};

pub struct XWaylandState {
    pub xwm: Option<X11Wm>,
    pub display: u32,
    #[allow(unused)]
    xwayland: XWayland,
}

impl State {
    pub fn launch_xwayland(&mut self, drm_node: Option<DrmNode>) {
        if self.common.xwayland_state.contains_key(&drm_node) {
            return;
        }

        let (xwayland, source) = XWayland::new(None, &self.common.display_handle);
        let token =
            match self
                .common
                .event_loop_handle
                .insert_source(source, move |event, _, data| match event {
                    XWaylandEvent::Ready {
                        connection,
                        client,
                        client_fd: _,
                        display: _,
                    } => {
                        let mut wm = match X11Wm::start_wm(
                            data.state.common.event_loop_handle.clone(),
                            data.state.common.display_handle.clone(),
                            connection,
                            client,
                            None,
                        ) {
                            Ok(wm) => wm,
                            Err(err) => {
                                slog_scope::error!("Failed to start Xwayland WM: {}", err);
                                return;
                            }
                        };

                        let cursor = Cursor::load();
                        let image = cursor.get_image(1, 0);
                        if let Err(err) = wm.set_cursor(
                            &image.pixels_rgba,
                            Size::from((image.width as u16, image.height as u16)),
                            Point::from((image.xhot as u16, image.yhot as u16)),
                        ) {
                            slog_scope::warn!(
                                "Failed to set default cursor for Xwayland WM ({:?}): {}",
                                wm.id(),
                                err
                            );
                        }

                        let mut xwayland_state =
                            data.state.common.xwayland_state.get_mut(&drm_node).unwrap();
                        xwayland_state.xwm = Some(wm);
                    }
                    XWaylandEvent::Exited => {
                        if let Some(mut xwayland_state) =
                            data.state.common.xwayland_state.remove(&drm_node)
                        {
                            xwayland_state.xwm = None;
                        }
                    }
                }) {
                Ok(token) => token,
                Err(err) => {
                    slog_scope::error!("Failed to listen for Xwayland: {}", err);
                    return;
                }
            };

        match xwayland.start(
            self.common.event_loop_handle.clone(),
            None,
            std::iter::empty::<(OsString, OsString)>(),
            //vec![("WAYLAND_DEBUG", "client")].into_iter(),
            |map| {
                if let Some(node) = drm_node {
                    map.insert_if_missing(|| node);
                }
            },
        ) {
            Ok(display) => {
                self.common.xwayland_state.insert(
                    drm_node,
                    XWaylandState {
                        xwayland,
                        xwm: None,
                        display,
                    },
                );
            }
            Err(err) => {
                slog_scope::error!("Failed to start Xwayland: {}", err);
                self.common.event_loop_handle.remove(token);
            }
        }
    }
}

impl XwmHandler for Data {
    fn xwm_state(&mut self, xwm: XwmId) -> &mut X11Wm {
        self.state
            .common
            .xwayland_state
            .values_mut()
            .flat_map(|state| &mut state.xwm)
            .find(|wm| wm.id() == xwm)
            .unwrap()
    }

    fn new_window(&mut self, _xwm: XwmId, _window: X11Surface) {}
    fn new_override_redirect_window(&mut self, _xwm: XwmId, _window: X11Surface) {}
    fn destroyed_window(&mut self, _xwm: XwmId, _window: X11Surface) {}

    fn map_window_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(err) = window.set_mapped(true) {
            slog_scope::warn!(
                "Failed to send Xwayland Mapped-Event (for window {:?}): {}",
                window,
                err
            );
        }

        let surface = CosmicSurface::X11(window.clone());
        if self
            .state
            .common
            .shell
            .element_for_surface(&surface)
            .is_some()
        {
            return;
        }

        let seat = self.state.common.last_active_seat().clone();
        self.state
            .common
            .shell
            .pending_windows
            .push((surface, seat));
    }

    fn map_window_notify(&mut self, _xwm: XwmId, surface: X11Surface) {
        if let Some((window, seat)) = self
            .state
            .common
            .shell
            .pending_windows
            .iter()
            .find(|(window, _)| {
                if let CosmicSurface::X11(window) = window {
                    window == &surface
                } else {
                    false
                }
            })
            .cloned()
        {
            let output = seat.active_output();
            Shell::map_window(&mut self.state, &window, &output);
        }
    }

    fn mapped_override_redirect_window(&mut self, _xwm: XwmId, window: X11Surface) {
        if self
            .state
            .common
            .shell
            .override_redirect_windows
            .iter()
            .any(|or| or == &window)
        {
            return;
        }
        Shell::map_override_redirect(&mut self.state, window)
    }

    fn unmapped_window(&mut self, _xwm: XwmId, window: X11Surface) {
        if window.is_override_redirect() {
            self.state
                .common
                .shell
                .override_redirect_windows
                .retain(|or| or != &window);
        } else if let Some((element, space)) = self
            .state
            .common
            .shell
            .element_for_surface(&CosmicSurface::X11(window.clone()))
            .cloned()
            .and_then(|element| {
                self.state
                    .common
                    .shell
                    .space_for_mut(&element)
                    .map(|space| (element, space))
            })
        {
            space.unmap(&element);
        }

        let outputs = if let Some(wl_surface) = window.wl_surface() {
            self.state
                .common
                .shell
                .visible_outputs_for_surface(&wl_surface)
                .collect::<Vec<_>>()
        } else {
            self.state
                .common
                .shell
                .outputs()
                .cloned()
                .collect::<Vec<_>>()
        };
        for output in outputs.iter() {
            self.state.common.shell.active_space_mut(output).refresh();
        }

        // screencopy
        let mut scheduled_sessions = window
            .wl_surface()
            .map(|wl_surface| self.state.schedule_workspace_sessions(&wl_surface))
            .unwrap_or_default();

        for output in outputs.into_iter() {
            if let Some(sessions) = output.user_data().get::<PendingScreencopyBuffers>() {
                scheduled_sessions
                    .get_or_insert_with(Vec::new)
                    .extend(sessions.borrow_mut().drain(..));
            }
            self.state.backend.schedule_render(
                &self.state.common.event_loop_handle,
                &output,
                scheduled_sessions.as_ref().map(|sessions| {
                    sessions
                        .iter()
                        .filter(|(s, _)| match s.session_type() {
                            SessionType::Output(o) | SessionType::Workspace(o, _)
                                if o == output =>
                            {
                                true
                            }
                            _ => false,
                        })
                        .cloned()
                        .collect::<Vec<_>>()
                }),
            );
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
        if let Some(mapped) = self
            .state
            .common
            .shell
            .element_for_surface(&CosmicSurface::X11(window.clone()))
        {
            let space = self.state.common.shell.space_for(mapped).unwrap();
            if space.is_floating(mapped) {
                mapped.set_geometry(Rectangle::from_loc_and_size(
                    current_geo.loc,
                    (
                        w.map(|w| w as i32).unwrap_or(current_geo.size.w),
                        h.map(|h| h as i32).unwrap_or(current_geo.size.h),
                    ),
                ))
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
            if let Some(id) = above {
                let or_windows = &mut self.state.common.shell.override_redirect_windows;
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

            let geo = window.geometry();
            for (output, overlap) in self.state.common.shell.outputs().cloned().map(|o| {
                let intersection = o.geometry().intersection(geo);
                (o, intersection)
            }) {
                if let Some(overlap) = overlap {
                    window.output_enter(&output, overlap);
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
            let seat = self.state.common.last_active_seat().clone();
            Shell::resize_request(
                &mut self.state,
                &wl_surface,
                &seat,
                None,
                resize_edge.into(),
            )
        }
    }

    fn move_request(&mut self, _xwm: XwmId, window: X11Surface, _button: u32) {
        if let Some(wl_surface) = window.wl_surface() {
            let seat = self.state.common.last_active_seat().clone();
            Shell::move_request(&mut self.state, &wl_surface, &seat, None)
        }
    }

    fn maximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let seat = self.state.common.last_active_seat();
        let output = seat.active_output();
        let surface = CosmicSurface::X11(window);

        if let Some(mapped) = self
            .state
            .common
            .shell
            .element_for_surface(&surface)
            .cloned()
        {
            if let Some(workspace) = self.state.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped.windows().find(|(w, _)| w == &surface).unwrap();
                workspace.maximize_request(&window, &output)
            }
        }
    }

    fn unmaximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let surface = CosmicSurface::X11(window);
        if let Some(mapped) = self
            .state
            .common
            .shell
            .element_for_surface(&surface)
            .cloned()
        {
            if let Some(workspace) = self.state.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped.windows().find(|(w, _)| w == &surface).unwrap();
                workspace.unmaximize_request(&window);
            }
        }
    }

    fn fullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let seat = self.state.common.last_active_seat();
        let output = seat.active_output();
        let surface = CosmicSurface::X11(window);

        if let Some(mapped) = self
            .state
            .common
            .shell
            .element_for_surface(&surface)
            .cloned()
        {
            if let Some(workspace) = self.state.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped.windows().find(|(w, _)| w == &surface).unwrap();
                workspace.fullscreen_request(&window, &output)
            }
        }
    }

    fn unfullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let surface = CosmicSurface::X11(window);
        if let Some(mapped) = self
            .state
            .common
            .shell
            .element_for_surface(&surface)
            .cloned()
        {
            if let Some(workspace) = self.state.common.shell.space_for_mut(&mapped) {
                let (window, _) = mapped.windows().find(|(w, _)| w == &surface).unwrap();
                workspace.unfullscreen_request(&window)
            }
        }
    }
}
