use std::{
    ffi::OsString,
    io::Write,
    os::unix::io::OwnedFd,
    process::Stdio,
    sync::mpsc::{self, Receiver, Sender},
};

use crate::{
    backend::render::cursor::{Cursor, load_cursor_env, load_cursor_theme},
    shell::{
        CosmicSurface, PendingWindow, Shell, focus::target::KeyboardFocusTarget, grabs::ReleaseMode,
    },
    state::State,
    utils::prelude::*,
    wayland::handlers::{selection::SelectionUserData, xdg_activation::ActivationContext},
};
use cosmic_comp_config::{EavesdroppingKeyboardMode, XwaylandDescaling};
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::DrmNode,
        input::{ButtonState, KeyState, Keycode},
        renderer::{
            Bind, Frame, Offscreen, Renderer,
            element::{
                Kind,
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
            },
            pixman::{PixmanError, PixmanRenderer},
            utils::{draw_render_elements, with_renderer_surface_state},
        },
    },
    desktop::space::SpaceElement,
    input::{keyboard::ModifiersState, pointer::CursorIcon},
    reexports::{wayland_server::Client, x11rb::protocol::xproto::Window as X11Window},
    utils::{
        Buffer as BufferCoords, Logical, Point, Rectangle, SERIAL_COUNTER, Serial, Size, Transform,
    },
    wayland::{
        selection::{
            SelectionTarget,
            data_device::{
                clear_data_device_selection, current_data_device_selection_userdata,
                request_data_device_client_selection, set_data_device_selection,
            },
            primary_selection::{
                clear_primary_selection, current_primary_selection_userdata,
                request_primary_client_selection, set_primary_selection,
            },
        },
        xdg_activation::XdgActivationToken,
    },
    xwayland::{
        X11Surface, X11Wm, XWayland, XWaylandClientData, XWaylandEvent, XwmHandler,
        xwm::{Reorder, WmWindowProperty, XwmId},
    },
};
use tracing::{debug, error, trace, warn};

use crate::logger::GAMING_TARGET;
use xcursor::parser::Image;
use xkbcommon::xkb::Keysym;

#[derive(Debug)]
pub struct XWaylandState {
    pub client: Client,
    pub xwm: Option<X11Wm>,
    pub display: u32,
    pub pressed_keys: Vec<Keycode>,
    pub pressed_buttons: Vec<u32>,
    pub last_modifier_state: Option<ModifiersState>,
    pub clipboard_selection_dirty: Option<Vec<String>>,
    pub primary_selection_dirty: Option<Vec<String>>,
    pub xrdb_thread: Sender<(String, u32, u32)>,
}

fn xrdb_thread(rx: Receiver<(String, u32, u32)>, display: u32) {
    while let Ok((cursor_theme, cursor_size, dpi)) = rx.recv() {
        if let Ok(mut child) = std::process::Command::new("xrdb")
            .arg("-merge")
            .env("DISPLAY", format!(":{}", display))
            .stdin(Stdio::piped())
            .spawn()
        {
            let resources = format!(
                "Xcursor.theme: {}\nXcursor.size: {}\nXft.dpi: {}\n",
                cursor_theme, cursor_size, dpi,
            );
            if let Some(mut stdin) = child.stdin.take()
                && let Err(err) = stdin.write_all(resources.as_bytes())
            {
                warn!("Failed to update xresources: {}", err);
            }
            match child.wait() {
                Ok(code) if code.success() => {}
                Ok(code) => {
                    warn!("xrdb failed with code: {}", code);
                }
                Err(err) => {
                    warn!("Failed to wait for child: {}", err);
                }
            }
        } else {
            warn!("`xrdb` not found, cannot update Xresources.");
        }
    }
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
            std::iter::empty::<OsString>(),
            true,
            Stdio::null(),
            Stdio::null(),
            |user_data| {
                if let Some(node) = render_node {
                    user_data.insert_if_missing_threadsafe(|| node);
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
                    let (tx, rx) = mpsc::channel();
                    std::thread::spawn(move || xrdb_thread(rx, display_number));

                    data.common.xwayland_state = Some(XWaylandState {
                        client: client.clone(),
                        xwm: None,
                        display: display_number,
                        pressed_keys: Vec::new(),
                        pressed_buttons: Vec::new(),
                        last_modifier_state: None,
                        clipboard_selection_dirty: None,
                        primary_selection_dirty: None,
                        xrdb_thread: tx,
                    });

                    let wm = match X11Wm::start_wm(
                        data.common.event_loop_handle.clone(),
                        &data.common.display_handle,
                        x11_socket,
                        client.clone(),
                    ) {
                        Ok(wm) => wm,
                        Err(err) => {
                            error!(?err, "Failed to start Xwayland WM");
                            return;
                        }
                    };

                    let xwayland_state = data.common.xwayland_state.as_mut().unwrap();
                    xwayland_state.xwm = Some(wm);
                    xwayland_state.reload_cursor(1.);
                    data.notify_ready();

                    data.common.update_xwayland_settings();
                    data.common.update_xwayland_primary_output();
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
            }
        }
    }
}

fn scale_cursor(
    scale: f64,
    cursor_size: u32,
    image: &Image,
) -> Result<(Vec<u8>, Size<u16, Logical>, Point<u16, Logical>), PixmanError> {
    let mut renderer = PixmanRenderer::new()?;
    let image_scale = (image.size / cursor_size).max(1);
    let pixel_size = (cursor_size as f64 * scale).round() as i32;
    let buffer_size = Size::<i32, BufferCoords>::from((pixel_size, pixel_size));
    let output_size = buffer_size.to_logical(1, Transform::Normal).to_physical(1);

    let image_buffer = MemoryRenderBuffer::from_slice(
        &image.pixels_rgba,
        Fourcc::Abgr8888,
        (image.width as i32, image.height as i32),
        image_scale as i32,
        Transform::Normal,
        None,
    );
    let element = MemoryRenderBufferRenderElement::from_buffer(
        &mut renderer,
        (0., 0.),
        &image_buffer,
        None,
        None,
        None,
        Kind::Unspecified,
    )?;

    let mut buffer = renderer.create_buffer(Fourcc::Abgr8888, buffer_size)?;
    let mut fb = renderer.bind(&mut buffer)?;
    let mut frame = renderer.render(&mut fb, output_size, Transform::Normal)?;
    draw_render_elements(
        &mut frame,
        scale,
        &[element],
        &[Rectangle::new((0, 0).into(), output_size)],
    )?;
    let sync = frame.finish()?;
    while sync.wait().is_err() {}

    let len = (buffer_size.w * buffer_size.h * 4) as usize;
    let mut data = Vec::with_capacity(len);
    assert_eq!(buffer.stride(), (buffer_size.w * 4) as usize);
    data.extend_from_slice(unsafe { std::slice::from_raw_parts(buffer.data() as *mut u8, len) });

    let hotspot = Point::<i32, BufferCoords>::from((image.xhot as i32, image.yhot as i32))
        .to_f64()
        .to_logical(
            image_scale as f64,
            Transform::Normal,
            &Size::from((image.width as f64, image.height as f64)),
        )
        .to_buffer(
            scale,
            Transform::Normal,
            &output_size.to_logical(1).to_f64(),
        )
        .to_i32_round::<i32>();
    Ok((
        data,
        Size::from((buffer_size.w as u16, buffer_size.h as u16)),
        Point::from((hotspot.x as u16, hotspot.y as u16)),
    ))
}

impl XWaylandState {
    pub fn reload_cursor(&mut self, scale: f64) {
        if let Some(wm) = self.xwm.as_mut() {
            let (theme, size) = load_cursor_theme();
            let cursor = Cursor::load(&theme, CursorIcon::Default, size);
            let image = cursor.get_image(scale.ceil() as u32, 0);

            let (pixels_rgba, size, hotspot) = match scale_cursor(scale, size, &image) {
                Ok(x) => x,
                Err(err) => {
                    warn!("Failed to scale Xwayland cursor image: {}", err);
                    (
                        image.pixels_rgba,
                        Size::from((image.width as u16, image.height as u16)),
                        Point::from((image.xhot as u16, image.yhot as u16)),
                    )
                }
            };

            if let Err(err) = wm.set_cursor(&pixels_rgba, size, hotspot) {
                warn!(
                    id = ?wm.id(),
                    ?err,
                    "Failed to set default cursor for Xwayland WM",
                );
            }
        }
    }
}

impl Common {
    pub fn has_x_keyboard_focus(&self, xwmid: XwmId) -> bool {
        let keyboard = self
            .shell
            .read()
            .seats
            .last_active()
            .get_keyboard()
            .unwrap();

        keyboard
            .current_focus()
            .is_some_and(|target| target.is_xwm(xwmid))
    }

    fn has_x_pointer_focus(&self, xwmid: XwmId) -> bool {
        let pointer = self.shell.read().seats.last_active().get_pointer().unwrap();

        if let Some(x_client) = self.xwayland_state.as_ref().and_then(|xstate| {
            xstate
                .xwm
                .as_ref()
                .is_some_and(|xwm| xwm.id() == xwmid)
                .then_some(&xstate.client)
        }) {
            pointer
                .current_focus()
                .is_some_and(|target| target.is_client(x_client))
        } else {
            false
        }
    }

    pub fn xwayland_notify_focus_change(
        &mut self,
        target: Option<KeyboardFocusTarget>,
        serial: Serial,
    ) {
        if let Some(xwm_id) = self
            .xwayland_state
            .as_ref()
            .and_then(|xstate| xstate.xwm.as_ref())
            .map(|xwm| xwm.id())
        {
            if target
                .as_ref()
                .is_some_and(|target| matches!(target, KeyboardFocusTarget::LockSurface(_)))
            {
                self.xwayland_reset_eavesdropping(serial);
                return;
            }

            if !self.has_x_keyboard_focus(xwm_id)
                && target.as_ref().is_some_and(|target| target.is_xwm(xwm_id))
            {
                self.xwayland_reset_eavesdropping(serial);

                let xstate = self.xwayland_state.as_mut().unwrap();
                if let Some(mime_types) = xstate.clipboard_selection_dirty.take()
                    && let Err(err) = xstate
                        .xwm
                        .as_mut()
                        .unwrap()
                        .new_selection(SelectionTarget::Clipboard, Some(mime_types))
                {
                    warn!(?err, "Failed to set Xwayland clipboard selection.");
                }
                if let Some(mime_types) = xstate.primary_selection_dirty.take()
                    && let Err(err) = xstate
                        .xwm
                        .as_mut()
                        .unwrap()
                        .new_selection(SelectionTarget::Primary, Some(mime_types))
                {
                    warn!(?err, "Failed to set Xwayland clipboard selection.");
                }
            }
        }
    }

    pub fn xwayland_reset_eavesdropping(&mut self, serial: Serial) {
        let seat = self.shell.read().seats.last_active().clone();
        let keyboard = seat.get_keyboard().unwrap();
        let pointer = seat.get_pointer().unwrap();

        let xstate = self.xwayland_state.as_mut().unwrap();
        xstate.last_modifier_state.take();
        for key in xstate.pressed_keys.drain(..).rev() {
            for wl_keyboard in keyboard.client_keyboards(&xstate.client) {
                wl_keyboard.key(serial.into(), 0, key.raw() - 8, KeyState::Released.into());
            }
        }
        for button in xstate.pressed_buttons.drain(..).rev() {
            for wl_pointer in pointer.client_pointers(&xstate.client) {
                wl_pointer.button(serial.into(), 0, button, ButtonState::Released.into());
            }
        }
    }

    #[profiling::function]
    pub fn xwayland_notify_key_event(
        &mut self,
        sym: Keysym,
        code: Keycode,
        state: KeyState,
        serial: Serial,
        time: u32,
    ) {
        let config = self.config.cosmic_conf.xwayland_eavesdropping.keyboard;
        if config == EavesdroppingKeyboardMode::None {
            return;
        }

        if self.xwayland_state.as_ref().is_none_or(|xstate| {
            xstate
                .xwm
                .as_ref()
                .is_none_or(|xwm| self.has_x_keyboard_focus(xwm.id()))
        }) {
            return;
        }

        let keyboard = self
            .shell
            .read()
            .seats
            .last_active()
            .get_keyboard()
            .unwrap();
        let modifiers = keyboard.modifier_state();
        let is_modifier = sym.is_modifier_key();

        let xstate = self.xwayland_state.as_mut().unwrap();
        if state == KeyState::Pressed {
            match config {
                EavesdroppingKeyboardMode::Modifiers => {
                    if !is_modifier {
                        return;
                    }
                }
                EavesdroppingKeyboardMode::Combinations => {
                    // don't forward alpha-numeric keys, just because shift is held, but forward shift itself
                    if !(is_modifier || modifiers.alt || modifiers.ctrl || modifiers.logo) {
                        return;
                    }
                }
                _ => {}
            }

            xstate.pressed_keys.push(code);
        } else {
            let mut removed = false;
            xstate.pressed_keys.retain(|c| {
                if *c == code {
                    removed = true;
                    false
                } else {
                    true
                }
            });

            if !removed {
                // Don't forward released events, we don't have a record off.
                return;
            }
        }

        tracing::trace!("Forwaring key {} {:?} to xwayland", code.raw() - 8, state);
        for wl_keyboard in keyboard.client_keyboards(&xstate.client) {
            wl_keyboard.key(serial.into(), time, code.raw() - 8, state.into());
            if xstate.last_modifier_state != Some(modifiers) {
                xstate.last_modifier_state = Some(modifiers);
                wl_keyboard.modifiers(
                    serial.into(),
                    modifiers.serialized.depressed,
                    modifiers.serialized.latched,
                    modifiers.serialized.locked,
                    modifiers.serialized.layout_effective,
                );
            }
        }
    }

    #[profiling::function]
    pub fn xwayland_notify_pointer_button_event(
        &mut self,
        button: u32,
        state: ButtonState,
        serial: Serial,
        time: u32,
    ) {
        if !self.config.cosmic_conf.xwayland_eavesdropping.pointer {
            return;
        }

        let pointer = self.shell.read().seats.last_active().get_pointer().unwrap();

        if self.xwayland_state.as_ref().is_none_or(|xstate| {
            xstate
                .xwm
                .as_ref()
                .is_none_or(|xwm| self.has_x_pointer_focus(xwm.id()))
        }) {
            return;
        }

        let xstate = self.xwayland_state.as_mut().unwrap();
        if state == ButtonState::Pressed {
            xstate.pressed_buttons.push(button);
        } else {
            let mut removed = false;
            xstate.pressed_buttons.retain(|b| {
                if *b == button {
                    removed = true;
                    false
                } else {
                    true
                }
            });

            if !removed {
                // Don't forward released events, we don't have a record off.
                // This can happen if `xwayland_reset_eavesdropping` was called in between
                return;
            }
        }

        tracing::trace!("Forwaring ptr button {} {:?} to Xwayland", button, state);
        for wl_pointer in pointer.client_pointers(&xstate.client) {
            wl_pointer.button(serial.into(), time, button, state.into());
        }
    }

    #[profiling::function]
    pub fn update_x11_stacking_order(&mut self) {
        let shell = self.shell.read();
        let seat = shell.seats.last_active();
        let active_output = seat.active_output();

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
                                    let focus_last =
                                        workspace.focus_stack.get(seat).last().cloned();
                                    workspace
                                        .get_fullscreen_surfaces()
                                        .filter(|f| {
                                            focus_last.as_ref().is_some_and(|t| t == &f.surface)
                                        })
                                        .map(|f| f.surface.clone())
                                        .collect::<Vec<_>>()
                                        .into_iter()
                                        .chain(workspace.mapped().flat_map(|mapped| {
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
                                        }))
                                        .chain(
                                            workspace
                                                .get_fullscreen_surfaces()
                                                .filter(|f| {
                                                    focus_last
                                                        .as_ref()
                                                        .is_none_or(|t| t != &f.surface)
                                                })
                                                .map(|f| f.surface.clone())
                                                .collect::<Vec<_>>()
                                                .into_iter(),
                                        )
                                        .chain(
                                            workspace
                                                .minimized_windows
                                                .iter()
                                                .flat_map(|m| m.windows()),
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

    pub fn update_xwayland_settings(&mut self) {
        let new_scale = match self.config.cosmic_conf.descale_xwayland {
            XwaylandDescaling::Disabled => 1.,
            XwaylandDescaling::Enabled => {
                let shell = self.shell.read();
                shell
                    .outputs()
                    .map(|o| o.current_scale().integer_scale())
                    .max()
                    .unwrap_or(1) as f64
            }
            XwaylandDescaling::Fractional => {
                let shell = self.shell.read();

                if let Some(output) = shell.outputs().find(|o| o.config().xwayland_primary) {
                    output.current_scale().fractional_scale().max(1f64)
                } else {
                    shell
                        .outputs()
                        .map(|o| o.current_scale().fractional_scale())
                        .fold(1f64, |acc, val| acc.max(val))
                }
            }
        };
        let (_, cursor_size) = load_cursor_env();

        // compare with current scale
        if let Some(xwayland) = self.xwayland_state.as_mut() {
            let geometries = if Some(new_scale) != self.xwayland_scale {
                // backup geometries
                let geometries = self
                    .shell
                    .read()
                    .mapped()
                    .flat_map(|m| m.windows().map(|(s, _)| s))
                    .filter_map(|s| s.0.x11_surface().map(|x| (x.clone(), x.geometry())))
                    .collect::<Vec<_>>();

                // update xorg dpi
                if let Some(xwm) = xwayland.xwm.as_mut() {
                    let base = 96. * 1024.;
                    let dpi = new_scale * base;
                    let fractional = new_scale.fract();
                    let integer = (new_scale - fractional).max(1.0);
                    let unscaled_dpi = base * (1.0 + fractional / integer);
                    if let Err(err) = xwm.set_xsettings(
                        [
                            ("Xft/DPI".into(), (dpi.round() as i32).into()),
                            (
                                "Gdk/UnscaledDPI".into(),
                                (unscaled_dpi.round() as i32).into(),
                            ),
                            (
                                "Gdk/WindowScalingFactor".into(),
                                (new_scale.floor() as i32).into(),
                            ),
                            (
                                "Gtk/CursorThemeSize".into(),
                                ((new_scale * cursor_size as f64).round() as i32).into(),
                            ),
                        ]
                        .into_iter(),
                    ) {
                        warn!(wm_id = ?xwm.id(), ?err, "Failed to update XSETTINGS.");
                    }
                }

                Some(geometries)
            } else {
                None
            };

            if xwayland
                .xrdb_thread
                .send((
                    crate::toolkit_config::icon_theme_default(),
                    (new_scale * cursor_size as f64).round() as u32,
                    (new_scale * 96.0).round() as u32,
                ))
                .is_err()
            {
                warn!("xrdb thread died");
            }

            // update cursor
            xwayland.reload_cursor(new_scale);

            if let Some(geometries) = geometries {
                // update client scale
                xwayland
                    .client
                    .get_data::<XWaylandClientData>()
                    .unwrap()
                    .compositor_state
                    .set_client_scale(new_scale);

                // update wl/xdg_outputs
                for output in self.shell.read().outputs() {
                    output.change_current_state(None, None, None, None);
                }

                // update geometries
                for (surface, geometry) in geometries.iter() {
                    if let Err(err) = surface.configure(*geometry) {
                        warn!(?err, surface = ?surface.window_id(), "Failed to update geometry after scale change");
                    }
                }
                self.update_x11_stacking_order();

                self.xwayland_scale = Some(new_scale);
            }
        }
    }

    pub fn update_xwayland_primary_output(&mut self) {
        let mut xwayland_primary_output = None;
        for output in self.output_configuration_state.outputs() {
            if output.config().xwayland_primary {
                xwayland_primary_output = Some(output);
                break;
            }
        }

        if let Some(xstate) = self.xwayland_state.as_mut()
            && let Some(xwm) = xstate.xwm.as_mut()
            && let Err(err) = xwm.set_randr_primary_output(xwayland_primary_output.as_ref())
        {
            warn!("Failed to set xwayland primary output: {}", err);
            return;
        };

        self.output_configuration_state.update();
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
    fn destroyed_window(&mut self, _xwm: XwmId, window: X11Surface) {
        debug!(
            target: GAMING_TARGET,
            window_id = window.window_id(),
            class = %window.class(),
            "x11 window destroyed"
        );
        let mut shell = self.common.shell.write();
        shell
            .pending_windows
            .retain(|p| p.surface.x11_surface() != Some(&window));

        let seat = shell.seats.last_active().clone();
        if shell
            .unmap_surface(&window, &seat, &mut self.common.toplevel_info_state)
            .is_some()
        {
            let outputs: Vec<_> = shell.outputs().cloned().collect();
            for output in &outputs {
                shell.refresh_active_space(output);
            }
            drop(shell);
            for output in &outputs {
                self.backend.schedule_render(output);
            }
        }
    }

    fn baselayer_changed(&mut self, _xwm: XwmId, appids: Vec<u32>, _window: Option<X11Window>) {
        // The session manager publishes base-layer priority on the root window to
        // say which app should be on top WITHOUT focusing it — that is how a custom
        // webview is shown over a still-running game. Record the order; game mode's
        // refresh applies it when it resolves what to render.
        debug!(
            target: GAMING_TARGET,
            ?appids,
            "base-layer priority changed"
        );
        let mut shell = self.common.shell.write();
        if shell.game_mode.baselayer_appids != appids {
            shell.game_mode.baselayer_appids = appids;
        }
    }

    fn property_notify(&mut self, _xwm: XwmId, window: X11Surface, property: WmWindowProperty) {
        // Live Steam window-role changes drive game mode. These run with no shell
        // lock held (XWM dispatch), so the handlers can take their own.
        // STEAM_GAME / STEAM_OVERLAY / STEAM_INPUT_FOCUS retagging underlies every
        // game-mode transition; logging the atom change is the first link.
        trace!(
            target: GAMING_TARGET,
            window_id = window.window_id(),
            class = %window.class(),
            ?property,
            "x11 property_notify"
        );
        match property {
            // STEAM_GAME changed — a deferred game-mode enter may now resolve to it
            // (e.g. the launcher tags the game window after it maps), or the active
            // game surface may have been retagged onto a different window. Defer to
            // an idle: these can enter/exit game mode (fullscreen/minimize), which
            // drops decoration iced elements that unregister calloop sources —
            // unsafe while this XWM dispatch borrows the loop's source registry.
            WmWindowProperty::SteamGame => {
                self.common.event_loop_handle.insert_idle(|state| {
                    state.try_resolve_pending_game_mode();
                    state.refresh_active_game_surface();
                });
            }
            // An overlay marker toggled — recompute overlay-visible + the fast-path gate.
            WmWindowProperty::SteamOverlay | WmWindowProperty::ExternalOverlay => {
                self.refresh_overlay_visible();
            }
            // STEAM_INPUT_FOCUS toggled — grant/release the input grab over the game.
            WmWindowProperty::SteamInputFocus => {
                self.update_game_mode_input_grab(&window);
            }
            _ => {}
        }
    }

    fn map_window_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(err) = window.set_mapped(true) {
            warn!(?window, ?err, "Failed to send Xwayland Mapped-Event",);
        }

        let mut shell = self.common.shell.write();
        let startup_id = window.startup_id();
        if shell.is_surface_mapped(&window) {
            warn!(
                ?window,
                "Got map_request for already mapped window? Ignoring"
            );
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
                *context,
            );
        }
        if !shell.pending_windows.iter().any(|w| w.surface == window) {
            let fullscreen = window.is_fullscreen().then(|| seat.active_output());
            let maximized = window.is_maximized();
            let surface = CosmicSurface::from(window);
            shell.pending_windows.push(PendingWindow {
                surface,
                seat,
                fullscreen,
                maximized,
                sticky: false,
                frame_notified: false,
            })
        }
    }

    fn map_window_notify(&mut self, _xwm: XwmId, surface: X11Surface) {
        let startup_id_opt = {
            let shell = self.common.shell.read();
            if !shell
                .pending_windows
                .iter()
                .any(|p| p.surface.x11_surface() == Some(&surface))
            {
                return; // Not a managed pending window — nothing to do.
            }
            shell
                .pending_windows
                .iter()
                .find(|p| p.surface.x11_surface() == Some(&surface))
                .and_then(|p| p.surface.x11_surface().and_then(|x| x.startup_id()))
        };

        let mut shell = self.common.shell.write();

        // Insert activation context if not already present.
        if let std::collections::hash_map::Entry::Vacant(e) = shell
            .pending_activations
            .entry(crate::shell::ActivationKey::X11(surface.window_id()))
            && let Some(startup_id) = startup_id_opt
            && let Some(context) = self
                .common
                .xdg_activation_state
                .data_for_token(&XdgActivationToken::from(startup_id))
                .and_then(|data| data.user_data.get::<ActivationContext>())
        {
            e.insert(*context);
        }

        std::mem::drop(shell);

        // If the surface supports _NET_WM_SYNC_REQUEST, send a forced sync request
        // using the current geometry (no actual resize). The client will render its
        // first frame and increment the counter; sync_request_acked fires and maps.
        // This eliminates the black-frame flash even when the first committed buffer
        // would otherwise be all-black (e.g. Zoom's OpenGL init clear).
        let geo = surface.geometry();
        let sync_supported = surface.sync_request_supported();
        debug!(
            target: GAMING_TARGET,
            window_id = surface.window_id(),
            class = %surface.class(),
            sync_supported,
            geo_w = geo.size.w,
            geo_h = geo.size.h,
            "x11 map_window_notify (forced-sync waits for first frame; otherwise maps on buffer)"
        );
        if sync_supported {
            let _ = surface.configure_with_forced_sync(geo, None);
            // Sync pending: sync_request_acked / sync_request_timeout will map.
            return;
        }

        // No sync support: fall back to frame_notified + commit-handler approach.
        let mut shell = self.common.shell.write();
        if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|p| p.surface.x11_surface() == Some(&surface))
        {
            pending.frame_notified = true;
        }

        // Case B fallback: buffer already committed, map immediately.
        let has_buffer = surface
            .wl_surface()
            .as_ref()
            .and_then(|wl| with_renderer_surface_state(wl, |s| s.buffer().is_some()))
            .unwrap_or(false);

        let window = if has_buffer {
            shell
                .pending_windows
                .iter()
                .find(|p| p.surface.x11_surface() == Some(&surface))
                .map(|p| p.surface.clone())
        } else {
            None
        };
        std::mem::drop(shell);

        if let Some(window) = window {
            self.map_x11_window_now(&window);
        }
    }

    fn sync_request_acked(&mut self, _xwm: XwmId, surface: X11Surface) {
        // Client finished rendering its first frame (or a resize). For pending windows
        // this is our signal to show the window — no black flash.
        let shell = self.common.shell.read();
        let window = shell
            .pending_windows
            .iter()
            .find(|p| p.surface.x11_surface() == Some(&surface))
            .map(|p| p.surface.clone());
        std::mem::drop(shell);

        if let Some(window) = window {
            let mut shell = self.common.shell.write();
            if let Some(pending) = shell
                .pending_windows
                .iter_mut()
                .find(|p| p.surface.x11_surface() == Some(&surface))
            {
                pending.frame_notified = true;
            }
            std::mem::drop(shell);
            self.map_x11_window_now(&window);
        }
    }

    fn sync_request_timeout(&mut self, _xwm: XwmId, surface: X11Surface) {
        // Client didn't ack within the timeout — map it anyway so it's not stuck forever.
        warn!(
            target: GAMING_TARGET,
            window_id = surface.window_id(),
            class = %surface.class(),
            "x11 sync_request TIMEOUT — mapping without a confirmed first frame (grey-frame risk)"
        );
        let shell = self.common.shell.read();
        let window = shell
            .pending_windows
            .iter()
            .find(|p| p.surface.x11_surface() == Some(&surface))
            .map(|p| p.surface.clone());
        std::mem::drop(shell);

        if let Some(window) = window {
            let mut shell = self.common.shell.write();
            if let Some(pending) = shell
                .pending_windows
                .iter_mut()
                .find(|p| p.surface.x11_surface() == Some(&surface))
            {
                pending.frame_notified = true;
            }
            std::mem::drop(shell);
            self.map_x11_window_now(&window);
        }
    }

    fn mapped_override_redirect_window(&mut self, xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        let already_present = shell
            .override_redirect_windows
            .iter()
            .any(|or| or == &window);
        if already_present {
            return;
        }
        // A real Steam/external overlay (Shift+Tab, MangoHud) is an OR window; its
        // presence gates overlay_window_present() and the game-mode overlay path (bug 2).
        debug!(
            target: GAMING_TARGET,
            window_id = window.window_id(),
            class = %window.class(),
            "x11 override-redirect window mapped"
        );
        shell.map_override_redirect(window.clone());
        drop(shell);

        // Retry any wl_surface association that was deferred because the OR window was
        // not yet mapped when WL_SURFACE_SERIAL arrived (Wayland-first race).
        smithay::wayland::xwayland_shell::try_associate_or_surface(self, xwm, &window);
    }

    fn unmapped_window(&mut self, _xwm: XwmId, window: X11Surface) {
        debug!(
            target: GAMING_TARGET,
            window_id = window.window_id(),
            class = %window.class(),
            override_redirect = window.is_override_redirect(),
            "x11 window unmapped"
        );
        let mut shell = self.common.shell.write();
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
            shell.refresh_active_space(output);
        }

        for output in outputs.into_iter() {
            self.backend.schedule_render(&output);
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
        // Exception: X11 transient children can also reposition themselves (to follow parent).
        let mut shell = self.common.shell.write();

        if window.is_fullscreen() {
            let _ = window.configure(None);
            return;
        }

        if let Some(mapped) = shell
            .element_for_surface(&window)
            .filter(|mapped| !mapped.is_minimized())
            .cloned()
        {
            let is_transient = window.is_transient_for().is_some();

            // Find current geometry only for floating windows
            let current_geo = if let Some(workspace) = shell.space_for(&mapped) {
                workspace
                    .element_geometry(&mapped)
                    .filter(|_| workspace.is_floating(&window))
                    .map(|geo| geo.to_global(workspace.output()))
            } else if let Some((output, set)) = shell
                .workspaces
                .sets
                .iter()
                .find(|(_, set)| set.sticky_layer.mapped().any(|m| m == &mapped))
            {
                Some(
                    set.sticky_layer
                        .element_geometry(&mapped)
                        .unwrap()
                        .to_global(output),
                )
            } else {
                None
            };

            if let Some(current_geo) = current_geo {
                let ssd_height = mapped.ssd_height(false).unwrap_or(0);
                // For transient children, also allow repositioning
                let new_loc: Point<i32, Global> = if is_transient && (x.is_some() || y.is_some()) {
                    Point::from((
                        x.unwrap_or(current_geo.loc.x),
                        y.unwrap_or(current_geo.loc.y),
                    ))
                } else {
                    current_geo.loc
                };
                let new_size: Size<i32, Global> = (
                    w.map(|w| w as i32).unwrap_or(current_geo.size.w),
                    h.map(|h| h as i32 + ssd_height)
                        .unwrap_or(current_geo.size.h),
                )
                    .into();

                let new_geo = Rectangle::new(new_loc, new_size);
                mapped.set_geometry(new_geo);

                // If the position changed, also update the floating space mapping
                // so the window actually renders at the new position.
                if new_loc != current_geo.loc {
                    if let Some(workspace) = shell.space_for_mut(&mapped) {
                        let output = workspace.output.clone();
                        let local_pos = new_loc.to_local(&output);
                        workspace.floating_layer.space.map_element(
                            mapped,
                            local_pos.as_logical(),
                            true,
                        );
                    } else if let Some((output, set)) = shell
                        .workspaces
                        .sets
                        .iter_mut()
                        .find(|(_, set)| set.sticky_layer.mapped().any(|m| m == &mapped))
                    {
                        let local_pos = new_loc.to_local(output);
                        set.sticky_layer
                            .space
                            .map_element(mapped, local_pos.as_logical(), true);
                    }
                }
            } else {
                let _ = window.configure(None); // ack and force old state
            }
        } else {
            let mut current_geo = window.geometry();
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
            let mut shell = self.common.shell.write();
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
            let mut shell = self.common.shell.write();
            let seat = shell.seats.last_active().clone();
            if let Some((grab, focus)) = shell.resize_request(
                &wl_surface,
                &seat,
                None,
                resize_edge.into(),
                self.common.config.cosmic_conf.edge_snap_threshold,
                true,
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

    fn move_request(&mut self, _xwm: XwmId, window: X11Surface, _button: u32) {
        if let Some(wl_surface) = window.wl_surface() {
            let mut shell = self.common.shell.write();
            let seat = shell.seats.last_active().clone();
            if let Some((grab, focus)) = shell.move_request(
                &wl_surface,
                &seat,
                None,
                ReleaseMode::NoMouseButtons,
                false,
                &self.common.config,
                &self.common.event_loop_handle,
                true,
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
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            let seat = shell.seats.last_active().clone();
            shell.maximize_request(&mapped, &seat, true, &self.common.event_loop_handle);
        } else if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.x11_surface() == Some(&window))
        {
            pending.maximized = true;
        }
    }

    fn unmaximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        if let Some(mapped) = shell.element_for_surface(&window).cloned() {
            shell.unmaximize_request(&mapped);
        } else if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.x11_surface() == Some(&window))
        {
            pending.maximized = false;
        }
    }

    fn minimize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        shell.minimize_request(&window);
    }

    fn unminimize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        shell.unminimize_request(&window, &seat, &self.common.event_loop_handle);
    }

    fn fullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let output = window
            .wl_surface()
            .and_then(|surface| shell.visible_output_for_surface(&surface).cloned())
            .unwrap_or_else(|| seat.focused_or_active_output());

        match shell.fullscreen_request(&window, output.clone(), &self.common.event_loop_handle) {
            Some(target) => {
                std::mem::drop(shell);
                Shell::set_focus(self, Some(&target), &seat, None, true);
            }
            None => {
                if let Some(pending) = shell
                    .pending_windows
                    .iter_mut()
                    .find(|pending| pending.surface.x11_surface() == Some(&window))
                {
                    pending.fullscreen = Some(output);
                }
            }
        }
    }

    fn unfullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        let seat = shell.seats.last_active().clone();
        let should_focus = seat
            .get_keyboard()
            .unwrap()
            .current_focus()
            .is_some_and(|target| {
                if let KeyboardFocusTarget::Fullscreen(s) = target {
                    s == window
                } else {
                    false
                }
            });

        if let Some(target) = shell.unfullscreen_request(&window, &self.common.event_loop_handle) {
            std::mem::drop(shell);
            if should_focus {
                Shell::set_focus(self, Some(&target), &seat, None, true);
            }
        } else if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.x11_surface() == Some(&window))
        {
            pending.fullscreen.take();
        }
    }

    fn stick_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.x11_surface() == Some(&window))
        {
            pending.sticky = true;
        }
    }

    fn unstick_request(&mut self, _xwm: XwmId, window: X11Surface) {
        let mut shell = self.common.shell.write();
        if let Some(pending) = shell
            .pending_windows
            .iter_mut()
            .find(|pending| pending.surface.x11_surface() == Some(&window))
        {
            pending.sticky = false;
        }
    }

    fn active_window_request(
        &mut self,
        _xwm: XwmId,
        window: X11Surface,
        _timestamp: u32,
        _currently_active_window: Option<X11Surface>,
    ) {
        if let Some(surface) = window.wl_surface() {
            self.activate_surface(&surface, None);
        }
    }

    fn send_selection(
        &mut self,
        _xwm: XwmId,
        selection: SelectionTarget,
        mime_type: String,
        fd: OwnedFd,
    ) {
        let seat = self.common.shell.read().seats.last_active().clone();
        match selection {
            SelectionTarget::Clipboard => {
                // A persisted (compositor-owned) clipboard can't be read back
                // via `request_data_device_client_selection` — serve it from our
                // own cache instead.
                if current_data_device_selection_userdata(&seat).as_deref()
                    == Some(&SelectionUserData::Persisted)
                {
                    crate::clipboard::serve(self, &mime_type, fd);
                } else if let Err(err) = request_data_device_client_selection(&seat, mime_type, fd)
                {
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
        self.common.has_x_keyboard_focus(xwm)
    }

    fn new_selection(&mut self, xwm: XwmId, selection: SelectionTarget, mime_types: Vec<String>) {
        trace!(?selection, ?mime_types, "Got Selection from Xwayland",);

        let seat = self.common.shell.read().seats.last_active().clone();
        match selection {
            SelectionTarget::Clipboard => set_data_device_selection(
                &self.common.display_handle,
                &seat,
                mime_types,
                SelectionUserData::Xwayland(xwm),
            ),
            SelectionTarget::Primary => set_primary_selection(
                &self.common.display_handle,
                &seat,
                mime_types,
                SelectionUserData::Xwayland(xwm),
            ),
        }
    }

    fn cleared_selection(&mut self, xwm: XwmId, selection: SelectionTarget) {
        let shell = self.common.shell.read();
        for seat in shell.seats.iter() {
            match selection {
                SelectionTarget::Clipboard => {
                    if current_data_device_selection_userdata(seat).as_deref()
                        == Some(&SelectionUserData::Xwayland(xwm))
                    {
                        clear_data_device_selection(&self.common.display_handle, seat)
                    }
                }
                SelectionTarget::Primary => {
                    if current_primary_selection_userdata(seat).as_deref()
                        == Some(&SelectionUserData::Xwayland(xwm))
                    {
                        clear_primary_selection(&self.common.display_handle, seat)
                    }
                }
            }
        }
    }

    fn randr_primary_output_change(&mut self, _xwm: XwmId, output_name: Option<String>) {
        for output in self.common.output_configuration_state.outputs() {
            output.config_mut().xwayland_primary =
                output_name.as_deref().is_some_and(|o| o == output.name());
        }
        self.common.output_configuration_state.update();
        self.common.update_xwayland_settings();
        self.common
            .config
            .write_outputs(self.common.output_configuration_state.outputs());
    }

    fn disconnected(&mut self, _xwm: XwmId) {
        let xwayland_state = self.common.xwayland_state.as_mut().unwrap();
        xwayland_state.xwm = None;
    }
}

impl State {
    pub(crate) fn map_x11_window_now(&mut self, window: &CosmicSurface) {
        // Guard against zombie maps: an X11 window can be unmapped/destroyed while
        // we wait for its _NET_WM_SYNC_REQUEST ack.
        if let Some(x11) = window.x11_surface()
            && (!x11.alive() || x11.mapped_window_id().is_none())
        {
            let win_id = x11.window_id();
            let mut shell = self.common.shell.write();
            shell
                .pending_windows
                .retain(|p| p.surface.x11_surface().map(|s| s.window_id()) != Some(win_id));
            return;
        }
        self.check_pending_pid_embeds_for_window(window);

        let mut shell = self.common.shell.write();
        let res = shell.map_window(
            window,
            &mut self.common.toplevel_info_state,
            &mut self.common.workspace_state,
            &self.common.event_loop_handle,
        );
        std::mem::drop(shell);

        self.ensure_embedded_on_parent_output(window);

        if let Some(target) = res {
            let shell = self.common.shell.read();
            let seat = shell.seats.last_active().clone();
            std::mem::drop(shell);
            Shell::set_focus(self, Some(&target), &seat, None, false);
        }

        if let Some(x11) = window.x11_surface().cloned() {
            self.update_game_mode_input_grab(&x11);
        }
    }
}
