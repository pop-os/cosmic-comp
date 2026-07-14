use std::os::unix::net::UnixStream;

use reis::eis;
use smithay::reexports::reis;

use smithay::backend::input::KeyState;
use smithay::backend::libei::{EiInput, EiInputEvent, EiRegion};
use smithay::input::keyboard::Keysym;
use smithay::reexports::calloop;
use smithay::wayland::input_method::InputMethodSeat;
use smithay::wayland::text_input::TextInputSeat;

use crate::config::xkb_config_to_wl;
use crate::input::InputBackendId;
use crate::state::{BackendData, State};
use crate::utils::prelude::{OutputExt, RectGlobalExt};

// Requested device types for an EI connection, mirroring the XDG RemoteDesktop portal `DeviceType` bitmask
const DEVICE_TYPE_KEYBOARD: u32 = 1;
const DEVICE_TYPE_POINTER: u32 = 2;
const DEVICE_TYPE_TOUCHSCREEN: u32 = 4;

// Name of the EI absolute-pointer device. Shared so the connect path and the
// re-advertise-on-output-change path recreate the same device.
const ABSOLUTE_POINTER_NAME: &str = "virtual absolute pointer";

pub type EiRequest = (UnixStream, u32);

/// Build the regions advertised on the EI absolute-pointer device for the current
/// output layout: one region per output, each at its **global logical** offset with
/// its logical size and scale.
pub fn absolute_pointer_regions(state: &State) -> Vec<EiRegion> {
    let shell = state.common.shell.read();
    shell
        .outputs()
        .map(|output| {
            let scale = output.current_scale().fractional_scale();
            EiRegion {
                // Keep the signed logical rect
                // the u32 clamp happens at the `ei_device.region` in smithay.
                rect: output.geometry().as_logical(),
                scale: scale as f32,
                // Tie the region to its output so a client can correlate it with the
                // matching screencast stream (the portal advertises the same name).
                mapping_id: Some(output.name()),
            }
        })
        .collect()
}

/// Re-advertise the absolute-pointer region on every active EI seat.
pub fn refresh_absolute_pointer_regions(state: &State) {
    if state.common.ei_seats.is_empty() {
        return;
    }
    let regions = absolute_pointer_regions(state);
    for seat in state.common.ei_seats.values() {
        seat.add_pointer_absolute(ABSOLUTE_POINTER_NAME, &regions);
    }
}

pub fn setup_ei(
    handle: &calloop::LoopHandle<'static, State>,
) -> calloop::channel::Sender<EiRequest> {
    let (sender, channel) = calloop::channel::channel::<EiRequest>();
    let handle_clone = handle.clone();
    handle
        .insert_source(channel, move |event, _, _| {
            let calloop::channel::Event::Msg((stream, device_types)) = event else {
                return;
            };
            let context = match eis::Context::new(stream) {
                Ok(context) => context,
                Err(err) => {
                    tracing::error!("Failed to create EI context: {}", err);
                    return;
                }
            };
            let source = EiInput::new(context);
            if let Err(err) =
                handle_clone.insert_source(source, move |event, connection, data| match event {
                    EiInputEvent::Connected => {
                        let conn = connection.eis_connection().clone();
                        let seat = connection.add_seat("default");
                        let wants_keyboard = device_types & DEVICE_TYPE_KEYBOARD != 0;
                        // build the per-connection isolated keyboard state
                        let isolated_kbd = if wants_keyboard {
                            let conf = data.common.config.xkb_config();
                            let _ = seat.add_keyboard("virtual keyboard", xkb_config_to_wl(&conf));
                            // The text device lets clients inject keysyms/utf8 directly independent of the keymap.
                            seat.add_text("virtual text");
                            match smithay::input::keyboard::IsolatedKeyboardState::new(
                                xkb_config_to_wl(&conf),
                            ) {
                                Ok(iso) => Some(iso),
                                Err(err) => {
                                    tracing::warn!(
                                        ?err,
                                        "Failed to create libei isolated keyboard state"
                                    );
                                    None
                                }
                            }
                        } else {
                            None
                        };
                        if device_types & DEVICE_TYPE_POINTER != 0 {
                            seat.add_pointer("virtual pointer");
                            let regions = absolute_pointer_regions(data);
                            seat.add_pointer_absolute(ABSOLUTE_POINTER_NAME, &regions);
                        }
                        if device_types & DEVICE_TYPE_TOUCHSCREEN != 0 {
                            seat.add_touch("virtual touch");
                        }
                        // Track the seat so its virtual keyboard can be re-created when the
                        // keyboard configuration changes at runtime.
                        if wants_keyboard {
                            if let Some(iso) = isolated_kbd {
                                data.common.ei_isolated_kbd.insert(conn.clone(), iso);
                            }
                            data.common.ei_seats.insert(conn, seat);
                            data.update_ei_input_method();
                        }
                    }
                    EiInputEvent::Disconnected => {
                        let conn = connection.eis_connection().clone();
                        let backend_id = InputBackendId::Ei(conn.clone());
                        data.release_ei_keyboard(&conn);
                        data.clear_input_source_state(&backend_id);
                        data.common.ei_seats.remove(&conn);
                        data.common.ei_isolated_kbd.remove(&conn);
                        data.common.ei_text_keycode_held.remove(&conn);
                        data.update_ei_input_method();
                    }
                    EiInputEvent::Event(event) => {
                        use smithay::backend::input::{InputEvent, KeyboardKeyEvent};
                        // Route keyboard input through the connection's isolated state
                        match event {
                            InputEvent::Keyboard { event } => {
                                data.inject_ei_key(
                                    connection.eis_connection(),
                                    event.key_code(),
                                    event.state(),
                                );
                            }
                            other => {
                                let backend_id =
                                    InputBackendId::Ei(connection.eis_connection().clone());
                                data.process_input_event(other, backend_id);
                                if matches!(data.backend, BackendData::Kms(_)) {
                                    for output in data.common.shell.read().outputs() {
                                        data.backend.kms().schedule_render(output);
                                    }
                                }
                            }
                        }
                    }
                    EiInputEvent::TextKeysym { keysym, state } => {
                        data.inject_ei_text_keysym(
                            connection.eis_connection(),
                            keysym,
                            state,
                            true, // explicit keysym could cause shortcuts
                        );
                    }
                    EiInputEvent::TextUtf8 { text } => {
                        data.inject_ei_text(connection.eis_connection(), &text);
                    }
                })
            {
                tracing::error!("Failed to insert EI input source: {}", err);
            }
        })
        .expect("Failed to insert EI channel source into the event loop");

    sender
}

impl State {
    /// Act as the input method for text injection while any text-capable EI connection is
    /// active, so `ei_text` UTF-8 can be committed into the focused app even without a real
    /// IME, but only when none is bound (a real IME always wins)
    pub(crate) fn update_ei_input_method(&mut self) {
        let active = !self.common.ei_seats.is_empty();
        let seats = self
            .common
            .shell
            .read()
            .seats
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        for seat in seats {
            let has_ime = seat.input_method().has_instance();
            let text_input = seat.text_input();
            if active {
                if !has_ime {
                    text_input.set_compositor_input_method(true);
                }
            } else {
                text_input.set_compositor_input_method(false);
            }
        }
    }

    /// Inject UTF-8 text (from an EI `ei_text` device) into the focused client.
    pub fn inject_ei_text(&mut self, conn: &smithay::reexports::reis::eis::Connection, text: &str) {
        let seat = self.common.shell.read().seats.last_active().clone();
        // Only commit through text-input when we're the active input method (no real IME)
        if !seat.input_method().has_instance() {
            let text_input = seat.text_input();
            let mut injected = false;
            text_input.with_active_text_input(|ti, _surface| {
                ti.commit_string(Some(text.to_owned()));
                injected = true;
            });
            if injected {
                text_input.done(false);
                return;
            }
        }

        // Bind the whole chunk to spare keycodes in one temporary keymap per batch, so we
        // change (and broadcast) the keymap ~once per chunk instead of once per character
        let keysyms: Vec<Keysym> = text
            .chars()
            .map(Keysym::from_char)
            .filter(|keysym| keysym.raw() != 0)
            .collect();
        // At most ~247 keysyms fit one spare keymap (keycodes 9..=255); leave margin.
        const BATCH: usize = 240;
        for batch in keysyms.chunks(BATCH) {
            self.inject_ei_text_batch(conn, batch);
        }
    }

    /// Inject a batch of keysyms by binding them to consecutive spare keycodes in a single
    /// temporary keymap (one keymap change for the whole batch), injecting each press+release,
    /// then restoring the keymap. Never triggers shortcuts (literal text).
    fn inject_ei_text_batch(
        &mut self,
        conn: &smithay::reexports::reis::eis::Connection,
        keysyms: &[Keysym],
    ) {
        let seat = self.common.shell.read().seats.last_active().clone();
        let backend_id = InputBackendId::Ei(conn.clone());
        let Some(mut iso) = self.common.ei_isolated_kbd.remove(conn) else {
            return;
        };
        let keycodes = iso.remap_keysyms_to_spare_keycodes(keysyms);
        for keycode in keycodes.into_iter().flatten() {
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                KeyState::Pressed,
                false,
            );
            self.inject_isolated_key(
                &backend_id,
                &seat,
                &mut iso,
                keycode,
                KeyState::Released,
                false,
            );
        }
        iso.restore_keymap();
        self.common.ei_isolated_kbd.insert(conn.clone(), iso);
    }
}
