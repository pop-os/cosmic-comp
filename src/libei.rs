use std::os::unix::net::UnixStream;

use reis::eis;
use smithay::reexports::reis;

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
                        if wants_keyboard {
                            let conf = data.common.config.xkb_config();
                            // The ei_keyboard device is given the compositor keymap; its key
                            // events feed the shared seat like any other keyboard.
                            let _ = seat.add_keyboard("virtual keyboard", xkb_config_to_wl(&conf));
                            // The text device lets clients inject keysyms/utf8 directly, delivered
                            seat.add_text("virtual text");
                        }
                        if device_types & DEVICE_TYPE_POINTER != 0 {
                            seat.add_pointer("virtual pointer");
                            let regions = absolute_pointer_regions(data);
                            seat.add_pointer_absolute(ABSOLUTE_POINTER_NAME, &regions);
                        }
                        if device_types & DEVICE_TYPE_TOUCHSCREEN != 0 {
                            seat.add_touch("virtual touch");
                        }
                        // Track the seat, and assign it a shared-seat source so its `ei_keyboard`
                        // key events feed the seat keyboard with independent hold tracking.
                        if wants_keyboard {
                            data.common.ei_keyboard_source.insert(
                                conn.clone(),
                                smithay::input::keyboard::KeyboardSource::new_auxiliary(),
                            );
                            data.common.ei_seats.insert(conn, seat);
                            data.update_ei_input_method();
                        }
                    }
                    EiInputEvent::Disconnected => {
                        let conn = connection.eis_connection().clone();
                        let backend_id = InputBackendId::Ei(conn.clone());
                        // Release any keys/modifiers this remote still holds on the shared seat
                        data.release_ei_keyboard(&conn);
                        data.clear_input_source_state(&backend_id);
                        data.common.ei_seats.remove(&conn);
                        data.common.ei_keyboard_source.remove(&conn);
                        data.update_ei_input_method();
                        // Notify the remaining libei clients of the now-cleared modifier state
                        let seat = data.common.shell.read().seats.last_active().clone();
                        data.broadcast_ei_keyboard_modifiers(&seat);
                    }
                    EiInputEvent::Event(event) => {
                        use smithay::backend::input::{InputEvent, KeyboardKeyEvent};
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
                        data.inject_ei_text_keysym(connection.eis_connection(), keysym, state);
                    }
                    EiInputEvent::TextUtf8 { text } => {
                        data.inject_ei_text(&text);
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
    pub fn inject_ei_text(&mut self, text: &str) {
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

        // Bind the whole chunk to spare keycodes in one temporary keymap per batch (delivered
        // to just the focused client), so we change the keymap ~once per chunk instead of once
        // per character. Leaves the seat's own keyboard state untouched.
        let keysyms: Vec<Keysym> = text
            .chars()
            .map(Keysym::from_char)
            .filter(|keysym| keysym.raw() != 0)
            .collect();
        let Some(keyboard) = seat.get_keyboard() else {
            return;
        };
        // At most ~247 keysyms fit one spare keymap (keycodes 9..=255); leave margin.
        const BATCH: usize = 240;
        for batch in keysyms.chunks(BATCH) {
            keyboard.inject_text_keysyms(self, batch);
        }
    }
}
