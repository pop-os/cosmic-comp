use std::os::unix::net::UnixStream;

use reis::eis;
use smithay::reexports::reis;

use smithay::backend::input::KeyState;
use smithay::backend::libei::{EiInput, EiInputEvent};
use smithay::input::keyboard::Keysym;
use smithay::reexports::calloop;
use smithay::wayland::text_input::TextInputSeat;

use crate::config::xkb_config_to_wl;
use crate::input::InputBackendId;
use crate::state::State;

// Requested device types for an EI connection, mirroring the XDG RemoteDesktop portal `DeviceType` bitmask
const DEVICE_TYPE_KEYBOARD: u32 = 1;
const DEVICE_TYPE_POINTER: u32 = 2;
const DEVICE_TYPE_TOUCHSCREEN: u32 = 4;

pub type EiRequest = (UnixStream, u32);

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
                            seat.add_pointer_absolute("virtual absolute pointer");
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
                        }
                    }
                    EiInputEvent::Disconnected => {
                        data.common.ei_seats.remove(connection.eis_connection());
                        data.common
                            .ei_isolated_kbd
                            .remove(connection.eis_connection());
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
                            }
                        }
                    }
                    EiInputEvent::TextKeysym { keysym, state } => {
                        data.inject_ei_text_keysym(connection.eis_connection(), keysym, state);
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
    /// Inject UTF-8 text (from an EI `ei_text` device) into the focused client.
    pub fn inject_ei_text(&mut self, conn: &smithay::reexports::reis::eis::Connection, text: &str) {
        let seat = self.common.shell.read().seats.last_active().clone();
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

        for c in text.chars() {
            let keysym = Keysym::from_char(c);
            if keysym.raw() != 0 {
                self.inject_ei_text_keysym(conn, keysym.raw(), KeyState::Pressed);
                self.inject_ei_text_keysym(conn, keysym.raw(), KeyState::Released);
            }
        }
    }
}
