use std::os::unix::net::UnixStream;

use reis::eis;
use smithay::reexports::reis;

use smithay::backend::input::KeyState;
use smithay::backend::libei::{EiInput, EiInputEvent};
use smithay::input::keyboard::{Keycode, Keysym, ModifiersState, xkb};
use smithay::reexports::calloop;
use smithay::utils::SERIAL_COUNTER;
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
                        let seat = connection.add_seat("default");
                        let wants_keyboard = device_types & DEVICE_TYPE_KEYBOARD != 0;
                        if wants_keyboard {
                            let conf = data.common.config.xkb_config();
                            let _ = seat.add_keyboard("virtual keyboard", xkb_config_to_wl(&conf));
                            // The text device lets clients inject keysyms/utf8 directly independent of the keymap.
                            seat.add_text("virtual text");
                        }
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
                            data.common
                                .ei_seats
                                .insert(connection.eis_connection().clone(), seat);
                        }
                    }
                    EiInputEvent::Disconnected => {
                        data.common.ei_seats.remove(connection.eis_connection());
                    }
                    EiInputEvent::Event(event) => {
                        let backend_id = InputBackendId::Ei(connection.eis_connection().clone());
                        data.process_input_event(event, backend_id);
                    }
                    EiInputEvent::TextKeysym { keysym, state } => {
                        data.inject_ei_keysym(keysym, state);
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
    /// Inject a single keysym (from an EI `ei_text` device) into the focused client.
    ///
    /// Since Wayland keyboard input is keycode-based, we resolve the keysym to a `(keycode, level)`
    /// in the active layout, derive the modifiers that level needs, advertise them, then forward the
    /// keycode
    pub fn inject_ei_keysym(&mut self, keysym: u32, key_state: KeyState) {
        let seat = self.common.shell.read().seats.last_active().clone();
        let Some(keyboard) = seat.get_keyboard() else {
            return;
        };
        let keysym = Keysym::new(keysym);

        // Resolve the keysym to a keycode + the modifier state its level requires.
        let resolved = keyboard.with_xkb_state(self, |ctx| {
            let xkb_guard = ctx.xkb().lock().unwrap();
            let layout = xkb_guard.active_layout();
            // SAFETY: the keymap is only read within this closure.
            let keymap = unsafe { xkb_guard.keymap() };
            for raw in keymap.min_keycode().raw()..=keymap.max_keycode().raw() {
                let keycode = Keycode::new(raw);
                if keymap.key_get_name(keycode).is_none() {
                    continue;
                }
                for level in 0..keymap.num_levels_for_key(keycode, layout.0) {
                    if keymap
                        .key_get_syms_by_level(keycode, layout.0, level)
                        .contains(&keysym)
                    {
                        let mut masks = [0u32; 1];
                        let count =
                            keymap.key_get_mods_for_level(keycode, layout.0, level, &mut masks);
                        let mods = if count > 0 && masks[0] != 0 {
                            let mut xkb_state = xkb::State::new(keymap);
                            xkb_state.update_mask(masks[0], 0, 0, 0, 0, layout.0);
                            let mut mods = ModifiersState::default();
                            mods.update_with(&xkb_state);
                            mods
                        } else {
                            ModifiersState::default()
                        };
                        return Some((keycode, mods));
                    }
                }
            }
            None
        });

        let Some((keycode, mods)) = resolved else {
            tracing::warn!(
                "EI text keysym {:?} is not in the active layout; ignoring",
                keysym
            );
            return;
        };

        let needs_mods = mods != ModifiersState::default();
        let serial = SERIAL_COUNTER.next_serial();
        let time = self.common.clock.now().as_millis();

        match key_state {
            KeyState::Pressed => {
                if needs_mods {
                    keyboard.set_modifier_state(mods);
                    keyboard.advertise_modifier_state(self);
                }
                keyboard.input_forward(self, keycode, KeyState::Pressed, serial, time, needs_mods);
            }
            KeyState::Released => {
                keyboard.input_forward(self, keycode, KeyState::Released, serial, time, false);
                if needs_mods {
                    keyboard.set_modifier_state(ModifiersState::default());
                    keyboard.advertise_modifier_state(self);
                }
            }
        }
    }

    /// Inject UTF-8 text (from an EI `ei_text` device) into the focused client.
    pub fn inject_ei_text(&mut self, text: &str) {
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
                self.inject_ei_keysym(keysym.raw(), KeyState::Pressed);
                self.inject_ei_keysym(keysym.raw(), KeyState::Released);
            }
        }
    }
}
