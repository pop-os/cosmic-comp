// SPDX-License-Identifier: GPL-3.0-only

use cosmic_comp_config::XkbConfig;
use cosmic_protocols::atspi::v1::server::cosmic_atspi_manager_v1::CosmicAtspiManagerV1;
use reis::{
    calloop::{EisRequestSource, EisRequestSourceEvent},
    eis::{self, device::DeviceType},
    request::{Connection, Device, DeviceCapability, EisRequest, Seat},
};
use smithay::{
    backend::input::{KeyState, Keycode},
    input::keyboard::ModifiersState,
    utils::SealedFile,
};
use std::{
    collections::{HashMap, HashSet},
    ffi::{CStr, CString},
    mem,
    os::unix::{io::AsFd, net::UnixStream},
};
use xkbcommon::xkb;

use crate::{
    state::State,
    wayland::protocols::atspi::{delegate_atspi, AtspiHandler},
};

#[derive(PartialEq, Debug)]
pub struct AtspiKeyGrab {
    pub mods: u32,
    pub virtual_mods: HashSet<Keycode>,
    pub key: Keycode,
}

#[derive(Debug, Default)]
struct AtspiClient {
    key_grabs: Vec<AtspiKeyGrab>,
    has_keyboard_grab: bool,
    // TODO: purge old instances
    keyboards: Vec<(Connection, Device, eis::Keyboard)>,
}

impl AtspiClient {
    fn add_keyboard(
        &mut self,
        connection: &Connection,
        seat: &Seat,
        keymap: &xkb::Keymap,
        modifiers: &ModifiersState,
    ) {
        let keymap_text = keymap.get_as_string(xkb::KEYMAP_FORMAT_TEXT_V1);
        let name = CStr::from_bytes_with_nul(b"eis-keymap\0").unwrap();
        let file = SealedFile::with_content(name, &CString::new(keymap_text).unwrap()).unwrap();

        let device = seat.add_device(
            Some("keyboard"),
            DeviceType::Virtual,
            &[DeviceCapability::Keyboard],
            |device| {
                let keyboard = device.interface::<eis::Keyboard>().unwrap();
                keyboard.keymap(
                    eis::keyboard::KeymapType::Xkb,
                    file.size() as u32 - 1,
                    file.as_fd(),
                );
            },
        );
        device.resumed();

        let keyboard = device.interface::<eis::Keyboard>().unwrap();

        connection.with_next_serial(|serial| {
            keyboard.modifiers(
                serial,
                modifiers.serialized.depressed,
                modifiers.serialized.locked,
                modifiers.serialized.latched,
                modifiers.serialized.layout_effective,
            )
        });

        device.start_emulating(0);

        self.keyboards.push((connection.clone(), device, keyboard));
    }
}

#[derive(Debug, Default)]
pub struct AtspiEiState {
    modifiers: ModifiersState,
    clients: HashMap<CosmicAtspiManagerV1, AtspiClient>,
    pub virtual_mods: HashSet<Keycode>,
    pub active_virtual_mods: HashSet<Keycode>,
}

impl AtspiEiState {
    pub fn input(
        &mut self,
        modifiers: &smithay::input::keyboard::ModifiersState,
        keysym: &smithay::input::keyboard::KeysymHandle,
        state: KeyState,
        time: u64,
    ) {
        let state = match state {
            KeyState::Pressed => eis::keyboard::KeyState::Press,
            KeyState::Released => eis::keyboard::KeyState::Released,
        };
        if &self.modifiers != modifiers {
            self.modifiers = *modifiers;
            for client in self.clients.values() {
                for (connection, _, keyboard) in &client.keyboards {
                    connection.with_next_serial(|serial| {
                        keyboard.modifiers(
                            serial,
                            modifiers.serialized.depressed,
                            modifiers.serialized.locked,
                            modifiers.serialized.latched,
                            modifiers.serialized.layout_effective,
                        )
                    });
                }
            }
        }
        for client in self.clients.values() {
            for (connection, device, keyboard) in &client.keyboards {
                keyboard.key(keysym.raw_code().raw() - 8, state);
                device.frame(time);
                let _ = connection.flush();
            }
        }
    }

    pub fn has_keyboard_grab(&self) -> bool {
        self.clients.values().any(|client| client.has_keyboard_grab)
    }

    /// Key grab exists for mods, key, with active virtual mods
    pub fn has_key_grab(&self, mods: u32, key: Keycode) -> bool {
        self.clients
            .values()
            .flat_map(|client| &client.key_grabs)
            .any(|grab| {
                grab.mods == mods
                    && grab.virtual_mods == self.active_virtual_mods
                    && grab.key == key
            })
    }

    fn update_virtual_mods(&mut self) {
        self.virtual_mods.clear();
        self.virtual_mods.extend(
            self.clients
                .values()
                .flat_map(|client| &client.key_grabs)
                .flat_map(|grab| &grab.virtual_mods),
        );
    }

    pub fn update_keymap(&mut self, xkb_config: XkbConfig) {
        let keymap = keymap_or_default(xkb_config);
        for client in self.clients.values_mut() {
            let old_keyboards = mem::take(&mut client.keyboards);
            for (connection, device, _keyboard) in old_keyboards {
                device.remove();
                client.add_keyboard(&connection, device.seat(), &keymap, &self.modifiers);
                let _ = connection.flush();
            }
        }
    }
}

impl AtspiHandler for State {
    fn client_connected(&mut self, manager: &CosmicAtspiManagerV1, socket: UnixStream) {
        self.common
            .atspi_ei
            .clients
            .insert(manager.clone(), AtspiClient::default());

        let context = eis::Context::new(socket).unwrap();
        let source = EisRequestSource::new(context, 0);
        let manager = manager.clone();
        self.common
            .event_loop_handle
            .insert_source(source, move |event, connected_state, state| {
                Ok(handle_event(&manager, event, connected_state, state))
            })
            .unwrap();
    }

    fn client_disconnected(&mut self, manager: &CosmicAtspiManagerV1) {
        self.common.atspi_ei.clients.remove(manager);
        self.common.atspi_ei.update_virtual_mods();
    }

    fn add_key_grab(
        &mut self,
        manager: &CosmicAtspiManagerV1,
        mods: u32,
        virtual_mods: Vec<Keycode>,
        key: Keycode,
    ) {
        let grab = AtspiKeyGrab {
            mods,
            virtual_mods: virtual_mods.into_iter().collect(),
            key,
        };
        let client = self.common.atspi_ei.clients.get_mut(manager).unwrap();
        client.key_grabs.push(grab);
        self.common.atspi_ei.update_virtual_mods();
    }

    fn remove_key_grab(
        &mut self,
        manager: &CosmicAtspiManagerV1,
        mods: u32,
        virtual_mods: Vec<Keycode>,
        key: Keycode,
    ) {
        let grab = AtspiKeyGrab {
            mods,
            virtual_mods: virtual_mods.into_iter().collect(),
            key,
        };
        let client = self.common.atspi_ei.clients.get_mut(manager).unwrap();
        if let Some(idx) = client.key_grabs.iter().position(|x| *x == grab) {
            client.key_grabs.remove(idx);
        }
        self.common.atspi_ei.update_virtual_mods();
    }

    fn grab_keyboard(&mut self, manager: &CosmicAtspiManagerV1) {
        let client = self.common.atspi_ei.clients.get_mut(manager).unwrap();
        client.has_keyboard_grab = true;
    }

    fn ungrab_keyboard(&mut self, manager: &CosmicAtspiManagerV1) {
        let client = self.common.atspi_ei.clients.get_mut(manager).unwrap();
        client.has_keyboard_grab = false;
    }
}

fn handle_event(
    manager: &CosmicAtspiManagerV1,
    event: Result<EisRequestSourceEvent, reis::Error>,
    connection: &Connection,
    state: &mut State,
) -> calloop::PostAction {
    let Some(client) = state.common.atspi_ei.clients.get_mut(manager) else {
        return calloop::PostAction::Remove;
    };
    match event {
        Ok(EisRequestSourceEvent::Connected) => {
            if connection.context_type() != reis::ei::handshake::ContextType::Receiver {
                return calloop::PostAction::Remove;
            }
            let _seat = connection.add_seat(Some("default"), &[DeviceCapability::Keyboard]);
        }
        Ok(EisRequestSourceEvent::Request(EisRequest::Disconnect)) => {
            return calloop::PostAction::Remove;
        }
        Ok(EisRequestSourceEvent::Request(EisRequest::Bind(request))) => {
            if connection.has_interface("ei_keyboard")
                && request.capabilities & 2 << DeviceCapability::Keyboard as u64 != 0
            {
                let keymap = keymap_or_default(state.common.config.xkb_config());
                client.add_keyboard(
                    connection,
                    &request.seat,
                    &keymap,
                    &state.common.atspi_ei.modifiers,
                );
            }
        }
        Ok(EisRequestSourceEvent::Request(_request)) => {
            // seat / keyboard / device release?
        }
        Ok(EisRequestSourceEvent::InvalidObject(_)) => {}
        Err(_) => {
            // TODO
        }
    }
    let _ = connection.flush();
    calloop::PostAction::Continue
}

// TODO: use keymap of seat?
fn keymap_or_default(xkb_config: XkbConfig) -> xkb::Keymap {
    keymap(xkb_config).unwrap_or_else(|| keymap(XkbConfig::default()).unwrap())
}

fn keymap(xkb_config: XkbConfig) -> Option<xkb::Keymap> {
    let context = xkb::Context::new(xkb::CONTEXT_NO_FLAGS);
    xkb::Keymap::new_from_names(
        &context,
        &xkb_config.rules,
        &xkb_config.model,
        &xkb_config.layout,
        &xkb_config.variant,
        xkb_config.options.clone(),
        xkb::KEYMAP_COMPILE_NO_FLAGS,
    )
}

delegate_atspi!(State);
