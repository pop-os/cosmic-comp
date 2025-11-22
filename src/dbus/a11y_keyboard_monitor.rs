// https://gitlab.gnome.org/GNOME/mutter/-/blob/main/data/dbus-interfaces/org.freedesktop.a11y.xml

use futures_executor::ThreadPool;
use smithay::{
    backend::input::KeyState,
    input::keyboard::{KeysymHandle, ModifiersState},
};
use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex, OnceLock},
};
use tracing::debug;
use xkbcommon::xkb::Keysym;
use zbus::{
    message::Header,
    names::{UniqueName, WellKnownName},
    object_server::SignalEmitter,
};

use super::name_owners::NameOwners;

static ALLOWED_NAMES: &'static [WellKnownName] = &[WellKnownName::from_static_str_unchecked(
    "org.gnome.Orca.KeyboardMonitor",
)];

// As defined in at-spi2-core
const ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START: u32 = 15;

#[derive(PartialEq, Eq, Debug)]
struct KeyGrab {
    pub mods: u32,
    pub virtual_mods: HashSet<Keysym>,
    pub key: Keysym,
}

impl KeyGrab {
    fn new(virtual_mods: &[Keysym], key: Keysym, raw_mods: u32) -> Self {
        let mods = raw_mods & ((1 << ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START) - 1);
        let virtual_mods = virtual_mods
            .iter()
            .copied()
            .enumerate()
            .filter(|(i, _)| {
                raw_mods & (1 << (ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START + *i as u32)) != 0
            })
            .map(|(_, x)| x)
            .collect();
        Self {
            mods,
            virtual_mods,
            key,
        }
    }
}

#[derive(Debug, Default)]
struct Client {
    grabbed: bool,
    watched: bool,
    virtual_mods: HashSet<Keysym>,
    key_grabs: Vec<KeyGrab>,
}

#[derive(Debug, Default)]
struct Clients(HashMap<UniqueName<'static>, Client>);

impl Clients {
    fn get(&mut self, name: &UniqueName<'_>) -> &mut Client {
        self.0.entry(name.to_owned()).or_default()
    }
}

#[derive(Debug)]
pub struct A11yKeyboardMonitorState {
    executor: ThreadPool,
    clients: Arc<Mutex<Clients>>,
    active_virtual_mods: HashSet<Keysym>,
    conn: Arc<OnceLock<zbus::Connection>>,
    name_owners: Arc<OnceLock<NameOwners>>,
}

impl A11yKeyboardMonitorState {
    pub fn new(executor: &ThreadPool) -> Self {
        let clients = Arc::new(Mutex::new(Clients::default()));
        let clients_clone = clients.clone();
        let conn_cell = Arc::new(OnceLock::new());
        let conn_cell_clone = conn_cell.clone();
        let name_owners_cell = Arc::new(OnceLock::new());
        let name_owners_cell_clone = name_owners_cell.clone();
        let executor_clone = executor.clone();
        executor.spawn_ok(async move {
            match serve(clients_clone, &executor_clone).await {
                Ok((conn, name_owners)) => {
                    conn_cell_clone.set(conn).unwrap();
                    name_owners_cell_clone.set(name_owners).unwrap();
                }
                Err(err) => {
                    tracing::error!("Failed to serve `org.freedesktop.a11y.Manager`: {err}");
                }
            }
        });
        Self {
            executor: executor.clone(),
            clients,
            active_virtual_mods: HashSet::new(),
            conn: conn_cell,
            name_owners: name_owners_cell,
        }
    }

    pub fn has_virtual_mod(&self, keysym: Keysym) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .any(|client| client.virtual_mods.contains(&keysym))
    }

    pub fn add_active_virtual_mod(&mut self, keysym: Keysym) {
        self.active_virtual_mods.insert(keysym);
    }

    pub fn remove_active_virtual_mod(&mut self, keysym: Keysym) -> bool {
        self.active_virtual_mods.remove(&keysym)
    }

    pub fn active_virtual_mods(&self) -> &HashSet<Keysym> {
        &self.active_virtual_mods
    }

    pub fn has_keyboard_grab(&self) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .any(|client| client.grabbed)
    }

    /// Key grab exists for mods, key, with active virtual mods
    pub fn has_key_grab(&self, modifiers: &ModifiersState, key: Keysym) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .flat_map(|client| &client.key_grabs)
            .any(|grab| {
                grab.mods == modifiers.serialized.depressed
                    && grab.virtual_mods == self.active_virtual_mods
                    && grab.key == key
            })
    }

    pub fn key_event(&self, modifiers: &ModifiersState, keysym: &KeysymHandle, state: KeyState) {
        let Some(conn) = self.conn.get() else {
            return;
        };

        let clients = self.clients.lock().unwrap();
        for (unique_name, client) in clients.0.iter() {
            if !client.watched && !self.has_key_grab(modifiers, keysym.modified_sym()) {
                continue;
            }

            let mut signal_context =
                SignalEmitter::new(conn, "/org/freedesktop/a11y/Manager").unwrap();
            // Instead of sending signal to all clients, send only to authorized
            // clients with registed watches.
            signal_context = signal_context.set_destination(unique_name.clone().into());

            let released = match state {
                KeyState::Pressed => false,
                KeyState::Released => true,
            };
            let unichar = {
                let xkb = keysym.xkb().lock().unwrap();
                unsafe { xkb.state() }.key_get_utf32(keysym.raw_code())
            };
            let future = KeyboardMonitor::key_event(
                signal_context,
                released,
                modifiers.serialized.depressed,
                keysym.modified_sym().raw(),
                unichar,
                keysym.raw_code().raw() as u16,
            );
            self.executor.spawn_ok(async {
                let _ = future.await;
            });
        }
    }

    pub fn refresh(&mut self) {
        // Remove clients and associated grabs when unique names are no longer
        // present on bus, or no longer hold approved name on bus.
        if let Some(name_owners) = self.name_owners.get() {
            self.clients
                .lock()
                .unwrap()
                .0
                .retain(|k, _| name_owners.check_owner_no_poll(k, ALLOWED_NAMES))
        }
    }
}

struct KeyboardMonitor {
    clients: Arc<Mutex<Clients>>,
    name_owners: NameOwners,
}

impl KeyboardMonitor {
    async fn check_sender_allowed(&self, sender: &UniqueName<'_>) -> zbus::fdo::Result<()> {
        if self.name_owners.check_owner(sender, ALLOWED_NAMES).await {
            Ok(())
        } else {
            Err(zbus::fdo::Error::AccessDenied("Access denied".to_string()))
        }
    }
}

#[zbus::interface(name = "org.freedesktop.a11y.KeyboardMonitor")]
impl KeyboardMonitor {
    async fn grab_keyboard(&mut self, #[zbus(header)] header: Header<'_>) -> zbus::fdo::Result<()> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).grabbed = true;
            debug!("grab keyboard by {}", sender);
        }
        Ok(())
    }

    async fn ungrab_keyboard(
        &mut self,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).grabbed = false;
            debug!("ungrab keyboard by {}", sender);
        }
        Ok(())
    }

    async fn watch_keyboard(
        &mut self,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).watched = true;
            debug!("watch keyboard by {}", sender);
        }
        Ok(())
    }

    async fn unwatch_keyboard(
        &mut self,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).watched = false;
            debug!("unwatch keyboard by {}", sender);
        }
        Ok(())
    }

    async fn set_key_grabs(
        &self,
        #[zbus(header)] header: Header<'_>,
        virtual_mods: Vec<u32>,
        keystrokes: Vec<(u32, u32)>,
    ) -> zbus::fdo::Result<()> {
        let virtual_mods = virtual_mods
            .into_iter()
            .map(Keysym::from)
            .collect::<Vec<_>>();
        let key_grabs = keystrokes
            .into_iter()
            .map(|(k, mods)| KeyGrab::new(&virtual_mods, Keysym::from(k), mods))
            .collect::<Vec<_>>();

        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
            let mut clients = self.clients.lock().unwrap();
            let client = clients.get(sender);
            debug!(
                "key grabs set by {}: {:?}",
                sender,
                (&virtual_mods, &key_grabs)
            );
            client.virtual_mods = virtual_mods.into_iter().collect::<HashSet<_>>();
            client.key_grabs = key_grabs;
        }
        Ok(())
    }

    #[zbus(signal)]
    async fn key_event(
        ctx: SignalEmitter<'_>,
        released: bool,
        state: u32,
        keysym: u32,
        unichar: u32,
        keycode: u16,
    ) -> zbus::Result<()>;
}

async fn serve(
    clients: Arc<Mutex<Clients>>,
    executor: &ThreadPool,
) -> zbus::Result<(zbus::Connection, NameOwners)> {
    let conn = zbus::Connection::session().await?;
    let name_owners = NameOwners::new(&conn, executor).await?;
    let keyboard_monitor = KeyboardMonitor {
        clients,
        name_owners: name_owners.clone(),
    };
    conn.object_server()
        .at("/org/freedesktop/a11y/Manager", keyboard_monitor)
        .await?;
    conn.request_name("org.freedesktop.a11y.Manager").await?;
    Ok((conn, name_owners))
}
