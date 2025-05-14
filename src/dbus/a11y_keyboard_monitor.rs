// https://gitlab.gnome.org/GNOME/mutter/-/blob/main/data/dbus-interfaces/org.freedesktop.a11y.xml
//
// TODO: Restrict protocol acccess?
// TODO remove client when not connected to server

use futures_executor::ThreadPool;
use smithay::backend::input::KeyState;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::OnceLock;
use std::sync::{Arc, Mutex};
use xkbcommon::xkb::{self, Keysym};
use zbus::message::Header;
use zbus::names::UniqueName;
use zbus::object_server::SignalEmitter;

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

    fn remove(&mut self, name: &UniqueName<'_>) -> bool {
        self.0.remove(&name.to_owned()).is_some()
    }
}

#[derive(Debug)]
pub struct A11yKeyboardMonitorState {
    executor: ThreadPool,
    clients: Arc<Mutex<Clients>>,
    active_virtual_mods: HashSet<Keysym>,
    conn: Arc<OnceLock<zbus::Connection>>,
}

impl A11yKeyboardMonitorState {
    pub fn new(executor: &ThreadPool) -> Self {
        let clients = Arc::new(Mutex::new(Clients::default()));
        let clients_clone = clients.clone();
        let conn_cell = Arc::new(OnceLock::new());
        let conn_cell_clone = conn_cell.clone();
        executor.spawn_ok(async move {
            match serve(clients_clone).await {
                Ok(conn) => {
                    conn_cell_clone.set(conn).unwrap();
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

    pub fn has_keyboard_grab(&self) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .any(|client| client.grabbed)
    }

    /// Key grab exists for mods, key, with active virtual mods
    pub fn has_key_grab(&self, mods: u32, key: Keysym) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .flat_map(|client| &client.key_grabs)
            .any(|grab| {
                grab.mods == mods
                    && grab.virtual_mods == self.active_virtual_mods
                    && grab.key == key
            })
    }

    pub fn key_event(
        &self,
        modifiers: &smithay::input::keyboard::ModifiersState,
        keysym: &smithay::input::keyboard::KeysymHandle,
        state: smithay::backend::input::KeyState,
    ) {
        let Some(conn) = self.conn.get() else {
            return;
        };

        // Test if any client is watching key input
        if !self
            .clients
            .lock()
            .unwrap()
            .0
            .values()
            .any(|client| client.watched)
        {
            return;
        }

        let signal_context = SignalEmitter::new(conn, "/org/freedesktop/a11y/Manager").unwrap();
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
            modifiers.serialized.layout_effective,
            keysym.modified_sym().raw(),
            unichar,
            keysym.raw_code().raw() as u16,
        );
        self.executor.spawn_ok(async {
            future.await;
        });
    }
}

struct KeyboardMonitor {
    clients: Arc<Mutex<Clients>>,
}

#[zbus::interface(name = "org.freedesktop.a11y.KeyboardMonitor")]
impl KeyboardMonitor {
    fn grab_keyboard(&mut self, #[zbus(header)] header: Header<'_>) {
        if let Some(sender) = header.sender() {
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).grabbed = true;
            eprintln!("grab keyboard by {}", sender);
        }
    }

    fn ungrab_keyboard(&mut self, #[zbus(header)] header: Header<'_>) {
        if let Some(sender) = header.sender() {
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).grabbed = false;
            eprintln!("ungrab keyboard by {}", sender);
        }
    }

    fn watch_keyboard(&mut self, #[zbus(header)] header: Header<'_>) {
        if let Some(sender) = header.sender() {
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).watched = true;
            eprintln!("watch keyboard by {}", sender);
        }
    }

    fn unwatch_keyboard(&mut self, #[zbus(header)] header: Header<'_>) {
        if let Some(sender) = header.sender() {
            let mut clients = self.clients.lock().unwrap();
            clients.get(sender).watched = false;
            eprintln!("unwatch keyboard by {}", sender);
        }
    }

    fn set_key_grabs(
        &self,
        #[zbus(header)] header: Header<'_>,
        virtual_mods: Vec<u32>,
        keystrokes: Vec<(u32, u32)>,
    ) {
        let virtual_mods = virtual_mods
            .into_iter()
            .map(Keysym::from)
            .collect::<Vec<_>>();
        let key_grabs = keystrokes
            .into_iter()
            .map(|(k, mods)| KeyGrab::new(&virtual_mods, Keysym::from(k), mods))
            .collect::<Vec<_>>();

        if let Some(sender) = header.sender() {
            let mut clients = self.clients.lock().unwrap();
            let client = clients.get(sender);
            eprintln!(
                "key grabs set by {}: {:?}",
                sender,
                (&virtual_mods, &key_grabs)
            );
            client.virtual_mods = virtual_mods.into_iter().collect::<HashSet<_>>();
            client.key_grabs = key_grabs;
        }
    }

    // TODO signal
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

async fn serve(clients: Arc<Mutex<Clients>>) -> zbus::Result<zbus::Connection> {
    let keyboard_monitor = KeyboardMonitor { clients };
    zbus::connection::Builder::session()?
        .name("org.freedesktop.a11y.Manager")?
        .serve_at("/org/freedesktop/a11y/Manager", keyboard_monitor)?
        .build()
        .await
}
