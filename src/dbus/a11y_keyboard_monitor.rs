// https://gitlab.gnome.org/GNOME/mutter/-/blob/main/data/dbus-interfaces/org.freedesktop.a11y.xml
//
// TODO: Restrict protocol acccess?

use futures_executor::ThreadPool;
use smithay::backend::input::KeyState;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use std::{error::Error, future::pending};
use xkbcommon::xkb::{self, Keysym};
use zbus::message::Header;
use zbus::names::UniqueName;
use zbus::SignalContext;
use std::sync::OnceLock;

// As defined in at-spi2-core
const ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START: u32 = 15;

#[derive(Debug, Default)]
struct Client {
    grabbed: bool,
    watched: bool,
    virtual_modifiers: Vec<Keysym>,
    keystrokes: Vec<(Keysym, Modifiers)>,
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
    active_virtual_modifiers: HashSet<Keysym>,
    conn: Arc<OnceLock<zbus::Connection>>,
}

impl A11yKeyboardMonitorState {
    pub fn new() -> Self {
        let clients = Arc::new(Mutex::new(Clients::default()));
        let clients_clone = clients.clone();
        let executor = ThreadPool::builder().pool_size(1).create().unwrap();
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
            clients,
            executor,
            active_virtual_modifiers: HashSet::new(),
            conn: conn_cell,
        }
    }

    pub fn has_virtual_modifier(&self, keysym: Keysym) -> bool {
        self.clients
            .lock()
            .unwrap()
            .0
            .values()
            .any(|client| client.virtual_modifiers.contains(&keysym))
    }

    pub fn key_event(
        &self,
        modifiers: &smithay::input::keyboard::ModifiersState,
        keysym: &smithay::input::keyboard::KeysymHandle,
        state: smithay::backend::input::KeyState,
        unichar: char,
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

        let signal_context = SignalContext::new(conn, "/org/freedesktop/a11y/Manager").unwrap();
        let released = match state {
            KeyState::Pressed => false,
            KeyState::Released => true,
        };
        // XXX keysym and keycode?
        // XXX need to add virtual modifiers?
        let future = KeyboardMonitor::key_event(
            signal_context,
            released,
            modifiers.serialized.depressed,
            keysym.modified_sym().raw(),
            unichar as u32,
            keysym.raw_code().raw() as u16,
        );
        self.executor.spawn_ok(async {
            future.await;
        });
    }
}

trait A11yKeyboardMonitorHandler {}

struct KeyboardMonitor {
    clients: Arc<Mutex<Clients>>,
    //mod_names: Vec<String>,
}

impl KeyboardMonitor {
    fn map_mods(&self, virtual_mods: &[Keysym], mods: u32) -> Vec<&str> {
        /*
        // TODO warn unrecognized modifier
        self.mod_names
            .iter()
            .map(|name| name.as_str())
            .enumerate()
            .filter(|(i, _)| {
                (*i as u32) < ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START && (mods & (1 << i) != 0)
            })
            .map(|(_, name)| name)
            // XXX unwrap
            .chain(
                virtual_mods
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| {
                        mods & (1 << (*i as u32 + ATSPI_DEVICE_A11Y_MANAGER_VIRTUAL_MOD_START)) != 0
                    })
                    .map(|(_, keysym)| keysym.name().unwrap()),
            )
            .collect()
        */
        Vec::new()
    }
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

    fn set_key_grabs(&self, virtual_modifiers: Vec<u32>, keystrokes: Vec<(u32, u32)>) {
        let virtual_modifiers = virtual_modifiers
            .into_iter()
            .map(Keysym::from)
            .collect::<Vec<_>>();
        let keystrokes = keystrokes
            .into_iter()
            .map(|(k, mods)| (Keysym::from(k), self.map_mods(&virtual_modifiers, mods)))
            .collect::<Vec<_>>();
        dbg!(virtual_modifiers, keystrokes);
    }

    // TODO signal
    #[zbus(signal)]
    async fn key_event(
        ctx: SignalContext<'_>,
        released: bool,
        state: u32,
        keysym: u32,
        unichar: u32,
        keycode: u16,
    ) -> zbus::Result<()>;
}

#[derive(Debug)]
struct Modifiers(u32);

async fn serve(
    clients: Arc<Mutex<Clients>>,
    ) -> zbus::Result<zbus::Connection> {
    let keyboard_monitor = KeyboardMonitor {clients};
    zbus::connection::Builder::session()?
        .name("org.freedesktop.a11y.Manager")?
        .serve_at("/org/freedesktop/a11y/Manager", keyboard_monitor)?
        .build()
        .await
}
