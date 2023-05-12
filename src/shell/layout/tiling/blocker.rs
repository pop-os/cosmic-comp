use crate::{
    shell::element::CosmicSurface, state::Data,
    wayland::handlers::compositor::client_compositor_state,
};
use calloop::LoopHandle;
use smithay::{
    reexports::wayland_server::{backend::ClientId, Client, Resource},
    utils::Serial,
    wayland::{
        compositor::{Blocker, BlockerState},
        seat::WaylandFocus,
    },
};
use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

#[derive(Debug, Clone)]
pub struct TilingBlocker {
    pub necessary_acks: Vec<(CosmicSurface, Serial)>,
    start: Instant,
}

impl Blocker for TilingBlocker {
    fn state(&self) -> BlockerState {
        if self.is_ready() {
            BlockerState::Released
        } else {
            BlockerState::Pending
        }
    }
}

impl TilingBlocker {
    pub fn new(configures: impl IntoIterator<Item = (CosmicSurface, Serial)>) -> Self {
        TilingBlocker {
            necessary_acks: configures.into_iter().collect(),
            start: Instant::now(),
        }
    }

    pub fn is_ready(&self) -> bool {
        Instant::now().duration_since(self.start) >= Duration::from_millis(200)
            || self
                .necessary_acks
                .iter()
                .all(|(surf, serial)| surf.serial_acked(serial))
    }

    pub fn signal_ready(&self, handle: &LoopHandle<'static, Data>) {
        let clients = self
            .necessary_acks
            .iter()
            .flat_map(|(surface, _)| surface.wl_surface().and_then(|s| s.client()))
            .map(|client| (client.id(), client))
            .collect::<HashMap<ClientId, Client>>();
        handle.insert_idle(move |data| {
            let dh = data.display.handle();
            for client in clients.values() {
                client_compositor_state(&client).blocker_cleared(&mut data.state, &dh);
            }
        });
    }
}
