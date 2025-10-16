use crate::shell::element::CosmicSurface;
use smithay::{
    reexports::wayland_server::{Client, Resource, backend::ClientId},
    utils::{IsAlive, Serial},
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
        Instant::now().duration_since(self.start) >= Duration::from_millis(300)
            || self
                .necessary_acks
                .iter()
                .all(|(surf, serial)| !surf.alive() || surf.serial_acked(serial))
    }

    pub fn is_processed(&self) -> bool {
        Instant::now().duration_since(self.start) >= Duration::from_millis(500)
            || self
                .necessary_acks
                .iter()
                .all(|(surf, serial)| !surf.alive() || surf.serial_past(serial))
    }

    pub fn clients(&self) -> HashMap<ClientId, Client> {
        self.necessary_acks
            .iter()
            .flat_map(|(surface, _)| surface.wl_surface().and_then(|s| s.client()))
            .map(|client| (client.id(), client))
            .collect::<HashMap<ClientId, Client>>()
    }
}
