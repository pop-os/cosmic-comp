use crate::shell::element::CosmicSurface;
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
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

#[derive(Debug, Clone)]
pub struct TilingBlocker {
    pub necessary_acks: Vec<(CosmicSurface, Serial)>,
    ready: Arc<AtomicBool>,
    signaled: Arc<AtomicBool>,
    start: Instant,
}

impl Blocker for TilingBlocker {
    fn state(&self) -> BlockerState {
        self.signaled.store(true, Ordering::SeqCst);
        if self.ready.load(Ordering::SeqCst) {
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
            ready: Arc::new(AtomicBool::new(false)),
            signaled: Arc::new(AtomicBool::new(false)),
            start: Instant::now(),
        }
    }

    pub fn is_ready(&self) -> bool {
        Instant::now().duration_since(self.start) >= Duration::from_millis(300)
            || self
                .necessary_acks
                .iter()
                .all(|(surf, serial)| surf.serial_acked(serial))
    }

    pub fn is_signaled(&self) -> bool {
        self.signaled.load(Ordering::SeqCst)
    }

    #[must_use]
    pub fn signal_ready(&self) -> HashMap<ClientId, Client> {
        self.ready.swap(true, Ordering::SeqCst);
        self.necessary_acks
            .iter()
            .flat_map(|(surface, _)| surface.wl_surface().and_then(|s| s.client()))
            .map(|client| (client.id(), client))
            .collect::<HashMap<ClientId, Client>>()
    }
}
