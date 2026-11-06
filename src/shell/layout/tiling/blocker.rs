use crate::shell::element::CosmicSurface;
use smithay::{
    reexports::wayland_server::{
        Client, Resource, backend::ClientId, protocol::wl_surface::WlSurface,
    },
    utils::{IsAlive, Serial},
    wayland::{
        compositor::{Blocker, BlockerState, add_blocker, with_states},
        seat::WaylandFocus,
        shell::xdg::XdgToplevelSurfaceData,
    },
};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex, Weak},
    time::{Duration, Instant},
};

#[derive(Debug, Clone)]
pub struct TilingBlocker(Arc<TilingBlockerInner>);

#[derive(Debug)]
struct TilingBlockerInner {
    necessary_acks: Vec<(CosmicSurface, Serial)>,
    start: Instant,
}

#[derive(Default)]
struct PendingTilingBlockers(Mutex<Vec<(Serial, Weak<TilingBlockerInner>)>>);

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
        TilingBlocker(Arc::new(TilingBlockerInner {
            necessary_acks: configures.into_iter().collect(),
            start: Instant::now(),
        }))
    }

    pub fn add_blockers(&self) {
        for (surface, serial) in &self.0.necessary_acks {
            if let Some(surface) = surface.wl_surface() {
                with_states(&surface, |states| {
                    let pending = states
                        .data_map
                        .get_or_insert_threadsafe(PendingTilingBlockers::default);
                    let mut pending = pending.0.lock().unwrap();
                    pending.retain(|(_, blocker)| blocker.strong_count() > 0);
                    pending.push((*serial, Arc::downgrade(&self.0)));
                });
            }
        }
    }

    pub fn pre_commit(surface: &WlSurface) {
        let blockers = with_states(surface, |states| {
            let Some(pending) = states.data_map.get::<PendingTilingBlockers>() else {
                return Vec::new();
            };
            let last_acked =
                states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .and_then(|attributes| {
                        attributes
                            .lock()
                            .unwrap()
                            .last_acked
                            .as_ref()
                            .map(|configure| configure.serial)
                    });
            let mut blockers = Vec::new();
            pending.0.lock().unwrap().retain(|(serial, blocker)| {
                let Some(blocker) = blocker.upgrade() else {
                    return false;
                };
                // Old-size commits must keep flowing so frame-paced clients can
                // draw and acknowledge the requested size without a timeout.
                if last_acked.is_some_and(|acked| acked >= *serial) {
                    blockers.push(TilingBlocker(blocker));
                    false
                } else {
                    Instant::now().duration_since(blocker.start) < Duration::from_millis(300)
                }
            });
            blockers
        });
        for blocker in blockers {
            add_blocker(surface, blocker);
        }
    }

    pub fn is_ready(&self) -> bool {
        Instant::now().duration_since(self.0.start) >= Duration::from_millis(300)
            || self
                .0
                .necessary_acks
                .iter()
                .all(|(surf, serial)| !surf.alive() || surf.serial_acked(serial))
    }

    pub fn is_processed(&self) -> bool {
        Instant::now().duration_since(self.0.start) >= Duration::from_millis(500)
            || self
                .0
                .necessary_acks
                .iter()
                .all(|(surf, serial)| !surf.alive() || surf.serial_past(serial))
    }

    pub fn clients(&self) -> HashMap<ClientId, Client> {
        self.0
            .necessary_acks
            .iter()
            .flat_map(|(surface, _)| surface.wl_surface().and_then(|s| s.client()))
            .map(|client| (client.id(), client))
            .collect::<HashMap<ClientId, Client>>()
    }
}
