// SPDX-License-Identifier: GPL-3.0-only

//! Answers adaptive foreground zone changes with a backdrop reading.
//!
//! The sampling itself lives in `backend::render::adaptive_foreground`; this
//! decides *when* to run it. Readings are taken on the main loop as an idle
//! callback rather than inline, both because the sample needs a renderer this
//! call does not hold and because a client's `set_region` should not block on a
//! GPU readback.

use crate::backend::render::adaptive_foreground::sample_and_send;
use crate::state::State;
use crate::wayland::protocols::adaptive_foreground::{
    AdaptiveForegroundHandler, AdaptiveForegroundState, delegate_adaptive_foreground,
};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;

impl AdaptiveForegroundHandler for State {
    fn adaptive_foreground_state(&mut self) -> &mut AdaptiveForegroundState {
        &mut self.common.adaptive_foreground_state
    }

    fn adaptive_foreground_zones_changed(&mut self, surface: &WlSurface) {
        self.schedule_adaptive_foreground_sample(surface.clone());
    }
}

impl State {
    /// Queue a backdrop reading for `surface` on the next idle turn.
    pub fn schedule_adaptive_foreground_sample(&mut self, surface: WlSurface) {
        self.common
            .event_loop_handle
            .insert_idle(move |state| sample_and_send(state, &surface));
    }

    /// Re-read every observed surface whose backdrop `changed_surface` is part of.
    ///
    /// Called when a wallpaper commits: the reading is only as fresh as the
    /// content it was taken from, and a wallpaper change is the one event that
    /// invalidates it without anything on the client side moving.
    pub fn adaptive_foreground_backdrop_changed(&mut self) {
        let observed = self.common.adaptive_foreground_state.observed_surfaces();
        for surface in observed {
            self.schedule_adaptive_foreground_sample(surface);
        }
    }
}

delegate_adaptive_foreground!(State);
