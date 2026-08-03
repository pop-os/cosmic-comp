// SPDX-License-Identifier: GPL-3.0-only

//! Bridges the KDE blur protocol onto the background-effect state.
//!
//! `ext_background_effect_v1` is upstream's staging protocol and carries only a
//! region -- no strength, no corner rounding, no frosted-glass appearance. Our
//! clients need all three, so they speak `org_kde_kwin_blur`, which we own and
//! which carries them. Rather than keep a second blur implementation alive for
//! it, translate its state into the same per-surface state the background-effect
//! handler writes, so both protocols end up driving one renderer.
//!
//! A surface that uses both wins nothing and loses nothing: the two write the
//! same fields, and the later commit is the one that takes effect.

use crate::state::State;
use crate::wayland::handlers::background_effect::ComputedBlurRegionCachedState;
use crate::wayland::protocols::blur::{BlurHandler, BlurState, CacheableBlurState, delegate_blur};
use smithay::reexports::wayland_server::Resource;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::wayland::compositor::with_states;

impl BlurHandler for State {
    fn blur_state(&mut self) -> &mut BlurState {
        &mut self.common.blur_state
    }

    fn blur_set(&mut self, surface: &WlSurface) {
        with_states(surface, |states| {
            // `pending`, not `current`: org_kde_kwin_blur.commit writes the new
            // state into the pending slot and calls straight into here, so
            // `current` still holds the previous surface commit -- every update
            // would land a cycle late, and the first would read the default
            // (no region, disabled) and blur nothing at all.
            let blur = states
                .cached_state
                .get::<CacheableBlurState>()
                .pending()
                .clone();

            let mut computed = states.cached_state.get::<ComputedBlurRegionCachedState>();
            let pending = computed.pending();

            match blur.data.as_ref().and_then(|data| data.region.clone()) {
                Some(region) => {
                    pending.blur_region = Some(region);
                    pending.whole_surface = false;
                }
                // KDE blur treats "no region" as the whole surface, which is
                // exactly what blur_whole_surface means here.
                None => {
                    pending.blur_region = None;
                    pending.whole_surface = true;
                }
            }

            // Strength and corner rounding carry across where the client sent
            // them; both are hints the renderer clamps.
            pending.blur_radius = blur
                .radius
                .or_else(|| blur.data.as_ref().and_then(|data| data.radius))
                .map(|r: f32| r.max(0.0) as u32)
                .filter(|r| *r != 0);
            // Per-rect radii carry across intact: the client rounds each card
            // to its own shape, and collapsing them to one would square the
            // rest.
            pending.region_radii = blur
                .data
                .as_ref()
                .and_then(|data| data.region_radii.as_ref())
                .map(|radii| {
                    radii
                        .iter()
                        .map(|r| r.map(|v: f32| v.max(0.0) as u32))
                        .collect()
                })
                .unwrap_or_default();

            // The exact sub-pixel area of each rect, where the client sent it.
            // Index-matched like the radii, and clamped to its own region rect:
            // the integer rect is what the capture and damage were sized for, so
            // a geometry reaching past it would blur pixels nothing captured.
            pending.region_geometry = blur
                .data
                .as_ref()
                .and_then(|data| data.region_geometry.as_ref())
                .map(|geometry| {
                    geometry
                        .iter()
                        .enumerate()
                        .map(|(idx, geo)| {
                            pending
                                .blur_region
                                .as_ref()
                                .and_then(|region| region.get(idx))
                                .and_then(|rect| geo.intersection(rect.to_f64()))
                                .unwrap_or(*geo)
                        })
                        .collect()
                })
                .unwrap_or_default();

            // Saturation, tint and border, which the KDE protocol carries from
            // its version 3. These were parsed into `CacheableBlurState` but
            // never reached the renderer, so a kwin client asking for them got
            // the compositor default silently.
            //
            // Each is `None` until the client sends it, which the renderer reads
            // as "use the default" -- 0 is a meaningful value for all three
            // (greyscale / no tint / no border) and cannot double as "unset".
            pending.saturation = blur
                .saturation
                .or_else(|| blur.data.as_ref().and_then(|data| data.saturation));
            pending.tint = blur
                .tint
                .or_else(|| blur.data.as_ref().and_then(|data| data.tint));
            pending.border = blur
                .border
                .or_else(|| blur.data.as_ref().and_then(|data| data.border));
        });

        let shell = self.common.shell.read();
        let output = shell.visible_output_for_surface(surface).cloned();
        std::mem::drop(shell);

        let surface_id = surface.id();
        tracing::trace!(
            surface_protocol_id = surface.id().protocol_id(),
            "blur_set: surface committed new blur, calling restart_layer_fade_in"
        );
        self.common.shell.write().restart_layer_fade_in(surface_id);

        if let Some(output) = output {
            self.backend.schedule_render(&output);
        }
    }

    fn blur_unset(&mut self, surface: &WlSurface) {
        with_states(surface, |states| {
            let mut computed = states.cached_state.get::<ComputedBlurRegionCachedState>();
            let pending = computed.pending();
            pending.blur_region = None;
            pending.whole_surface = false;
        });

        tracing::debug!(
            surface_protocol_id = surface.id().protocol_id(),
            "blur_unset: blur removed from surface, scheduling render"
        );
        if let Some(output) = self.common.shell.read().visible_output_for_surface(surface) {
            self.backend.schedule_render(output);
        }
    }
}

delegate_blur!(State);
