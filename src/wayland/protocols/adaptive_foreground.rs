// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the adaptive foreground protocol (adaptive_foreground_v1)
//!
//! Reports the relative luminance of what the compositor composited beneath
//! marked zones of a client's surface, so the client can pick a foreground
//! colour that stays legible there. The motivating case is text drawn straight
//! onto the wallpaper -- a desktop greeting -- which disappears when the
//! wallpaper's lightness happens to match the colour the active colour scheme
//! chose.
//!
//! The compositor reports and the client restyles; the compositor never touches
//! client pixels. That split is what lets the client move text, icons and
//! dividers together, which recolouring already-rasterised pixels could not do,
//! and it needs no agreement about how those pixels were rendered.
//!
//! Zone state is *not* double-buffered, which is the one place this departs from
//! `blur`. A blur region has to land in the same frame as the card it sits
//! behind or the backdrop visibly lags the card; a zone only says where to
//! sample, and a reading one frame stale is not observable.

pub use generated::{adaptive_foreground_manager_v1, adaptive_foreground_surface_v1};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/adaptive-foreground.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/adaptive-foreground.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::{ClientId, GlobalId},
        protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Rectangle},
};
use std::sync::Mutex;

/// Below this delta a reading is not worth an event, let alone a client reflow.
/// One 256th is the protocol's own quantum, so anything under it is noise the
/// wire could not carry anyway.
const LUMINANCE_EPSILON: f32 = 2.0 / 256.0;

/// Encode a 0..=1 luminance as the protocol's 8.8 fixed point.
fn to_fixed(luminance: f32) -> u32 {
    (luminance.clamp(0.0, 1.0) * 256.0).round() as u32
}

/// State for the adaptive foreground manager protocol.
#[derive(Debug)]
pub struct AdaptiveForegroundState {
    global: GlobalId,
    /// Every live surface object, so a reading computed elsewhere (the sampling
    /// pass) can find the object to send it on. Entries are removed in
    /// `destroyed`, so a dead client cannot accumulate here.
    surfaces: Vec<adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1>,
}

impl AdaptiveForegroundState {
    pub fn new<D>(dh: &DisplayHandle) -> AdaptiveForegroundState
    where
        D: GlobalDispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
            + Dispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
            + Dispatch<
                adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
                AdaptiveForegroundData,
            > + AdaptiveForegroundHandler
            + 'static,
    {
        let global = dh
            .create_global::<D, adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, _>(
                1,
                (),
            );
        AdaptiveForegroundState {
            global,
            surfaces: Vec::new(),
        }
    }

    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }

    /// The zones currently set on `surface`, in the order the client added them.
    pub fn zones(&self, surface: &WlSurface) -> Vec<Rectangle<i32, Logical>> {
        self.object_for(surface)
            .and_then(|obj| obj.data::<AdaptiveForegroundData>())
            .map(|data| data.lock().unwrap().zones.clone())
            .unwrap_or_default()
    }

    /// Every surface that currently has at least one zone. This is the work list
    /// for the sampling pass.
    pub fn observed_surfaces(&self) -> Vec<WlSurface> {
        self.surfaces
            .iter()
            .filter_map(|obj| {
                let data = obj.data::<AdaptiveForegroundData>()?;
                let guard = data.lock().unwrap();
                (!guard.zones.is_empty())
                    .then(|| guard.surface.upgrade().ok())
                    .flatten()
            })
            .collect()
    }

    /// Send readings for `surface`, one per zone index.
    ///
    /// Only zones whose reading moved past [`LUMINANCE_EPSILON`] are sent, and
    /// `done` follows only if at least one did -- a client that reflows on `done`
    /// would otherwise do it on every sample of an unchanged wallpaper.
    pub fn send_luminance(&mut self, surface: &WlSurface, readings: &[f32]) {
        let Some(obj) = self.object_for(surface).cloned() else {
            return;
        };
        let Some(data) = obj.data::<AdaptiveForegroundData>() else {
            return;
        };

        let mut guard = data.lock().unwrap();
        guard.last_sent.resize(readings.len(), None);

        let mut sent_any = false;
        for (index, &luminance) in readings.iter().enumerate() {
            let changed = guard.last_sent[index]
                .is_none_or(|previous| (previous - luminance).abs() >= LUMINANCE_EPSILON);
            if !changed {
                continue;
            }
            guard.last_sent[index] = Some(luminance);
            obj.luminance(index as u32, to_fixed(luminance));
            sent_any = true;
        }

        if sent_any {
            obj.done();
        }
    }

    fn object_for(
        &self,
        surface: &WlSurface,
    ) -> Option<&adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1> {
        self.surfaces.iter().find(|obj| {
            obj.data::<AdaptiveForegroundData>().is_some_and(|data| {
                data.lock()
                    .unwrap()
                    .surface
                    .upgrade()
                    .is_ok_and(|s| &s == surface)
            })
        })
    }
}

/// Handler trait for adaptive foreground.
pub trait AdaptiveForegroundHandler {
    fn adaptive_foreground_state(&mut self) -> &mut AdaptiveForegroundState;

    /// The zones on `surface` changed. The compositor should take a fresh
    /// reading and answer with [`AdaptiveForegroundState::send_luminance`].
    fn adaptive_foreground_zones_changed(&mut self, surface: &WlSurface);
}

impl<D> GlobalDispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, (), D>
    for AdaptiveForegroundState
where
    D: GlobalDispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<
            adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
            AdaptiveForegroundData,
        > + AdaptiveForegroundHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, (), D>
    for AdaptiveForegroundState
where
    D: GlobalDispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<
            adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
            AdaptiveForegroundData,
        > + AdaptiveForegroundHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        resource: &adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1,
        request: adaptive_foreground_manager_v1::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            adaptive_foreground_manager_v1::Request::Destroy => {}
            adaptive_foreground_manager_v1::Request::GetAdaptiveForeground { id, surface } => {
                let already = state
                    .adaptive_foreground_state()
                    .object_for(&surface)
                    .is_some();
                if already {
                    resource.post_error(
                        adaptive_foreground_manager_v1::Error::AlreadyConstructed as u32,
                        format!(
                            "{resource:?}: AdaptiveForegroundSurfaceV1 already exists for this surface"
                        ),
                    );
                    return;
                }

                let data = Mutex::new(AdaptiveForegroundInternal {
                    surface: surface.downgrade(),
                    zones: Vec::new(),
                    last_sent: Vec::new(),
                });
                let obj = data_init.init(id, data);
                state.adaptive_foreground_state().surfaces.push(obj);
            }
        }
    }
}

impl<D>
    Dispatch<adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1, AdaptiveForegroundData, D>
    for AdaptiveForegroundState
where
    D: GlobalDispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1, ()>
        + Dispatch<
            adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
            AdaptiveForegroundData,
        > + AdaptiveForegroundHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
        request: adaptive_foreground_surface_v1::Request,
        data: &AdaptiveForegroundData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            adaptive_foreground_surface_v1::Request::Destroy => {}
            adaptive_foreground_surface_v1::Request::SetRegion { region } => {
                let zones = region.map(|r| parse_region(&r)).unwrap_or_default();

                let surface = {
                    let mut guard = data.lock().unwrap();
                    if guard.zones == zones {
                        return;
                    }
                    guard.zones = zones;
                    // A changed zone list invalidates every reading: index N is a
                    // different rectangle than it was, so the epsilon check in
                    // `send_luminance` must not suppress the value for it.
                    guard.last_sent.clear();
                    guard.surface.upgrade().ok()
                };

                if let Some(surface) = surface {
                    state.adaptive_foreground_zones_changed(&surface);
                }
            }
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1,
        _data: &AdaptiveForegroundData,
    ) {
        state
            .adaptive_foreground_state()
            .surfaces
            .retain(|obj| obj != resource);
    }
}

/// The Add rectangles of a region, in the order the client added them.
///
/// A `wl_region` accumulates Add/Subtract ops; the protocol says zones are only
/// ever added, but a client that subtracts anyway would otherwise silently shift
/// every later zone's index. Treat the last Subtract as a clear, matching how
/// `blur` reads its own region.
fn parse_region(
    region: &smithay::reexports::wayland_server::protocol::wl_region::WlRegion,
) -> Vec<Rectangle<i32, Logical>> {
    use smithay::wayland::compositor::{RectangleKind, get_region_attributes};

    let attrs = get_region_attributes(region);
    let start = attrs
        .rects
        .iter()
        .rposition(|(kind, _)| matches!(kind, RectangleKind::Subtract))
        .map_or(0, |i| i + 1);

    attrs.rects[start..]
        .iter()
        .filter(|(kind, _)| matches!(kind, RectangleKind::Add))
        .map(|(_, rect)| *rect)
        .collect()
}

pub type AdaptiveForegroundData = Mutex<AdaptiveForegroundInternal>;

#[derive(Debug)]
pub struct AdaptiveForegroundInternal {
    pub surface: Weak<WlSurface>,
    /// Sample zones, surface-local, indexed as the client added them.
    pub zones: Vec<Rectangle<i32, Logical>>,
    /// Last value sent per zone index, so unchanged readings stay off the wire.
    pub last_sent: Vec<Option<f32>>,
}

macro_rules! delegate_adaptive_foreground {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::adaptive_foreground::adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1: ()
        ] => $crate::wayland::protocols::adaptive_foreground::AdaptiveForegroundState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::adaptive_foreground::adaptive_foreground_manager_v1::AdaptiveForegroundManagerV1: ()
        ] => $crate::wayland::protocols::adaptive_foreground::AdaptiveForegroundState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::adaptive_foreground::adaptive_foreground_surface_v1::AdaptiveForegroundSurfaceV1: $crate::wayland::protocols::adaptive_foreground::AdaptiveForegroundData
        ] => $crate::wayland::protocols::adaptive_foreground::AdaptiveForegroundState);
    };
}
pub(crate) use delegate_adaptive_foreground;

#[cfg(test)]
mod tests {
    use super::{LUMINANCE_EPSILON, to_fixed};

    #[test]
    fn luminance_spans_the_full_fixed_point_range() {
        assert_eq!(to_fixed(0.0), 0);
        assert_eq!(to_fixed(1.0), 256);
        assert_eq!(to_fixed(0.5), 128);
    }

    #[test]
    fn out_of_range_luminance_clamps_rather_than_wrapping() {
        // A sample of a region rendered with an alpha channel, or a readback in
        // the wrong colour space, can land outside 0..=1. Wrapping that into a
        // u32 would send the client a nonsense reading instead of a saturated one.
        assert_eq!(to_fixed(-0.5), 0);
        assert_eq!(to_fixed(2.0), 256);
    }

    #[test]
    fn epsilon_is_at_least_the_wire_quantum() {
        // Anything below 1/256 cannot change the encoded value, so an epsilon
        // under it would let through events that carry no new information.
        const { assert!(LUMINANCE_EPSILON >= 1.0 / 256.0) };
    }
}
