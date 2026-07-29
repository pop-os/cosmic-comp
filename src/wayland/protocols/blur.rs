// SPDX-License-Identifier: GPL-3.0-only

//! Implementation of the KDE blur protocol (org_kde_kwin_blur_manager)
//!
//! This protocol allows clients to request blur effects for surfaces.
//! The blur effect can be applied to a specific region or the entire surface.

// Re-export only the actual code, and then only use this re-export
// The `generated` module below is just some boilerplate to properly isolate stuff
// and avoid exposing internal details.
pub use generated::{org_kde_kwin_blur, org_kde_kwin_blur_manager};

#[allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]
mod generated {
    use smithay::reexports::wayland_server::{self, protocol::*};

    pub mod __interfaces {
        use smithay::reexports::wayland_server::protocol::__interfaces::*;
        use wayland_backend;
        wayland_scanner::generate_interfaces!("resources/protocols/blur.xml");
    }
    use self::__interfaces::*;

    wayland_scanner::generate_server_code!("resources/protocols/blur.xml");
}

use smithay::{
    reexports::wayland_server::{
        Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource, Weak,
        backend::GlobalId, protocol::wl_surface::WlSurface,
    },
    utils::{Logical, Rectangle},
    wayland::compositor::{Cacheable, with_states},
};
use std::sync::Mutex;

/// Blur region data stored per-surface
#[derive(Debug, Clone, Default)]
pub struct BlurRegionData {
    /// The blur region. If None, the entire surface should be blurred.
    pub region: Option<Vec<Rectangle<i32, Logical>>>,
    /// Per-rect corner radii `[top-left, top-right, bottom-right, bottom-left]`
    /// in logical px, index-matched to `region` (a shorter vec leaves the
    /// remaining rects on the surface's single radius). `None` when the client
    /// didn't send `set_region_radii`, i.e. one radius rounds every rect.
    pub region_radii: Option<Vec<[f32; 4]>>,
    /// Custom blur radius in pixels. If None, the compositor default is used.
    pub radius: Option<f32>,
    /// Backdrop saturation multiplier (1.0 = unchanged). If None, no saturation
    /// adjustment is applied (matches CSS `backdrop-filter: saturate(x)`).
    pub saturation: Option<f32>,
    /// Frosted-glass white-tint strength (0.0 = no tint, faithful backdrop-filter).
    /// If None, the compositor default tint is used.
    pub tint: Option<f32>,
    /// Frosted-glass border alpha (0.0 = no border). If None, compositor default.
    pub border: Option<f32>,
}

/// Cacheable blur state for double-buffering
#[derive(Debug, Clone, Default)]
pub struct CacheableBlurState {
    /// Whether blur is enabled for this surface
    pub enabled: bool,
    /// The blur region data
    pub data: Option<BlurRegionData>,
    /// Custom blur radius in pixels. If None, the compositor default is used.
    pub radius: Option<f32>,
    /// Backdrop saturation multiplier (1.0 = unchanged). If None, no saturation
    /// adjustment is applied.
    pub saturation: Option<f32>,
    /// Frosted-glass white-tint strength (0.0 = no tint). If None, compositor default.
    pub tint: Option<f32>,
    /// Frosted-glass border alpha (0.0 = no border). If None, compositor default.
    pub border: Option<f32>,
}

impl Cacheable for CacheableBlurState {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        self.clone()
    }
    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

/// User data for the blur object
pub struct BlurData {
    surface: Weak<WlSurface>,
    pending_region: Mutex<Option<Vec<Rectangle<i32, Logical>>>>,
    pending_region_radii: Mutex<Option<Vec<[f32; 4]>>>,
    pending_radius: Mutex<Option<f32>>,
    pending_saturation: Mutex<Option<f32>>,
    pending_tint: Mutex<Option<f32>>,
    pending_border: Mutex<Option<f32>>,
}

/// State for the blur manager protocol
#[derive(Debug)]
pub struct BlurState {
    global: GlobalId,
}

impl BlurState {
    /// Create a new blur manager global
    pub fn new<D>(dh: &DisplayHandle) -> BlurState
    where
        D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
            + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
            + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
            + BlurHandler
            + 'static,
    {
        let global =
            dh.create_global::<D, org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, _>(4, ());
        BlurState { global }
    }

    /// Get the global ID of the blur manager
    pub fn global_id(&self) -> GlobalId {
        self.global.clone()
    }
}

/// Handler trait for blur events
pub trait BlurHandler {
    /// Get the blur state
    fn blur_state(&mut self) -> &mut BlurState;

    /// Called when blur is set on a surface
    fn blur_set(&mut self, surface: &WlSurface);

    /// Called when blur is unset from a surface
    fn blur_unset(&mut self, surface: &WlSurface);
}

impl<D> GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, (), D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn bind(
        _state: &mut D,
        _handle: &DisplayHandle,
        _client: &Client,
        resource: New<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, (), D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &org_kde_kwin_blur_manager::OrgKdeKwinBlurManager,
        request: org_kde_kwin_blur_manager::Request,
        _data: &(),
        _dhandle: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            org_kde_kwin_blur_manager::Request::Create { id, surface } => {
                let blur_data = BlurData {
                    surface: surface.downgrade(),
                    pending_region: Mutex::new(None),
                    pending_region_radii: Mutex::new(None),
                    pending_radius: Mutex::new(None),
                    pending_saturation: Mutex::new(None),
                    pending_tint: Mutex::new(None),
                    pending_border: Mutex::new(None),
                };
                data_init.init(id, blur_data);
            }
            org_kde_kwin_blur_manager::Request::Unset { surface } => {
                // Remove blur from the surface
                with_states(&surface, |surface_data| {
                    let mut cached = surface_data.cached_state.get::<CacheableBlurState>();
                    let pending = cached.pending();
                    pending.enabled = false;
                    pending.data = None;
                });
                state.blur_unset(&surface);
            }
        }
    }
}

impl<D> Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData, D> for BlurState
where
    D: GlobalDispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur_manager::OrgKdeKwinBlurManager, ()>
        + Dispatch<org_kde_kwin_blur::OrgKdeKwinBlur, BlurData>
        + BlurHandler
        + 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _resource: &org_kde_kwin_blur::OrgKdeKwinBlur,
        request: org_kde_kwin_blur::Request,
        data: &BlurData,
        _dhandle: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            org_kde_kwin_blur::Request::Commit => {
                let Ok(surface) = data.surface.upgrade() else {
                    return;
                };

                let pending_region = data.pending_region.lock().unwrap().take();
                let pending_region_radii = data.pending_region_radii.lock().unwrap().take();
                let pending_radius = data.pending_radius.lock().unwrap().take();
                let pending_saturation = data.pending_saturation.lock().unwrap().take();
                let pending_tint = data.pending_tint.lock().unwrap().take();
                let pending_border = data.pending_border.lock().unwrap().take();

                tracing::trace!(
                    surface_id = surface.id().protocol_id(),
                    has_region = pending_region.is_some(),
                    region_count = pending_region.as_ref().map(|r| r.len()).unwrap_or(0),
                    radii_count = pending_region_radii.as_ref().map(|r| r.len()).unwrap_or(0),
                    ?pending_radius,
                    ?pending_saturation,
                    ?pending_tint,
                    ?pending_border,
                    "Blur Commit for surface"
                );

                with_states(&surface, |surface_data| {
                    let mut cached = surface_data.cached_state.get::<CacheableBlurState>();
                    let pending = cached.pending();
                    pending.enabled = true;
                    pending.radius = pending_radius;
                    pending.saturation = pending_saturation;
                    pending.tint = pending_tint;
                    pending.border = pending_border;
                    pending.data = Some(BlurRegionData {
                        region: pending_region,
                        region_radii: pending_region_radii,
                        radius: pending_radius,
                        saturation: pending_saturation,
                        tint: pending_tint,
                        border: pending_border,
                    });
                });

                state.blur_set(&surface);
            }
            org_kde_kwin_blur::Request::SetRegion { region } => {
                let regions = region.map(|r| {
                    use smithay::wayland::compositor::{RectangleKind, get_region_attributes};
                    let attrs = get_region_attributes(&r);

                    // The WlRegion accumulates Add/Subtract ops across calls.
                    // Find the last Subtract (which acts as a "clear"), then
                    // collect only the Add rects after it.
                    let last_subtract_idx = attrs
                        .rects
                        .iter()
                        .rposition(|(kind, _)| matches!(kind, RectangleKind::Subtract));

                    let start = last_subtract_idx.map_or(0, |i| i + 1);
                    let result: Vec<_> = attrs.rects[start..]
                        .iter()
                        .filter(|(kind, _)| matches!(kind, RectangleKind::Add))
                        .map(|(_, rect)| *rect)
                        .collect();

                    tracing::trace!(
                        total_rects = attrs.rects.len(),
                        add_rects = result.len(),
                        "Blur SetRegion parsed"
                    );
                    result
                });
                *data.pending_region.lock().unwrap() = regions;
            }
            org_kde_kwin_blur::Request::SetRegionRadii { radii } => {
                let radii = parse_region_radii(&radii);
                tracing::trace!(rect_count = radii.len(), "Blur SetRegionRadii");
                // Both this and the region are pending state consumed by the next
                // commit, so the client may send them in either order.
                *data.pending_region_radii.lock().unwrap() = Some(radii);
            }
            org_kde_kwin_blur::Request::Release => {
                // Release is a destructor for the blur *object*, NOT an unset.
                // Clients create a new blur object and commit it
                // before releasing the old one. We must not disable blur here,
                // otherwise the new object's commit is undone by the old release.
                // Only the explicit `unset` request on the manager should disable blur.
            }
            org_kde_kwin_blur::Request::SetRadius { radius_fixed } => {
                // Fixed-point 24.8: divide by 256 to get pixels
                let radius_px = radius_fixed as f32 / 256.0;
                tracing::trace!(radius_fixed, radius_px, "Blur SetRadius");
                *data.pending_radius.lock().unwrap() = Some(radius_px);
            }
            org_kde_kwin_blur::Request::SetSaturation { saturation_fixed } => {
                // Fixed-point 8.8 (same encoding as radius): divide by 256.
                // 1.0 = unchanged, >1.0 = more saturated (e.g. 1.3 → 332).
                let saturation = saturation_fixed as f32 / 256.0;
                tracing::trace!(saturation_fixed, saturation, "Blur SetSaturation");
                *data.pending_saturation.lock().unwrap() = Some(saturation);
            }
            org_kde_kwin_blur::Request::SetTint { tint_fixed } => {
                // Fixed-point 8.8: divide by 256. 0.0 = no white tint (faithful
                // backdrop-filter), 0.15 ≈ the compositor default.
                let tint = tint_fixed as f32 / 256.0;
                tracing::trace!(tint_fixed, tint, "Blur SetTint");
                *data.pending_tint.lock().unwrap() = Some(tint);
            }
            org_kde_kwin_blur::Request::SetBorder { border_fixed } => {
                // Fixed-point 8.8: divide by 256. 0.0 = no border, 0.2 ≈ default.
                let border = border_fixed as f32 / 256.0;
                tracing::trace!(border_fixed, border, "Blur SetBorder");
                *data.pending_border.lock().unwrap() = Some(border);
            }
        }
    }
}

/// Parse a `set_region_radii` array: four native-endian `u32` radii
/// (top-left, top-right, bottom-right, bottom-left) per region rect, in the
/// order the rects were added to the region. A trailing partial quadruple is
/// ignored, as the protocol specifies.
fn parse_region_radii(bytes: &[u8]) -> Vec<[f32; 4]> {
    bytes
        .chunks_exact(std::mem::size_of::<u32>() * 4)
        .map(|quad| {
            let at = |i: usize| {
                u32::from_ne_bytes([quad[i], quad[i + 1], quad[i + 2], quad[i + 3]]) as f32
            };
            [at(0), at(4), at(8), at(12)]
        })
        .collect()
}

/// Helper function to get the blur state for a surface
pub fn get_blur_state(surface: &WlSurface) -> Option<CacheableBlurState> {
    Some(with_states(surface, |surface_data| {
        surface_data
            .cached_state
            .get::<CacheableBlurState>()
            .current()
            .clone()
    }))
}

/// Check if a surface has blur enabled
pub fn has_blur(surface: &WlSurface) -> bool {
    let result = get_blur_state(surface)
        .map(|state| state.enabled)
        .unwrap_or(false);
    // Only log when blur is enabled to avoid spam
    if result {
        tracing::trace!(
            surface_id = surface.id().protocol_id(),
            "Surface has blur enabled"
        );
    }
    result
}

/// Get the custom blur radius for a surface, if set.
/// Returns `None` if the surface uses the compositor default.
pub fn get_blur_radius(surface: &WlSurface) -> Option<f32> {
    get_blur_state(surface).and_then(|state| state.radius)
}

/// Get the backdrop saturation multiplier for a surface, if set.
/// Returns `None` (i.e. no saturation change, 1.0) if the surface didn't request one.
pub fn get_blur_saturation(surface: &WlSurface) -> Option<f32> {
    get_blur_state(surface).and_then(|state| state.saturation)
}

/// Get the frosted-glass white-tint strength for a surface, if set.
/// Returns `None` (use the compositor default tint) if the surface didn't request one.
pub fn get_blur_tint(surface: &WlSurface) -> Option<f32> {
    get_blur_state(surface).and_then(|state| state.tint)
}

/// Get the frosted-glass border alpha for a surface, if set.
/// Returns `None` (use the compositor default border) if the surface didn't request one.
pub fn get_blur_border(surface: &WlSurface) -> Option<f32> {
    get_blur_state(surface).and_then(|state| state.border)
}

/// Macro to delegate blur protocol handling
#[macro_export]
macro_rules! delegate_blur {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur_manager::OrgKdeKwinBlurManager: ()
        ] => $crate::wayland::protocols::blur::BlurState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur_manager::OrgKdeKwinBlurManager: ()
        ] => $crate::wayland::protocols::blur::BlurState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            $crate::wayland::protocols::blur::org_kde_kwin_blur::OrgKdeKwinBlur: $crate::wayland::protocols::blur::BlurData
        ] => $crate::wayland::protocols::blur::BlurState);
    };
}
pub(crate) use delegate_blur;

#[cfg(test)]
mod tests {
    use super::parse_region_radii;

    fn wl_array(radii: &[u32]) -> Vec<u8> {
        radii.iter().flat_map(|r| r.to_ne_bytes()).collect()
    }

    #[test]
    fn parses_one_quadruple_per_rect_in_order() {
        // Two rects: a 16px card, then a "pill" the compositor will clamp per-rect.
        let bytes = wl_array(&[16, 16, 16, 16, 9999, 9999, 9999, 9999]);
        assert_eq!(
            parse_region_radii(&bytes),
            vec![[16.0; 4], [9999.0; 4]],
            "radii must stay grouped per rect, in region order"
        );
    }

    #[test]
    fn keeps_per_corner_order_tl_tr_br_bl() {
        let bytes = wl_array(&[1, 2, 3, 4]);
        assert_eq!(parse_region_radii(&bytes), vec![[1.0, 2.0, 3.0, 4.0]]);
    }

    #[test]
    fn ignores_a_trailing_partial_quadruple() {
        // A short/garbled tail must not shift the radii of earlier rects.
        let mut bytes = wl_array(&[8, 8, 8, 8, 12, 12]);
        assert_eq!(parse_region_radii(&bytes), vec![[8.0; 4]]);
        // Not even a whole u32 left over.
        bytes.push(0);
        assert_eq!(parse_region_radii(&bytes), vec![[8.0; 4]]);
    }

    #[test]
    fn empty_array_yields_no_radii() {
        // Distinct from "no set_region_radii at all" (None) — an empty array just
        // leaves every rect on the surface's single radius.
        assert!(parse_region_radii(&[]).is_empty());
    }
}
