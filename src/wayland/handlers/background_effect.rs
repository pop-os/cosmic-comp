use smithay::{
    reexports::wayland_server::{DisplayHandle, protocol::wl_surface::WlSurface},
    utils::{Logical, Rectangle},
    wayland::compositor::{Cacheable, RectangleKind, RegionAttributes, with_states},
};

use crate::{
    state::State,
    wayland::protocols::background_effect::{
        BackgroundEffectHandler, ext_background_effect_manager_v1::Capability,
    },
};

#[derive(Debug, Clone, Default)]
pub struct ComputedBlurRegionCachedState {
    /// Region of the surface that will have its background blurred.
    ///
    /// `None` means no blur. When `whole_surface` is set this is left `None`
    /// and the area is taken from the surface geometry at render time instead,
    /// so it stays correct across resizes without the client resending it.
    pub blur_region: Option<Vec<Rectangle<i32, Logical>>>,
    /// Blur the entire surface, tracking its size (protocol version 2).
    pub whole_surface: bool,
    /// Requested blur strength in surface-local coordinates, or `None` for the
    /// compositor default (protocol version 2).
    ///
    /// A hint: the compositor clamps this to what it can render.
    pub blur_radius: Option<u32>,
    /// Corner radii of the blurred area, clockwise from top-left, so the
    /// backdrop can follow the shape the client actually draws (protocol
    /// version 2).
    ///
    /// One entry per rect in `blur_region`, index-matched. A shorter list
    /// leaves the remaining rects square, and a single entry rounds every rect
    /// the same way -- which is what a client sending one radius for a
    /// whole-surface blur means.
    pub region_radii: Vec<[u32; 4]>,
    /// Saturation applied to the blurred backdrop, matching CSS
    /// `backdrop-filter: saturate()` (protocol version 3).
    ///
    /// `1.0` leaves saturation unchanged. `None` means the compositor default.
    pub saturation: Option<f32>,
    /// Strength of the white overlay blended onto the backdrop -- the frosted
    /// lightening (protocol version 3).
    ///
    /// `0.0` disables it, leaving a faithful `backdrop-filter: blur()` with the
    /// surface's own background providing the glass colour. `None` means the
    /// compositor default.
    pub tint: Option<f32>,
    /// Alpha of the 1px frosted border drawn around the backdrop (protocol
    /// version 3).
    ///
    /// `0.0` disables it, which is what a surface drawing its own border wants.
    /// `None` means the compositor default.
    pub border: Option<f32>,
}

impl Cacheable for ComputedBlurRegionCachedState {
    fn commit(&mut self, _dh: &DisplayHandle) -> Self {
        self.clone()
    }

    fn merge_into(self, into: &mut Self, _dh: &DisplayHandle) {
        *into = self;
    }
}

impl BackgroundEffectHandler for State {
    fn capabilities(&self) -> Capability {
        Capability::Blur
    }

    fn set_blur_region(&mut self, surface: WlSurface, region: RegionAttributes) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();

            blur_state.pending().whole_surface = false;
            blur_state.pending().blur_region = Some({
                let (added, subtracted) = region
                    .rects
                    .iter()
                    .cloned()
                    .partition::<Vec<_>, _>(|(op, _)| matches!(op, RectangleKind::Add));
                let added = added.into_iter().map(|(_, rect)| rect).collect::<Vec<_>>();
                Rectangle::subtract_rects_many_in_place(
                    added,
                    subtracted.into_iter().map(|(_, rect)| rect),
                )
            })
        })
    }

    fn unset_blur_region(&mut self, surface: WlSurface) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            let pending = blur_state.pending();

            // A NULL region removes the effect outright, so the whole-surface
            // mode has to go with it or the blur would survive its own removal.
            pending.blur_region.take();
            pending.whole_surface = false;
        })
    }

    fn set_blur_whole_surface(&mut self, surface: WlSurface) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            let pending = blur_state.pending();

            // The area is resolved from the surface at render time, so any
            // explicit region is dropped rather than left to compete with it.
            pending.whole_surface = true;
            pending.blur_region.take();
        })
    }

    fn set_blur_radius(&mut self, surface: WlSurface, radius: Option<u32>) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            blur_state.pending().blur_radius = radius;
        })
    }

    fn set_region_radii(&mut self, surface: WlSurface, radii: Vec<[u32; 4]>) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            blur_state.pending().region_radii = radii;
        })
    }

    fn set_saturation(&mut self, surface: WlSurface, saturation: Option<f32>) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            blur_state.pending().saturation = saturation;
        })
    }

    fn set_tint(&mut self, surface: WlSurface, tint: Option<f32>) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            blur_state.pending().tint = tint;
        })
    }

    fn set_border(&mut self, surface: WlSurface, border: Option<f32>) {
        with_states(&surface, |states| {
            let mut blur_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
            blur_state.pending().border = border;
        })
    }
}

crate::delegate_background_effect!(State);
