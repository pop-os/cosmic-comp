use smithay::{
    reexports::wayland_server::{DisplayHandle, protocol::wl_surface::WlSurface},
    utils::{Logical, Rectangle},
    wayland::{
        background_effect::{Capability, ExtBackgroundEffectHandler},
        compositor::{Cacheable, RectangleKind, RegionAttributes, with_states},
    },
};

use crate::state::State;

/// The blur state the renderer reads, resolved from whichever protocol the
/// client spoke.
///
/// `ext_background_effect_v1` is upstream's staging protocol and carries only a
/// region; everything below it comes from `org_kde_kwin_blur`, which we own and
/// which our own clients speak. Both write here so the renderer has one type to
/// read rather than a protocol to branch on.
#[derive(Debug, Clone, Default)]
pub struct ComputedBlurRegionCachedState {
    /// Region of the surface that will have its background blurred.
    ///
    /// `None` means no blur. When `whole_surface` is set this is left `None`
    /// and the area is taken from the surface geometry at render time instead,
    /// so it stays correct across resizes without the client resending it.
    pub blur_region: Option<Vec<Rectangle<i32, Logical>>>,
    /// Blur the entire surface, tracking its size.
    pub whole_surface: bool,
    /// Requested blur strength in surface-local coordinates, or `None` for the
    /// compositor default.
    ///
    /// A hint: the compositor clamps this to what it can render.
    pub blur_radius: Option<u32>,
    /// Corner radii of the blurred area, clockwise from top-left, so the
    /// backdrop can follow the shape the client actually draws.
    ///
    /// One entry per rect in `blur_region`, index-matched. A shorter list
    /// leaves the remaining rects square, and a single entry rounds every rect
    /// the same way -- which is what a client sending one radius for a
    /// whole-surface blur means.
    pub region_radii: Vec<[u32; 4]>,
    /// The exact sub-pixel area of each rect, index-matched to `blur_region`.
    ///
    /// `blur_region` is whole logical pixels, which is not where a surface
    /// generally draws: a card laid out from measured text, or one under a
    /// scale animation, sits at a fractional position and size. Rounding it
    /// either way is visible -- out and the backdrop escapes past the shape
    /// drawn over it, in and that shape's translucent border has no backdrop
    /// beneath it -- so the client sends what it actually means and the integer
    /// rect stays only as the conservative bound for capture and damage.
    ///
    /// A shorter vec leaves the remaining rects on their integer rect, which is
    /// also what an older client that never sends it gets.
    pub region_geometry: Vec<Rectangle<f64, Logical>>,
    /// Saturation applied to the blurred backdrop, matching CSS
    /// `backdrop-filter: saturate()`.
    ///
    /// `1.0` leaves saturation unchanged. `None` means the compositor default.
    pub saturation: Option<f32>,
    /// Strength of the white overlay blended onto the backdrop -- the frosted
    /// lightening.
    ///
    /// `0.0` disables it, leaving a faithful `backdrop-filter: blur()` with the
    /// surface's own background providing the glass colour. `None` means the
    /// compositor default.
    pub tint: Option<f32>,
    /// Alpha of the 1px frosted border drawn around the backdrop.
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

impl ExtBackgroundEffectHandler for State {
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
}
