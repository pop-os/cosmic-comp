use smithay::{
    delegate_background_effect,
    reexports::wayland_server::{DisplayHandle, protocol::wl_surface::WlSurface},
    utils::{Logical, Rectangle},
    wayland::{
        background_effect::{Capability, ExtBackgroundEffectHandler},
        compositor::{Cacheable, RectangleKind, RegionAttributes, with_states},
    },
};

use crate::state::State;

#[derive(Debug, Clone, Default)]
pub struct ComputedBlurRegionCachedState {
    /// Region of the surface that will have its background blurred.
    pub blur_region: Option<Vec<Rectangle<i32, Logical>>>,
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

            blur_state.pending().blur_region.take();
        })
    }
}

delegate_background_effect!(State);
