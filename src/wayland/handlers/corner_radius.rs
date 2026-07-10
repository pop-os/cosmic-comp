use smithay::utils::{Logical, Rectangle, Size};
use smithay::wayland::compositor::SurfaceData;

use crate::wayland::protocols::corner_radius::{
    CacheableCorners, CacheablePadding, CornerRadiusData, CornerRadiusHandler, CornerRadiusState,
    CornerRadiusSurface, delegate_corner_radius,
};

use crate::state::State;

impl CornerRadiusHandler for State {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState {
        &mut self.common.corner_radius_state
    }

    fn set_corner_radius(&mut self, _data: &CornerRadiusData) {}

    fn unset_corner_radius(&mut self, _data: &CornerRadiusData) {}

    fn set_padding(&mut self, _data: &CornerRadiusData) {}

    fn unset_padding(&mut self, _data: &CornerRadiusData) {}
}

pub fn surface_corners(states: &SurfaceData, size: Size<i32, Logical>) -> Option<[u8; 4]> {
    let mut guard = states.cached_state.get::<CacheableCorners>();

    // guard against corner radius being too large, potentially disconnecting the outline
    let half_min_dim = u8::try_from(size.w.min(size.h) / 2).unwrap_or(u8::MAX);
    let corners = guard.current().0?;

    Some([
        u8::try_from(corners.top_left)
            .unwrap_or(u8::MAX)
            .min(half_min_dim),
        u8::try_from(corners.top_right)
            .unwrap_or(u8::MAX)
            .min(half_min_dim),
        u8::try_from(corners.bottom_right)
            .unwrap_or(u8::MAX)
            .min(half_min_dim),
        u8::try_from(corners.bottom_left)
            .unwrap_or(u8::MAX)
            .min(half_min_dim),
    ])
}

pub fn surface_padding(states: &SurfaceData, size: Size<i32, Logical>) -> Option<[i32; 4]> {
    let mut guard = states.cached_state.get::<CacheablePadding>();

    let padding = guard.current().0?;

    // guard against padding being too large
    Some([
        padding.top.min(size.h / 2),
        padding.right.min(size.w / 2),
        padding.bottom.min(size.h / 2),
        padding.left.min(size.w / 2),
    ])
}

pub fn pad_rect(
    mut rect: Rectangle<i32, Logical>,
    padding: &[i32; 4],
) -> Option<Rectangle<i32, Logical>> {
    rect.size.h = rect.size.h.checked_sub(padding[0])?;
    rect.loc.x += padding[3];
    rect.size.w = rect.size.w.checked_sub(padding[1])?;
    rect.size.h = rect.size.h.checked_sub(padding[2])?;
    rect.size.w = rect.size.w.checked_sub(padding[3])?;
    rect.loc.y += padding[0];
    Some(rect)
}

delegate_corner_radius!(State);
