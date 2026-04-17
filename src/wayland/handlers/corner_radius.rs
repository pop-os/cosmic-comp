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

    fn set_corner_radius(&mut self, data: &CornerRadiusData) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius change.");
        }
    }

    fn unset_corner_radius(&mut self, data: &CornerRadiusData) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius reset.");
        }
    }

    fn set_padding(&mut self, data: &CornerRadiusData) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius change.");
        }
    }

    fn unset_padding(&mut self, data: &CornerRadiusData) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius reset.");
        }
    }
}

fn force_redraw(state: &mut State, data: &CornerRadiusData) -> Option<()> {
    let guard = data.lock().unwrap();
    let shell = state.common.shell.read();

    let output = match &guard.surface {
        CornerRadiusSurface::Toplevel(toplevel) => {
            let toplevel = toplevel.upgrade().ok()?;
            let surface = state.common.xdg_shell_state.get_toplevel(&toplevel)?;
            shell.visible_output_for_surface(surface.wl_surface())?
        }
        CornerRadiusSurface::Popup(popup) => {
            let popup = popup.upgrade().ok()?;
            let surface = state.common.xdg_shell_state.get_popup(&popup)?;
            shell.visible_output_for_surface(surface.wl_surface())?
        }
        CornerRadiusSurface::Layer(layer) => {
            let layer = layer.upgrade().ok()?;
            let surface = state
                .common
                .layer_shell_state
                .layer_surfaces()
                .find(|l| l.shell_surface() == &layer)?;
            shell.visible_output_for_surface(surface.wl_surface())?
        }
    };

    state.backend.schedule_render(output);
    Some(())
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

    // guard against padding being too large
    let half_min_dim = size.w.min(size.h) / 2;
    let padding = guard.current().0?;

    Some([
        padding.top.min(half_min_dim),
        padding.right.min(half_min_dim),
        padding.bottom.min(half_min_dim),
        padding.left.min(half_min_dim),
    ])
}

pub fn pad_rect(
    mut rect: Rectangle<i32, Logical>,
    padding: &[i32; 4],
) -> Option<Rectangle<i32, Logical>> {
    rect.size.h = rect.size.h.checked_sub(padding[0])?;
    rect.loc.x += padding[0];
    rect.size.w = rect.size.w.checked_sub(padding[1])?;
    rect.size.h = rect.size.h.checked_sub(padding[2])?;
    rect.size.w = rect.size.w.checked_sub(padding[3])?;
    rect.loc.y += padding[3];
    Some(rect)
}

delegate_corner_radius!(State);
