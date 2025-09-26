use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel;
use smithay::reexports::wayland_server::Resource;
use smithay::wayland::compositor::{add_pre_commit_hook, with_states};
use smithay::wayland::shell::xdg::{SurfaceCachedState, ToplevelSurface};

use crate::wayland::protocols::corner_radius::{
    delegate_corner_radius, CacheableCorners, CornerRadiusData, CornerRadiusHandler,
    CornerRadiusState,
};

use crate::state::State;

impl CornerRadiusHandler for State {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState {
        &mut self.common.corner_radius_state
    }

    fn toplevel_from_resource(
        &mut self,
        toplevel: &xdg_toplevel::XdgToplevel,
    ) -> Option<ToplevelSurface> {
        self.common.xdg_shell_state.get_toplevel(toplevel)
    }

    fn set_corner_radius(
        &mut self,
        _: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    ) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius change.");
        }
    }

    fn unset_corner_radius(
        &mut self,
        _: &cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        data: &CornerRadiusData,
    ) {
        if force_redraw(self, data).is_none() {
            tracing::warn!("Failed to force redraw for corner radius reset.");
        }
    }

    fn add_corners(
        &mut self,
        toplevel: &xdg_toplevel::XdgToplevel,
        toplevel_obj: cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
    ) {
        if let Some(surface) = self.toplevel_from_resource(&toplevel) {
            add_pre_commit_hook::<Self, _>(surface.wl_surface(), move |_, _dh, surface| {
                let corner_radii_too_big = with_states(surface, |surface_data| {
                    let corners = surface_data
                        .cached_state
                        .get::<CacheableCorners>()
                        .pending()
                        .clone();
                    surface_data
                        .cached_state
                        .get::<SurfaceCachedState>()
                        .pending()
                        .geometry
                        .zip(corners.0.as_ref())
                        .is_some_and(|(geo, corners)| {
                            let half_min_dim =
                                u8::try_from(geo.size.w.min(geo.size.h) / 2).unwrap_or(u8::MAX);
                            corners.top_right > half_min_dim
                                || corners.top_left > half_min_dim
                                || corners.bottom_right > half_min_dim
                                || corners.bottom_left > half_min_dim
                        })
                });

                if corner_radii_too_big {
                    toplevel_obj.post_error(
                        cosmic_corner_radius_toplevel_v1::Error::RadiusTooLarge as u32,
                        format!("{toplevel_obj:?} corner radius too large"),
                    );
                }
            });
        }
    }
}

fn force_redraw(state: &mut State, data: &CornerRadiusData) -> Option<()> {
    let guard = data.lock().unwrap();

    let toplevel = guard.toplevel.upgrade().ok()?;

    let surface = state.common.xdg_shell_state.get_toplevel(&toplevel)?;

    let guard = state.common.shell.read();
    let output = guard.visible_output_for_surface(surface.wl_surface())?;

    state.backend.schedule_render(output);
    Some(())
}

delegate_corner_radius!(State);
