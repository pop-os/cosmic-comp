use smithay::wayland::shell::xdg::ToplevelSurface;

use crate::wayland::protocols::corner_radius::{
    delegate_corner_radius, CornerRadiusData, CornerRadiusHandler,
};

use crate::state::State;

impl CornerRadiusHandler for State {
    fn corner_radius_state(
        &mut self,
    ) -> &mut crate::wayland::protocols::corner_radius::CornerRadiusState {
        &mut self.common.corner_radius_state
    }

    fn toplevel_from_resource(
        &mut self,
        toplevel: &smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::XdgToplevel,
    ) -> Option<ToplevelSurface> {
        self.common.xdg_shell_state.get_toplevel(toplevel)
    }

    fn set_corner_radius(
        &mut self,
        _toplevel: &cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        _data: &CornerRadiusData,
        _top_left: u32,
        _top_right: u32,
        _bottom_right: u32,
        _bottom_left: u32,
    ) {
        // TODO force redraw? of focus element?
    }

    fn unset_corner_radius(
        &mut self,
        _toplevel: &cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1::CosmicCornerRadiusToplevelV1,
        _data: &CornerRadiusData,
    ) {
        // TODO force redraw?
    }
}

delegate_corner_radius!(State);
