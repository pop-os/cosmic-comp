use cosmic_protocols::corner_radius::v1::server::cosmic_corner_radius_toplevel_v1;

use crate::wayland::protocols::corner_radius::{
    CornerRadiusData, CornerRadiusHandler, CornerRadiusState, delegate_corner_radius,
};

use crate::state::State;

impl CornerRadiusHandler for State {
    fn corner_radius_state(&mut self) -> &mut CornerRadiusState {
        &mut self.common.corner_radius_state
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
