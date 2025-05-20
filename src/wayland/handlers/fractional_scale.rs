use crate::{state::State, utils::prelude::SeatExt};
use smithay::{
    delegate_fractional_scale,
    desktop::utils::surface_primary_scanout_output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::{
        compositor::{get_parent, with_states},
        fractional_scale::{with_fractional_scale, FractionalScaleHandler},
    },
};

impl FractionalScaleHandler for State {
    fn new_fractional_scale(&mut self, surface: WlSurface) {
        // Here we can set the initial fractional scale
        //
        // First we look if the surface already has a primary scan-out output, if not
        // we test if the surface is a subsurface and try to use the primary scan-out output
        // of the root surface. If the root also has no primary scan-out output we just try
        // to use the first output of the toplevel.
        // If the surface is the root we also try to use the first output of the toplevel.
        //
        // If all the above tests do not lead to a output we just use the active output
        // of the last active seat (which will also be the output a toplevel will
        // initially be placed on)
        let output = with_states(&surface, |states| {
            surface_primary_scanout_output(&surface, states)
        })
        .or_else(|| {
            let mut root = surface.clone();
            while let Some(parent) = get_parent(&root) {
                root = parent;
            }

            (root != surface)
                .then(|| {
                    with_states(&root, |states| {
                        surface_primary_scanout_output(&root, states)
                    })
                })
                .flatten()
                .or_else(|| {
                    self.common
                        .shell
                        .read()
                        .visible_output_for_surface(&surface)
                        .cloned()
                })
        })
        .unwrap_or_else(|| self.common.shell.read().seats.last_active().active_output());

        with_states(&surface, |states| {
            with_fractional_scale(states, |fractional_scale| {
                fractional_scale.set_preferred_scale(output.current_scale().fractional_scale());
            });
        });
    }
}

delegate_fractional_scale!(State);
