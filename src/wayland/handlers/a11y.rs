use tracing::warn;

use crate::{
    config::ColorFilter,
    state::State,
    wayland::protocols::a11y::{A11yHandler, A11yState, delegate_a11y},
};

impl A11yHandler for State {
    fn a11y_state(&mut self) -> &mut A11yState {
        &mut self.common.a11y_state
    }

    fn request_screen_magnifier(&mut self, enabled: bool) {
        let mut shell = self.common.shell.write();

        if shell
            .zoom_state()
            .is_some_and(|state| shell.outputs().any(|o| state.current_level(o) != 1.0))
            != enabled
        {
            let seat = shell.seats.last_active().clone();
            let level = if enabled {
                1.0 + (self.common.config.cosmic_conf.accessibility_zoom.increment as f64 / 100.0)
            } else {
                1.0
            };
            let zoom_config = &self.common.config.cosmic_conf.accessibility_zoom;

            shell.trigger_zoom(
                &seat,
                None,
                level,
                zoom_config,
                true,
                &self.common.event_loop_handle,
            );
        }
    }

    fn request_screen_invert(&mut self, inverted: bool) {
        let mut config = self.common.config.dynamic_conf.screen_filter_mut();
        let mut updated = (*config).clone();
        updated.inverted = inverted;
        if let Err(err) = self.backend.update_screen_filter(&updated) {
            warn!("Failed to apply screen color invert: {}", err);
        } else {
            *config = updated;
            self.common.a11y_state.set_screen_inverted(inverted);
        }
    }

    fn request_screen_filter(&mut self, filter: Option<ColorFilter>) {
        let mut config = self.common.config.dynamic_conf.screen_filter_mut();
        let mut updated = (*config).clone();
        updated.color_filter = filter;
        if let Err(err) = self.backend.update_screen_filter(&updated) {
            warn!("Failed to apply screen color filter: {}", err);
        } else {
            *config = updated;
            self.common.a11y_state.set_screen_filter(filter);
        }
    }
}

delegate_a11y!(State);
