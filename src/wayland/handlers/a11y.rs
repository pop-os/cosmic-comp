use crate::{
    state::State,
    wayland::protocols::a11y::{delegate_a11y, A11yHandler, A11yState},
};

impl A11yHandler for State {
    fn a11y_state(&mut self) -> &mut A11yState {
        &mut self.common.a11y_state
    }

    fn request_screen_magnifier(&mut self, enabled: bool) {
        let mut shell = self.common.shell.write().unwrap();

        if shell
            .zoom_state()
            .is_some_and(|state| state.current_level() != 1.0)
            != enabled
        {
            let seat = shell.seats.last_active().clone();
            let level = if enabled {
                self.common.config.dynamic_conf.zoom_state().last_level
            } else {
                1.0
            };
            let zoom_config = &self.common.config.cosmic_conf.accessibility_zoom;

            shell.trigger_zoom(
                &seat,
                level,
                zoom_config,
                true,
                &self.common.event_loop_handle,
            );
        }
    }
}

delegate_a11y!(State);
