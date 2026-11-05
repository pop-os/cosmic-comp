// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::reexports::wayland_protocols::wp::text_input::zv3::server::zwp_text_input_v3::{
    ContentHint, ContentPurpose,
};
use smithay::wayland::text_input::TextInputActivation;

impl TextInputActivation for State {
    fn activated(&mut self, content_type: Option<(ContentHint, ContentPurpose)>) {
        if let Some(ei_state) = self.common.dbus_state.ei_state() {
            ei_state.activated(content_type);
        }
    }

    fn deactivated(&mut self) {
        if let Some(ei_state) = self.common.dbus_state.ei_state() {
            ei_state.deactivated();
        }
    }
}
