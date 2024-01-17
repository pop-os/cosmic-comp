// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::BackendData, utils::prelude::*};
use smithay::{
    reexports::wayland_server::{protocol::wl_buffer::WlBuffer, Resource},
    wayland::buffer::BufferHandler,
};

impl BufferHandler for State {
    fn buffer_destroyed(&mut self, buffer: &WlBuffer) {
        if let BackendData::Kms(kms_state) = &mut self.backend {
            for device in kms_state.devices.values_mut() {
                if device.active_buffers.remove(&buffer.downgrade()) {
                    if !device.in_use(&kms_state.primary_node) {
                        kms_state.api.as_mut().remove_node(&device.render_node);
                    }
                    break;
                }
            }
        }
    }
}
