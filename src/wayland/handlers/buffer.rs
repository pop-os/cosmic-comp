// SPDX-License-Identifier: GPL-3.0-only

use crate::{state::BackendData, utils::prelude::*};
use smithay::{
    reexports::wayland_server::{protocol::wl_buffer::WlBuffer, Resource},
    wayland::buffer::BufferHandler,
};

impl BufferHandler for State {
    fn buffer_destroyed(&mut self, buffer: &WlBuffer) {
        if let BackendData::Kms(kms_state) = &mut self.backend {
            for device in kms_state.drm_devices.values_mut() {
                if device.active_buffers.remove(&buffer.downgrade()) {
                    if !device.in_use(kms_state.primary_node.as_ref()) {
                        kms_state.refresh_used_devices();
                        break;
                    }
                }
            }
        }
    }
}
