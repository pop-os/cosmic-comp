// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    reexports::wayland_server::protocol::wl_buffer::WlBuffer,
    wayland::buffer::BufferHandler,
};
use crate::utils::prelude::*;

impl BufferHandler for State {
    fn buffer_destroyed(&mut self, _buffer: &WlBuffer) {}
}
