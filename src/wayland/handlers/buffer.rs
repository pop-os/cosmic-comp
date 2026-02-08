// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use smithay::{
    reexports::wayland_server::protocol::wl_buffer::WlBuffer, wayland::buffer::BufferHandler,
};

impl BufferHandler for State {
    fn buffer_destroyed(&mut self, _buffer: &WlBuffer) {}
}
