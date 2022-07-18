// SPDX-License-Identifier: GPL-3.0-only

pub mod buffer;
pub mod compositor;
pub mod data_device;
pub mod dmabuf;
pub mod layer_shell;
pub mod output;
pub mod output_configuration;
pub mod primary_selection;
pub mod seat;
pub mod shm;
pub mod toplevel_info;
pub mod toplevel_management;
pub mod viewporter;
pub mod wl_drm;
pub mod workspace;
pub mod xdg_shell;

use crate::state::Common;
use smithay::{
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::compositor::{add_destruction_hook, with_states},
};

fn mark_dirty_on_drop(state: &Common, wl_surface: &WlSurface) {
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    };

    let dirty = state.dirty_flag.clone();
    struct DirtyFlag(Arc<AtomicBool>);

    with_states(wl_surface, |data| {
        data.data_map.insert_if_missing(|| DirtyFlag(dirty));
    });
    add_destruction_hook(wl_surface, |data| {
        if let Some(DirtyFlag(dirty)) = data.data_map.get::<DirtyFlag>() {
            dirty.store(true, Ordering::SeqCst);
        }
    })
}
