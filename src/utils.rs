// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::wayland_server::{Global, Interface, Resource};
use std::{
    convert::{AsRef, From},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

pub struct GlobalDrop<I: Interface + AsRef<Resource<I>> + From<Resource<I>>>(Option<Global<I>>);

impl<I: Interface + AsRef<Resource<I>> + From<Resource<I>>> From<Global<I>> for GlobalDrop<I> {
    fn from(g: Global<I>) -> GlobalDrop<I> {
        GlobalDrop(Some(g))
    }
}

impl<I: Interface + AsRef<Resource<I>> + From<Resource<I>>> Drop for GlobalDrop<I> {
    fn drop(&mut self) {
        if let Some(global) = self.0.take() {
            global.destroy();
        }
    }
}

// This hack will hopefully will be superseeded by a better solution, when smithay transitions to wayland-rs 0.30.
// But until then there is not really a better way to schedule a repaint on surface destruction
#[derive(Debug)]
pub struct SurfaceDropNotifier(Arc<AtomicBool>);

impl From<&crate::state::Common> for SurfaceDropNotifier {
    fn from(state: &crate::state::Common) -> Self {
        SurfaceDropNotifier(state.dirty_flag.clone())
    }
}

impl Drop for SurfaceDropNotifier {
    fn drop(&mut self) {
        self.0.store(true, Ordering::SeqCst);
    }
}
