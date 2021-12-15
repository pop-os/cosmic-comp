// SPDX-License-Identifier: GPL-3.0-only

use smithay::reexports::wayland_server::{Global, Interface, Resource};
use std::convert::{AsRef, From};

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
