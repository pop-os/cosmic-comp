// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use anyhow::Result;
use smithay::reexports::calloop::EventLoop;

pub mod x11;
// TODO
// pub mod wayland; // tbd in smithay
// pub mod udev;

pub fn init_backend_auto(event_loop: &mut EventLoop<State>, state: &mut State) -> Result<()> {
    if std::env::var_os("DISPLAY").is_some() {
        x11::init_backend(event_loop, state)
    } else {
        unimplemented!("Currently this runs only nested")
    }
}
