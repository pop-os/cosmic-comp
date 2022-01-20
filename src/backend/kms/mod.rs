// SPDX-License-Identifier: GPL-3.0-only

use crate::state::State;
use anyhow::Result;
use smithay::reexports::calloop::EventLoop;

pub struct KmsState {}

pub fn init_backend(event_loop: &mut EventLoop<State>, state: &mut State) -> Result<()> {
    unimplemented!()
}
