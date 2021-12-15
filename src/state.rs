// SPDX-License-Identifier: GPL-3.0-only

use std::{cell::RefCell, rc::Rc};

use smithay::reexports::wayland_server::Display;

pub struct State {
    pub display: Rc<RefCell<Display>>,
    pub should_stop: bool,
}

impl State {
    pub fn new(display: Display) -> State {
        State {
            display: Rc::new(RefCell::new(display)),
            should_stop: false,
        }
    }
}
