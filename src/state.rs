// SPDX-License-Identifier: GPL-3.0-only

use crate::{backend::x11::X11State, shell::workspaces::Workspaces};
use smithay::reexports::wayland_server::Display;
use std::{cell::RefCell, rc::Rc, time::Instant};

pub struct State {
    pub display: Rc<RefCell<Display>>,
    pub spaces: Workspaces,

    pub start_time: Instant,
    pub should_stop: bool,
    pub backend: BackendData,
}

pub enum BackendData {
    X11(X11State),
    // TODO
    // Wayland(WaylandState),
    // Udev(UdevState),
    Unset,
}

impl BackendData {
    pub fn x11(&mut self) -> &mut X11State {
        match self {
            BackendData::X11(ref mut x11_state) => x11_state,
            _ => unreachable!("Called x11 in non x11 backend"),
        }
    }
}

impl State {
    pub fn new(display: Display) -> State {
        State {
            display: Rc::new(RefCell::new(display)),
            spaces: Workspaces::new(),

            start_time: Instant::now(),
            should_stop: false,
            backend: BackendData::Unset,
        }
    }
}
