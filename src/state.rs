// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::x11::X11State,
    shell::{init_shell, workspaces::Workspaces, ShellStates},
};
use smithay::{
    reexports::wayland_server::Display,
    wayland::{
        data_device::{default_action_chooser, init_data_device},
        output::xdg::init_xdg_output_manager,
        seat::Seat,
        shell::xdg::ToplevelSurface,
        shm::init_shm_global,
    },
};
use std::{cell::RefCell, rc::Rc, time::Instant};

pub struct State {
    pub common: Common,
    pub backend: BackendData,
}

pub struct Common {
    pub display: Rc<RefCell<Display>>,

    pub spaces: Workspaces,
    pub shell: ShellStates,
    pub pending_toplevels: Vec<ToplevelSurface>,

    pub seats: Vec<Seat>,
    pub last_active_seat: Seat,

    pub start_time: Instant,
    pub should_stop: bool,
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
    pub fn new(mut display: Display) -> State {
        init_shm_global(&mut display, vec![], None);
        init_xdg_output_manager(&mut display, None);
        let shell_handles = init_shell(&mut display);
        let initial_seat = crate::input::add_seat(&mut display, "seat-0".into());
        init_data_device(
            &mut display,
            |_dnd_event| { /* TODO */ },
            default_action_chooser,
            None,
        );

        State {
            common: Common {
                display: Rc::new(RefCell::new(display)),

                spaces: Workspaces::new(),
                shell: shell_handles,
                pending_toplevels: Vec::new(),

                seats: vec![initial_seat.clone()],
                last_active_seat: initial_seat,

                start_time: Instant::now(),
                should_stop: false,
            },
            backend: BackendData::Unset,
        }
    }
}
