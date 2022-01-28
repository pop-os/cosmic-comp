// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    shell::{init_shell, workspaces::Workspaces, ShellStates},
};
use smithay::{
    reexports::{
        calloop::LoopHandle,
        wayland_server::Display,
    },
    wayland::{
        data_device::{default_action_chooser, init_data_device},
        output::{xdg::init_xdg_output_manager, Output},
        seat::Seat,
        shell::xdg::ToplevelSurface,
        shm::init_shm_global,
    },
};

use std::{cell::RefCell, rc::Rc, time::Instant};
#[cfg(feature = "debug")]
use std::{collections::VecDeque, time::Duration};

pub struct State {
    pub common: Common,
    pub backend: BackendData,
}

pub struct Common {
    pub display: Rc<RefCell<Display>>,
    pub event_loop_handle: LoopHandle<'static, State>,

    pub spaces: Workspaces,
    pub shell: ShellStates,
    pub pending_toplevels: Vec<ToplevelSurface>,

    pub seats: Vec<Seat>,
    pub last_active_seat: Seat,

    pub start_time: Instant,
    pub should_stop: bool,

    #[cfg(feature = "debug")]
    pub egui: Egui,
}

#[cfg(feature = "debug")]
pub struct Egui {
    pub state: smithay_egui::EguiState,
    pub modifiers: smithay::wayland::seat::ModifiersState,
    pub active: bool,
    pub alpha: f32,
    pub spaces: bool,
    pub outputs: bool,
}

#[cfg(feature = "debug")]
pub struct Fps {
    pub frames: VecDeque<Duration>,
    pub last: Instant,
}

pub enum BackendData {
    X11(X11State),
    Winit(WinitState),
    Kms(KmsState),
    // TODO
    // Wayland(WaylandState),
    Unset,
}

impl BackendData {
    pub fn kms(&mut self) -> &mut KmsState {
        match self {
            BackendData::Kms(ref mut kms_state) => kms_state,
            _ => unreachable!("Called kms in non kms backend"),
        }
    }

    pub fn x11(&mut self) -> &mut X11State {
        match self {
            BackendData::X11(ref mut x11_state) => x11_state,
            _ => unreachable!("Called x11 in non x11 backend"),
        }
    }

    pub fn winit(&mut self) -> &mut WinitState {
        match self {
            BackendData::Winit(ref mut winit_state) => winit_state,
            _ => unreachable!("Called winit in non winit backend"),
        }
    }

    pub fn schedule_render(&mut self, output: &Output) {
        match self {
            BackendData::Winit(_) => {}, // We cannot do this on the winit backend.
            // Winit has a very strict render-loop and skipping frames breaks atleast the wayland winit-backend.
            // Swapping with damage (which should be empty on these frames) is likely good enough anyway.
            BackendData::X11(ref mut state) => state.schedule_render(output),
            BackendData::Kms(ref mut state) => state.schedule_render(output),
            _ => unreachable!("No backend was initialized"),
        }
    }
}

impl State {
    pub fn new(mut display: Display, handle: LoopHandle<'static, State>) -> State {
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
                event_loop_handle: handle,

                spaces: Workspaces::new(),
                shell: shell_handles,
                pending_toplevels: Vec::new(),

                seats: vec![initial_seat.clone()],
                last_active_seat: initial_seat,

                start_time: Instant::now(),
                should_stop: false,

                #[cfg(feature = "debug")]
                egui: Egui {
                    state: smithay_egui::EguiState::new(),
                    modifiers: Default::default(),
                    active: false,
                    alpha: 1.0,
                    outputs: false,
                    spaces: false,
                },
            },
            backend: BackendData::Unset,
        }
    }
}

#[cfg(feature = "debug")]
impl Fps {
    const WINDOW_SIZE: usize = 100;

    pub fn tick(&mut self) {
        let next = Instant::now();
        let frame_time = next.duration_since(self.last);

        self.frames.push_back(frame_time);
        if self.frames.len() > Fps::WINDOW_SIZE {
            self.frames.pop_front();
        }
        self.last = next;
    }

    pub fn max_frametime(&self) -> &Duration {
        self.frames.iter().max().unwrap_or(&Duration::ZERO)
    }

    pub fn min_frametime(&self) -> &Duration {
        self.frames.iter().min().unwrap_or(&Duration::ZERO)
    }

    pub fn avg_frametime(&self) -> Duration {
        if self.frames.is_empty() {
            return Duration::ZERO;
        }
        self.frames.iter().cloned().sum::<Duration>() / (self.frames.len() as u32)
    }

    pub fn avg_fps(&self) -> f64 {
        if self.frames.is_empty() {
            return 0.0;
        }
        let sum_secs = self.frames.iter().map(|d| d.as_secs_f64()).sum::<f64>();
        1.0 / (sum_secs / self.frames.len() as f64)
    }
}

#[cfg(feature = "debug")]
impl Default for Fps {
    fn default() -> Fps {
        Fps {
            frames: VecDeque::with_capacity(Fps::WINDOW_SIZE + 1),
            last: Instant::now(),
        }
    }
}

#[cfg(feature = "debug")]
pub fn avg_fps<'a>(iter: impl Iterator<Item = &'a Duration>) -> f64 {
    let sum_secs = iter.map(|d| d.as_secs_f64()).sum::<f64>();
    1.0 / (sum_secs / Fps::WINDOW_SIZE as f64)
}
