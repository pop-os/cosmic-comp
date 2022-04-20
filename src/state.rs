// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    config::{Config, OutputConfig},
    logger::LogState,
    shell::{init_shell, Shell},
};
use smithay::{
    reexports::{
        calloop::LoopHandle,
        wayland_server::{protocol::wl_surface::WlSurface, Display},
    },
    wayland::{
        data_device::{default_action_chooser, init_data_device, DataDeviceEvent},
        output::{
            wlr_configuration::{
                self, init_wlr_output_configuration, ConfigurationManager, ModeConfiguration,
            },
            xdg::init_xdg_output_manager,
            Mode as OutputMode, Output, Scale,
        },
        seat::Seat,
        shell::xdg::ToplevelSurface,
        shm::init_shm_global,
    },
};

use std::{
    cell::RefCell,
    ffi::OsString,
    rc::Rc,
    sync::{atomic::AtomicBool, Arc},
    time::Instant,
};
#[cfg(feature = "debug")]
use std::{collections::VecDeque, time::Duration};

pub struct State {
    pub backend: BackendData,
    pub common: Common,
}

pub struct Common {
    pub config: Config,

    pub display: Rc<RefCell<Display>>,
    pub socket: OsString,
    pub event_loop_handle: LoopHandle<'static, State>,

    pub output_conf: ConfigurationManager,
    pub shell: Shell,
    pub pending_toplevels: Vec<ToplevelSurface>,
    pub dirty_flag: Arc<AtomicBool>,

    pub seats: Vec<Seat>,
    pub last_active_seat: Seat,

    pub start_time: Instant,
    pub should_stop: bool,

    pub log: LogState,
    #[cfg(feature = "debug")]
    pub egui: Egui,
}

#[cfg(feature = "debug")]
pub struct Egui {
    pub debug_state: smithay_egui::EguiState,
    pub log_state: smithay_egui::EguiState,
    pub modifiers: smithay::wayland::seat::ModifiersState,
    pub active: bool,
    pub alpha: f32,
}

#[cfg(feature = "debug")]
pub struct Fps {
    pub state: smithay_egui::EguiState,
    pub modifiers: smithay::wayland::seat::ModifiersState,
    pub frames: VecDeque<(Instant, Duration)>,
    pub start: Instant,
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

    pub fn apply_config_for_output(
        &mut self,
        output: &Output,
        test_only: bool,
        shell: &mut Shell,
    ) -> Result<(), anyhow::Error> {
        let result = match self {
            BackendData::Kms(ref mut state) => {
                state.apply_config_for_output(output, shell, test_only)
            }
            BackendData::Winit(ref mut state) => state.apply_config_for_output(output, test_only),
            BackendData::X11(ref mut state) => state.apply_config_for_output(output, test_only),
            _ => unreachable!("No backend set when applying output config"),
        };

        if result.is_ok() {
            // apply to Output
            let final_config = output
                .user_data()
                .get::<RefCell<OutputConfig>>()
                .unwrap()
                .borrow();
            let mode = Some(OutputMode {
                size: final_config.mode_size(),
                refresh: final_config.mode_refresh() as i32,
            })
            .filter(|m| match output.current_mode() {
                None => true,
                Some(c_m) => m.size != c_m.size || m.refresh != c_m.refresh,
            });
            let transform =
                Some(final_config.transform.into()).filter(|x| *x != output.current_transform());
            let scale =
                Some(final_config.scale).filter(|x| *x != output.current_scale().fractional_scale());
            let location =
                Some(final_config.position.into()).filter(|x| *x != output.current_location());
            output.change_current_state(mode, transform, scale.map(Scale::Fractional), location);
        }

        result
    }

    pub fn schedule_render(&mut self, output: &Output) {
        match self {
            BackendData::Winit(_) => {} // We cannot do this on the winit backend.
            // Winit has a very strict render-loop and skipping frames breaks atleast the wayland winit-backend.
            // Swapping with damage (which should be empty on these frames) is likely good enough anyway.
            BackendData::X11(ref mut state) => state.schedule_render(output),
            BackendData::Kms(ref mut state) => state.schedule_render(output),
            _ => unreachable!("No backend was initialized"),
        }
    }
}

struct DnDIcon {
    surface: RefCell<Option<WlSurface>>,
}

pub fn get_dnd_icon(seat: &Seat) -> Option<WlSurface> {
    let userdata = seat.user_data();
    userdata
        .get::<DnDIcon>()
        .and_then(|x| x.surface.borrow().clone())
}

impl State {
    pub fn new(
        mut display: Display,
        socket: OsString,
        handle: LoopHandle<'static, State>,
        log: LogState,
    ) -> State {
        let config = Config::load();
        init_shm_global(&mut display, vec![], None);
        init_xdg_output_manager(&mut display, None);
        let shell = init_shell(&config, &mut display);
        let initial_seat = crate::input::add_seat(&mut display, "seat-0".into());
        init_data_device(
            &mut display,
            |dnd_event| match dnd_event {
                DataDeviceEvent::DnDStarted { icon, seat, .. } => {
                    let user_data = seat.user_data();
                    user_data.insert_if_missing(|| DnDIcon {
                        surface: RefCell::new(None),
                    });
                    *user_data.get::<DnDIcon>().unwrap().surface.borrow_mut() = icon;
                }
                DataDeviceEvent::DnDDropped { seat } => {
                    seat.user_data()
                        .get::<DnDIcon>()
                        .unwrap()
                        .surface
                        .borrow_mut()
                        .take();
                }
                _ => {}
            },
            default_action_chooser,
            None,
        );
        let (output_conf, _) = init_wlr_output_configuration(
            &mut display,
            |_| true,
            |conf, test_only, mut ddata| {
                let state = ddata.get::<State>().unwrap();
                if conf.iter().all(|(_, conf)| conf.is_none()) {
                    return false; // we don't allow the user to accidentally disable all their outputs
                }

                let mut backups = Vec::new();
                for (output, conf) in &conf {
                    {
                        let mut current_config = output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow_mut();
                        backups.push((output, current_config.clone()));

                        if let Some(conf) = conf {
                            match conf.mode {
                                Some(ModeConfiguration::Mode(mode)) => {
                                    current_config.mode =
                                        ((mode.size.w, mode.size.h), Some(mode.refresh as u32));
                                }
                                Some(ModeConfiguration::Custom { size, refresh }) => {
                                    current_config.mode =
                                        ((size.w, size.h), refresh.map(|x| x as u32));
                                }
                                _ => {}
                            }
                            if let Some(scale) = conf.scale {
                                current_config.scale = scale;
                            }
                            if let Some(transform) = conf.transform {
                                current_config.transform = transform;
                            }
                            if let Some(position) = conf.position {
                                current_config.position = position.into();
                            }
                            current_config.enabled = true;
                        } else {
                            current_config.enabled = false;
                        }
                    }

                    if let Err(err) = state.backend.apply_config_for_output(
                        output,
                        test_only,
                        &mut state.common.shell,
                    ) {
                        slog_scope::warn!(
                            "Failed to apply config to {}: {}. Resetting",
                            output.name(),
                            err
                        );
                        for (output, backup) in backups {
                            {
                                let mut current_config = output
                                    .user_data()
                                    .get::<RefCell<OutputConfig>>()
                                    .unwrap()
                                    .borrow_mut();
                                *current_config = backup;
                            }
                            if !test_only {
                                if let Err(err) = state.backend.apply_config_for_output(
                                    output,
                                    false,
                                    &mut state.common.shell,
                                ) {
                                    slog_scope::error!(
                                        "Failed to reset output config for {}: {}",
                                        output.name(),
                                        err
                                    );
                                }
                            }
                        }
                        return false;
                    }

                }

                for output in conf.iter().filter(|(_, c)| c.is_some()).map(|(o, _)| o) {
                    wlr_configuration::enable_head(output);
                }
                for output in conf.iter().filter(|(_, c)| c.is_none()).map(|(o, _)| o) {
                    wlr_configuration::disable_head(output);
                }
                state.common.config.write_outputs(state.common.output_conf.outputs());
                state.common.event_loop_handle.insert_idle(move |state| {
                    state
                        .common
                        .output_conf
                        .update(&mut *state.common.display.borrow_mut());
                });

                true
            },
            None,
        );

        #[cfg(not(feature = "debug"))]
        let dirty_flag = Arc::new(AtomicBool::new(false));
        #[cfg(feature = "debug")]
        let dirty_flag = log.dirty_flag.clone();

        State {
            common: Common {
                config,
                display: Rc::new(RefCell::new(display)),
                socket,
                event_loop_handle: handle,

                output_conf,
                shell,
                pending_toplevels: Vec::new(),
                dirty_flag,

                seats: vec![initial_seat.clone()],
                last_active_seat: initial_seat,

                start_time: Instant::now(),
                should_stop: false,

                log,
                #[cfg(feature = "debug")]
                egui: Egui {
                    debug_state: smithay_egui::EguiState::new(smithay_egui::EguiMode::Continuous),
                    log_state: {
                        let mut state =
                            smithay_egui::EguiState::new(smithay_egui::EguiMode::Continuous);
                        state.set_zindex(0);
                        state
                    },
                    modifiers: Default::default(),
                    active: false,
                    alpha: 1.0,
                },
            },
            backend: BackendData::Unset,
        }
    }

    pub fn destroy(self) -> LogState {
        self.common.log
    }
}

#[cfg(feature = "debug")]
impl Fps {
    const WINDOW_SIZE: usize = 100;

    pub fn start(&mut self) {
        self.start = Instant::now();
    }

    pub fn end(&mut self) {
        let frame_time = Instant::now().duration_since(self.start);

        self.frames.push_back((self.start, frame_time));
        if self.frames.len() > Fps::WINDOW_SIZE {
            self.frames.pop_front();
        }
    }

    pub fn max_frametime(&self) -> &Duration {
        self.frames
            .iter()
            .map(|(_, f)| f)
            .max()
            .unwrap_or(&Duration::ZERO)
    }

    pub fn min_frametime(&self) -> &Duration {
        self.frames
            .iter()
            .map(|(_, f)| f)
            .min()
            .unwrap_or(&Duration::ZERO)
    }

    pub fn avg_frametime(&self) -> Duration {
        if self.frames.is_empty() {
            return Duration::ZERO;
        }
        self.frames
            .iter()
            .map(|(_, f)| f)
            .cloned()
            .sum::<Duration>()
            / (self.frames.len() as u32)
    }

    pub fn avg_fps(&self) -> f64 {
        if self.frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.frames.front(), self.frames.back()) {
            (Some((start, _)), Some((end, dur))) => end.duration_since(*start) + *dur,
            _ => Duration::ZERO,
        }
        .as_secs_f64();
        1.0 / (secs / self.frames.len() as f64)
    }
}

#[cfg(feature = "debug")]
impl Default for Fps {
    fn default() -> Fps {
        Fps {
            state: {
                let mut state = smithay_egui::EguiState::new(smithay_egui::EguiMode::Continuous);
                let mut visuals: egui::style::Visuals = Default::default();
                visuals.window_shadow.extrusion = 0.0;
                state.context().set_visuals(visuals);
                state.set_zindex(110); // always render on top
                state
            },
            modifiers: Default::default(),
            frames: VecDeque::with_capacity(Fps::WINDOW_SIZE + 1),
            start: Instant::now(),
        }
    }
}

#[cfg(feature = "debug")]
pub fn avg_fps<'a>(iter: impl Iterator<Item = &'a Duration>) -> f64 {
    let sum_secs = iter.map(|d| d.as_secs_f64()).sum::<f64>();
    1.0 / (sum_secs / Fps::WINDOW_SIZE as f64)
}
