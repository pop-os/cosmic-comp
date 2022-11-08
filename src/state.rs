// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    config::{Config, OutputConfig},
    logger::LogState,
    shell::Shell,
    utils::prelude::*,
    wayland::protocols::{
        drm::WlDrmState,
        output_configuration::OutputConfigurationState,
        screencopy::{BufferParams, ScreencopyState, Session as ScreencopySession},
        workspace::WorkspaceClientState,
    },
};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_manager_v1::CursorMode;
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::element::{default_primary_scanout_output_compare, RenderElementStates},
    },
    desktop::utils::{surface_primary_scanout_output, update_surface_primary_scanout_output},
    input::{Seat, SeatState},
    output::{Mode as OutputMode, Output, Scale},
    reexports::{
        calloop::{LoopHandle, LoopSignal},
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            Display, DisplayHandle,
        },
    },
    wayland::{
        compositor::CompositorState, data_device::DataDeviceState, dmabuf::DmabufState,
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitState, output::OutputManagerState,
        primary_selection::PrimarySelectionState, shm::ShmState, viewporter::ViewporterState,
    },
};

use std::{
    cell::RefCell,
    ffi::OsString,
    time::{Duration, Instant},
};
#[cfg(feature = "debug")]
use std::{collections::VecDeque, time::Duration};

pub struct ClientState {
    pub workspace_client_state: WorkspaceClientState,
    pub drm_node: Option<DrmNode>,
    pub privileged: bool,
}
impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}
    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {}
}

pub struct Data {
    pub display: Display<State>,
    pub state: State,
}

pub struct State {
    pub backend: BackendData,
    pub common: Common,
}

pub struct Common {
    pub config: Config,

    pub socket: OsString,
    pub display_handle: DisplayHandle,
    pub event_loop_handle: LoopHandle<'static, Data>,
    pub event_loop_signal: LoopSignal,

    //pub output_conf: ConfigurationManager,
    pub shell: Shell,

    seats: Vec<Seat<State>>,
    last_active_seat: Option<Seat<State>>,

    pub start_time: Instant,
    pub should_stop: bool,

    pub log: LogState,
    #[cfg(feature = "debug")]
    pub egui: Egui,

    // wayland state
    pub compositor_state: CompositorState,
    pub data_device_state: DataDeviceState,
    pub dmabuf_state: DmabufState,
    pub keyboard_shortcuts_inhibit_state: KeyboardShortcutsInhibitState,
    pub output_state: OutputManagerState,
    pub output_configuration_state: OutputConfigurationState<State>,
    pub primary_selection_state: PrimarySelectionState,
    pub screencopy_state: ScreencopyState,
    pub seat_state: SeatState<State>,
    pub shm_state: ShmState,
    pub wl_drm_state: WlDrmState,
    pub viewporter_state: ViewporterState,
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
        loop_handle: &LoopHandle<'_, Data>,
    ) -> Result<(), anyhow::Error> {
        let result = match self {
            BackendData::Kms(ref mut state) => {
                state.apply_config_for_output(output, shell, test_only, loop_handle)
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
            let scale = Some(final_config.scale)
                .filter(|x| *x != output.current_scale().fractional_scale());
            let location =
                Some(final_config.position.into()).filter(|x| *x != output.current_location());
            output.change_current_state(mode, transform, scale.map(Scale::Fractional), location);
        }

        result
    }

    pub fn schedule_render(
        &mut self,
        loop_handle: &LoopHandle<'_, Data>,
        output: &Output,
        screencopy: Option<Vec<(ScreencopySession, BufferParams)>>,
    ) {
        match self {
            BackendData::Winit(ref mut state) => state.pending_screencopy(screencopy), // We cannot do this on the winit backend.
            // Winit has a very strict render-loop and skipping frames breaks atleast the wayland winit-backend.
            // Swapping with damage (which should be empty on these frames) is likely good enough anyway.
            BackendData::X11(ref mut state) => state.schedule_render(output, screencopy),
            BackendData::Kms(ref mut state) => {
                if let Err(err) = state.schedule_render(loop_handle, output, screencopy) {
                    slog_scope::crit!("Failed to schedule event, are we shutting down? {:?}", err);
                }
            }
            _ => unreachable!("No backend was initialized"),
        }
    }
}

impl State {
    pub fn new(
        dh: &DisplayHandle,
        socket: OsString,
        handle: LoopHandle<'static, Data>,
        signal: LoopSignal,
        log: LogState,
    ) -> State {
        let config = Config::load();
        let compositor_state = CompositorState::new::<Self, _>(dh, None);
        let data_device_state = DataDeviceState::new::<Self, _>(dh, None);
        let dmabuf_state = DmabufState::new();
        let keyboard_shortcuts_inhibit_state = KeyboardShortcutsInhibitState::new::<Self>(dh);
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(dh);
        let output_configuration_state = OutputConfigurationState::new(dh, |_| true);
        let primary_selection_state = PrimarySelectionState::new::<Self, _>(dh, None);
        let screencopy_state = ScreencopyState::new::<Self, _, _>(
            dh,
            vec![CursorMode::Embedded, CursorMode::Hidden],
            |_| true,
        ); // TODO: privileged
        let shm_state = ShmState::new::<Self, _>(dh, vec![], None);
        let seat_state = SeatState::<Self>::new();
        let viewporter_state = ViewporterState::new::<Self, _>(dh, None);
        let wl_drm_state = WlDrmState;

        let shell = Shell::new(&config, dh);

        State {
            common: Common {
                config,
                socket,
                display_handle: dh.clone(),
                event_loop_handle: handle,
                event_loop_signal: signal,

                shell,

                seats: Vec::new(),
                last_active_seat: None,

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

                compositor_state,
                data_device_state,
                dmabuf_state,
                screencopy_state,
                shm_state,
                seat_state,
                keyboard_shortcuts_inhibit_state,
                output_state,
                output_configuration_state,
                primary_selection_state,
                viewporter_state,
                wl_drm_state,
            },
            backend: BackendData::Unset,
        }
    }

    pub fn new_client_state(&self) -> ClientState {
        ClientState {
            workspace_client_state: WorkspaceClientState::default(),
            drm_node: match &self.backend {
                BackendData::Kms(kms_state) => {
                    match std::env::var("COSMIC_RENDER_AUTO_ASSIGN").map(|val| val.to_lowercase()) {
                        Ok(val) if val == "y" || val == "yes" || val == "true" => Some(
                            kms_state
                                .target_node_for_output(
                                    &self.common.last_active_seat().active_output(),
                                )
                                .unwrap_or(kms_state.primary),
                        ),
                        _ => Some(kms_state.primary),
                    }
                }
                _ => None,
            },
            privileged: false,
        }
    }

    pub fn new_client_state_with_node(&self, drm_node: DrmNode) -> ClientState {
        ClientState {
            workspace_client_state: WorkspaceClientState::default(),
            drm_node: Some(drm_node),
            privileged: false,
        }
    }

    pub fn new_privileged_client_state(&self) -> ClientState {
        ClientState {
            workspace_client_state: WorkspaceClientState::default(),
            drm_node: match &self.backend {
                BackendData::Kms(kms_state) => Some(kms_state.primary),
                _ => None,
            },
            privileged: true,
        }
    }

    pub fn destroy(self) -> LogState {
        self.common.log
    }
}

impl Common {
    pub fn add_seat(&mut self, seat: Seat<State>) {
        if self.seats.is_empty() {
            self.last_active_seat = Some(seat.clone());
        }
        self.seats.push(seat);
    }

    pub fn remove_seat(&mut self, seat: &Seat<State>) {
        self.seats.retain(|s| s != seat);
        if self.seats.is_empty() {
            self.last_active_seat = None;
        } else if self.last_active_seat() == seat {
            self.last_active_seat = Some(self.seats[0].clone());
        }
    }

    pub fn seats(&self) -> impl Iterator<Item = &Seat<State>> {
        self.seats.iter()
    }

    pub fn last_active_seat(&self) -> &Seat<State> {
        self.last_active_seat.as_ref().expect("No seat?")
    }

    pub fn send_frames(&self, output: &Output, render_element_states: &RenderElementStates) {
        let time = self.start_time.elapsed();
        let throttle = Some(Duration::from_secs(1));

        let active = self.shell.active_space(output);
        active.mapped().for_each(|mapped| {
            if active.outputs_for_element(mapped).any(|o| &o == output) {
                let window = mapped.active_window();
                window.with_surfaces(|surface, states| {
                    update_surface_primary_scanout_output(
                        surface,
                        output,
                        states,
                        render_element_states,
                        default_primary_scanout_output_compare,
                    )
                });
                window.send_frame(output, time, throttle, surface_primary_scanout_output);
            }
        });

        for space in self
            .shell
            .workspaces
            .spaces()
            .filter(|w| w.handle != active.handle)
        {
            space.mapped().for_each(|mapped| {
                if space.outputs_for_element(mapped).any(|o| &o == output) {
                    let window = mapped.active_window();
                    window.send_frame(output, time, throttle, |_, _| None);
                }
            });
        }

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.with_surfaces(|surface, states| {
                update_surface_primary_scanout_output(
                    surface,
                    output,
                    states,
                    render_element_states,
                    default_primary_scanout_output_compare,
                )
            });
            layer_surface.send_frame(output, time, throttle, surface_primary_scanout_output);
        }
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
