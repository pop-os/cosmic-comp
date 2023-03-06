// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    config::{Config, OutputConfig},
    shell::{layout::floating::SeatMoveGrabState, Shell},
    utils::prelude::*,
    wayland::protocols::{
        drm::WlDrmState,
        output_configuration::OutputConfigurationState,
        screencopy::{BufferParams, ScreencopyState, Session as ScreencopySession},
        workspace::WorkspaceClientState,
    },
    xwayland::XWaylandState,
};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_manager_v1::CursorMode;
#[cfg(feature = "debug")]
use smithay::utils::Rectangle;
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            element::{default_primary_scanout_output_compare, RenderElementStates},
            glow::GlowRenderer,
        },
    },
    desktop::utils::{
        send_frames_surface_tree, surface_presentation_feedback_flags_from_states,
        surface_primary_scanout_output, take_presentation_feedback_surface_tree,
        update_surface_primary_scanout_output, with_surfaces_surface_tree,
        OutputPresentationFeedback,
    },
    input::{pointer::CursorImageStatus, Seat, SeatState},
    output::{Mode as OutputMode, Output, Scale},
    reexports::{
        calloop::{LoopHandle, LoopSignal},
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration_manager::Mode,
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            protocol::wl_shm,
            Display, DisplayHandle,
        },
    },
    utils::{Clock, IsAlive, Monotonic},
    wayland::{
        compositor::CompositorState,
        data_device::DataDeviceState,
        dmabuf::DmabufState,
        keyboard_shortcuts_inhibit::KeyboardShortcutsInhibitState,
        output::OutputManagerState,
        presentation::PresentationState,
        primary_selection::PrimarySelectionState,
        shell::{kde::decoration::KdeDecorationState, xdg::decoration::XdgDecorationState},
        shm::ShmState,
        viewporter::ViewporterState,
    },
};
use tracing::error;

use std::{cell::RefCell, collections::HashMap, ffi::OsString, time::Duration};
use std::{collections::VecDeque, time::Instant};

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

    pub clock: Clock<Monotonic>,
    pub should_stop: bool,

    #[cfg(feature = "debug")]
    pub egui: Egui,

    // wayland state
    pub compositor_state: CompositorState,
    pub data_device_state: DataDeviceState,
    pub dmabuf_state: DmabufState,
    pub keyboard_shortcuts_inhibit_state: KeyboardShortcutsInhibitState,
    pub output_state: OutputManagerState,
    pub output_configuration_state: OutputConfigurationState<State>,
    pub presentation_state: PresentationState,
    pub primary_selection_state: PrimarySelectionState,
    pub screencopy_state: ScreencopyState,
    pub seat_state: SeatState<State>,
    pub shm_state: ShmState,
    pub wl_drm_state: WlDrmState,
    pub viewporter_state: ViewporterState,
    pub kde_decoration_state: KdeDecorationState,
    pub xdg_decoration_state: XdgDecorationState,

    // xwayland state
    pub xwayland_state: HashMap<Option<DrmNode>, XWaylandState>,
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
        seats: impl Iterator<Item = Seat<State>>,
        loop_handle: &LoopHandle<'_, Data>,
    ) -> Result<(), anyhow::Error> {
        let result = match self {
            BackendData::Kms(ref mut state) => {
                state.apply_config_for_output(output, seats, shell, test_only, loop_handle)
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
                if let Err(err) = state.schedule_render(loop_handle, output, None, screencopy) {
                    error!(?err, "Failed to schedule event, are we shutting down?");
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
    ) -> State {
        let clock = Clock::new().expect("Failed to initialize clock");
        let config = Config::load();
        let compositor_state = CompositorState::new::<Self>(dh);
        let data_device_state = DataDeviceState::new::<Self>(dh);
        let dmabuf_state = DmabufState::new();
        let keyboard_shortcuts_inhibit_state = KeyboardShortcutsInhibitState::new::<Self>(dh);
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(dh);
        let output_configuration_state = OutputConfigurationState::new(dh, |_| true);
        let presentation_state = PresentationState::new::<Self>(dh, clock.id() as u32);
        let primary_selection_state = PrimarySelectionState::new::<Self>(dh);
        let screencopy_state = ScreencopyState::new::<Self, _, _>(
            dh,
            vec![CursorMode::Embedded, CursorMode::Hidden],
            |_| true,
        ); // TODO: privileged
        let shm_state =
            ShmState::new::<Self>(dh, vec![wl_shm::Format::Xbgr8888, wl_shm::Format::Abgr8888]);
        let seat_state = SeatState::<Self>::new();
        let viewporter_state = ViewporterState::new::<Self>(dh);
        let wl_drm_state = WlDrmState;
        let kde_decoration_state = KdeDecorationState::new::<Self>(&dh, Mode::Client);
        let xdg_decoration_state = XdgDecorationState::new::<Self>(&dh);

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

                clock,
                should_stop: false,

                #[cfg(feature = "debug")]
                egui: Egui {
                    active: false,
                    state: smithay_egui::EguiState::new(Rectangle::from_loc_and_size(
                        (0, 0),
                        (800, 600),
                    )),
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
                presentation_state,
                primary_selection_state,
                viewporter_state,
                wl_drm_state,
                kde_decoration_state,
                xdg_decoration_state,

                xwayland_state: HashMap::new(),
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
        let time = self.clock.now();
        let throttle = Some(Duration::from_secs(1));

        for seat in self.seats.iter() {
            if &seat.active_output() == output {
                let cursor_status = seat
                    .user_data()
                    .get::<RefCell<CursorImageStatus>>()
                    .map(|cell| {
                        let mut cursor_status = cell.borrow_mut();
                        if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                            if !surface.alive() {
                                *cursor_status = CursorImageStatus::Default;
                            }
                        }
                        cursor_status.clone()
                    })
                    .unwrap_or(CursorImageStatus::Default);

                if let CursorImageStatus::Surface(wl_surface) = cursor_status {
                    send_frames_surface_tree(
                        &wl_surface,
                        output,
                        time,
                        Some(Duration::ZERO),
                        |_, _| None,
                    )
                }

                if let Some(move_grab) = seat.user_data().get::<SeatMoveGrabState>() {
                    if let Some(grab_state) = move_grab.borrow().as_ref() {
                        grab_state.send_frames(
                            output,
                            time,
                            throttle,
                            surface_primary_scanout_output,
                        );
                    }
                }
            }
        }

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
                    );
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

        self.shell.override_redirect_windows.iter().for_each(|or| {
            if let Some(wl_surface) = or.wl_surface() {
                with_surfaces_surface_tree(&wl_surface, |surface, states| {
                    update_surface_primary_scanout_output(
                        surface,
                        output,
                        states,
                        render_element_states,
                        default_primary_scanout_output_compare,
                    );
                });
                send_frames_surface_tree(
                    &wl_surface,
                    output,
                    time,
                    throttle,
                    surface_primary_scanout_output,
                )
            }
        });

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.with_surfaces(|surface, states| {
                update_surface_primary_scanout_output(
                    surface,
                    output,
                    states,
                    render_element_states,
                    default_primary_scanout_output_compare,
                );
            });
            layer_surface.send_frame(output, time, throttle, surface_primary_scanout_output);
        }
    }

    pub fn take_presentation_feedback(
        &self,
        output: &Output,
        render_element_states: &RenderElementStates,
    ) -> OutputPresentationFeedback {
        let mut output_presentation_feedback = OutputPresentationFeedback::new(output);

        let active = self.shell.active_space(output);
        active.mapped().for_each(|mapped| {
            mapped.active_window().take_presentation_feedback(
                &mut output_presentation_feedback,
                surface_primary_scanout_output,
                |surface, _| {
                    surface_presentation_feedback_flags_from_states(surface, render_element_states)
                },
            );
        });

        self.shell.override_redirect_windows.iter().for_each(|or| {
            if let Some(wl_surface) = or.wl_surface() {
                take_presentation_feedback_surface_tree(
                    &wl_surface,
                    &mut output_presentation_feedback,
                    surface_primary_scanout_output,
                    |surface, _| {
                        surface_presentation_feedback_flags_from_states(
                            surface,
                            render_element_states,
                        )
                    },
                )
            }
        });

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.take_presentation_feedback(
                &mut output_presentation_feedback,
                surface_primary_scanout_output,
                |surface, _| {
                    surface_presentation_feedback_flags_from_states(surface, render_element_states)
                },
            );
        }

        output_presentation_feedback
    }
}

#[cfg(feature = "debug")]
pub struct Egui {
    pub active: bool,
    pub state: smithay_egui::EguiState,
}

pub struct Fps {
    #[cfg(feature = "debug")]
    pub rd: Option<renderdoc::RenderDoc<renderdoc::V110>>,
    #[cfg(feature = "debug")]
    pub state: smithay_egui::EguiState,
    pending_frame: Option<PendingFrame>,
    pub frames: VecDeque<Frame>,
}

#[derive(Debug)]
struct PendingFrame {
    start: Instant,
    duration_elements: Option<Duration>,
    duration_render: Option<Duration>,
    duration_screencopy: Option<Duration>,
    duration_displayed: Option<Duration>,
}

#[derive(Debug)]
pub struct Frame {
    pub start: Instant,
    pub duration_elements: Duration,
    pub duration_render: Duration,
    pub duration_screencopy: Option<Duration>,
    pub duration_displayed: Duration,
}

impl Frame {
    fn render_time(&self) -> Duration {
        self.duration_elements + self.duration_render
    }

    fn frame_time(&self) -> Duration {
        self.duration_elements
            + self.duration_render
            + self.duration_screencopy.clone().unwrap_or(Duration::ZERO)
    }

    fn time_to_display(&self) -> Duration {
        self.duration_elements
            + self.duration_render
            + self.duration_screencopy.clone().unwrap_or(Duration::ZERO)
            + self.duration_displayed
    }
}

impl From<PendingFrame> for Frame {
    fn from(pending: PendingFrame) -> Self {
        Frame {
            start: pending.start,
            duration_elements: pending.duration_elements.unwrap_or(Duration::ZERO),
            duration_render: pending.duration_render.unwrap_or(Duration::ZERO),
            duration_screencopy: pending.duration_screencopy,
            duration_displayed: pending.duration_displayed.unwrap_or(Duration::ZERO),
        }
    }
}

impl Fps {
    const WINDOW_SIZE: usize = 360;

    pub fn start(&mut self) {
        self.pending_frame = Some(PendingFrame {
            start: Instant::now(),
            duration_elements: None,
            duration_render: None,
            duration_screencopy: None,
            duration_displayed: None,
        });
    }

    pub fn elements(&mut self) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.duration_elements = Some(Instant::now().duration_since(frame.start));
        }
    }

    pub fn render(&mut self) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.duration_render = Some(
                Instant::now().duration_since(frame.start)
                    - frame.duration_elements.clone().unwrap_or(Duration::ZERO),
            );
        }
    }

    pub fn screencopy(&mut self) {
        if let Some(frame) = self.pending_frame.as_mut() {
            frame.duration_screencopy = Some(
                Instant::now().duration_since(frame.start)
                    - frame.duration_elements.clone().unwrap_or(Duration::ZERO)
                    - frame.duration_render.clone().unwrap_or(Duration::ZERO),
            );
        }
    }

    pub fn displayed(&mut self) {
        if let Some(mut frame) = self.pending_frame.take() {
            frame.duration_displayed = Some(
                Instant::now().duration_since(frame.start)
                    - frame.duration_elements.clone().unwrap_or(Duration::ZERO)
                    - frame.duration_render.clone().unwrap_or(Duration::ZERO)
                    - frame.duration_screencopy.clone().unwrap_or(Duration::ZERO),
            );

            self.frames.push_back(frame.into());
            while self.frames.len() > Fps::WINDOW_SIZE {
                self.frames.pop_front();
            }
        }
    }

    pub fn max_frametime(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.frame_time())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_frametime(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.frame_time())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn max_time_to_display(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.time_to_display())
            .max()
            .unwrap_or(Duration::ZERO)
    }

    pub fn min_time_to_display(&self) -> Duration {
        self.frames
            .iter()
            .map(|f| f.time_to_display())
            .min()
            .unwrap_or(Duration::ZERO)
    }

    pub fn avg_frametime(&self) -> Duration {
        if self.frames.is_empty() {
            return Duration::ZERO;
        }
        self.frames.iter().map(|f| f.frame_time()).sum::<Duration>() / (self.frames.len() as u32)
    }

    pub fn avg_rendertime(&self, window: usize) -> Duration {
        self.frames
            .iter()
            .take(window)
            .map(|f| f.render_time())
            .sum::<Duration>()
            / window as u32
    }

    pub fn avg_fps(&self) -> f64 {
        if self.frames.is_empty() {
            return 0.0;
        }
        let secs = match (self.frames.front(), self.frames.back()) {
            (Some(Frame { start, .. }), Some(end_frame)) => {
                end_frame.start.duration_since(*start) + end_frame.frame_time()
            }
            _ => Duration::ZERO,
        }
        .as_secs_f64();
        1.0 / (secs / self.frames.len() as f64)
    }
}

#[cfg(feature = "debug")]
static INTEL_LOGO: &'static [u8] = include_bytes!("../resources/icons/intel.svg");
#[cfg(feature = "debug")]
static AMD_LOGO: &'static [u8] = include_bytes!("../resources/icons/amd.svg");
#[cfg(feature = "debug")]
static NVIDIA_LOGO: &'static [u8] = include_bytes!("../resources/icons/nvidia.svg");

impl Fps {
    pub fn new(_renderer: &mut GlowRenderer) -> Fps {
        #[cfg(feature = "debug")]
        let state = {
            let state = smithay_egui::EguiState::new(smithay::utils::Rectangle::from_loc_and_size(
                (0, 0),
                (400, 800),
            ));
            let mut visuals: egui::style::Visuals = Default::default();
            visuals.window_shadow.extrusion = 0.0;
            state.context().set_visuals(visuals);
            state
                .load_svg(_renderer, String::from("intel"), INTEL_LOGO)
                .unwrap();
            state
                .load_svg(_renderer, String::from("amd"), AMD_LOGO)
                .unwrap();
            state
                .load_svg(_renderer, String::from("nvidia"), NVIDIA_LOGO)
                .unwrap();
            state
        };

        Fps {
            #[cfg(feature = "debug")]
            state,
            #[cfg(feature = "debug")]
            rd: renderdoc::RenderDoc::new().ok(),
            pending_frame: None,
            frames: VecDeque::with_capacity(Fps::WINDOW_SIZE + 1),
        }
    }
}

pub fn avg_fps<'a>(iter: impl Iterator<Item = &'a Duration>) -> f64 {
    let sum_secs = iter.map(|d| d.as_secs_f64()).sum::<f64>();
    1.0 / (sum_secs / Fps::WINDOW_SIZE as f64)
}
