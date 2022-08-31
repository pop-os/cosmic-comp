// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{kms::KmsState, winit::WinitState, x11::X11State},
    config::{Config, OutputConfig},
    logger::LogState,
    shell::Shell,
    utils::prelude::*,
    wayland::protocols::{
        drm::WlDrmState, export_dmabuf::ExportDmabufState,
        output_configuration::OutputConfigurationState, workspace::WorkspaceClientState,
    },
};
use smithay::{
    backend::drm::DrmNode,
    input::{Seat, SeatState},
    reexports::{
        calloop::{LoopHandle, LoopSignal},
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            Display, DisplayHandle,
        },
    },
    utils::{Buffer, Size},
    wayland::{
        compositor::CompositorState,
        data_device::DataDeviceState,
        dmabuf::DmabufState,
        output::{Mode as OutputMode, Output, OutputManagerState, Scale},
        primary_selection::PrimarySelectionState,
        shm::ShmState,
        viewporter::ViewporterState,
    },
};

use std::{
    cell::RefCell,
    ffi::OsString,
    sync::{atomic::AtomicBool, Arc},
    time::Instant,
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
    pub dirty_flag: Arc<AtomicBool>,

    pub seats: Vec<Seat<State>>,
    pub last_active_seat: Seat<State>,

    pub start_time: Instant,
    pub should_stop: bool,

    pub log: LogState,
    #[cfg(feature = "debug")]
    pub egui: Egui,

    // wayland state
    pub compositor_state: CompositorState,
    pub data_device_state: DataDeviceState,
    pub dmabuf_state: DmabufState,
    pub export_dmabuf_state: ExportDmabufState,
    pub output_state: OutputManagerState,
    pub output_configuration_state: OutputConfigurationState<State>,
    pub primary_selection_state: PrimarySelectionState,
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

    pub fn schedule_render(&mut self, loop_handle: &LoopHandle<'_, Data>, output: &Output) {
        match self {
            BackendData::Winit(_) => {} // We cannot do this on the winit backend.
            // Winit has a very strict render-loop and skipping frames breaks atleast the wayland winit-backend.
            // Swapping with damage (which should be empty on these frames) is likely good enough anyway.
            BackendData::X11(ref mut state) => state.schedule_render(output),
            BackendData::Kms(ref mut state) => {
                if let Err(err) = state.schedule_render(loop_handle, output) {
                    slog_scope::crit!("Failed to schedule event, are we shutting down? {:?}", err);
                }
            }
            _ => unreachable!("No backend was initialized"),
        }
    }

    pub fn offscreen_for_output(
        &mut self,
        output: &Output,
        state: &mut Common,
    ) -> anyhow::Result<(Vec<u8>, Size<i32, Buffer>)> {
        use crate::backend::render::{render_output, AsGles2Renderer, CustomElem};
        use anyhow::Context;
        use smithay::backend::renderer::{ImportAll, Renderer};
        use smithay::desktop::space::RenderElement;
        use smithay::{
            backend::{
                drm::NodeType,
                renderer::{gles2::Gles2Renderbuffer, Bind, ExportMem, Offscreen},
            },
            utils::Rectangle,
        };

        fn capture<E, T, R>(
            gpu: Option<DrmNode>,
            renderer: &mut R,
            output: &Output,
            state: &mut Common,
        ) -> anyhow::Result<(Vec<u8>, Size<i32, Buffer>)>
        where
            E: std::error::Error + Send + Sync + 'static,
            T: Clone + 'static,
            R: Renderer<Error = E, TextureId = T>
                + ImportAll
                + AsGles2Renderer
                + Offscreen<Gles2Renderbuffer>
                + Bind<Gles2Renderbuffer>
                + ExportMem,
            CustomElem: RenderElement<R>,
        {
            let size = output
                .geometry()
                .size
                .to_f64()
                .to_buffer(
                    output.current_scale().fractional_scale(),
                    output.current_transform().into(),
                )
                .to_i32_round();
            let buffer = Offscreen::<Gles2Renderbuffer>::create_buffer(renderer, size)?;
            renderer.bind(buffer)?;
            render_output(
                gpu.as_ref(),
                renderer,
                0,
                state,
                output,
                false,
                #[cfg(feature = "debug")]
                None,
            )
            .map_err(|err| anyhow::anyhow!("Failed to render output: {:?}", err))?; // lifetime issue, grrr
            let mapping = renderer.copy_framebuffer(Rectangle::from_loc_and_size((0, 0), size))?;
            let data = Vec::from(renderer.map_texture(&mapping)?);

            Ok((data, size))
        }

        match self {
            BackendData::Winit(winit) => capture(None, winit.backend.renderer(), output, state),
            BackendData::X11(x11) => capture(None, &mut x11.renderer, output, state),
            BackendData::Kms(kms) => {
                let node = kms
                    .target_node_for_output(output)
                    .unwrap_or(kms.primary)
                    .node_with_type(NodeType::Render)
                    .with_context(|| "Unable to find node")??;
                capture(
                    Some(node),
                    &mut kms.api.renderer::<Gles2Renderbuffer>(&node, &node)?,
                    output,
                    state,
                )
            }
            BackendData::Unset => unreachable!(),
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
        let export_dmabuf_state = ExportDmabufState::new::<Self, _>(
            dh,
            //|client| client.get_data::<ClientState>().unwrap().privileged,
            |_| true,
        );
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(dh);
        let output_configuration_state = OutputConfigurationState::new(dh, |_| true);
        let primary_selection_state = PrimarySelectionState::new::<Self, _>(dh, None);
        let shm_state = ShmState::new::<Self, _>(dh, vec![], None);
        let mut seat_state = SeatState::<Self>::new();
        let viewporter_state = ViewporterState::new::<Self, _>(dh, None);
        let wl_drm_state = WlDrmState;

        let shell = Shell::new(&config, dh);
        let initial_seat = crate::input::add_seat(dh, &mut seat_state, &config, "seat-0".into());

        #[cfg(not(feature = "debug"))]
        let dirty_flag = Arc::new(AtomicBool::new(false));
        #[cfg(feature = "debug")]
        let dirty_flag = log.dirty_flag.clone();

        State {
            common: Common {
                config,
                socket,
                display_handle: dh.clone(),
                event_loop_handle: handle,
                event_loop_signal: signal,

                shell,
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

                compositor_state,
                data_device_state,
                dmabuf_state,
                export_dmabuf_state,
                shm_state,
                seat_state,
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
                                .target_node_for_output(&active_output(
                                    &self.common.last_active_seat,
                                    &self.common,
                                ))
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
