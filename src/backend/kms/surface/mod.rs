// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        CLEAR_COLOR, CursorMode, GlMultiError, GlMultiRenderer, PostprocessOutputConfig,
        PostprocessShader, PostprocessState,
        element::{AsGlowRenderer, CosmicElement, DamageElement},
        init_shaders, output_elements,
    },
    config::ScreenFilter,
    shell::Shell,
    state::SurfaceDmabufFeedback,
    utils::prelude::*,
    wayland::handlers::{
        compositor::recursive_frame_time_estimation,
        image_copy_capture::{FrameHolder, PendingImageCopyData, SessionData, submit_buffer},
    },
};

use anyhow::{Context, Result};
use calloop::channel::Channel;
use cosmic_comp_config::output::comp::AdaptiveSync;

use crate::dbus::game_mode::VrrMode;
use smithay::{
    backend::{
        allocator::{
            Fourcc,
            dmabuf::Dmabuf,
            format::FormatSet,
            gbm::{GbmAllocator, GbmBuffer},
        },
        drm::{
            DrmDeviceFd, DrmEventMetadata, DrmEventTime, DrmNode, VrrSupport,
            compositor::{
                BlitFrameResultError, FrameError, FrameFlags, PrimaryPlaneElement,
                RenderFrameResult,
            },
            exporter::gbm::GbmFramebufferExporter,
            gbm::GbmFramebuffer,
            output::DrmOutput,
        },
        egl::EGLContext,
        renderer::{
            Bind, Blit, BufferType, Frame, ImportDma, Offscreen, Renderer, RendererSuper, Texture,
            TextureFilter, buffer_dimensions, buffer_type,
            damage::Error as RenderError,
            element::{
                Element, Kind, RenderElementStates,
                texture::TextureRenderElement,
                utils::{
                    ConstrainAlign, ConstrainScaleBehavior, Relocate, RelocateRenderElement,
                    constrain_render_elements,
                },
            },
            gles::{
                GlesError, GlesRenderbuffer, GlesRenderer, GlesTexture, Uniform,
                element::TextureShaderElement,
            },
            glow::GlowRenderer,
            multigpu::{ApiDevice, Error as MultiError, GpuManager},
            sync::SyncPoint,
            utils::with_renderer_surface_state,
        },
    },
    desktop::utils::OutputPresentationFeedback,
    output::{Output, OutputNoMode},
    reexports::{
        calloop::{
            EventLoop, LoopHandle, RegistrationToken,
            channel::{Event, Sender, channel},
            timer::{TimeoutAction, Timer},
        },
        drm::control::{connector, crtc},
        wayland_protocols::wp::{
            linux_dmabuf::zv1::server::zwp_linux_dmabuf_feedback_v1,
            presentation_time::server::wp_presentation_feedback,
        },
        wayland_server::protocol::wl_surface::WlSurface,
    },
    utils::{Clock, Monotonic, Physical, Point, Rectangle, Transform},
    wayland::{
        dmabuf::{DmabufFeedbackBuilder, get_dmabuf},
        image_copy_capture::{
            CaptureFailureReason, Frame as ScreencopyFrame, SessionRef as ScreencopySessionRef,
        },
        presentation::Refresh,
        seat::WaylandFocus,
        shm::{shm_format_to_fourcc, with_buffer_contents},
    },
};
use tracing::{debug, error, info, trace, warn};

use crate::logger::GAMING_TARGET;

use std::{
    borrow::{Borrow, BorrowMut},
    collections::{HashMap, HashSet, hash_map},
    mem,
    sync::{
        Arc, RwLock,
        atomic::{AtomicBool, AtomicUsize, Ordering},
        mpsc::{Receiver, SyncSender},
    },
    thread::JoinHandle,
    time::Duration,
};

mod timings;
pub use self::timings::Timings;

use super::{drm_helpers, render::gles::GbmGlowBackend};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

#[derive(Debug)]
pub struct Surface {
    pub(crate) connector: connector::Handle,
    pub(super) crtc: crtc::Handle,
    pub(crate) output: Output,
    known_nodes: HashSet<DrmNode>,

    active: Arc<AtomicBool>,
    pub feedback: HashMap<DrmNode, SurfaceDmabufFeedback>,
    pub(super) primary_plane_formats: FormatSet,
    overlay_plane_formats: Option<FormatSet>,

    loop_handle: LoopHandle<'static, State>,
    thread_command: Sender<ThreadCommand>,
    thread_token: RegistrationToken,
    thread: Option<JoinHandle<()>>,
    pub perf: Arc<crate::perf::OutputPerf>,

    dpms: bool,
}

/// The outgoing session's frozen scanout, composited over the incoming one until its
/// own content is ready, then cross-faded out.
/// Outputs currently covered by an adopted handoff frame, for [`handoff_active`].
static HANDOFF_OVERLAYS: AtomicUsize = AtomicUsize::new(0);

/// Whether a handoff frame is still covering any output.
///
/// The overlay is opaque until the cross-fade ends, so the desktop beneath it is
/// live but invisible — input must not reach it.
pub fn handoff_active() -> bool {
    HANDOFF_OVERLAYS.load(Ordering::Relaxed) > 0
}

struct AdoptFrame {
    dmabuf: Dmabuf,
    texture: Option<GlesTexture>,
    /// Stable: a fresh `Id` per frame would force a full repaint every frame.
    id: smithay::backend::renderer::element::Id,
    started: std::time::Instant,
    /// `None` while held opaque; `Some` once the fade-out has begun.
    fade_start: Option<std::time::Instant>,
}

impl AdoptFrame {
    fn new(dmabuf: Dmabuf) -> Self {
        HANDOFF_OVERLAYS.fetch_add(1, Ordering::Relaxed);
        Self {
            dmabuf,
            texture: None,
            id: smithay::backend::renderer::element::Id::new(),
            started: std::time::Instant::now(),
            fade_start: None,
        }
    }
}

impl Drop for AdoptFrame {
    // Counted here rather than at the assignment sites so every path that drops the
    // frame — fade done, failed import, surface torn down — releases input.
    fn drop(&mut self) {
        HANDOFF_OVERLAYS.fetch_sub(1, Ordering::Relaxed);
    }
}

pub struct SurfaceThreadState {
    // rendering
    api: GpuManager<GbmGlowBackend<DrmDeviceFd>>,
    primary_node: Arc<RwLock<Option<DrmNode>>>,
    target_node: DrmNode,
    active: Arc<AtomicBool>,
    vrr_mode: AdaptiveSync,
    frame_flags: FrameFlags,
    compositor: Option<GbmDrmOutput>,
    adopt: Option<AdoptFrame>,

    state: QueueState,
    timings: Timings,
    perf: Arc<crate::perf::OutputPerf>,
    frame_callback_seq: usize,
    thread_sender: Sender<SurfaceCommand>,

    output: Output,
    mirroring: Option<Output>,
    screen_filter: ScreenFilter,
    postprocess_textures: HashMap<DrmNode, PostprocessState>,

    shell: Arc<parking_lot::RwLock<Shell>>,

    loop_handle: LoopHandle<'static, Self>,
    clock: Clock<Monotonic>,

    #[cfg(feature = "debug")]
    egui: EguiState,

    last_sequence: Option<u32>,
    /// Tracy frame that goes from vblank to vblank.
    vblank_frame: Option<tracy_client::Frame>,
    /// Frame name for the VBlank frame.
    vblank_frame_name: tracy_client::FrameName,
    /// Plot name for the time since presentation plot.
    time_since_presentation_plot_name: tracy_client::PlotName,
    /// Plot name for the presentation misprediction plot.
    presentation_misprediction_plot_name: tracy_client::PlotName,
    sequence_delta_plot_name: tracy_client::PlotName,

    /// Per-output frame profiler for performance analysis.
    frame_profiler: crate::backend::render::gpu_profiler::FrameProfiler,

    /// Count of consecutive render failures for backoff calculation.
    render_failure_count: u32,

    /// Rate-limit for GPU-reset recovery requests to the main thread: one reset makes
    /// every output signal every frame. Not a gate on rendering (that would risk getting
    /// stuck) — purely to avoid flooding the channel. `None` until the first request.
    last_gpu_reset_sent: Option<std::time::Instant>,

    /// True once this surface has ever had a renderer node. Distinguishes "renderer lost"
    /// (recovery needed) from "not set up yet", so a missing renderer at startup doesn't
    /// spuriously request recovery, but a renderer that vanished after a failed recovery
    /// does — keeping a still-wedged GPU under retry instead of frozen.
    has_had_node: bool,

    /// Last tearing / VRR state, for change-only `gaming`-target logs.
    last_tearing: bool,
    last_vrr: bool,
    /// Throttle for the periodic frame-pacing log (once/second while a game is
    /// fullscreen).
    last_pacing_log: Option<std::time::Instant>,
}

pub type GbmDrmOutput = DrmOutput<
    GbmAllocator<DrmDeviceFd>,
    GbmFramebufferExporter<DrmDeviceFd>,
    Option<(
        OutputPresentationFeedback,
        Receiver<PendingImageCopyData>,
        Duration,
    )>,
    DrmDeviceFd,
>;

#[derive(Debug, Default)]
pub enum QueueState {
    #[default]
    Idle,
    /// A redraw is queued.
    Queued(RegistrationToken),
    /// We submitted a frame to the KMS and waiting for it to be presented.
    WaitingForVBlank { redraw_needed: bool },
    /// We did not submit anything to KMS and made a timer to fire at the estimated VBlank.
    WaitingForEstimatedVBlank(RegistrationToken),
    /// A redraw is queued on top of the above.
    WaitingForEstimatedVBlankAndQueued {
        estimated_vblank: RegistrationToken,
        queued_render: RegistrationToken,
    },
}

#[derive(Debug)]
pub enum ThreadCommand {
    Suspend(SyncSender<()>),
    Resume {
        compositor: GbmDrmOutput,
    },
    NodeAdded {
        node: DrmNode,
        gbm: GbmAllocator<DrmDeviceFd>,
        egl: EGLContext,
        sync: SyncSender<()>,
    },
    NodeRemoved {
        node: DrmNode,
        sync: SyncSender<()>,
    },
    UpdateMirroring(Option<Output>),
    UpdateScreenFilter(ScreenFilter),
    VBlank(Option<DrmEventMetadata>),
    ScheduleRender,
    AdaptiveSyncAvailable(SyncSender<Result<VrrSupport>>),
    UseAdaptiveSync(AdaptiveSync),
    AdoptFrozenFrame(Dmabuf),
    AllowFrameFlags(bool, FrameFlags),
    End,
    DpmsOff,
}

#[derive(Debug)]
pub enum SurfaceCommand {
    SendFrames(usize),
    RenderStates(RenderElementStates),
    /// The surface hit a GPU context reset; ask the main thread (owner of the device
    /// EGL + share group) to recreate this GPU's renderers.
    GpuReset,
}

#[derive(Debug, Default)]
struct PrePostprocessData {
    states: Option<RenderElementStates>,
    texture: Option<GlesTexture>,
    cursor_texture: Option<GlesTexture>,
    cursor_geometry: Option<Rectangle<i32, Physical>>,
}

impl Surface {
    pub fn new(
        output: &Output,
        crtc: crtc::Handle,
        connector: connector::Handle,
        primary_node: Arc<RwLock<Option<DrmNode>>>,
        dev_node: DrmNode,
        target_node: DrmNode,
        evlh: &LoopHandle<'static, State>,
        screen_filter: ScreenFilter,
        shell: Arc<parking_lot::RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
    ) -> Result<Self> {
        let (tx, rx) = channel::<ThreadCommand>();
        let (tx2, rx2) = channel::<SurfaceCommand>();
        let active = Arc::new(AtomicBool::new(false));

        let active_clone = active.clone();
        let output_clone = output.clone();
        let perf = crate::perf::OutputPerf::new();
        let perf_clone = perf.clone();

        let thread = std::thread::Builder::new()
            .name(format!("surface-{}", output.name()))
            .spawn(move || {
                if let Err(err) = surface_thread(
                    output_clone,
                    primary_node,
                    target_node,
                    shell,
                    active_clone,
                    screen_filter,
                    tx2,
                    rx,
                    startup_done,
                    perf_clone,
                ) {
                    error!("Surface thread crashed: {}", err);
                }
            })
            .context("Failed to spawn surface thread")?;

        let output_clone = output.clone();
        let thread_token = evlh
            .insert_source(rx2, move |command, _, state| match command {
                Event::Msg(SurfaceCommand::SendFrames(sequence)) => {
                    if output_clone.mirroring().is_some() {
                        return;
                    }
                    state.common.send_frames(&output_clone, Some(sequence));
                }
                Event::Msg(SurfaceCommand::RenderStates(states)) => {
                    if output_clone.mirroring().is_some() {
                        return;
                    }
                    state.common.update_primary_output(&output_clone, &states);
                    let kms = state.backend.kms();
                    let surface = &mut kms
                        .drm_devices
                        .get_mut(&dev_node)
                        .unwrap()
                        .inner
                        .surfaces
                        .get_mut(&crtc)
                        .unwrap();

                    state
                        .common
                        .send_dmabuf_feedback(&output_clone, &states, |source_node| {
                            if let Some(cached_feedback) = surface.feedback.get(&source_node) {
                                Some(cached_feedback.clone())
                            } else {
                                // If we have freed the node, because it didn't have any active buffers/surfaces,
                                // we might not be able to evaluate surface feedback yet.
                                let render_formats =
                                    kms.api.single_renderer(&source_node).ok()?.dmabuf_formats();
                                // In contrast we must have the target node, if we have an active surface
                                let target_formats = kms
                                    .api
                                    .single_renderer(&target_node)
                                    .unwrap()
                                    .dmabuf_formats();
                                let feedback = get_surface_dmabuf_feedback(
                                    source_node,
                                    target_node,
                                    render_formats,
                                    target_formats,
                                    surface.primary_plane_formats.clone(),
                                    surface.overlay_plane_formats.clone(),
                                );
                                surface.feedback.insert(source_node, feedback.clone());
                                Some(feedback)
                            }
                        });
                }
                Event::Msg(SurfaceCommand::GpuReset) => {
                    warn!(node = ?dev_node, "Surface reported a GPU reset; recreating renderers");
                    state.backend.kms().recover_from_gpu_reset();
                }
                Event::Closed => {}
            })
            .map_err(|_| anyhow::anyhow!("Failed to establish channel to surface thread"))?;

        Ok(Surface {
            connector,
            crtc,
            output: output.clone(),
            known_nodes: HashSet::new(),
            active,
            feedback: HashMap::new(),
            primary_plane_formats: FormatSet::default(),
            overlay_plane_formats: None,
            loop_handle: evlh.clone(),
            thread_command: tx,
            thread_token,
            thread: Some(thread),
            perf,
            dpms: true,
        })
    }

    pub fn known_nodes(&self) -> &HashSet<DrmNode> {
        &self.known_nodes
    }

    pub fn is_active(&self) -> bool {
        self.active.load(Ordering::SeqCst)
    }

    pub fn add_node(&mut self, node: DrmNode, gbm: GbmAllocator<DrmDeviceFd>, egl: EGLContext) {
        self.known_nodes.insert(node);
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self.thread_command.send(ThreadCommand::NodeAdded {
            node,
            gbm,
            egl,
            sync: tx,
        });
        let _ = rx.recv();
    }

    pub fn remove_node(&mut self, node: DrmNode) {
        self.known_nodes.remove(&node);
        self.feedback.remove(&node);
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self
            .thread_command
            .send(ThreadCommand::NodeRemoved { node, sync: tx });
        // Block so we can be sure the file descriptor is closed
        // (which is relevant for the udev device_removed callback).
        let _ = rx.recv();
    }

    pub fn on_vblank(&self, metadata: Option<DrmEventMetadata>) {
        let _ = self.thread_command.send(ThreadCommand::VBlank(metadata));
    }

    pub fn schedule_render(&self) {
        if self.dpms {
            let _ = self.thread_command.send(ThreadCommand::ScheduleRender);
        }
    }

    /// Hand the frozen frame to the render thread (see [`AdoptFrame`]).
    pub fn adopt_frozen_frame(&self, dmabuf: Dmabuf) {
        let _ = self
            .thread_command
            .send(ThreadCommand::AdoptFrozenFrame(dmabuf));
    }

    pub fn set_mirroring(&mut self, output: Option<Output>) {
        let _ = self
            .thread_command
            .send(ThreadCommand::UpdateMirroring(output));
    }

    pub fn set_screen_filter(&mut self, config: ScreenFilter) {
        let _ = self
            .thread_command
            .send(ThreadCommand::UpdateScreenFilter(config));
    }

    pub fn adaptive_sync_support(&self) -> Result<VrrSupport> {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self
            .thread_command
            .send(ThreadCommand::AdaptiveSyncAvailable(tx));
        rx.recv().context("Surface thread died")?
    }

    pub fn use_adaptive_sync(&mut self, vrr: AdaptiveSync) {
        let _ = self
            .thread_command
            .send(ThreadCommand::UseAdaptiveSync(vrr));
    }

    pub fn allow_frame_flags(&mut self, flag: bool, flags: FrameFlags) {
        let _ = self
            .thread_command
            .send(ThreadCommand::AllowFrameFlags(flag, flags));
    }

    pub fn suspend(&mut self) {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self.thread_command.send(ThreadCommand::Suspend(tx));
        let _ = rx.recv();
    }

    pub fn resume(
        &mut self,
        compositor: GbmDrmOutput,
        primary_plane_formats: FormatSet,
        overlay_plane_formats: Option<FormatSet>,
    ) {
        self.primary_plane_formats = primary_plane_formats;
        self.overlay_plane_formats = overlay_plane_formats;
        self.feedback.clear();
        self.active.store(true, Ordering::SeqCst);
        self.dpms = true;

        let _ = self
            .thread_command
            .send(ThreadCommand::Resume { compositor });
    }

    pub fn get_dpms(&mut self) -> bool {
        self.dpms
    }

    pub fn set_dpms(&mut self, on: bool) {
        if self.dpms != on {
            self.dpms = on;
            if on {
                self.schedule_render();
            } else {
                let _ = self.thread_command.send(ThreadCommand::DpmsOff);
            }
        }
    }

    pub fn drop_and_join(mut self) {
        let thread = self.thread.take();
        std::mem::drop(self);
        if let Some(thread) = thread {
            let name = thread.thread().name().unwrap().to_string();
            let _ = thread.join();
            info!("Thread {} terminated.", name)
        }
    }
}

impl Drop for Surface {
    fn drop(&mut self) {
        let _ = self.thread_command.send(ThreadCommand::End);
        self.loop_handle.remove(self.thread_token);
        if let Some(thread) = self.thread.take() {
            let _ = thread;
            // We want to do this, but this currently deadlocks on `apply_config_for_outputs`.
            /*
                let name = thread.thread().name().unwrap().to_string();
                let _ = thread.join();
                info!("Thread {} terminated.", name)
            */
        }
    }
}

fn surface_thread(
    output: Output,
    primary_node: Arc<RwLock<Option<DrmNode>>>,
    target_node: DrmNode,
    shell: Arc<parking_lot::RwLock<Shell>>,
    active: Arc<AtomicBool>,
    screen_filter: ScreenFilter,
    thread_sender: Sender<SurfaceCommand>,
    thread_receiver: Channel<ThreadCommand>,
    startup_done: Arc<AtomicBool>,
    perf: Arc<crate::perf::OutputPerf>,
) -> Result<()> {
    let name = output.name();
    profiling::register_thread!(&format!("Surface Thread {}", name));

    let mut event_loop = EventLoop::try_new().unwrap();

    let api = GpuManager::new(GbmGlowBackend::<DrmDeviceFd>::default())
        .context("Failed to initialize rendering api")?;

    #[cfg(feature = "debug")]
    let egui = {
        let state =
            smithay_egui::EguiState::new(smithay::utils::Rectangle::from_size((400, 800).into()));
        let visuals = egui::style::Visuals {
            window_shadow: egui::Shadow::NONE,
            ..Default::default()
        };
        state.context().set_visuals(visuals);
        state
    };

    let vblank_frame_name = tracy_client::FrameName::new_leak(format!("vblank on {name}"));
    let time_since_presentation_plot_name =
        tracy_client::PlotName::new_leak(format!("{name} time since presentation, ms"));
    let presentation_misprediction_plot_name =
        tracy_client::PlotName::new_leak(format!("{name} presentation misprediction, ms"));
    let sequence_delta_plot_name =
        tracy_client::PlotName::new_leak(format!("{name} sequence delta"));

    let mut state = SurfaceThreadState {
        api,
        primary_node,
        target_node,
        active,
        compositor: None,
        adopt: None,
        frame_flags: FrameFlags::DEFAULT,
        vrr_mode: AdaptiveSync::Disabled,

        state: QueueState::Idle,
        timings: Timings::new(None, None, false, target_node),
        perf,
        frame_callback_seq: 0,
        thread_sender,

        output,
        mirroring: None,
        screen_filter,
        postprocess_textures: HashMap::new(),

        shell,
        loop_handle: event_loop.handle(),
        clock: Clock::new(),
        #[cfg(feature = "debug")]
        egui,

        last_sequence: None,
        vblank_frame: None,
        vblank_frame_name,
        time_since_presentation_plot_name,
        presentation_misprediction_plot_name,
        sequence_delta_plot_name,

        frame_profiler: crate::backend::render::gpu_profiler::FrameProfiler::new(&name),
        render_failure_count: 0,
        last_gpu_reset_sent: None,
        has_had_node: false,

        last_tearing: false,
        last_vrr: false,
        last_pacing_log: None,
    };

    let signal = event_loop.get_signal();
    event_loop
        .handle()
        .insert_source(thread_receiver, move |command, _, state| match command {
            Event::Msg(ThreadCommand::Suspend(tx)) => state.suspend(tx),
            Event::Msg(ThreadCommand::Resume { compositor }) => {
                state.resume(compositor);
            }
            Event::Msg(ThreadCommand::NodeAdded {
                node,
                gbm,
                egl,
                sync,
            }) => {
                if let Err(err) = state.node_added(node, gbm, egl) {
                    warn!(?err, ?node, "Failed to add node to surface-thread");
                }
                let _ = sync.send(());
            }
            Event::Msg(ThreadCommand::NodeRemoved { node, sync }) => {
                state.node_removed(node);
                let _ = sync.send(());
            }
            Event::Msg(ThreadCommand::VBlank(metadata)) => {
                state.on_vblank(metadata);
            }
            Event::Msg(ThreadCommand::ScheduleRender) => {
                if !startup_done.load(Ordering::SeqCst) {
                    return;
                }

                state.queue_redraw(false);
            }
            Event::Msg(ThreadCommand::UpdateMirroring(mirroring_output)) => {
                state.update_mirroring(mirroring_output);
            }
            Event::Msg(ThreadCommand::UpdateScreenFilter(filter_config)) => {
                state.update_screen_filter(filter_config);
            }
            Event::Msg(ThreadCommand::AdaptiveSyncAvailable(result)) => {
                if let Some(compositor) = state.compositor.as_mut() {
                    let _ = result.send(
                        compositor
                            .with_compositor(|c| {
                                c.vrr_supported(c.pending_connectors().into_iter().next().unwrap())
                            })
                            .map_err(Into::into),
                    );
                } else {
                    let _ = result.send(Err(anyhow::anyhow!("Set vrr with inactive surface")));
                }
            }
            Event::Msg(ThreadCommand::UseAdaptiveSync(vrr)) => {
                state.vrr_mode = vrr;
            }
            Event::Msg(ThreadCommand::AdoptFrozenFrame(dmabuf)) => {
                state.adopt = Some(AdoptFrame::new(dmabuf));
                // Same guard as ScheduleRender: redraw needs the active seat, which does
                // not exist until startup completes (panics "No seat?").
                if startup_done.load(Ordering::SeqCst) {
                    state.queue_redraw(false);
                }
            }
            Event::Msg(ThreadCommand::DpmsOff) => {
                if let Some(compositor) = state.compositor.as_mut() {
                    if let Err(err) = compositor.with_compositor(|c| c.clear()) {
                        error!("Failed to set DPMS off: {:?}", err);
                    }
                    match std::mem::replace(&mut state.state, QueueState::Idle) {
                        QueueState::Idle => {}
                        QueueState::Queued(token)
                        | QueueState::WaitingForEstimatedVBlank(token) => {
                            state.loop_handle.remove(token);
                        }
                        QueueState::WaitingForVBlank { .. } => {
                            state.timings.discard_current_frame()
                        }
                        QueueState::WaitingForEstimatedVBlankAndQueued {
                            estimated_vblank,
                            queued_render,
                        } => {
                            state.loop_handle.remove(estimated_vblank);
                            state.loop_handle.remove(queued_render);
                        }
                    };
                }
            }
            Event::Msg(ThreadCommand::AllowFrameFlags(flag, mut flags)) => {
                if crate::utils::env::bool_var("COSMIC_DISABLE_DIRECT_SCANOUT").unwrap_or(false) {
                    flags.remove(FrameFlags::ALLOW_SCANOUT);
                }
                if crate::utils::env::bool_var("COSMIC_DISABLE_OVERLAY_SCANOUT").unwrap_or(false) {
                    flags.remove(FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT);
                }

                if flag {
                    state.frame_flags.insert(flags);
                } else {
                    state.frame_flags.remove(flags);
                }
            }
            Event::Closed | Event::Msg(ThreadCommand::End) => {
                signal.stop();
                signal.wakeup();
            }
        })
        .map_err(|insert_error| insert_error.error)
        .context("Failed to listen for events")?;

    event_loop.run(None, &mut state, |_| {}).map_err(Into::into)
}

impl SurfaceThreadState {
    fn suspend(&mut self, tx: SyncSender<()>) {
        self.active.store(false, Ordering::SeqCst);
        let _ = self.compositor.take();

        match std::mem::replace(&mut self.state, QueueState::Idle) {
            QueueState::Idle => {}
            QueueState::Queued(token) | QueueState::WaitingForEstimatedVBlank(token) => {
                self.loop_handle.remove(token);
            }
            QueueState::WaitingForVBlank { .. } => self.timings.discard_current_frame(),
            QueueState::WaitingForEstimatedVBlankAndQueued {
                estimated_vblank,
                queued_render,
            } => {
                self.loop_handle.remove(estimated_vblank);
                self.loop_handle.remove(queued_render);
            }
        };

        let _ = tx.send(());
    }

    fn resume(&mut self, compositor: GbmDrmOutput) {
        let (mode, min_hz) = compositor.with_compositor(|c| {
            (
                c.surface().pending_mode(),
                drm_helpers::get_minimum_refresh_rate(
                    c.surface(),
                    c.pending_connectors().into_iter().next().unwrap(),
                )
                .ok()
                .flatten(),
            )
        });
        let interval =
            Duration::from_secs_f64(1_000. / drm_helpers::calculate_refresh_rate(mode) as f64);
        self.timings.set_refresh_interval(Some(interval));

        // Update profiler target FPS based on display refresh rate
        let target_fps = 1.0 / interval.as_secs_f64();
        self.frame_profiler.set_target_fps(target_fps);
        info!(
            output = %self.output.name(),
            refresh_hz = format!("{:.1}", target_fps),
            "Output refresh rate configured for profiler"
        );

        const SAFETY_MARGIN: u32 = 2; // Magic two frames margin taken from kwin to not trigger low-framerate-compensation
        let min_min_refresh_interval = Duration::from_secs_f64(1. / 30.); // 30Hz
        self.timings.set_min_refresh_interval(Some(
            min_hz
                .map(|min| Duration::from_secs_f64(1. / (min + SAFETY_MARGIN) as f64))
                .unwrap_or(min_min_refresh_interval) // alternatively use 30Hz
                .max(min_min_refresh_interval),
        ));

        if crate::utils::env::bool_var("COSMIC_DISABLE_DIRECT_SCANOUT").unwrap_or(false) {
            self.frame_flags.remove(FrameFlags::ALLOW_SCANOUT);
        } else if crate::utils::env::bool_var("COSMIC_DISABLE_OVERLAY_SCANOUT").unwrap_or(false) {
            self.frame_flags
                .remove(FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT);
        }
        self.compositor = Some(compositor);
    }

    fn node_added(
        &mut self,
        node: DrmNode,
        gbm: GbmAllocator<DrmDeviceFd>,
        egl: EGLContext,
    ) -> Result<()> {
        let mut renderer =
            unsafe { GlowRenderer::new(egl) }.context("Failed to create renderer")?;
        init_shaders(renderer.borrow_mut()).context("Failed to initialize shaders")?;

        self.api.as_mut().add_node(node, gbm, renderer);
        self.has_had_node = true;

        // If this surface is live (has a compositor), a freshly (re)added renderer means
        // we should repaint promptly — this is how a surface recovers after a GPU-reset
        // teardown. During initial setup `compositor` is still None, so this is a no-op.
        if self.compositor.is_some() {
            self.render_failure_count = 0;
            self.queue_redraw(true);
        }

        Ok(())
    }

    fn node_removed(&mut self, node: DrmNode) {
        self.api.as_mut().remove_node(&node);
        // force enumeration
        let _ = self.api.devices();
        // Drop GL handles tied to the removed renderer so they reallocate on the
        // new context after resume.
        self.postprocess_textures.clear();
    }

    #[profiling::function]
    fn on_vblank(&mut self, metadata: Option<DrmEventMetadata>) {
        let Some(compositor) = self.compositor.as_mut() else {
            return;
        };

        // handle edge-cases right after resume
        if !matches!(
            self.state,
            QueueState::WaitingForVBlank { .. } | QueueState::Idle
        ) {
            match mem::replace(&mut self.state, QueueState::Idle) {
                QueueState::WaitingForVBlank { .. } | QueueState::Idle => unreachable!(),
                QueueState::Queued(token) | QueueState::WaitingForEstimatedVBlank(token) => {
                    self.loop_handle.remove(token);
                }
                QueueState::WaitingForEstimatedVBlankAndQueued {
                    estimated_vblank,
                    queued_render,
                } => {
                    self.loop_handle.remove(estimated_vblank);
                    self.loop_handle.remove(queued_render);
                }
            }
        }
        if matches!(self.state, QueueState::Idle) {
            return;
        }

        let now = self.clock.now();
        let presentation_time = match metadata.as_ref().map(|data| &data.time) {
            Some(DrmEventTime::Monotonic(tp)) => Some(*tp),
            _ => None,
        };
        let sequence = metadata.as_ref().map(|data| data.sequence).unwrap_or(0);

        // finish tracy frame
        let _ = self.vblank_frame.take();

        // mark last frame completed
        if let Ok(Some(Some((mut feedback, frames, estimated_presentation_time)))) =
            compositor.frame_submitted()
            && self.mirroring.is_none()
        {
            let name = self.output.name();
            let message = if let Some(presentation_time) = presentation_time {
                let misprediction_s =
                    presentation_time.as_secs_f64() - estimated_presentation_time.as_secs_f64();
                tracy_client::Client::running().unwrap().plot(
                    self.presentation_misprediction_plot_name,
                    misprediction_s * 1000.,
                );

                let now = Duration::from(now);
                if presentation_time > now {
                    let diff = presentation_time - now;
                    tracy_client::Client::running().unwrap().plot(
                        self.time_since_presentation_plot_name,
                        -diff.as_secs_f64() * 1000.,
                    );
                    format!("vblank on {name}, presentation is {diff:?} later")
                } else {
                    let diff = now - presentation_time;
                    tracy_client::Client::running().unwrap().plot(
                        self.time_since_presentation_plot_name,
                        diff.as_secs_f64() * 1000.,
                    );
                    format!("vblank on {name}, presentation was {diff:?} ago")
                }
            } else {
                format!("vblank on {name}, presentation time unknown")
            };
            tracy_client::Client::running()
                .unwrap()
                .message(&message, 0);

            let (clock, flags) = if let Some(tp) = presentation_time {
                (
                    tp.into(),
                    wp_presentation_feedback::Kind::Vsync
                        | wp_presentation_feedback::Kind::HwClock
                        | wp_presentation_feedback::Kind::HwCompletion,
                )
            } else {
                (
                    now,
                    wp_presentation_feedback::Kind::Vsync
                        | wp_presentation_feedback::Kind::HwCompletion,
                )
            };

            let rate = self
                .output
                .current_mode()
                .map(|mode| Duration::from_secs_f64(1_000.0 / mode.refresh as f64));
            let refresh = match rate {
                Some(rate)
                    if self
                        .compositor
                        .as_ref()
                        .is_some_and(|comp| comp.with_compositor(|c| c.vrr_enabled())) =>
                {
                    Refresh::Variable(rate)
                }
                Some(rate) => Refresh::Fixed(rate),
                None => Refresh::Unknown,
            };

            let seq_delta = self
                .last_sequence
                .map(|last_sequence| sequence as i64 - last_sequence as i64);
            if let Some(delta) = seq_delta {
                tracy_client::Client::running()
                    .unwrap()
                    .plot(self.sequence_delta_plot_name, delta as f64);
            }
            self.last_sequence = Some(sequence);

            feedback.presented(clock, refresh, sequence as u64, flags);

            self.timings.presented(clock);

            // Record this presented frame for the UI perf report — but only while
            // a capture window is running. Off the capture path this is a single
            // relaxed atomic load, so frame pacing pays nothing for it.
            if crate::perf::is_capturing() {
                let composited = self.timings.compositing();
                if let Some(frame) = self.timings.previous_frames.back() {
                    let render_start_ns = Duration::from(frame.render_start)
                        .as_nanos()
                        .min(u64::MAX as u128) as u64;
                    let present_latency = smithay::utils::Time::elapsed(
                        &frame.presentation_submitted,
                        frame.presentation_presented,
                    );
                    let production = smithay::utils::Time::elapsed(
                        &frame.render_start,
                        frame.presentation_submitted,
                    );
                    self.perf.record(crate::perf::FrameRecord {
                        present: clock,
                        elements: frame.render_duration_elements,
                        draw: frame.render_duration_draw,
                        present_latency,
                        production,
                        composited,
                        seq_delta,
                    });
                    let present_ns = Duration::from(clock).as_nanos().min(u64::MAX as u128) as u64;
                    self.perf
                        .record_input_latencies(render_start_ns, present_ns);
                }
            }

            while let Ok(pending_image_copy_data) = frames.recv() {
                pending_image_copy_data.send_success_when_ready(
                    self.output.current_transform(),
                    &self.loop_handle,
                    clock,
                );
            }
        }

        let redraw_needed = match mem::replace(&mut self.state, QueueState::Idle) {
            QueueState::Idle => unreachable!(),
            QueueState::Queued(_) => unreachable!(),
            QueueState::WaitingForVBlank { redraw_needed } => redraw_needed,
            QueueState::WaitingForEstimatedVBlank(_) => unreachable!(),
            QueueState::WaitingForEstimatedVBlankAndQueued { .. } => unreachable!(),
        };

        if redraw_needed
            || crate::perf::is_stressing()
            || self.shell.read().animations_going()
            || self.adopt.is_some()
        {
            let vblank_frame = tracy_client::Client::running()
                .unwrap()
                .non_continuous_frame(self.vblank_frame_name);
            self.vblank_frame = Some(vblank_frame);

            self.queue_redraw(false);
        }
        self.send_frame_callbacks();
    }

    #[profiling::function]
    fn on_estimated_vblank(&mut self, force: bool) {
        match mem::replace(&mut self.state, QueueState::Idle) {
            QueueState::Idle => unreachable!(),
            QueueState::Queued(_) => unreachable!(),
            QueueState::WaitingForVBlank { .. } => unreachable!(),
            QueueState::WaitingForEstimatedVBlank(_) => (),
            // The timer fired just in front of a redraw.
            QueueState::WaitingForEstimatedVBlankAndQueued { queued_render, .. } => {
                self.state = QueueState::Queued(queued_render);
                return;
            }
        }

        self.frame_callback_seq = self.frame_callback_seq.wrapping_add(1);

        if force
            || crate::perf::is_stressing()
            || self.shell.read().animations_going()
            || self.adopt.is_some()
        {
            self.queue_redraw(false);
        }
        self.send_frame_callbacks();
    }

    fn queue_redraw(&mut self, force: bool) {
        let Some(_compositor) = self.compositor.as_mut() else {
            return;
        };

        if let QueueState::WaitingForVBlank { .. } = &self.state {
            // We're waiting for VBlank, request a redraw afterwards.
            self.state = QueueState::WaitingForVBlank {
                redraw_needed: true,
            };
            return;
        }

        if !force {
            match &self.state {
                QueueState::Idle | QueueState::WaitingForEstimatedVBlank(_) => {}

                // A redraw is already queued.
                QueueState::Queued(_) | QueueState::WaitingForEstimatedVBlankAndQueued { .. } => {
                    return;
                }
                _ => unreachable!(),
            };
        }

        let estimated_presentation = self.timings.next_presentation_time(&self.clock);
        let render_start = self.timings.next_render_time(&self.clock);

        let timer = if render_start.is_zero() {
            trace!("Running late for frame.");
            // TODO triple buffering
            Timer::immediate()
        } else {
            Timer::from_duration(render_start)
        };

        let token = self
            .loop_handle
            .insert_source(timer, move |_time, _, state| {
                if let Err(err) = state.redraw(estimated_presentation) {
                    // Recognize a GPU context reset anywhere in the error's source chain —
                    // render_frame and the offscreen/postprocess passes both preserve the
                    // typed chain down to GlesError::ContextReset — and ask the main thread
                    // to recreate the contexts (rate-limited inside). This single choke
                    // point covers every render path; other errors fall through untouched.
                    if err.chain().any(|e| {
                        matches!(e.downcast_ref::<GlesError>(), Some(GlesError::ContextReset))
                    }) {
                        Self::request_recovery(
                            &mut state.last_gpu_reset_sent,
                            &state.thread_sender,
                        );
                    }
                    let name = state.output.name();
                    state.render_failure_count = state.render_failure_count.saturating_add(1);
                    warn!(
                        ?name,
                        failure_count = state.render_failure_count,
                        "Failed to submit rendering: {:?}",
                        err
                    );
                    // Only retry if the surface is still active.
                    // Use exponential backoff to avoid busy-looping when GPU is stalled
                    // or experiencing contention.
                    if state.active.load(Ordering::SeqCst) {
                        // Cap backoff at ~500ms to stay responsive
                        let backoff_ms = (16u64 << state.render_failure_count.min(5)).min(500);
                        let backoff_timer = Timer::from_duration(Duration::from_millis(backoff_ms));
                        if let Ok(token) = state.loop_handle.insert_source(
                            backoff_timer,
                            move |_time, _, state| {
                                state.queue_redraw(true);
                                TimeoutAction::Drop
                            },
                        ) {
                            // Store the backoff timer if we need to track it
                            // For now just let it fire
                            let _ = token;
                        }
                    }
                } else {
                    // Reset failure count on success
                    state.render_failure_count = 0;
                }
                TimeoutAction::Drop
            })
            .expect("Failed to schedule render");

        match &self.state {
            QueueState::Idle => {
                self.state = QueueState::Queued(token);
            }
            QueueState::WaitingForEstimatedVBlank(estimated_vblank) => {
                self.state = QueueState::WaitingForEstimatedVBlankAndQueued {
                    estimated_vblank: *estimated_vblank,
                    queued_render: token,
                };
            }
            QueueState::Queued(old_token) if force => {
                self.loop_handle.remove(*old_token);
                self.state = QueueState::Queued(token);
            }
            QueueState::WaitingForEstimatedVBlankAndQueued {
                estimated_vblank,
                queued_render,
            } if force => {
                self.loop_handle.remove(*queued_render);
                self.state = QueueState::WaitingForEstimatedVBlankAndQueued {
                    estimated_vblank: *estimated_vblank,
                    queued_render: token,
                };
            }
            _ => unreachable!(),
        }
    }

    /// Rate-limited request to the main thread to recreate this GPU's renderers after a
    /// context reset. Takes the fields explicitly (not `&mut self`) so it can be called
    /// while `self.compositor` is mutably borrowed in `redraw`.
    fn request_recovery(
        last_sent: &mut Option<std::time::Instant>,
        sender: &Sender<SurfaceCommand>,
    ) {
        let now = std::time::Instant::now();
        if last_sent.is_some_and(|t| now.duration_since(t) < Duration::from_millis(500)) {
            return;
        }
        *last_sent = Some(now);
        warn!("GPU context reset — requesting renderer recovery");
        let _ = sender.send(SurfaceCommand::GpuReset);
    }

    #[profiling::function]
    fn redraw(&mut self, estimated_presentation: Duration) -> Result<()> {
        let Some(compositor) = self.compositor.as_mut() else {
            return Ok(());
        };

        // Present nothing so the last real frame stays latched for the exit freeze: the
        // session tears its components down seconds before we exit, and those frames are
        // empty. A still-armed estimated_vblank timer MUST be disarmed before parking in
        // Idle, or it fires into on_estimated_vblank's unreachable!() and wedges output.
        if self.shell.read().logout_hold {
            if let QueueState::WaitingForEstimatedVBlankAndQueued {
                estimated_vblank, ..
            } = std::mem::replace(&mut self.state, QueueState::Idle)
            {
                self.loop_handle.remove(estimated_vblank);
            }
            return Ok(());
        }

        let frame_start = std::time::Instant::now();
        let mut profile = crate::backend::render::gpu_profiler::FrameProfile::default();
        profile.frame_start = frame_start;

        let render_node = render_node_for_output(
            self.mirroring.as_ref().unwrap_or(&self.output),
            self.primary_node
                .read()
                .unwrap()
                .as_ref()
                .unwrap_or(&self.target_node),
            &self.target_node,
            &self.shell.read(),
        );

        // Acquire the renderer gracefully. During GPU-reset recovery the main thread
        // briefly removes this node while it recreates the context, so the node can be
        // absent here. A missing renderer must NOT panic — bail so the normal backoff
        // retry repaints once it is re-added (this is what makes recovery panic-free for
        // every surface, not just the one that reported the reset).
        // If we previously HAD a renderer and it is now gone (e.g. a recovery attempt that
        // failed to recreate the context while the GPU was still wedged), re-request
        // recovery so a still-wedged GPU keeps being retried rather than leaving this
        // output frozen. `has_had_node` gates out the benign startup case (no node yet).
        let mut renderer = if render_node != self.target_node {
            match self
                .api
                .renderer(&render_node, &self.target_node, compositor.format())
            {
                Ok(renderer) => renderer,
                Err(err) => {
                    if self.has_had_node {
                        Self::request_recovery(&mut self.last_gpu_reset_sent, &self.thread_sender);
                    }
                    anyhow::bail!("renderer for {:?} unavailable: {:?}", render_node, err);
                }
            }
        } else {
            match self.api.single_renderer(&self.target_node) {
                Ok(renderer) => renderer,
                Err(err) => {
                    if self.has_had_node {
                        Self::request_recovery(&mut self.last_gpu_reset_sent, &self.thread_sender);
                    }
                    anyhow::bail!("renderer for {:?} unavailable: {:?}", self.target_node, err);
                }
            }
        };

        self.timings.start_render(&self.clock);

        let mut additional_frame_flags = FrameFlags::empty();
        let mut remove_frame_flags = FrameFlags::empty();

        let (
            has_active_fullscreen,
            fullscreen_drives_refresh_rate,
            animations_going,
            game_mode_tearing,
            game_mode_fps_limit,
            game_mode_active,
            game_mode_vrr,
        ) = {
            let shell = self.shell.read();
            let animations_going = shell.animations_going();
            // Tearing is allowed for an exclusive fullscreen game when the
            // game-mode `SetTearing` flag is on — but NOT while an overlay is up
            // (the overlay must composite above the game, which rules out the
            // single-plane tearing/scanout fast path). The scanout/VRR/compositing
            // checks happen below.
            let game_mode_tearing =
                shell.game_mode.active && shell.tearing_allowed && !shell.game_mode.overlay_active;
            let fps_limit = shell.game_mode_fps_limit;
            let game_mode_active = shell.game_mode.active;
            let game_mode_vrr = shell.game_mode_vrr;
            let output = self.mirroring.as_ref().unwrap_or(&self.output);
            if let Some((_, workspace)) = shell.workspaces.active(output) {
                let seat = shell.seats.last_active();
                if let Some(fullscreen_surface) = workspace.get_fullscreen(seat) {
                    const _30_FPS: Duration = Duration::from_nanos(1_000_000_000 / 30);
                    (
                        true,
                        fullscreen_surface
                            .surface
                            .wl_surface()
                            .is_some_and(|surface| {
                                recursive_frame_time_estimation(&self.clock, &surface)
                                    .is_some_and(|dur| dur <= _30_FPS)
                            }),
                        animations_going,
                        game_mode_tearing,
                        fps_limit,
                        game_mode_active,
                        game_mode_vrr,
                    )
                } else {
                    (
                        false,
                        false,
                        animations_going,
                        game_mode_tearing,
                        fps_limit,
                        game_mode_active,
                        game_mode_vrr,
                    )
                }
            } else {
                (
                    false,
                    false,
                    animations_going,
                    game_mode_tearing,
                    fps_limit,
                    game_mode_active,
                    game_mode_vrr,
                )
            }
        };

        // Cap the presentation rate to the game-mode fps limit, but only for the
        // output actually showing the fullscreen game.
        self.timings.set_fps_limit(if has_active_fullscreen {
            game_mode_fps_limit
        } else {
            0
        });
        // Feed the game's live frame time to the game-mode D-Bus AppFrametimeNs
        // reader (Auto-TDP). Only the output showing the game writes; when the game
        // exits, `Active` goes false and the reader returns 0 regardless.
        if has_active_fullscreen {
            self.shell
                .read()
                .game_mode_frametime_ns
                .store(self.timings.recent_frametime_ns(30), Ordering::Relaxed);
        }

        if has_active_fullscreen || animations_going {
            // skip overlay plane assign if we have a fullscreen surface or dynamic contents to save on tests
            remove_frame_flags |= FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT;
        }

        let mut vrr = matches!(self.vrr_mode, AdaptiveSync::Force);

        if self.vrr_mode == AdaptiveSync::Enabled {
            vrr = has_active_fullscreen;
        }

        // Game-mode VRR policy (one.playtron.GameMode SetVrr) overrides the
        // output's adaptive-sync setting while a game is fullscreen — gated on
        // the fullscreen surface so a stale policy never leaks onto the desktop.
        if game_mode_active && has_active_fullscreen {
            match game_mode_vrr {
                VrrMode::On => vrr = true,
                VrrMode::Off => vrr = false,
                VrrMode::Auto => {}
            }
        }

        if vrr != self.last_vrr {
            self.last_vrr = vrr;
            info!(
                target: GAMING_TARGET,
                vrr,
                output = %self.output.name(),
                "VRR {}",
                if vrr { "engaged" } else { "disabled" }
            );
        }

        // MERGE: our fork's dual-Kawase blur pass (process_blur + BlurRenderState +
        // ElementFilter::{BlurCapture,LayerBlurCapture} capture) was removed here in
        // favour of upstream's frosted-glass implementation, which blits the region
        // under an element straight out of the live framebuffer during element
        // collection. The gpu_profiler `blur_*` FrameProfile fields consequently stay
        // at their defaults (no separate blur phase to time on this thread any more).
        let elements_phase_start = std::time::Instant::now();
        let mut elements = output_elements(
            Some(&render_node),
            &mut renderer,
            &self.shell,
            self.clock.now(),
            self.mirroring.as_ref().unwrap_or(&self.output),
            CursorMode::All,
            #[cfg(not(feature = "debug"))]
            None,
            #[cfg(feature = "debug")]
            Some((&self.egui, &self.timings)),
            Some(self.target_node),
        )
        .map_err(|err| {
            anyhow::format_err!("Failed to accumulate elements for rendering: {:?}", err)
        })?;
        profile.elements_duration = elements_phase_start.elapsed();
        profile.element_count = elements.len();

        // Force full-screen damage during layer slide animations.
        // Without this, the DRM compositor's damage tracker may not detect
        // that surface elements moved positions between frames.
        {
            let shell = self.shell.read();
            if shell
                .layer_slides
                .iter()
                .any(|s| s.visibility.is_animating())
                && let Some(mode) = self.output.current_mode()
            {
                let scale = self.output.current_scale().fractional_scale();
                let logical_w = (mode.size.w as f64 / scale).round() as i32;
                let logical_h = (mode.size.h as f64 / scale).round() as i32;
                let damage_rect = Rectangle::new((0, 0).into(), (logical_w, logical_h).into());
                elements.push(DamageElement::new(damage_rect).into());
                tracing::trace!(
                    "layer_slide: forced full-screen damage {}x{}",
                    logical_w,
                    logical_h
                );
            }
        }

        // Hold the frozen frame opaque over the starting session, then ease it out to
        // reveal the real content.
        // Safety cap so a session that never paints can't strand a stale frame. A slow cold
        // start can exceed this, in which case the fade lands on the grey clear.
        const ADOPT_HOLD_CAP: std::time::Duration = std::time::Duration::from_secs(3);
        const ADOPT_FADE: std::time::Duration = std::time::Duration::from_millis(400);

        if let Some(adopt) = self.adopt.as_mut() {
            // Content must be VISIBLE, not merely present: layer surfaces sit in the render
            // list at alpha 0 while pending their first buffer, then fade in over
            // `motion.layer_fade_in`. Fading the held frame out over those dissolves to
            // BLACK and only then does their own fade bring the image up.
            let has_content = elements
                .iter()
                .any(|e| !matches!(e, CosmicElement::Cursor(_) | CosmicElement::Dnd(_)));
            let still_fading_in = self.shell.read().layer_fade_in_active();
            let content_visible = has_content && !still_fading_in;
            let timed_out = adopt.started.elapsed() > ADOPT_HOLD_CAP;
            if adopt.fade_start.is_none() && (content_visible || timed_out) {
                debug!(
                    held_ms = adopt.started.elapsed().as_millis(),
                    has_content,
                    still_fading_in,
                    reason = if content_visible {
                        "content visible"
                    } else {
                        "hold cap"
                    },
                    "handoff: starting cross-fade of frozen frame"
                );
                adopt.fade_start = Some(std::time::Instant::now());
                crate::utils::timing::mark(if content_visible {
                    "fade-start (content visible)"
                } else {
                    "fade-start (hold cap)"
                });
            }
            if adopt.texture.is_none() {
                let dmabuf = adopt.dmabuf.clone();
                match renderer.glow_renderer_mut().import_dmabuf(&dmabuf, None) {
                    Ok(tex) => adopt.texture = Some(tex),
                    Err(err) => {
                        warn!(?err, "adopt: import_dmabuf failed; dropping backdrop");
                        self.adopt = None;
                    }
                }
            }
        }
        if let Some(adopt) = self.adopt.as_ref() {
            // Ease-out cubic: mirrors the layer fade-in elsewhere in this fork
            // (`1.0 - (1.0 - t).powi(3)`, 0->1) but inverted for a 1->0 fade OUT.
            let alpha = match adopt.fade_start {
                None => 1.0,
                Some(start) => {
                    let t = (start.elapsed().as_secs_f32() / ADOPT_FADE.as_secs_f32()).min(1.0);
                    (1.0 - t).powi(3)
                }
            };
            if alpha <= 0.0 {
                debug!("handoff: cross-fade complete, frozen frame released");
                crate::utils::timing::mark("fade-complete");
                self.adopt = None;
            } else if let Some(tex) = adopt.texture.clone() {
                let ctx = renderer.glow_renderer().context_id();
                let scale = self.output.current_scale().integer_scale();
                let elem = TextureRenderElement::from_static_texture(
                    adopt.id.clone(),
                    ctx,
                    (0., 0.),
                    tex,
                    scale,
                    Transform::Normal,
                    Some(alpha),
                    None,
                    None,
                    None,
                    smithay::backend::renderer::element::Kind::Unspecified,
                );
                // On TOP (index 0 = highest z here — elements are drawn front-to-back) so
                // fading it out progressively uncovers the real content, not the reverse.
                elements.insert(0, CosmicElement::Adopt(elem));
            }
        }

        if vrr && fullscreen_drives_refresh_rate && !self.timings.past_min_render_time(&self.clock)
        {
            additional_frame_flags |= FrameFlags::SKIP_CURSOR_ONLY_UPDATES;
        };
        self.timings.set_vrr(vrr);
        self.timings.elements_done(&self.clock);

        // we can't use the elements after `compositor.render_frame`,
        // so let's collect everything we need for screencopy now
        let mut has_cursor_mode_none = false;
        let frames = if self.mirroring.is_none() {
            take_screencopy_frames(&self.output, &elements, &mut has_cursor_mode_none)
        } else {
            Default::default()
        };

        // actual rendering
        let draw_phase_start = std::time::Instant::now();
        let has_layer_slides = self
            .shell
            .read()
            .layer_slides
            .iter()
            .any(|s| s.visibility.is_animating());
        let source_output = self
            .mirroring
            .as_ref()
            .or((!self.screen_filter.is_noop()).then_some(&self.output))
            .filter(|output| {
                PostprocessOutputConfig::for_output_untransformed(output)
                    != PostprocessOutputConfig::for_output(&self.output)
                    || !self.screen_filter.is_noop()
            });
        if has_layer_slides {
            tracing::trace!(
                postprocess = source_output.is_some(),
                element_count = elements.len(),
                "layer_slide: render path info"
            );
        }

        let mut pre_postprocess_data = PrePostprocessData::default();

        let res = if let Some(source_output) = source_output {
            let offscreen_output_config =
                PostprocessOutputConfig::for_output_untransformed(source_output);
            let postprocess_state = match self.postprocess_textures.entry(self.target_node) {
                hash_map::Entry::Occupied(occupied) => {
                    let postprocess_state = occupied.into_mut();
                    // If output config is different, re-create offscreen state
                    if postprocess_state.output_config != offscreen_output_config {
                        *postprocess_state = PostprocessState::new_with_renderer(
                            &mut renderer,
                            compositor.format(),
                            offscreen_output_config,
                        )?
                    }
                    postprocess_state
                }
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(PostprocessState::new_with_renderer(
                        &mut renderer,
                        compositor.format(),
                        offscreen_output_config,
                    )?)
                }
            };

            if has_cursor_mode_none && self.mirroring.is_none() {
                // TODO: use `extract_if` once stablized
                let cursor_element_count = elements
                    .iter()
                    .take_while(|elem| elem.kind() == Kind::Cursor)
                    .count();
                let cursor_elements = elements.drain(..cursor_element_count).collect::<Vec<_>>();
                let scale = source_output.current_scale().fractional_scale().into();

                let geometry: Option<Rectangle<i32, Physical>> =
                    cursor_elements.iter().fold(None, |acc, elem| {
                        let geometry = elem.geometry(scale);
                        if let Some(acc) = acc {
                            Some(acc.merge(geometry))
                        } else {
                            Some(geometry)
                        }
                    });

                if let Some(geometry) = geometry {
                    let cursor_elements = cursor_elements
                        .into_iter()
                        .map(|elem| {
                            RelocateRenderElement::from_element(
                                elem,
                                Point::from((-geometry.loc.x, -geometry.loc.y)),
                                Relocate::Relative,
                            )
                        })
                        .collect::<Vec<_>>();

                    postprocess_state.track_cursor(
                        &mut renderer,
                        Fourcc::Abgr8888,
                        geometry.size,
                        scale,
                    )?;

                    postprocess_state
                        .cursor_texture
                        .as_mut()
                        .unwrap()
                        .render()
                        .draw::<_, <GlMultiRenderer as RendererSuper>::Error>(|tex| {
                            if self.mirroring.is_none() {
                                pre_postprocess_data.cursor_geometry = Some(geometry);
                                pre_postprocess_data.cursor_texture = Some(tex.clone());
                            }

                            let mut fb = renderer.bind(tex)?;
                            let res = match postprocess_state
                                .cursor_damage_tracker
                                .as_mut()
                                .unwrap()
                                .render_output(
                                    &mut renderer,
                                    &mut fb,
                                    1,
                                    &cursor_elements,
                                    [0.0, 0.0, 0.0, 0.0],
                                ) {
                                Ok(res) => res,
                                Err(RenderError::Rendering(err)) => return Err(err),
                                Err(RenderError::OutputNoMode(_)) => unreachable!(),
                            };

                            if self.mirroring.is_none() {
                                pre_postprocess_data.states = Some(res.states);
                            }

                            renderer.wait(&res.sync)?;
                            std::mem::drop(fb);

                            let transform = source_output.current_transform();
                            let area = tex.size().to_logical(1, transform);

                            Ok(res
                                .damage
                                .cloned()
                                .map(|v| {
                                    v.into_iter()
                                        .map(|r| r.to_logical(1).to_buffer(1, transform, &area))
                                        .collect::<Vec<_>>()
                                })
                                .unwrap_or_default())
                        })
                        .context("Failed to draw to offscreen render target")?;
                }
            } else {
                postprocess_state.remove_cursor();
            }

            postprocess_state
                .texture
                .render()
                .draw::<_, <GlMultiRenderer as RendererSuper>::Error>(|tex| {
                    if self.mirroring.is_none() {
                        pre_postprocess_data.texture = Some(tex.clone());
                    }

                    let mut fb = renderer.bind(tex)?;
                    let res = match postprocess_state.damage_tracker.render_output(
                        &mut renderer,
                        &mut fb,
                        1,
                        &elements,
                        *CLEAR_COLOR,
                    ) {
                        Ok(res) => res,
                        Err(RenderError::Rendering(err)) => return Err(err),
                        Err(RenderError::OutputNoMode(_)) => unreachable!(),
                    };

                    if self.mirroring.is_none() {
                        if let Some(states) = pre_postprocess_data.states.as_mut() {
                            states.states.extend(res.states.states);
                        } else {
                            pre_postprocess_data.states = Some(res.states);
                        }
                    }

                    renderer.wait(&res.sync)?;
                    std::mem::drop(fb);

                    let transform = source_output.current_transform();
                    let area = tex.size().to_logical(1, transform);

                    Ok(res
                        .damage
                        .cloned()
                        .map(|v| {
                            v.into_iter()
                                .map(|r| r.to_logical(1).to_buffer(1, transform, &area))
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default())
                })
                .context("Failed to draw to offscreen render target")?;

            renderer = self
                .api
                .single_renderer(&self.target_node)
                .map_err(|err| anyhow::format_err!("Failed to create renderer: {:?}", err))?;

            elements = postprocess_elements(
                &mut renderer,
                &self.output,
                &pre_postprocess_data,
                postprocess_state,
                &self.screen_filter,
            );

            if let Err(err) = compositor.with_compositor(|c| c.use_vrr(vrr)) {
                warn!("Unable to set adaptive VRR state: {}", err);
            }
            compositor.render_frame(
                &mut renderer,
                &elements,
                [0.0, 0.0, 0.0, 0.0],
                self.frame_flags
                    .union(additional_frame_flags)
                    .difference(remove_frame_flags),
            )
        } else {
            if let Err(err) = compositor.with_compositor(|c| c.use_vrr(vrr)) {
                warn!("Unable to set adaptive VRR state: {}", err);
            }
            // For an exclusive fullscreen game, clear to black: smithay only
            // direct-scans-out the topmost full-output element if the clear color
            // is black/transparent OR the element is opaque. A black clear lets a
            // game that doesn't declare an opaque region (common for Xwayland/
            // Proton titles) still take the primary plane — harmless since the
            // game covers the whole output.
            let clear = if game_mode_active && has_active_fullscreen {
                smithay::backend::renderer::Color32F::new(0.0, 0.0, 0.0, 1.0)
            } else {
                *CLEAR_COLOR // TODO use a theme neutral color
            };
            // Game mode forces black even when COSMIC_CLEAR_COLOR overrides the default:
            // during the launch entrance has_active_fullscreen can still be false, and
            // any non-black clear shows through behind the few elements present.
            tracing::trace!(
                target: GAMING_TARGET,
                output = %self.output.name(),
                game_mode_active,
                has_active_fullscreen,
                clear_is_black = game_mode_active && has_active_fullscreen,
                element_count = elements.len(),
                "frame clear-color + content"
            );
            compositor.render_frame(
                &mut renderer,
                &elements,
                clear,
                self.frame_flags
                    .union(additional_frame_flags)
                    .difference(remove_frame_flags),
            )
        };
        self.timings.draw_done(&self.clock);
        profile.draw_duration = draw_phase_start.elapsed();

        // Read back what the frame actually drew. smithay has already decided,
        // per element, whether it was composited, taken by a plane, or skipped
        // as invisible, and how much of it was on screen -- so this is a walk of
        // an existing map rather than new measurement.
        if let Ok(frame_result) = res.as_ref() {
            use smithay::backend::renderer::element::RenderElementPresentationState;

            let mut draw = crate::backend::render::gpu_profiler::DrawStats {
                output_px: self
                    .output
                    .current_mode()
                    .map(|m| (m.size.w as usize) * (m.size.h as usize))
                    .unwrap_or(0),
                primary_scanout: !matches!(
                    frame_result.primary_element,
                    smithay::backend::drm::compositor::PrimaryPlaneElement::Swapchain(_)
                ),
                ..Default::default()
            };
            for state in frame_result.states.states.values() {
                match state.presentation_state {
                    RenderElementPresentationState::Rendering { .. } => {
                        draw.rendered += 1;
                        draw.overdraw_px += state.visible_area;
                    }
                    RenderElementPresentationState::ZeroCopy => draw.zero_copy += 1,
                    RenderElementPresentationState::Skipped => draw.skipped += 1,
                }
            }
            profile.draw = draw;
        }

        if has_layer_slides {
            match &res {
                Ok(frame_result) => {
                    tracing::trace!(
                        is_empty = frame_result.is_empty,
                        "layer_slide: frame_result after render_frame"
                    );
                }
                Err(e) => {
                    tracing::warn!("layer_slide: render_frame FAILED: {:?}", e);
                }
            }
        }

        let submit_phase_start = std::time::Instant::now();
        match res {
            Ok(frame_result) => {
                let (tx, rx) = std::sync::mpsc::channel();

                let feedback = if !frame_result.is_empty && self.mirroring.is_none() {
                    Some((
                        self.shell
                            .read()
                            .take_presentation_feedback(&self.output, &frame_result.states),
                        rx,
                        estimated_presentation,
                    ))
                } else {
                    None
                };

                if frame_result.needs_sync()
                    && let PrimaryPlaneElement::Swapchain(elem) = &frame_result.primary_element
                {
                    elem.sync.wait()?;
                }

                // Tearing / immediate-flip decision: only on the pure direct-
                // scanout path (a composited frame can't tear without an explicit
                // fence), never under VRR, and only if the driver supports async
                // flips.
                let scanout = !matches!(
                    &frame_result.primary_element,
                    PrimaryPlaneElement::Swapchain(_)
                );

                // Upscale scanout check: if this game frame requested an
                // upscale (`scale_to`) but did NOT scan out (composited to the
                // swapchain), the DRM plane rejected the scale. Latch it so game
                // mode letterboxes instead of compositing a scanout-only buffer to
                // black, and log the rejected config so the reject can be fixed.
                if game_mode_active && has_active_fullscreen && !scanout {
                    use smithay::desktop::space::SpaceElement as _;
                    let shell = self.shell.read();
                    // Only attribute compositing to a plane scale-REJECT when it's a
                    // SETTLED game on THIS (the game's) output with NO overlay up.
                    // The entrance/exit animation always composites; an active
                    // overlay disables overlay planes and forces the game to
                    // composite; another output compositing is unrelated. None of
                    // those mean the plane rejected the scale, so don't latch on
                    // them (that would spuriously letterbox a scalable game).
                    // A controlled-set child (a game dialog / in-prefix login window)
                    // composited above the game also forces composition, so a frame
                    // with children present must never be blamed on the plane
                    // rejecting the scale — latching there would letterbox the game
                    // for the rest of the session over a dialog that has since closed.
                    let eligible = shell.game_mode.output.as_ref() == Some(&self.output)
                        && !shell.game_mode.overlay_active
                        && shell.game_mode.children.is_empty();
                    let scaled = eligible
                        .then(|| {
                            shell.game_mode.game_surface.as_ref().and_then(|game| {
                                shell.workspaces.spaces().find_map(|ws| {
                                    ws.get_fullscreen_surfaces()
                                        .find(|f| &f.surface == game && !f.is_animating())
                                        .and_then(|f| f.scale_to)
                                        .map(|rect| {
                                            (
                                                game.bbox().size,
                                                rect.size,
                                                ws.output().geometry().size,
                                            )
                                        })
                                })
                            })
                        })
                        .flatten();
                    if let Some((buffer, target, out_size)) = scaled
                        && !shell.game_mode_scale_rejected.swap(true, Ordering::Relaxed)
                    {
                        warn!(
                            target: GAMING_TARGET,
                            output = %self.output.name(),
                            buffer_w = buffer.w,
                            buffer_h = buffer.h,
                            target_w = target.w,
                            target_h = target.h,
                            output_w = out_size.w,
                            output_h = out_size.h,
                            "game upscale rejected by the DRM plane (composited, not \
                             scanned out) -- letterboxing. The buffer/target/output \
                             sizes are the rejected scale config."
                        );
                    }
                }

                let supports_tearing = compositor.with_compositor(|c| c.supports_tearing());
                // Publish the game's-output tearing capability for the game-mode
                // `TearingSupported` D-Bus property (only the surface thread can
                // probe it).
                if has_active_fullscreen {
                    self.shell
                        .read()
                        .game_mode_tearing_supported
                        .store(supports_tearing, Ordering::Relaxed);
                }
                let tearing = game_mode_tearing && !vrr && scanout && supports_tearing;
                compositor.with_compositor(|c| c.set_tearing(tearing));

                if tearing != self.last_tearing {
                    self.last_tearing = tearing;
                    info!(
                        target: GAMING_TARGET,
                        tearing,
                        output = %self.output.name(),
                        requested = game_mode_tearing,
                        vrr,
                        scanout,
                        supports_tearing,
                        "tearing {} (async page flips {})",
                        if tearing { "engaged" } else { "disabled" },
                        if tearing { "on" } else { "off" },
                    );
                }

                // Periodic frame-pacing instrumentation (once/second while a game
                // is fullscreen): the pacer's wake-ahead budget + the achieved fps.
                if has_active_fullscreen {
                    let now = std::time::Instant::now();
                    if self
                        .last_pacing_log
                        .is_none_or(|t| now.duration_since(t) >= Duration::from_secs(1))
                    {
                        self.last_pacing_log = Some(now);
                        let fps = self.timings.recent_fps(30);
                        let min_interval_ms = self.timings.recent_min_interval_ms(30);
                        let wake_ahead_ms = self.timings.rolling_draw_time().as_secs_f64() * 1000.0;
                        // Scanout diagnostics (input element list, top-first). When
                        // scanout=false: element_count==1 ⇒ the game alone failed the
                        // primary plane (opacity/geometry/format); >1 ⇒ something sits
                        // above it (top_kind shows the topmost — Cursor, or a panel/
                        // dock/overlay surface as Unspecified) and isn't being lifted
                        // to its own plane.
                        let element_count = elements.len();
                        let top_kind = elements.first().map(|e| e.kind());
                        let cursor_present = elements.iter().any(|e| e.kind() == Kind::Cursor);
                        // overlay_active + which app is fullscreen: BUG 2 signature is
                        // overlay_active=true WHILE scanout=true (the game holds the
                        // primary plane so the QAM can't composite over it); BUG 3 is
                        // this line still firing with the game's app id after a
                        // return-to-launcher (paused game still the scanned-out primary).
                        let (overlay_active, game_app_id) = {
                            let shell = self.shell.read();
                            (shell.game_mode.overlay_active, shell.game_mode.app_id)
                        };
                        info!(
                            target: GAMING_TARGET,
                            fps,
                            min_interval_ms,
                            wake_ahead_ms,
                            fps_cap = game_mode_fps_limit,
                            tearing,
                            vrr,
                            scanout,
                            overlay_active,
                            game_app_id = ?game_app_id,
                            element_count,
                            ?top_kind,
                            cursor_present,
                            output = %self.output.name(),
                            "frame pacing",
                        );
                    }
                }

                match compositor.queue_frame(feedback) {
                    x @ Ok(()) | x @ Err(FrameError::EmptyFrame) => {
                        if has_layer_slides {
                            info!(
                                ok = x.is_ok(),
                                "layer_slide: queue_frame result (ok=true means submitted, ok=false means EmptyFrame)"
                            );
                        }
                        self.timings.submitted_for_presentation(&self.clock);

                        // Tell the pacer whether this frame was GPU-composited or
                        // a pure direct scanout (e.g. a fullscreen game) — the
                        // latter skips the compositing draw-time floor for the
                        // tightest, lowest-latency pacing.
                        self.timings.set_compositing(matches!(
                            &frame_result.primary_element,
                            PrimaryPlaneElement::Swapchain(_)
                        ));

                        // Update `state` after `queue_frame`, before any early return from errors
                        if x.is_ok() {
                            let new_state = QueueState::WaitingForVBlank {
                                redraw_needed: false,
                            };
                            match mem::replace(&mut self.state, new_state) {
                                QueueState::Idle => unreachable!(),
                                QueueState::Queued(_) => (),
                                QueueState::WaitingForVBlank { .. } => unreachable!(),
                                QueueState::WaitingForEstimatedVBlank(estimated_vblank)
                                | QueueState::WaitingForEstimatedVBlankAndQueued {
                                    estimated_vblank,
                                    ..
                                } => {
                                    self.loop_handle.remove(estimated_vblank);
                                }
                            };
                        }

                        let now = self.clock.now();
                        for (session, frame, res) in frames {
                            if let Err(err) = send_screencopy_result(
                                &mut renderer,
                                &self.output,
                                &mut pre_postprocess_data,
                                &tx,
                                &frame_result,
                                &elements,
                                (&session, frame, res),
                                now.into(),
                            ) {
                                tracing::warn!(?err, "Failed to screencopy");
                            }
                        }

                        if self.mirroring.is_none() {
                            // If postprocessing, use states from first render
                            let states = pre_postprocess_data.states.unwrap_or(frame_result.states);
                            self.send_dmabuf_feedback(states);
                        }

                        if x.is_ok() {
                            if self.mirroring.is_none() {
                                self.frame_callback_seq = self.frame_callback_seq.wrapping_add(1);
                                self.send_frame_callbacks();
                            }
                        } else {
                            // we don't expect a vblank
                            let _ = self.vblank_frame.take();

                            self.queue_estimated_vblank(
                                estimated_presentation,
                                // Make sure we redraw to reevaluate, if we intentionally missed content
                                additional_frame_flags
                                    .contains(FrameFlags::SKIP_CURSOR_ONLY_UPDATES),
                            );
                        }
                    }
                    Err(err) => {
                        for (_session, frame, _) in frames {
                            frame.fail(CaptureFailureReason::Unknown);
                        }
                        // Cleanup texture cache even on error to prevent dmabuf leaks
                        if let Ok(devices) = self.api.devices_mut() {
                            for device in devices {
                                let _ = device.renderer_mut().cleanup_texture_cache();
                            }
                        }
                        return Err(err).with_context(|| "Failed to submit result for display");
                    }
                };
            }
            Err(err) => {
                compositor.reset_buffers();
                // Cleanup texture cache even on error to prevent dmabuf leaks
                if let Ok(devices) = self.api.devices_mut() {
                    for device in devices {
                        let _ = device.renderer_mut().cleanup_texture_cache();
                    }
                }
                // Propagate the TYPED error (do NOT stringify) so the render-timer closure
                // can walk the source chain and recognize a GPU context reset
                // (GlesError::ContextReset) uniformly — whether it surfaced here or in an
                // earlier offscreen/postprocess pass (which also preserves the chain via
                // `.context(...)?`). Centralizing detection there is what stops a reset on a
                // postprocess-active output (e.g. Night Light) from being missed, and keeps
                // non-reset errors on their existing log-and-backoff path unchanged.
                return Err(err).context("Rendering failed");
            }
        }

        for device in self.api.devices_mut()? {
            device.renderer_mut().cleanup_texture_cache()?;
        }

        // Record frame profile for performance analysis
        profile.submit_duration = submit_phase_start.elapsed();
        profile.total_duration = frame_start.elapsed();
        if crate::backend::render::gpu_profiler::perf_logging_enabled() {
            self.frame_profiler.record(profile);
            self.frame_profiler.maybe_report();
        }

        Ok(())
    }

    fn queue_estimated_vblank(&mut self, target_presentation_time: Duration, force: bool) {
        match mem::take(&mut self.state) {
            QueueState::Idle => unreachable!(),
            QueueState::Queued(_) => (),
            QueueState::WaitingForVBlank { .. } => unreachable!(),
            QueueState::WaitingForEstimatedVBlank(token)
            | QueueState::WaitingForEstimatedVBlankAndQueued {
                estimated_vblank: token,
                ..
            } => {
                self.state = QueueState::WaitingForEstimatedVBlank(token);
                return;
            }
        }

        let now = self.clock.now();
        let mut duration = target_presentation_time.saturating_sub(now.into());

        // No use setting a zero timer, since we'll send frame callbacks anyway right after the call to
        // render(). This can happen for example with unknown presentation time from DRM.
        if duration.is_zero() {
            duration += self.timings.refresh_interval();
        }

        trace!("queueing estimated vblank timer to fire in {duration:?}");

        let timer = Timer::from_duration(duration);
        let token = self
            .loop_handle
            .insert_source(timer, move |_, _, data| {
                data.on_estimated_vblank(force);
                TimeoutAction::Drop
            })
            .unwrap();
        self.state = QueueState::WaitingForEstimatedVBlank(token);
    }

    fn update_mirroring(&mut self, mirroring_output: Option<Output>) {
        self.mirroring = mirroring_output;
        self.postprocess_textures.clear();
    }

    fn update_screen_filter(&mut self, filter_config: ScreenFilter) {
        self.screen_filter = filter_config;
        self.postprocess_textures.clear();
    }

    fn send_frame_callbacks(&mut self) {
        if self.mirroring.is_none() {
            let _ = self
                .thread_sender
                .send(SurfaceCommand::SendFrames(self.frame_callback_seq));
        }
    }

    fn send_dmabuf_feedback(&mut self, states: RenderElementStates) {
        let _ = self
            .thread_sender
            .send(SurfaceCommand::RenderStates(states));
    }
}

fn source_node_for_surface(w: &WlSurface) -> Option<DrmNode> {
    with_renderer_surface_state(w, |state| {
        state
            .buffer()
            .and_then(|buffer| get_dmabuf(buffer).ok().and_then(|dmabuf| dmabuf.node()))
    })
    .flatten()
}

// TODO: Introduce can_shared_dmabuf_framebuffer for cases where we might select another gpu
//  and composite on target if not possible to finally get rid of "primary"
#[profiling::function]
fn render_node_for_output(
    output: &Output,
    primary_node: &DrmNode,
    target_node: &DrmNode,
    shell: &Shell,
) -> DrmNode {
    if target_node == primary_node {
        return *target_node;
    }

    let Some(workspace) = shell.active_space(output) else {
        return *target_node;
    };
    let fullscreens: Vec<_> = workspace
        .get_fullscreen_surfaces()
        .map(|f| f.surface.clone())
        .collect();
    let nodes = if !fullscreens.is_empty() {
        fullscreens
    } else {
        workspace
            .mapped()
            .map(|mapped| mapped.active_window())
            .collect::<Vec<_>>()
    }
    .into_iter()
    .flat_map(|w| w.wl_surface().and_then(|s| source_node_for_surface(&s)))
    .collect::<Vec<_>>();

    if nodes.contains(target_node) || nodes.is_empty() {
        *target_node
    } else {
        *primary_node
    }
}

fn get_surface_dmabuf_feedback(
    render_node: DrmNode,
    target_node: DrmNode,
    render_formats: FormatSet,
    target_formats: FormatSet,
    primary_plane_formats: FormatSet,
    overlay_plane_formats: Option<FormatSet>,
) -> SurfaceDmabufFeedback {
    // We limit the scan-out trache to formats we can also render from
    // so that there is always a fallback render path available in case
    // the supplied buffer can not be scanned out directly

    let primary_plane_formats = primary_plane_formats
        .intersection(&render_formats)
        .cloned()
        .collect::<FormatSet>();
    let overlay_plane_formats = overlay_plane_formats.map(|formats| {
        formats
            .intersection(&render_formats)
            .cloned()
            .collect::<FormatSet>()
    });

    let mut builder = DmabufFeedbackBuilder::new(render_node.dev_id(), render_formats.clone());

    if target_node != render_node {
        builder = builder.add_preference_tranche(
            target_node.dev_id(),
            Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Sampling),
            target_formats,
        );
    };
    let render_feedback = builder.clone().build().unwrap();

    let primary_scanout_feedback = builder
        .clone()
        .add_preference_tranche(
            target_node.dev_id(),
            Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
            primary_plane_formats,
        )
        .build()
        .unwrap();
    let overlay_scanout_feedback = overlay_plane_formats.map(|formats| {
        builder
            .add_preference_tranche(
                target_node.dev_id(),
                Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
                formats,
            )
            .build()
            .unwrap()
    });

    SurfaceDmabufFeedback {
        render_feedback,
        overlay_scanout_feedback,
        primary_scanout_feedback,
    }
}

fn take_screencopy_frames(
    output: &Output,
    elements: &[CosmicElement<GlMultiRenderer>],
    has_cursor_mode_none: &mut bool,
) -> Vec<(
    ScreencopySessionRef,
    ScreencopyFrame,
    Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), OutputNoMode>,
)> {
    output
        .take_pending_frames()
        .into_iter()
        .map(|(session, frame)| {
            let additional_damage = frame.damage();
            let session_data = session.user_data().get::<SessionData>().unwrap();
            let mut damage_tracking = session_data.lock().unwrap();

            let buffer = frame.buffer();
            let age = if matches!(buffer_type(&buffer), Some(BufferType::Shm)) {
                // TODO re-use offscreen buffer to damage track screencopy to shm
                0
            } else {
                1
            };

            if !additional_damage.is_empty() {
                let area = output
                    .current_mode()
                    .unwrap()
                    /* TODO: Mode is Buffer..., why is this Physical in the first place */
                    .size
                    .to_logical(1)
                    .to_buffer(1, Transform::Normal)
                    .to_f64();

                let additional_damage_elements: Vec<_> = additional_damage
                    .into_iter()
                    .map(|rect| {
                        rect.to_f64()
                            .to_logical(
                                output.current_scale().fractional_scale(),
                                output.current_transform(),
                                &area,
                            )
                            .to_i32_round()
                    })
                    .map(DamageElement::new)
                    .collect();
                let _ = damage_tracking
                    .dt
                    .damage_output(age, &additional_damage_elements);
            };

            let res = damage_tracking.dt.damage_output(age, elements);

            if !session.draw_cursor() {
                *has_cursor_mode_none = true;
            }

            let res = res.map(|(a, b)| (a.cloned(), b));
            std::mem::drop(damage_tracking);
            (session, frame, res)
        })
        .collect()
}

fn send_screencopy_result<'a>(
    renderer: &mut GlMultiRenderer<'a>,
    output: &Output,
    pre_postprocess_data: &mut PrePostprocessData,
    tx: &std::sync::mpsc::Sender<PendingImageCopyData>,
    frame_result: &RenderFrameResult<GbmBuffer, GbmFramebuffer, CosmicElement<GlMultiRenderer<'a>>>,
    elements: &[CosmicElement<GlMultiRenderer<'a>>],
    (session, frame, res): (
        &ScreencopySessionRef,
        ScreencopyFrame,
        Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), OutputNoMode>,
    ),
    presentation_time: Duration,
) -> Result<()> {
    let (damage, _) = res?;

    let mut sync = SyncPoint::default();
    let mut dmabuf_clone;
    let mut render_buffer;
    let buffer = frame.buffer();
    let mut shm_buffer = false;
    let buffer_size = buffer_dimensions(&buffer).ok_or(RenderError::<
        <GlMultiRenderer as RendererSuper>::Error,
    >::Rendering(
        MultiError::ImportFailed
    ))?;
    let mut fb = if let Ok(dmabuf) = get_dmabuf(&buffer) {
        dmabuf_clone = dmabuf.clone();
        Some(
            renderer
                .bind(&mut dmabuf_clone)
                .map_err(RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering)?,
        )
    } else {
        shm_buffer = true;
        let format = with_buffer_contents(&buffer, |_, _, data| shm_format_to_fourcc(data.format))
            .map_err(|_| OutputNoMode)? // eh, we have to do some error
            .expect("We should be able to convert all hardcoded shm screencopy formats");

        if pre_postprocess_data
            .texture
            .as_ref()
            .is_some_and(|tex| tex.format() == Some(format))
            && (!session.draw_cursor() || pre_postprocess_data.cursor_texture.is_none())
        {
            None
        } else {
            render_buffer =
                Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, buffer_size)
                    .map_err(RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering)?;
            Some(
                renderer
                    .bind(&mut render_buffer)
                    .map_err(RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering)?,
            )
        }
    };

    if let Some(ref damage) = damage {
        let (output_size, output_scale, output_transform) = (
            output.current_mode().ok_or(OutputNoMode)?.size,
            output.current_scale().fractional_scale(),
            output.current_transform(),
        );

        let filter = (!session.draw_cursor())
            .then(|| {
                elements.iter().filter_map(|elem| {
                    if let CosmicElement::Cursor(_) = elem {
                        Some(elem.id().clone())
                    } else {
                        None
                    }
                })
            })
            .into_iter()
            .flatten();

        // If the screen is rotated, we must convert damage to match output.
        let adjusted = damage
            .iter()
            .copied()
            .map(|rect| {
                let logical = rect.to_logical(1);
                logical
                    .to_buffer(
                        1,
                        output_transform.invert(),
                        &buffer_size.to_logical(1, output_transform),
                    )
                    .to_logical(1, Transform::Normal, &buffer_size)
                    .to_physical(1)
            })
            .collect::<Vec<_>>();

        if let Some(tex) = pre_postprocess_data.texture.as_mut() {
            let tex_fb = renderer
                .bind(tex)
                .map_err(RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering)?;

            if let Some(fb) = fb.as_mut() {
                for rect in adjusted.iter().copied() {
                    // TODO: On Vulkan, may need to combine sync points instead of just using latest?
                    sync = renderer
                        .blit(&tex_fb, fb, rect, rect, TextureFilter::Linear)
                        .map_err(
                            RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering,
                        )?;
                }
                if let Some(cursor_geometry) = pre_postprocess_data
                    .cursor_geometry
                    .as_ref()
                    .filter(|_| session.draw_cursor())
                {
                    let cursor_damage = adjusted
                        .iter()
                        .filter_map(|rect| cursor_geometry.intersection(*rect))
                        .map(|rect| Rectangle::new(rect.loc - cursor_geometry.loc, rect.size))
                        .collect::<Vec<_>>();
                    let mut frame = renderer.render(fb, output_size, output_transform).map_err(
                        RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering,
                    )?;
                    frame
                        .as_mut()
                        .render_texture_from_to(
                            pre_postprocess_data.cursor_texture.as_ref().unwrap(),
                            Rectangle::new(
                                Point::from((0., 0.)),
                                cursor_geometry
                                    .size
                                    .to_logical(1)
                                    .to_buffer(1, Transform::Normal)
                                    .to_f64(),
                            ),
                            *cursor_geometry,
                            &cursor_damage,
                            &[*cursor_geometry],
                            Transform::Normal,
                            1.0,
                        )
                        .map_err(GlMultiError::Render)
                        .map_err(
                            RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering,
                        )?;
                    let sync = frame.finish().map_err(
                        RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering,
                    )?;
                    renderer.wait(&sync).map_err(
                        RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering,
                    )?;
                }
            } else {
                fb = Some(tex_fb);
            }
        } else {
            sync = frame_result
                .blit_frame_result(
                    output_size,
                    output_transform,
                    output_scale,
                    renderer,
                    fb.as_mut().unwrap(),
                    adjusted,
                    filter,
                )
                .map_err(|err| match err {
                    BlitFrameResultError::Rendering(err) => {
                        RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering(err)
                    }
                    BlitFrameResultError::Export(_) => {
                        RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering(
                            MultiError::DeviceMissing,
                        )
                    }
                })?;
        };
    }

    let transform = output.current_transform();

    if let Some(data) = submit_buffer(
        frame,
        renderer,
        shm_buffer.then_some(fb.as_mut().unwrap()),
        transform,
        damage.as_deref(),
        sync,
        // Don't reference `Buffer`s since we blit from framebuffer/postprocess buffer
        vec![],
    )? {
        if frame_result.is_empty {
            data.frame
                .success(transform, data.damage, presentation_time);
        } else {
            let _ = tx.send(data);
        }
    }

    Ok(())
}

fn postprocess_elements<'a>(
    renderer: &mut GlMultiRenderer<'a>,
    output: &Output,
    pre_postprocess_data: &PrePostprocessData,
    postprocess_state: &PostprocessState,
    screen_filter: &ScreenFilter,
) -> Vec<CosmicElement<GlMultiRenderer<'a>>> {
    let postprocess_texture_shader = Borrow::<GlesRenderer>::borrow(renderer.as_ref())
        .egl_context()
        .user_data()
        .get::<PostprocessShader>()
        .expect("OffscreenShader should be available through `init_shaders`");

    let mut elements: [Option<TextureShaderElement>; 2] = [None, None];
    if let Some(cursor_texture) = postprocess_state.cursor_texture.as_ref() {
        let cursor_geometry = pre_postprocess_data.cursor_geometry.unwrap();
        let texture_elem = TextureRenderElement::from_texture_render_buffer(
            cursor_geometry.loc.to_f64(),
            cursor_texture,
            None,
            Some(Rectangle::new(
                Point::from((0., 0.)),
                cursor_geometry.size.to_logical(1).to_f64(),
            )),
            Some(
                cursor_geometry
                    .size
                    .to_f64()
                    .to_logical(output.current_scale().fractional_scale())
                    .to_i32_round(),
            ),
            Kind::Cursor,
        );

        elements[0] = Some(TextureShaderElement::new(
            texture_elem,
            postprocess_texture_shader.0.clone(),
            vec![
                Uniform::new("invert", if screen_filter.inverted { 1. } else { 0. }),
                Uniform::new(
                    "color_mode",
                    screen_filter
                        .color_filter
                        .map(|val| val as u8 as f32)
                        .unwrap_or(0.),
                ),
                Uniform::new("night_shift", f32::from(screen_filter.night_shift)),
            ],
        ));
    }

    let texture_elem = TextureRenderElement::from_texture_render_buffer(
        (0., 0.),
        &postprocess_state.texture,
        None,
        Some(Rectangle::new(
            Point::from((0., 0.)),
            postprocess_state.output_config.size.to_logical(1).to_f64(),
        )),
        Some(
            postprocess_state
                .output_config
                .size
                .to_f64()
                .to_logical(postprocess_state.output_config.fractional_scale)
                .to_i32_round(),
        ),
        Kind::Unspecified,
    );
    elements[1] = Some(TextureShaderElement::new(
        texture_elem,
        postprocess_texture_shader.0.clone(),
        vec![
            Uniform::new("invert", if screen_filter.inverted { 1. } else { 0. }),
            Uniform::new(
                "color_mode",
                screen_filter
                    .color_filter
                    .map(|val| val as u8 as f32)
                    .unwrap_or(0.),
            ),
            Uniform::new("night_shift", f32::from(screen_filter.night_shift)),
        ],
    ));

    constrain_render_elements(
        elements.into_iter().flatten(),
        (0, 0),
        Rectangle::from_size(
            output
                .geometry()
                .size
                .as_logical()
                .to_physical_precise_round(output.current_scale().fractional_scale()),
        ),
        Rectangle::new(Point::from((0, 0)), postprocess_state.output_config.size),
        ConstrainScaleBehavior::Fit,
        ConstrainAlign::CENTER,
        postprocess_state.output_config.fractional_scale,
    )
    .map(CosmicElement::<GlMultiRenderer>::Postprocess)
    .collect::<Vec<_>>()
}
