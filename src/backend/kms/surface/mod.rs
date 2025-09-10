// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        element::{CosmicElement, DamageElement},
        init_shaders, output_elements, CursorMode, GlMultiError, GlMultiRenderer,
        PostprocessOutputConfig, PostprocessShader, PostprocessState, CLEAR_COLOR,
    },
    config::ScreenFilter,
    shell::Shell,
    state::SurfaceDmabufFeedback,
    utils::prelude::*,
    wayland::{
        handlers::{
            compositor::recursive_frame_time_estimation,
            screencopy::{submit_buffer, FrameHolder, PendingImageCopyData, SessionData},
        },
        protocols::screencopy::{
            FailureReason, Frame as ScreencopyFrame, SessionRef as ScreencopySessionRef,
        },
    },
};

use anyhow::{Context, Result};
use calloop::channel::Channel;
use cosmic_comp_config::output::comp::AdaptiveSync;
use smithay::{
    backend::{
        allocator::{
            format::FormatSet,
            gbm::{GbmAllocator, GbmBuffer},
            Fourcc,
        },
        drm::{
            compositor::{
                BlitFrameResultError, FrameError, FrameFlags, PrimaryPlaneElement,
                RenderFrameResult,
            },
            exporter::gbm::GbmFramebufferExporter,
            gbm::GbmFramebuffer,
            output::DrmOutput,
            DrmDeviceFd, DrmEventMetadata, DrmEventTime, DrmNode, VrrSupport,
        },
        egl::EGLContext,
        renderer::{
            buffer_dimensions, buffer_type,
            damage::Error as RenderError,
            element::{
                texture::TextureRenderElement,
                utils::{
                    constrain_render_elements, ConstrainAlign, ConstrainScaleBehavior, Relocate,
                    RelocateRenderElement,
                },
                Element, Kind, RenderElementStates,
            },
            gles::{
                element::TextureShaderElement, GlesRenderbuffer, GlesRenderer, GlesTexture, Uniform,
            },
            glow::GlowRenderer,
            multigpu::{ApiDevice, Error as MultiError, GpuManager},
            sync::SyncPoint,
            utils::with_renderer_surface_state,
            Bind, Blit, BufferType, Frame, ImportDma, Offscreen, Renderer, RendererSuper, Texture,
            TextureFilter,
        },
    },
    desktop::utils::OutputPresentationFeedback,
    output::{Output, OutputNoMode},
    reexports::{
        calloop::{
            channel::{channel, Event, Sender},
            timer::{TimeoutAction, Timer},
            EventLoop, LoopHandle, RegistrationToken,
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
        dmabuf::{get_dmabuf, DmabufFeedbackBuilder},
        presentation::Refresh,
        seat::WaylandFocus,
        shm::{shm_format_to_fourcc, with_buffer_contents},
    },
};
use tracing::{error, info, trace, warn};

use std::{
    borrow::{Borrow, BorrowMut},
    collections::{hash_map, HashMap, HashSet},
    mem,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{Receiver, SyncSender},
        Arc, RwLock,
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
    pub(super) feedback: HashMap<DrmNode, SurfaceDmabufFeedback>,
    pub(super) primary_plane_formats: FormatSet,
    overlay_plane_formats: FormatSet,

    loop_handle: LoopHandle<'static, State>,
    thread_command: Sender<ThreadCommand>,
    thread_token: RegistrationToken,
    thread: Option<JoinHandle<()>>,

    dpms: bool,
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

    state: QueueState,
    timings: Timings,
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

#[derive(Debug)]
pub enum QueueState {
    Idle,
    /// A redraw is queued.
    Queued(RegistrationToken),
    /// We submitted a frame to the KMS and waiting for it to be presented.
    WaitingForVBlank {
        redraw_needed: bool,
    },
    /// We did not submit anything to KMS and made a timer to fire at the estimated VBlank.
    WaitingForEstimatedVBlank(RegistrationToken),
    /// A redraw is queued on top of the above.
    WaitingForEstimatedVBlankAndQueued {
        estimated_vblank: RegistrationToken,
        queued_render: RegistrationToken,
    },
}

impl Default for QueueState {
    fn default() -> Self {
        QueueState::Idle
    }
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
    AllowFrameFlags(bool, FrameFlags),
    End,
    DpmsOff,
}

#[derive(Debug)]
pub enum SurfaceCommand {
    SendFrames(usize),
    RenderStates(RenderElementStates),
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
            overlay_plane_formats: FormatSet::default(),
            loop_handle: evlh.clone(),
            thread_command: tx,
            thread_token,
            thread: Some(thread),
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
        overlay_plane_formats: FormatSet,
    ) {
        self.primary_plane_formats = primary_plane_formats;
        self.overlay_plane_formats = overlay_plane_formats;
        self.active.store(true, Ordering::SeqCst);

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
        let _ = self;
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
        let mut visuals: egui::style::Visuals = Default::default();
        visuals.window_shadow = egui::Shadow::NONE;
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
        frame_flags: FrameFlags::DEFAULT,
        vrr_mode: AdaptiveSync::Disabled,

        state: QueueState::Idle,
        timings: Timings::new(None, None, false, target_node),
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

        Ok(())
    }

    fn node_removed(&mut self, node: DrmNode) {
        self.api.as_mut().remove_node(&node);
        // force enumeration
        let _ = self.api.devices();
    }

    #[profiling::function]
    fn on_vblank(&mut self, metadata: Option<DrmEventMetadata>) {
        let Some(compositor) = self.compositor.as_mut() else {
            return;
        };
        if matches!(self.state, QueueState::Idle) {
            // can happen right after resume
            return;
        }

        let now = self.clock.now();
        let presentation_time = match metadata.as_ref().map(|data| &data.time) {
            Some(DrmEventTime::Monotonic(tp)) => Some(tp.clone()),
            _ => None,
        };
        let sequence = metadata.as_ref().map(|data| data.sequence).unwrap_or(0);

        // finish tracy frame
        let _ = self.vblank_frame.take();

        // mark last frame completed
        if let Ok(Some(Some((mut feedback, frames, estimated_presentation_time)))) =
            compositor.frame_submitted()
        {
            if self.mirroring.is_none() {
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

                if let Some(last_sequence) = self.last_sequence {
                    let delta = sequence as f64 - last_sequence as f64;
                    tracy_client::Client::running()
                        .unwrap()
                        .plot(self.sequence_delta_plot_name, delta);
                }
                self.last_sequence = Some(sequence);

                feedback.presented(clock, refresh, sequence as u64, flags);

                self.timings.presented(clock);

                while let Ok(pending_image_copy_data) = frames.recv() {
                    pending_image_copy_data.send_success_when_ready(
                        self.output.current_transform(),
                        &self.loop_handle,
                        clock,
                    );
                }
            }
        }

        let redraw_needed = match mem::replace(&mut self.state, QueueState::Idle) {
            QueueState::Idle => unreachable!(),
            QueueState::Queued(_) => unreachable!(),
            QueueState::WaitingForVBlank { redraw_needed } => redraw_needed,
            QueueState::WaitingForEstimatedVBlank(_) => unreachable!(),
            QueueState::WaitingForEstimatedVBlankAndQueued { .. } => unreachable!(),
        };

        if redraw_needed || self.shell.read().animations_going() {
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

        if force || self.shell.read().animations_going() {
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
                    let name = state.output.name();
                    warn!(?name, "Failed to submit rendering: {:?}", err);
                    state.queue_redraw(true);
                }
                return TimeoutAction::Drop;
            })
            .expect("Failed to schedule render");

        match &self.state {
            QueueState::Idle => {
                self.state = QueueState::Queued(token);
            }
            QueueState::WaitingForEstimatedVBlank(estimated_vblank) => {
                self.state = QueueState::WaitingForEstimatedVBlankAndQueued {
                    estimated_vblank: estimated_vblank.clone(),
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
                    estimated_vblank: estimated_vblank.clone(),
                    queued_render: token,
                };
            }
            _ => unreachable!(),
        }
    }

    #[profiling::function]
    fn redraw(&mut self, estimated_presentation: Duration) -> Result<()> {
        let Some(compositor) = self.compositor.as_mut() else {
            return Ok(());
        };

        let render_node = render_node_for_output(
            self.mirroring.as_ref().unwrap_or(&self.output),
            self.primary_node
                .read()
                .unwrap()
                .as_ref()
                .unwrap_or(&self.target_node),
            &self.target_node,
            &*self.shell.read(),
        );

        let mut renderer = if render_node != self.target_node {
            self.api
                .renderer(&render_node, &self.target_node, compositor.format())
                .unwrap()
        } else {
            self.api.single_renderer(&self.target_node).unwrap()
        };

        self.timings.start_render(&self.clock);

        let mut additional_frame_flags = FrameFlags::empty();
        let mut remove_frame_flags = FrameFlags::empty();

        let (has_active_fullscreen, fullscreen_drives_refresh_rate, animations_going) = {
            let shell = self.shell.read();
            let animations_going = shell.animations_going();
            let output = self.mirroring.as_ref().unwrap_or(&self.output);
            if let Some((_, workspace)) = shell.workspaces.active(output) {
                if let Some(fullscreen_surface) = workspace.get_fullscreen() {
                    const _30_FPS: Duration = Duration::from_nanos(1_000_000_000 / 30);
                    (
                        true,
                        fullscreen_surface.wl_surface().is_some_and(|surface| {
                            recursive_frame_time_estimation(&self.clock, &*surface)
                                .is_some_and(|dur| dur <= _30_FPS)
                        }),
                        animations_going,
                    )
                } else {
                    (false, false, animations_going)
                }
            } else {
                (false, false, animations_going)
            }
        };

        if has_active_fullscreen || animations_going {
            // skip overlay plane assign if we have a fullscreen surface or dynamic contents to save on tests
            remove_frame_flags |= FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT;
        }

        let mut vrr = match self.vrr_mode {
            AdaptiveSync::Force => true,
            _ => false,
        };

        if self.vrr_mode == AdaptiveSync::Enabled {
            vrr = has_active_fullscreen;
        }

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
        )
        .map_err(|err| {
            anyhow::format_err!("Failed to accumulate elements for rendering: {:?}", err)
        })?;

        if vrr && fullscreen_drives_refresh_rate && !self.timings.past_min_render_time(&self.clock)
        {
            additional_frame_flags |= FrameFlags::SKIP_CURSOR_ONLY_UPDATES;
        };
        self.timings.set_vrr(vrr);
        self.timings.elements_done(&self.clock);

        // we can't use the elements after `compositor.render_frame`,
        // so let's collect everything we need for screencopy now
        let mut has_cursor_mode_none = false;
        let frames = self
            .mirroring
            .is_none()
            .then(|| take_screencopy_frames(&self.output, &mut elements, &mut has_cursor_mode_none))
            .unwrap_or_default();

        // actual rendering
        let source_output = self
            .mirroring
            .as_ref()
            .or((!self.screen_filter.is_noop()).then(|| &self.output))
            .filter(|output| {
                PostprocessOutputConfig::for_output_untransformed(output)
                    != PostprocessOutputConfig::for_output(&self.output)
                    || !self.screen_filter.is_noop()
            });

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
                        CLEAR_COLOR,
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

            renderer = self.api.single_renderer(&self.target_node).unwrap();

            elements = postprocess_elements(
                &mut renderer,
                &self.output,
                &pre_postprocess_data,
                &postprocess_state,
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
            compositor.render_frame(
                &mut renderer,
                &elements,
                CLEAR_COLOR, // TODO use a theme neutral color
                self.frame_flags
                    .union(additional_frame_flags)
                    .difference(remove_frame_flags),
            )
        };
        self.timings.draw_done(&self.clock);

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

                if frame_result.needs_sync() {
                    if let PrimaryPlaneElement::Swapchain(elem) = &frame_result.primary_element {
                        elem.sync.wait()?;
                    }
                }

                match compositor.queue_frame(feedback) {
                    x @ Ok(()) | x @ Err(FrameError::EmptyFrame) => {
                        self.timings.submitted_for_presentation(&self.clock);

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
                                session
                                    .user_data()
                                    .get::<SessionData>()
                                    .unwrap()
                                    .lock()
                                    .unwrap()
                                    .reset();
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
                        for (session, frame, _) in frames {
                            session
                                .user_data()
                                .get::<SessionData>()
                                .unwrap()
                                .lock()
                                .unwrap()
                                .reset();
                            frame.fail(FailureReason::Unknown);
                        }
                        return Err(err).with_context(|| "Failed to submit result for display");
                    }
                };
            }
            Err(err) => {
                compositor.reset_buffers();
                anyhow::bail!("Rendering failed: {}", err);
            }
        }

        for device in self.api.devices_mut()? {
            device.renderer_mut().cleanup_texture_cache()?;
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
    let nodes = workspace
        .get_fullscreen()
        .map(|w| vec![w.clone()])
        .unwrap_or_else(|| {
            workspace
                .mapped()
                .map(|mapped| mapped.active_window())
                .collect::<Vec<_>>()
        })
        .into_iter()
        .flat_map(|w| w.wl_surface().and_then(|s| source_node_for_surface(&s)))
        .collect::<Vec<_>>();

    if nodes.contains(&target_node) || nodes.is_empty() {
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
    overlay_plane_formats: FormatSet,
) -> SurfaceDmabufFeedback {
    let combined_formats = render_formats
        .intersection(&target_formats)
        .copied()
        .collect::<FormatSet>();

    // We limit the scan-out trache to formats we can also render from
    // so that there is always a fallback render path available in case
    // the supplied buffer can not be scanned out directly
    let primary_plane_formats = primary_plane_formats
        .intersection(&combined_formats)
        .copied()
        .collect::<FormatSet>();
    let overlay_plane_formats = overlay_plane_formats
        .intersection(&combined_formats)
        .copied()
        .collect::<FormatSet>();

    let builder = DmabufFeedbackBuilder::new(render_node.dev_id(), render_formats);
    /*
    // iris doesn't handle nvidia buffers very well (it hangs).
    // so only do this in the future with v6 and clients telling us the gpu
    if target_node != render_node.dev_id() && !combined_formats.is_empty() {
        builder = builder.add_preference_tranche(
            target_node,
            Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
            combined_formats,
        );
    };
    */

    let render_feedback = builder.clone().build().unwrap();
    // we would want to do this in other cases as well, but same thing as above applies
    let primary_scanout_feedback = if target_node == render_node {
        builder
            .clone()
            .add_preference_tranche(
                target_node.dev_id(),
                Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
                primary_plane_formats.clone(),
            )
            .build()
            .unwrap()
    } else {
        builder.clone().build().unwrap()
    };
    let scanout_feedback = if target_node == render_node {
        builder
            .add_preference_tranche(
                target_node.dev_id(),
                Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
                FormatSet::from_iter(
                    primary_plane_formats
                        .into_iter()
                        .chain(overlay_plane_formats.into_iter()),
                ),
            )
            .build()
            .unwrap()
    } else {
        builder.build().unwrap()
    };

    SurfaceDmabufFeedback {
        render_feedback,
        scanout_feedback,
        primary_scanout_feedback,
    }
}

// TODO: Don't mutate `elements`
fn take_screencopy_frames(
    output: &Output,
    elements: &mut Vec<CosmicElement<GlMultiRenderer>>,
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

            let old_len = if !additional_damage.is_empty() {
                let area = output
                    .current_mode()
                    .unwrap()
                    /* TODO: Mode is Buffer..., why is this Physical in the first place */
                    .size
                    .to_logical(1)
                    .to_buffer(1, Transform::Normal)
                    .to_f64();

                let old_len = elements.len();
                elements.extend(
                    additional_damage
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
                        .map(Into::into),
                );

                Some(old_len)
            } else {
                None
            };

            let buffer = frame.buffer();
            let age = if matches!(buffer_type(&frame.buffer()), Some(BufferType::Shm)) {
                // TODO re-use offscreen buffer to damage track screencopy to shm
                0
            } else {
                damage_tracking.age_for_buffer(&buffer)
            };
            let res = damage_tracking.dt.damage_output(age, &elements);

            if let Some(old_len) = old_len {
                elements.truncate(old_len);
            }

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
    elements: &[CosmicElement<GlMultiRenderer>],
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
            && (session.draw_cursor() == false || pre_postprocess_data.cursor_texture.is_none())
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
            let mut tex_fb = renderer
                .bind(tex)
                .map_err(RenderError::<<GlMultiRenderer as RendererSuper>::Error>::Rendering)?;

            if let Some(fb) = fb.as_mut() {
                for rect in adjusted.iter().copied() {
                    // TODO: On Vulkan, may need to combine sync points instead of just using latest?
                    sync = renderer
                        .blit(&mut tex_fb, fb, rect, rect, TextureFilter::Linear)
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
