// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        element::{CosmicElement, DamageElement},
        init_shaders, output_elements, CursorMode, GlMultiRenderer, CLEAR_COLOR,
    },
    config::AdaptiveSync,
    shell::Shell,
    state::SurfaceDmabufFeedback,
    utils::prelude::*,
    wayland::{
        handlers::screencopy::{submit_buffer, FrameHolder, SessionData},
        protocols::screencopy::{
            FailureReason, Frame as ScreencopyFrame, Session as ScreencopySession,
        },
    },
};

use anyhow::{Context, Result};
use calloop::channel::Channel;
use smithay::{
    backend::{
        allocator::{
            format::FormatSet,
            gbm::{GbmAllocator, GbmDevice},
            Fourcc,
        },
        drm::{
            compositor::{BlitFrameResultError, FrameError, FrameFlags, PrimaryPlaneElement},
            output::DrmOutput,
            DrmDeviceFd, DrmEventMetadata, DrmEventTime, DrmNode, VrrSupport,
        },
        egl::EGLContext,
        renderer::{
            buffer_dimensions,
            damage::{Error as RenderError, OutputDamageTracker},
            element::{
                texture::{TextureRenderBuffer, TextureRenderElement},
                utils::{constrain_render_elements, ConstrainAlign, ConstrainScaleBehavior},
                Element, Kind, RenderElementStates,
            },
            gles::{GlesRenderbuffer, GlesTexture},
            glow::GlowRenderer,
            multigpu::{Error as MultiError, GpuManager},
            sync::SyncPoint,
            utils::with_renderer_surface_state,
            Bind, ImportDma, Offscreen, Renderer, Texture,
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
    utils::{Buffer as BufferCoords, Clock, Monotonic, Physical, Rectangle, Transform},
    wayland::{
        dmabuf::{get_dmabuf, DmabufFeedbackBuilder},
        presentation::Refresh,
        seat::WaylandFocus,
        shm::{shm_format_to_fourcc, with_buffer_contents},
    },
};
use tracing::{error, trace, warn};

use std::{
    borrow::BorrowMut,
    collections::{hash_map, HashMap, HashSet},
    mem,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{Receiver, SyncSender},
        Arc, RwLock,
    },
    time::Duration,
};

mod timings;
pub use self::timings::Timings;

use super::{drm_helpers, render::gles::GbmGlowBackend};

#[cfg(feature = "debug")]
use smithay_egui::EguiState;

#[cfg(feature = "debug")]
static INTEL_LOGO: &'static [u8] = include_bytes!("../../../../resources/icons/intel.svg");
#[cfg(feature = "debug")]
static AMD_LOGO: &'static [u8] = include_bytes!("../../../../resources/icons/amd.svg");
#[cfg(feature = "debug")]
static NVIDIA_LOGO: &'static [u8] = include_bytes!("../../../../resources/icons/nvidia.svg");

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

    dpms: bool,
}

pub struct SurfaceThreadState {
    // rendering
    api: GpuManager<GbmGlowBackend<DrmDeviceFd>>,
    primary_node: DrmNode,
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
    mirroring_textures: HashMap<DrmNode, MirroringState>,

    shell: Arc<RwLock<Shell>>,

    loop_handle: LoopHandle<'static, Self>,
    clock: Clock<Monotonic>,

    #[cfg(feature = "debug")]
    egui: EguiState,
}

#[derive(Debug)]
struct MirroringState {
    texture: TextureRenderBuffer<GlesTexture>,
    damage_tracker: OutputDamageTracker,
}

impl MirroringState {
    fn new_with_renderer(
        renderer: &mut GlMultiRenderer,
        format: Fourcc,
        output: &Output,
    ) -> Result<Self> {
        // Apply inverse of output transform to mode size to get correct size
        // for an untransformed render.
        let size = output.current_transform().invert().transform_size(
            output
                .current_mode()
                .map(|mode| mode.size)
                .unwrap_or_default(),
        );
        let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
        let opaque_regions = vec![Rectangle::from_size(buffer_size)];

        let texture = Offscreen::<GlesTexture>::create_buffer(renderer, format, buffer_size)?;
        let texture_buffer = TextureRenderBuffer::from_texture(
            renderer,
            texture,
            1,
            Transform::Normal,
            Some(opaque_regions),
        );

        // Don't use `from_output` to avoid applying output transform
        let damage_tracker = OutputDamageTracker::new(
            size,
            output.current_scale().fractional_scale(),
            Transform::Normal,
        );

        Ok(MirroringState {
            texture: texture_buffer,
            damage_tracker,
        })
    }
}

pub type GbmDrmOutput = DrmOutput<
    GbmAllocator<DrmDeviceFd>,
    GbmDevice<DrmDeviceFd>,
    Option<(
        OutputPresentationFeedback,
        Receiver<(ScreencopyFrame, Vec<Rectangle<i32, BufferCoords>>)>,
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
        result: SyncSender<Result<()>>,
    },
    NodeAdded {
        node: DrmNode,
        gbm: GbmAllocator<DrmDeviceFd>,
        egl: EGLContext, // TODO: Option for software rendering
    },
    NodeRemoved {
        node: DrmNode,
    },
    UpdateMirroring(Option<Output>),
    VBlank(Option<DrmEventMetadata>),
    ScheduleRender,
    AdaptiveSyncAvailable(SyncSender<Result<VrrSupport>>),
    UseAdaptiveSync(AdaptiveSync),
    AllowFrameFlags(bool, FrameFlags, SyncSender<()>),
    End,
    DpmsOff,
}

#[derive(Debug)]
pub enum SurfaceCommand {
    SendFrames(usize),
    RenderStates(RenderElementStates),
}

impl Surface {
    pub fn new(
        output: &Output,
        crtc: crtc::Handle,
        connector: connector::Handle,
        primary_node: DrmNode,
        dev_node: DrmNode,
        target_node: DrmNode,
        evlh: &LoopHandle<'static, State>,
        shell: Arc<RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
    ) -> Result<Self> {
        let (tx, rx) = channel::<ThreadCommand>();
        let (tx2, rx2) = channel::<SurfaceCommand>();
        let active = Arc::new(AtomicBool::new(false));

        let active_clone = active.clone();
        let output_clone = output.clone();

        std::thread::Builder::new()
            .name(format!("surface-{}", output.name()))
            .spawn(move || {
                if let Err(err) = surface_thread(
                    output_clone,
                    primary_node,
                    target_node,
                    shell,
                    active_clone,
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
                        .surfaces
                        .get_mut(&crtc)
                        .unwrap();

                    state
                        .common
                        .send_dmabuf_feedback(&output_clone, &states, |source_node| {
                            Some(
                                surface
                                    .feedback
                                    .entry(source_node)
                                    .or_insert_with(|| {
                                        let render_formats = kms
                                            .api
                                            .single_renderer(&source_node)
                                            .unwrap()
                                            .dmabuf_formats();
                                        let target_formats = kms
                                            .api
                                            .single_renderer(&target_node)
                                            .unwrap()
                                            .dmabuf_formats();
                                        get_surface_dmabuf_feedback(
                                            source_node,
                                            target_node,
                                            render_formats,
                                            target_formats,
                                            surface.primary_plane_formats.clone(),
                                            surface.overlay_plane_formats.clone(),
                                        )
                                    })
                                    .clone(),
                            )
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
        let _ = self
            .thread_command
            .send(ThreadCommand::NodeAdded { node, gbm, egl });
    }

    pub fn remove_node(&mut self, node: DrmNode) {
        self.known_nodes.remove(&node);
        let _ = self
            .thread_command
            .send(ThreadCommand::NodeRemoved { node });
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

    pub fn adaptive_sync_support(&self) -> Result<VrrSupport> {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self
            .thread_command
            .send(ThreadCommand::AdaptiveSyncAvailable(tx));
        rx.recv().context("Surface thread died")?
    }

    pub fn use_adaptive_sync(&mut self, vrr: AdaptiveSync) -> Result<bool> {
        if vrr != AdaptiveSync::Disabled {
            let (tx, rx) = std::sync::mpsc::sync_channel(1);
            let _ = self
                .thread_command
                .send(ThreadCommand::AdaptiveSyncAvailable(tx));
            match rx.recv().context("Surface thread died")?? {
                VrrSupport::RequiresModeset if vrr == AdaptiveSync::Enabled => return Ok(false),
                VrrSupport::NotSupported => return Ok(false),
                _ => {}
            };
        }

        let _ = self
            .thread_command
            .send(ThreadCommand::UseAdaptiveSync(vrr));
        Ok(true)
    }

    pub fn allow_frame_flags(&mut self, flag: bool, flags: FrameFlags) {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self
            .thread_command
            .send(ThreadCommand::AllowFrameFlags(flag, flags, tx));
        let _ = rx.recv();
    }

    pub fn suspend(&mut self) {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let _ = self.thread_command.send(ThreadCommand::Suspend(tx));
        let _ = rx.recv();
    }

    pub fn resume(&mut self, compositor: GbmDrmOutput) -> Result<()> {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        (self.primary_plane_formats, self.overlay_plane_formats) =
            compositor.with_compositor(|c| {
                (
                    c.surface().plane_info().formats.clone(),
                    c.surface()
                        .planes()
                        .overlay
                        .iter()
                        .flat_map(|p| p.formats.iter().cloned())
                        .collect::<FormatSet>(),
                )
            });

        let _ = self.thread_command.send(ThreadCommand::Resume {
            compositor,
            result: tx,
        });

        rx.recv().context("Surface thread died")?
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
}

impl Drop for Surface {
    fn drop(&mut self) {
        let _ = self.thread_command.send(ThreadCommand::End);
        self.loop_handle.remove(self.thread_token);
    }
}

fn surface_thread(
    output: Output,
    primary_node: DrmNode,
    target_node: DrmNode,
    shell: Arc<RwLock<Shell>>,
    active: Arc<AtomicBool>,
    thread_sender: Sender<SurfaceCommand>,
    thread_receiver: Channel<ThreadCommand>,
    startup_done: Arc<AtomicBool>,
) -> Result<()> {
    profiling::register_thread!(&format!("Surface Thread {}", output.name()));

    let mut event_loop = EventLoop::try_new().unwrap();

    let api = GpuManager::new(GbmGlowBackend::<DrmDeviceFd>::default())
        .context("Failed to initialize rendering api")?;
    /*
    let software_api = GpuManager::new(GbmPixmanBackend::<DrmDeviceFd>::with_allocator_flags(
        gbm_flags,
    ))
    .context("Failed to initialize software rendering");
    */

    #[cfg(feature = "debug")]
    let egui = {
        let state =
            smithay_egui::EguiState::new(smithay::utils::Rectangle::from_size((400, 800).into()));
        let mut visuals: egui::style::Visuals = Default::default();
        visuals.window_shadow = egui::Shadow::NONE;
        state.context().set_visuals(visuals);
        state
    };

    let mut state = SurfaceThreadState {
        api,
        primary_node,
        target_node,
        active,
        compositor: None,
        frame_flags: FrameFlags::DEFAULT,
        vrr_mode: AdaptiveSync::Disabled,

        state: QueueState::Idle,
        timings: Timings::new(None, None, false),
        frame_callback_seq: 0,
        thread_sender,

        output,
        mirroring: None,
        mirroring_textures: HashMap::new(),

        shell,
        loop_handle: event_loop.handle(),
        clock: Clock::new(),
        #[cfg(feature = "debug")]
        egui,
    };

    let signal = event_loop.get_signal();
    event_loop
        .handle()
        .insert_source(thread_receiver, move |command, _, state| match command {
            Event::Msg(ThreadCommand::Suspend(tx)) => state.suspend(tx),
            Event::Msg(ThreadCommand::Resume { compositor, result }) => {
                let _ = result.send(state.resume(compositor));
            }
            Event::Msg(ThreadCommand::NodeAdded { node, gbm, egl }) => {
                if let Err(err) = state.node_added(node, gbm, egl) {
                    warn!(?err, ?node, "Failed to add node to surface-thread");
                }
            }
            Event::Msg(ThreadCommand::NodeRemoved { node }) => {
                state.node_removed(node);
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
            Event::Msg(ThreadCommand::AllowFrameFlags(flag, mut flags, tx)) => {
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
                let _ = tx.send(());
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

    fn resume(&mut self, compositor: GbmDrmOutput) -> Result<()> {
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

        let min_min_refresh_interval = Duration::from_secs_f64(1. / 30.); // 30Hz
        self.timings.set_min_refresh_interval(Some(
            min_hz
                .map(|min| Duration::from_secs_f64(1. / min as f64))
                .unwrap_or(min_min_refresh_interval) // alternatively use 30Hz
                .max(min_min_refresh_interval),
        ));

        if crate::utils::env::bool_var("COSMIC_DISABLE_DIRECT_SCANOUT").unwrap_or(false) {
            self.frame_flags.remove(FrameFlags::ALLOW_SCANOUT);
        } else if crate::utils::env::bool_var("COSMIC_DISABLE_OVERLAY_SCANOUT").unwrap_or(false) {
            self.frame_flags
                .remove(FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT);
        }
        self.active.store(true, Ordering::SeqCst);
        self.compositor = Some(compositor);
        Ok(())
    }

    fn node_added(
        &mut self,
        node: DrmNode,
        gbm: GbmAllocator<DrmDeviceFd>,
        egl: EGLContext,
    ) -> Result<()> {
        //if let Some(egl) = egl {
        let mut renderer =
            unsafe { GlowRenderer::new(egl) }.context("Failed to create renderer")?;
        init_shaders(renderer.borrow_mut()).context("Failed to initialize shaders")?;

        #[cfg(feature = "debug")]
        {
            self.egui
                .load_svg(&mut renderer, String::from("intel"), INTEL_LOGO)
                .unwrap();
            self.egui
                .load_svg(&mut renderer, String::from("amd"), AMD_LOGO)
                .unwrap();
            self.egui
                .load_svg(&mut renderer, String::from("nvidia"), NVIDIA_LOGO)
                .unwrap();
        }

        self.api.as_mut().add_node(node, gbm, renderer);
        /*
        } else {
            self.software_api.as_mut().add_node(node, gbm);
        }
        */

        Ok(())
    }

    fn node_removed(&mut self, node: DrmNode) {
        self.api.as_mut().remove_node(&node);
        //self.software_api.as_mut().remove_node(node);
    }

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

        // mark last frame completed
        if let Ok(Some(Some((mut feedback, frames)))) = compositor.frame_submitted() {
            if self.mirroring.is_none() {
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

                feedback.presented(clock, refresh, sequence as u64, flags);

                self.timings.presented(clock);

                while let Ok((frame, damage)) = frames.recv() {
                    frame.success(self.output.current_transform(), damage, clock);
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

        if redraw_needed || self.shell.read().unwrap().animations_going() {
            self.queue_redraw(false);
        } else {
            self.send_frame_callbacks();
        }
    }

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

        if force || self.shell.read().unwrap().animations_going() {
            self.queue_redraw(false);
        } else {
            self.send_frame_callbacks();
        }
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

    fn redraw(&mut self, estimated_presentation: Duration) -> Result<()> {
        let Some(compositor) = self.compositor.as_mut() else {
            return Ok(());
        };

        let render_node = render_node_for_output(
            self.mirroring.as_ref().unwrap_or(&self.output),
            &self.primary_node,
            &self.target_node,
            &*self.shell.read().unwrap(),
        );

        let mut renderer = if render_node != self.target_node {
            self.api
                .renderer(&render_node, &self.target_node, compositor.format())
                .unwrap()
        } else {
            self.api.single_renderer(&self.target_node).unwrap()
        };

        self.timings.start_render(&self.clock);

        let mut vrr = match self.vrr_mode {
            AdaptiveSync::Force => true,
            _ => false,
        };

        let has_active_fullscreen = {
            let shell = self.shell.read().unwrap();
            let output = self.mirroring.as_ref().unwrap_or(&self.output);
            if let Some((_, workspace)) = shell.workspaces.active(output) {
                workspace.get_fullscreen().is_some()
            } else {
                false
            }
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
        let additional_frame_flags = if vrr
            && has_active_fullscreen
            && !self.timings.past_min_presentation_time(&self.clock)
        {
            FrameFlags::SKIP_CURSOR_ONLY_UPDATES
        } else {
            FrameFlags::empty()
        };
        self.timings.set_vrr(vrr);
        self.timings.elements_done(&self.clock);

        // we can't use the elements after `compositor.render_frame`,
        // so let's collect everything we need for screencopy now
        let frames: Vec<(
            ScreencopySession,
            ScreencopyFrame,
            Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), OutputNoMode>,
        )> = self
            .mirroring
            .is_none()
            .then(|| {
                self.output
                    .take_pending_frames()
                    .into_iter()
                    .map(|(session, frame)| {
                        let additional_damage = frame.damage();
                        let session_data = session.user_data().get::<SessionData>().unwrap();
                        let mut damage_tracking = session_data.lock().unwrap();

                        let old_len = if !additional_damage.is_empty() {
                            let area = self
                                .output
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
                                                self.output.current_scale().fractional_scale(),
                                                self.output.current_transform(),
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
                        let age = damage_tracking.age_for_buffer(&buffer);
                        let res = damage_tracking.dt.damage_output(age, &elements);

                        if let Some(old_len) = old_len {
                            elements.truncate(old_len);
                        }

                        let res = res.map(|(a, b)| (a.cloned(), b));
                        std::mem::drop(damage_tracking);
                        (session, frame, res)
                    })
                    .collect()
            }).unwrap_or_default();

        // actual rendering
        let res = if let Some(mirrored_output) = self.mirroring.as_ref().filter(|mirrored_output| {
            mirrored_output.current_mode().is_some_and(|mirror_mode| {
                self.output
                    .current_mode()
                    .is_some_and(|mode| mode != mirror_mode)
            }) || mirrored_output.current_scale().fractional_scale()
                != self.output.current_scale().fractional_scale()
        }) {
            let mirroring_state = match self.mirroring_textures.entry(self.target_node) {
                hash_map::Entry::Occupied(occupied) => occupied.into_mut(),
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(MirroringState::new_with_renderer(
                        &mut renderer,
                        compositor.format(),
                        mirrored_output,
                    )?)
                }
            };

            mirroring_state
                .texture
                .render()
                .draw::<_, <GlMultiRenderer as Renderer>::Error>(|tex| {
                    let res = match mirroring_state.damage_tracker.render_output_with(
                        &mut renderer,
                        tex.clone(),
                        1,
                        &elements,
                        CLEAR_COLOR,
                    ) {
                        Ok(res) => res,
                        Err(RenderError::Rendering(err)) => return Err(err),
                        Err(RenderError::OutputNoMode(_)) => unreachable!(),
                    };

                    renderer.wait(&res.sync)?;

                    let transform = mirrored_output.current_transform();
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

            let texture_elem = TextureRenderElement::from_texture_render_buffer(
                (0., 0.),
                &mirroring_state.texture,
                Some(1.0),
                None,
                None,
                Kind::Unspecified,
            );
            let texture_geometry =
                texture_elem.geometry(self.output.current_scale().fractional_scale().into());
            elements = constrain_render_elements(
                std::iter::once(texture_elem),
                (0, 0),
                Rectangle::from_size(
                    self.output
                        .geometry()
                        .size
                        .as_logical()
                        .to_f64()
                        .to_physical(self.output.current_scale().fractional_scale())
                        .to_i32_round(),
                ),
                texture_geometry,
                ConstrainScaleBehavior::Fit,
                ConstrainAlign::CENTER,
                1.0,
            )
            .map(CosmicElement::Mirror)
            .collect::<Vec<_>>();

            renderer = self.api.single_renderer(&self.target_node).unwrap();
            if let Err(err) = compositor.with_compositor(|c| c.use_vrr(vrr)) {
                warn!("Unable to set adaptive VRR state: {}", err);
            }
            compositor.render_frame(
                &mut renderer,
                &elements,
                [0.0, 0.0, 0.0, 1.0],
                self.frame_flags.union(additional_frame_flags),
            )
        } else {
            if let Err(err) = compositor.with_compositor(|c| c.use_vrr(vrr)) {
                warn!("Unable to set adaptive VRR state: {}", err);
            }
            compositor.render_frame(
                &mut renderer,
                &elements,
                CLEAR_COLOR, // TODO use a theme neutral color
                self.frame_flags.union(additional_frame_flags),
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
                            .unwrap()
                            .take_presentation_feedback(&self.output, &frame_result.states),
                        rx,
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

                        for (session, frame, res) in frames {
                            let damage = match res {
                                Ok((damage, _)) => damage,
                                Err(err) => {
                                    tracing::warn!(?err, "Failed to screencopy");
                                    session
                                        .user_data()
                                        .get::<SessionData>()
                                        .unwrap()
                                        .lock()
                                        .unwrap()
                                        .reset();
                                    frame.fail(FailureReason::Unknown);
                                    continue;
                                }
                            };

                            let mut sync = SyncPoint::default();

                            if let Some(ref damage) = damage {
                                let buffer = frame.buffer();
                                if let Ok(dmabuf) = get_dmabuf(&buffer) {
                                    renderer
                                        .bind(dmabuf.clone())
                                        .map_err(RenderError::<<GlMultiRenderer as Renderer>::Error>::Rendering)?;
                                } else {
                                    let size = buffer_dimensions(&buffer).ok_or(RenderError::<
                                        <GlMultiRenderer as Renderer>::Error,
                                    >::Rendering(
                                        MultiError::ImportFailed,
                                    ))?;
                                    let format =
                                        with_buffer_contents(&buffer, |_, _, data| shm_format_to_fourcc(data.format))
                                            .map_err(|_| OutputNoMode)? // eh, we have to do some error
                                            .expect("We should be able to convert all hardcoded shm screencopy formats");
                                    let render_buffer =
                                        Offscreen::<GlesRenderbuffer>::create_buffer(
                                            &mut renderer,
                                            format,
                                            size,
                                        )
                                        .map_err(RenderError::<<GlMultiRenderer as Renderer>::Error>::Rendering)?;
                                    renderer
                                        .bind(render_buffer)
                                        .map_err(RenderError::<<GlMultiRenderer as Renderer>::Error>::Rendering)?;
                                }

                                let (output_size, output_scale, output_transform) = (
                                    self.output.current_mode().ok_or(OutputNoMode)?.size,
                                    self.output.current_scale().fractional_scale(),
                                    self.output.current_transform(),
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
                                let adjusted = damage.iter().copied().map(|mut d| {
                                    d.size = d
                                        .size
                                        .to_logical(1)
                                        .to_buffer(1, output_transform)
                                        .to_logical(1, Transform::Normal)
                                        .to_physical(1);
                                    d
                                });
                                match frame_result
                                    .blit_frame_result(
                                        output_size,
                                        output_transform,
                                        output_scale,
                                        &mut renderer,
                                        adjusted,
                                        filter,
                                    )
                                    .map_err(|err| match err {
                                        BlitFrameResultError::Rendering(err) => RenderError::<
                                            <GlMultiRenderer as Renderer>::Error,
                                        >::Rendering(
                                            err
                                        ),
                                        BlitFrameResultError::Export(_) => RenderError::<
                                            <GlMultiRenderer as Renderer>::Error,
                                        >::Rendering(
                                            MultiError::DeviceMissing,
                                        ),
                                    }) {
                                    Ok(new_sync) => {
                                        sync = new_sync;
                                    }
                                    Err(err) => {
                                        tracing::warn!(?err, "Failed to screencopy");
                                        session
                                            .user_data()
                                            .get::<SessionData>()
                                            .unwrap()
                                            .lock()
                                            .unwrap()
                                            .reset();
                                        frame.fail(FailureReason::Unknown);
                                        continue;
                                    }
                                };
                            }

                            let transform = self.output.current_transform();

                            match submit_buffer(
                                frame,
                                &mut renderer,
                                transform,
                                damage.as_deref(),
                                sync,
                            ) {
                                Ok(Some((frame, damage))) => {
                                    if frame_result.is_empty {
                                        frame.success(transform, damage, self.clock.now());
                                    } else {
                                        let _ = tx.send((frame, damage));
                                    }
                                }
                                Ok(None) => {}
                                Err(err) => {
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
                        }

                        if self.mirroring.is_none() {
                            let states = frame_result.states;
                            self.send_dmabuf_feedback(states);
                        }

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

                            if self.mirroring.is_none() {
                                self.frame_callback_seq = self.frame_callback_seq.wrapping_add(1);
                                self.send_frame_callbacks();
                            }
                        } else {
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
        self.mirroring_textures.clear();
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
