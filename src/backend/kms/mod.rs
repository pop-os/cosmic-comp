// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::OutputConfig,
    shell::Shell,
    state::{BackendData, ClientState, Common, Data, Fps},
    utils::prelude::*,
    wayland::{
        handlers::screencopy::UserdataExt,
        protocols::screencopy::{BufferParams, Session as ScreencopySession},
    },
};

use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::{
            dmabuf::{AnyError, Dmabuf, DmabufAllocator},
            gbm::{GbmAllocator, GbmBufferFlags, GbmDevice},
            vulkan::{ImageUsageFlags, VulkanAllocator},
            Allocator, Format,
        },
        drm::{
            DrmDevice, DrmDeviceFd, DrmEvent, DrmEventTime, DrmNode, GbmBufferedSurface, NodeType,
        },
        egl::{EGLContext, EGLDevice, EGLDisplay},
        input::InputEvent,
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{
            damage::DamageTrackedRenderer,
            gles2::Gles2Renderbuffer,
            glow::GlowRenderer,
            multigpu::{gbm::GbmGlesBackend, GpuManager},
        },
        session::{libseat::LibSeatSession, Event as SessionEvent, Session},
        udev::{all_gpus, primary_gpu, UdevBackend, UdevEvent},
        vulkan::{version::Version, Instance, PhysicalDevice},
    },
    desktop::utils::OutputPresentationFeedback,
    input::Seat,
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::{
        calloop::{
            timer::{TimeoutAction, Timer},
            Dispatcher, EventLoop, InsertError, LoopHandle, RegistrationToken,
        },
        drm::control::{connector, crtc, Device as ControlDevice, ModeTypeFlags},
        input::Libinput,
        nix::{fcntl::OFlag, sys::stat::dev_t},
        wayland_protocols::wp::presentation_time::server::wp_presentation_feedback,
        wayland_server::{protocol::wl_surface::WlSurface, DisplayHandle, Resource},
    },
    utils::{DeviceFd, Size, Transform},
    wayland::{dmabuf::DmabufGlobal, seat::WaylandFocus},
    xwayland::XWaylandClientData,
};
use tracing::{debug, error, info, warn};

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ffi::CStr,
    os::unix::io::FromRawFd,
    path::PathBuf,
    time::Duration,
};

mod drm_helpers;
mod socket;
use socket::*;

use super::render::{CursorMode, GlMultiRenderer};
// for now we assume we need at least 3ms
const MIN_RENDER_TIME: Duration = Duration::from_millis(3);

pub struct KmsState {
    devices: HashMap<DrmNode, Device>,
    pub api: GpuManager<GbmGlesBackend<GlowRenderer>>,
    pub primary: DrmNode,
    session: LibSeatSession,
    _tokens: Vec<RegistrationToken>,
}

pub struct Device {
    render_node: DrmNode,
    surfaces: HashMap<crtc::Handle, Surface>,
    drm: Dispatcher<'static, DrmDevice, Data>,
    gbm: GbmDevice<DrmDeviceFd>,
    allocator: Box<dyn Allocator<Buffer = Dmabuf, Error = AnyError>>,
    formats: HashSet<Format>,
    supports_atomic: bool,
    event_token: Option<RegistrationToken>,
    socket: Option<Socket>,
}

pub struct Surface {
    surface:
        Option<GbmBufferedSurface<GbmAllocator<DrmDeviceFd>, Option<OutputPresentationFeedback>>>,
    damage_tracker: DamageTrackedRenderer,
    connector: connector::Handle,
    output: Output,
    refresh_rate: u32,
    vrr: bool,
    pending: bool,
    dirty: bool,
    render_timer_token: Option<RegistrationToken>,
    fps: Fps,
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<'static, Data>,
    state: &mut State,
) -> Result<()> {
    let (session, notifier) = LibSeatSession::new().context("Failed to acquire session")?;

    let udev_backend = UdevBackend::new(session.seat())?;
    let mut libinput_context =
        Libinput::new_with_udev::<LibinputSessionInterface<LibSeatSession>>(session.clone().into());
    libinput_context
        .udev_assign_seat(&session.seat())
        .map_err(|_| anyhow::anyhow!("Failed to assign seat to libinput"))?;
    let libinput_backend = LibinputInputBackend::new(libinput_context.clone());

    let libinput_event_source = event_loop
        .handle()
        .insert_source(libinput_backend, move |mut event, _, data| {
            if let &mut InputEvent::DeviceAdded { ref mut device } = &mut event {
                data.state.common.config.read_device(device);
            }
            data.state.process_input_event(event);
            for output in data.state.common.shell.outputs() {
                if let Err(err) = data.state.backend.kms().schedule_render(
                    &data.state.common.event_loop_handle,
                    output,
                    None,
                    None,
                ) {
                    error!(
                        ?err,
                        "Error scheduling event loop for output {}.",
                        output.name(),
                    );
                }
            }
        })
        .map_err(|err| err.error)
        .context("Failed to initialize libinput event source")?;
    let api = GpuManager::new(GbmGlesBackend::<GlowRenderer>::default())
        .context("Failed to initialize renderers")?;

    // TODO get this info from system76-power, if available and setup a watcher
    let primary = if let Some(path) = std::env::var("COSMIC_RENDER_DEVICE")
        .ok()
        .and_then(|x| DrmNode::from_path(x).ok())
    {
        path
    } else {
        primary_gpu(session.seat())
            .ok()
            .flatten()
            .and_then(|x| DrmNode::from_path(x).ok())
            .and_then(|x| x.node_with_type(NodeType::Render).and_then(Result::ok))
            .unwrap_or_else(|| {
                for dev in all_gpus(session.seat()).expect("No GPU found") {
                    if let Some(node) = DrmNode::from_path(dev)
                        .ok()
                        .and_then(|x| x.node_with_type(NodeType::Render).and_then(Result::ok))
                    {
                        return node;
                    }
                }
                panic!("Failed to initialize any GPU");
            })
    };
    info!("Using {} as primary gpu for rendering.", primary);

    let udev_dispatcher = Dispatcher::new(udev_backend, move |event, _, data: &mut Data| {
        match match event {
            UdevEvent::Added { device_id, path } => data
                .state
                .device_added(device_id, path, &data.display.handle(), true)
                .with_context(|| format!("Failed to add drm device: {}", device_id)),
            UdevEvent::Changed { device_id } => data
                .state
                .device_changed(device_id)
                .with_context(|| format!("Failed to update drm device: {}", device_id)),
            UdevEvent::Removed { device_id } => data
                .state
                .device_removed(device_id, &data.display.handle())
                .with_context(|| format!("Failed to remove drm device: {}", device_id)),
        } {
            Ok(()) => {
                debug!("Successfully handled udev event.")
            }
            Err(err) => {
                error!(?err, "Error while handling udev event.")
            }
        }
    });
    let udev_event_source = event_loop
        .handle()
        .register_dispatcher(udev_dispatcher.clone())
        .unwrap();

    let handle = event_loop.handle();
    let loop_signal = state.common.event_loop_signal.clone();
    let dispatcher = udev_dispatcher.clone();
    let session_event_source = event_loop
        .handle()
        .insert_source(notifier, move |event, &mut (), data| match event {
            SessionEvent::ActivateSession => {
                if let Err(err) = libinput_context.resume() {
                    error!(?err, "Failed to resume libinput context.");
                }
                for device in data.state.backend.kms().devices.values() {
                    device.drm.as_source_ref().activate();
                }
                let dispatcher = dispatcher.clone();
                handle.insert_idle(move |data| {
                    for (dev, path) in dispatcher.as_source_ref().device_list() {
                        let drm_node = match DrmNode::from_dev_id(dev) {
                            Ok(node) => node,
                            Err(err) => {
                                error!(?err, "Failed to read drm device {}.", path.display(),);
                                continue;
                            }
                        };
                        if data.state.backend.kms().devices.contains_key(&drm_node) {
                            if let Err(err) = data.state.device_changed(dev) {
                                error!(?err, "Failed to update drm device {}.", path.display(),);
                            }
                        } else {
                            if let Err(err) = data.state.device_added(
                                dev,
                                path.into(),
                                &data.display.handle(),
                                true,
                            ) {
                                error!(?err, "Failed to add drm device {}.", path.display(),);
                            }
                        }
                    }

                    let seats = data.state.common.seats().cloned().collect::<Vec<_>>();
                    data.state.common.config.read_outputs(
                        &mut data.state.common.output_configuration_state,
                        &mut data.state.backend,
                        &mut data.state.common.shell,
                        seats.into_iter(),
                        &data.state.common.event_loop_handle,
                    );
                    for surface in data
                        .state
                        .backend
                        .kms()
                        .devices
                        .values_mut()
                        .flat_map(|d| d.surfaces.values_mut())
                    {
                        surface.pending = false;
                    }
                    for output in data.state.common.shell.outputs() {
                        let sessions = output.pending_buffers().collect::<Vec<_>>();
                        if let Err(err) = data.state.backend.kms().schedule_render(
                            &data.state.common.event_loop_handle,
                            output,
                            None,
                            if !sessions.is_empty() {
                                Some(sessions)
                            } else {
                                None
                            },
                        ) {
                            error!(
                                ?err,
                                "Error scheduling event loop for output {}.",
                                output.name(),
                            );
                        }
                    }
                });
                loop_signal.wakeup();
            }
            SessionEvent::PauseSession => {
                libinput_context.suspend();
                for device in data.state.backend.kms().devices.values() {
                    device.drm.as_source_ref().pause();
                }
            }
        })
        .map_err(|err| err.error)
        .context("Failed to initialize session event source")?;

    state.backend = BackendData::Kms(KmsState {
        api,
        _tokens: vec![
            libinput_event_source,
            session_event_source,
            udev_event_source,
        ],
        primary,
        session,
        devices: HashMap::new(),
    });

    // TODO: Do multiple Xwaylands for better multigpu
    state.launch_xwayland(Some(primary));

    for (dev, path) in udev_dispatcher.as_source_ref().device_list() {
        state
            .device_added(dev, path.into(), dh, false)
            .with_context(|| format!("Failed to add drm device: {}", path.display()))?;
    }

    // HACK: amdgpu doesn't like us initializing vulkan too early..
    //      so lets do that delayed until mesa fixes that.
    let devices = state
        .backend
        .kms()
        .devices
        .iter()
        .map(|(drm_node, device)| (*drm_node, device.render_node))
        .collect::<Vec<_>>();
    for (drm_node, render_node) in devices {
        state.init_vulkan(drm_node, render_node);
    }
    Ok(())
}

impl State {
    fn device_added(
        &mut self,
        dev: dev_t,
        path: PathBuf,
        dh: &DisplayHandle,
        try_vulkan: bool,
    ) -> Result<()> {
        if !self.backend.kms().session.is_active() {
            return Ok(());
        }

        let fd = DrmDeviceFd::new(unsafe {
            DeviceFd::from_raw_fd(
                self.backend
                    .kms()
                    .session
                    .open(
                        &path,
                        OFlag::O_RDWR | OFlag::O_CLOEXEC | OFlag::O_NOCTTY | OFlag::O_NONBLOCK,
                    )
                    .with_context(|| {
                        format!(
                            "Failed to optain file descriptor for drm device: {}",
                            path.display()
                        )
                    })?,
            )
        });
        let drm = DrmDevice::new(fd.clone(), false)
            .with_context(|| format!("Failed to initialize drm device for: {}", path.display()))?;
        let drm_node = DrmNode::from_dev_id(dev)?;
        let supports_atomic = drm.is_atomic();

        let gbm = GbmDevice::new(fd)
            .with_context(|| format!("Failed to initialize GBM device for {}", path.display()))?;
        let (render_node, formats) = {
            let egl_display = EGLDisplay::new(gbm.clone()).with_context(|| {
                format!("Failed to create EGLDisplay for device: {}", path.display())
            })?;
            let egl_device = EGLDevice::device_for_display(&egl_display).with_context(|| {
                format!("Unable to find matching egl device for {}", path.display())
            })?;
            let render_node = egl_device
                .try_get_render_node()
                .ok()
                .and_then(std::convert::identity)
                .unwrap_or(drm_node);
            let egl_context = EGLContext::new(&egl_display).with_context(|| {
                format!(
                    "Failed to create EGLContext for device {:?}:{}",
                    egl_device,
                    path.display()
                )
            })?;
            let formats = egl_context.dmabuf_render_formats().clone();
            (render_node, formats)
            // NOTE: We need the to drop the EGL types here again,
            //  otherwise the EGLDisplay created below might share the same GBM context
        };

        let dispatcher =
            Dispatcher::new(drm, move |event, metadata, data: &mut Data| match event {
                DrmEvent::VBlank(crtc) => {
                    let rescheduled =
                        if let Some(device) = data.state.backend.kms().devices.get_mut(&drm_node) {
                            if let Some(surface) = device.surfaces.get_mut(&crtc) {
                                #[cfg(feature = "debug")]
                                surface.fps.displayed();

                                match surface.surface.as_mut().map(|x| x.frame_submitted()) {
                                    Some(Ok(feedback)) => {
                                        if let Some(mut feedback) = feedback.flatten() {
                                            let submit_time =
                                                match metadata.take().map(|data| data.time) {
                                                    Some(DrmEventTime::Monotonic(tp)) => Some(tp),
                                                    _ => None,
                                                };
                                            let seq = metadata
                                                .as_ref()
                                                .map(|metadata| metadata.sequence)
                                                .unwrap_or(0);

                                            let (clock, flags) = if let Some(tp) = submit_time {
                                                (
                                                tp.into(),
                                                wp_presentation_feedback::Kind::Vsync
                                                    | wp_presentation_feedback::Kind::HwClock
                                                    | wp_presentation_feedback::Kind::HwCompletion,
                                            )
                                            } else {
                                                (
                                                    data.state.common.clock.now(),
                                                    wp_presentation_feedback::Kind::Vsync,
                                                )
                                            };

                                            feedback.presented(
                                                clock,
                                                surface
                                                    .output
                                                    .current_mode()
                                                    .map(|mode| mode.refresh as u32)
                                                    .unwrap_or_default(),
                                                seq as u64,
                                                flags,
                                            );
                                        }

                                        surface.pending = false;
                                        surface.dirty.then(|| {
                                            (surface.output.clone(), surface.fps.avg_rendertime(5))
                                        })
                                    }
                                    Some(Err(err)) => {
                                        warn!(?err, "Failed to submit frame.");
                                        None
                                    }
                                    _ => None, // got disabled
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                    if let Some((output, avg_rendertime)) = rescheduled {
                        let mut scheduled_sessions =
                            data.state.workspace_session_for_output(&output);
                        let mut output_sessions = output.pending_buffers().peekable();
                        if output_sessions.peek().is_some() {
                            scheduled_sessions
                                .get_or_insert_with(Vec::new)
                                .extend(output_sessions);
                        }

                        let repaint_delay = std::cmp::max(avg_rendertime, MIN_RENDER_TIME);
                        if let Err(err) = data.state.backend.kms().schedule_render(
                            &data.state.common.event_loop_handle,
                            &output,
                            Some(repaint_delay),
                            scheduled_sessions,
                        ) {
                            warn!(?err, "Failed to schedule render.");
                        }
                    }
                }
                DrmEvent::Error(err) => {
                    warn!(?err, "Failed to read events of device {:?}.", dev);
                }
            });
        let token = self
            .common
            .event_loop_handle
            .register_dispatcher(dispatcher.clone())
            .with_context(|| format!("Failed to add drm device to event loop: {}", dev))?;

        let socket = match self.create_socket(dh, render_node, formats.clone().into_iter()) {
            Ok(socket) => Some(socket),
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to initialize hardware-acceleration for clients on {}.", render_node,
                );
                None
            }
        };

        let allocator = Box::new(DmabufAllocator(GbmAllocator::new(
            gbm.clone(),
            GbmBufferFlags::RENDERING,
        )));
        let mut device = Device {
            render_node,
            surfaces: HashMap::new(),
            gbm: gbm.clone(),
            allocator,
            drm: dispatcher,
            formats,
            supports_atomic,
            event_token: Some(token),
            socket,
        };

        let outputs = device.enumerate_surfaces()?.added; // There are no removed outputs on newly added devices
        let mut wl_outputs = Vec::new();
        let mut w = self.common.shell.global_space().size.w;
        {
            let backend = self.backend.kms();
            backend
                .api
                .as_mut()
                .add_node(render_node, gbm)
                .with_context(|| {
                    format!(
                        "Failed to initialize renderer for device: {}, skipping",
                        render_node
                    )
                })?;
            for (crtc, conn) in outputs {
                let mut renderer = match backend.api.single_renderer(&render_node) {
                    Ok(renderer) => renderer,
                    Err(err) => {
                        warn!(?err, "Failed to initialize output.");
                        continue;
                    }
                };
                match device.setup_surface(crtc, conn, (w, 0), &mut renderer) {
                    Ok(output) => {
                        w += output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow()
                            .mode_size()
                            .w;
                        wl_outputs.push(output);
                    }
                    Err(err) => warn!(?err, "Failed to initialize output."),
                };
            }
            backend.devices.insert(drm_node, device);
        }

        self.common
            .output_configuration_state
            .add_heads(wl_outputs.iter());
        let seats = self.common.seats().cloned().collect::<Vec<_>>();
        self.common.config.read_outputs(
            &mut self.common.output_configuration_state,
            &mut self.backend,
            &mut self.common.shell,
            seats.into_iter(),
            &self.common.event_loop_handle,
        );

        if try_vulkan {
            self.init_vulkan(drm_node, render_node);
        }

        Ok(())
    }

    fn init_vulkan(&mut self, drm_node: DrmNode, render_node: DrmNode) {
        if let Ok(instance) = Instance::new(Version::VERSION_1_2, None) {
            if let Some(physical_device) =
                PhysicalDevice::enumerate(&instance)
                    .ok()
                    .and_then(|devices| {
                        devices
                            .filter(|phd| {
                                phd.has_device_extension(unsafe {
                                    CStr::from_bytes_with_nul_unchecked(
                                        b"VK_EXT_physical_device_drm\0",
                                    )
                                })
                            })
                            .find(|phd| {
                                phd.primary_node().unwrap() == Some(render_node)
                                    || phd.render_node().unwrap() == Some(render_node)
                            })
                    })
            {
                match VulkanAllocator::new(
                    &physical_device,
                    ImageUsageFlags::COLOR_ATTACHMENT | ImageUsageFlags::SAMPLED,
                ) {
                    Ok(allocator) => {
                        self.backend
                            .kms()
                            .devices
                            .get_mut(&drm_node)
                            .unwrap()
                            .allocator = Box::new(DmabufAllocator(allocator));
                    }
                    Err(err) => {
                        warn!(?err, "Failed to create vulkan allocator.");
                    }
                }
            }
        }
    }

    fn device_changed(&mut self, dev: dev_t) -> Result<()> {
        if !self.backend.kms().session.is_active() {
            return Ok(());
        }

        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        let mut outputs_added = Vec::new();
        {
            let backend = self.backend.kms();
            if let Some(device) = backend.devices.get_mut(&drm_node) {
                let changes = device.enumerate_surfaces()?;
                let mut w = self.common.shell.global_space().size.w;
                for crtc in changes.removed {
                    if let Some(surface) = device.surfaces.remove(&crtc) {
                        if let Some(token) = surface.render_timer_token {
                            self.common.event_loop_handle.remove(token);
                        }
                        w -= surface.output.current_mode().map(|m| m.size.w).unwrap_or(0);
                        outputs_removed.push(surface.output.clone());
                    }
                }
                for (crtc, conn) in changes.added {
                    let mut renderer = match backend.api.single_renderer(&device.render_node) {
                        Ok(renderer) => renderer,
                        Err(err) => {
                            warn!(?err, "Failed to initialize output.");
                            continue;
                        }
                    };
                    match device.setup_surface(crtc, conn, (w, 0), &mut renderer) {
                        Ok(output) => {
                            w += output
                                .user_data()
                                .get::<RefCell<OutputConfig>>()
                                .unwrap()
                                .borrow()
                                .mode_size()
                                .w;
                            outputs_added.push(output);
                        }
                        Err(err) => warn!(?err, "Failed to initialize output."),
                    };
                }
            }
        }

        self.common
            .output_configuration_state
            .remove_heads(outputs_removed.iter());
        self.common
            .output_configuration_state
            .add_heads(outputs_added.iter());
        let seats = self.common.seats().cloned().collect::<Vec<_>>();
        self.common.config.read_outputs(
            &mut self.common.output_configuration_state,
            &mut self.backend,
            &mut self.common.shell,
            seats.iter().cloned(),
            &self.common.event_loop_handle,
        );
        // Don't remove the outputs, before potentially new ones have been initialized.
        // reading a new config means outputs might become enabled, that were previously disabled.
        // If we have 0 outputs at some point, we won't quit, but shell doesn't know where to move
        // windows and workspaces to.
        for output in outputs_removed {
            self.common
                .shell
                .remove_output(&output, seats.iter().cloned());
        }

        Ok(())
    }

    fn device_removed(&mut self, dev: dev_t, dh: &DisplayHandle) -> Result<()> {
        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        let backend = self.backend.kms();
        if let Some(mut device) = backend.devices.remove(&drm_node) {
            backend.api.as_mut().remove_node(&device.render_node);
            for surface in device.surfaces.values_mut() {
                if let Some(token) = surface.render_timer_token.take() {
                    self.common.event_loop_handle.remove(token);
                }
                outputs_removed.push(surface.output.clone());
            }
            if let Some(token) = device.event_token.take() {
                self.common.event_loop_handle.remove(token);
            }
            if let Some(socket) = device.socket.take() {
                self.common.event_loop_handle.remove(socket.token);
                self.common
                    .dmabuf_state
                    .destroy_global::<State>(dh, socket.dmabuf_global);
                dh.remove_global::<State>(socket.drm_global);
            }
        }
        self.common
            .output_configuration_state
            .remove_heads(outputs_removed.iter());

        let seats = self.common.seats().cloned().collect::<Vec<_>>();
        if self.backend.kms().session.is_active() {
            for output in outputs_removed {
                self.common
                    .shell
                    .remove_output(&output, seats.iter().cloned());
            }
            self.common.config.read_outputs(
                &mut self.common.output_configuration_state,
                &mut self.backend,
                &mut self.common.shell,
                seats.into_iter(),
                &self.common.event_loop_handle,
            );
        } else {
            self.common.output_configuration_state.update();
        }

        Ok(())
    }
}

pub struct OutputChanges {
    pub added: Vec<(crtc::Handle, connector::Handle)>,
    pub removed: Vec<crtc::Handle>,
}

impl Device {
    pub fn enumerate_surfaces(&mut self) -> Result<OutputChanges> {
        let drm = &mut *self.drm.as_source_mut();

        // enumerate our outputs
        let config = drm_helpers::display_configuration(drm, self.supports_atomic)?;

        let surfaces = self
            .surfaces
            .iter()
            .map(|(c, s)| (*c, s.connector))
            .collect::<HashMap<crtc::Handle, connector::Handle>>();

        let added = config
            .iter()
            .filter(|(conn, crtc)| surfaces.get(&crtc).map(|c| c != *conn).unwrap_or(true))
            .map(|(conn, crtc)| (crtc, conn))
            .map(|(crtc, conn)| (*crtc, *conn))
            .collect::<Vec<_>>();
        let removed = surfaces
            .iter()
            .filter(|(crtc, conn)| config.get(conn).map(|c| c != *crtc).unwrap_or(true))
            .map(|(crtc, _)| *crtc)
            .collect::<Vec<_>>();

        Ok(OutputChanges { added, removed })
    }

    fn setup_surface(
        &mut self,
        crtc: crtc::Handle,
        conn: connector::Handle,
        position: (i32, i32),
        renderer: &mut GlMultiRenderer<'_, '_>,
    ) -> Result<Output> {
        let drm = &mut *self.drm.as_source_mut();
        let crtc_info = drm.get_crtc(crtc)?;
        let conn_info = drm.get_connector(conn, false)?;
        let vrr = drm_helpers::set_vrr(drm, crtc, conn, true).unwrap_or(false);
        let interface = drm_helpers::interface_name(drm, conn)?;
        let edid_info = drm_helpers::edid_info(drm, conn);
        let mode = crtc_info.mode().unwrap_or_else(|| {
            conn_info
                .modes()
                .iter()
                .find(|mode| mode.mode_type().contains(ModeTypeFlags::PREFERRED))
                .copied()
                .unwrap_or(conn_info.modes()[0])
        });
        let refresh_rate = drm_helpers::calculate_refresh_rate(mode);
        let output_mode = OutputMode {
            size: (mode.size().0 as i32, mode.size().1 as i32).into(),
            refresh: refresh_rate as i32,
        };
        let (phys_w, phys_h) = conn_info.size().unwrap_or((0, 0));
        let output = Output::new(
            interface,
            PhysicalProperties {
                size: (phys_w as i32, phys_h as i32).into(),
                // TODO: We need to read that from the connector properties
                subpixel: Subpixel::Unknown,
                make: edid_info
                    .as_ref()
                    .map(|info| info.manufacturer.clone())
                    .unwrap_or_else(|_| String::from("Unknown")),
                model: edid_info
                    .as_ref()
                    .map(|info| info.model.clone())
                    .unwrap_or_else(|_| String::from("Unknown")),
            },
        );
        for mode in conn_info.modes() {
            let refresh_rate = drm_helpers::calculate_refresh_rate(*mode);
            let mode = OutputMode {
                size: (mode.size().0 as i32, mode.size().1 as i32).into(),
                refresh: refresh_rate as i32,
            };
            output.add_mode(mode);
        }
        output.set_preferred(output_mode);
        output.change_current_state(
            Some(output_mode),
            // TODO: Readout property for monitor rotation
            Some(Transform::Normal),
            None,
            Some(position.into()),
        );
        output.user_data().insert_if_missing(|| {
            RefCell::new(OutputConfig {
                mode: ((output_mode.size.w, output_mode.size.h), Some(refresh_rate)),
                vrr,
                position,
                ..Default::default()
            })
        });

        let data = Surface {
            output: output.clone(),
            damage_tracker: DamageTrackedRenderer::from_output(&output),
            surface: None,
            connector: conn,
            vrr,
            refresh_rate,
            pending: false,
            dirty: false,
            render_timer_token: None,
            fps: Fps::new(renderer.as_mut()),
        };
        self.surfaces.insert(crtc, data);

        Ok(output)
    }
}

const MAX_CPU_COPIES: usize = 3;

fn render_node_for_output(
    dh: &DisplayHandle,
    output: &Output,
    target_node: DrmNode,
    shell: &Shell,
) -> DrmNode {
    let workspace = shell.active_space(output);
    let nodes = workspace
        .get_fullscreen(output)
        .map(|w| vec![w.clone()])
        .unwrap_or_else(|| workspace.windows().collect::<Vec<_>>())
        .into_iter()
        .flat_map(|w| {
            let client = dh.get_client(w.wl_surface()?.id()).ok()?;
            if let Some(normal_client) = client.get_data::<ClientState>() {
                return normal_client.drm_node.clone();
            }
            if let Some(xwayland_client) = client.get_data::<XWaylandClientData>() {
                return xwayland_client.user_data().get::<DrmNode>().cloned();
            }
            None
        })
        .collect::<Vec<_>>();
    if nodes.contains(&target_node) || nodes.len() < MAX_CPU_COPIES {
        target_node
    } else {
        nodes
            .iter()
            .fold(HashMap::new(), |mut count_map, node| {
                let count = count_map.entry(node).or_insert(0);
                *count += 1;
                count_map
            })
            .into_iter()
            .reduce(|a, b| if a.1 > b.1 { a } else { b })
            .map(|(node, _)| *node)
            .unwrap_or(target_node)
    }
}

impl Surface {
    pub fn render_output(
        &mut self,
        api: &mut GpuManager<GbmGlesBackend<GlowRenderer>>,
        render_node: Option<(
            &DrmNode,
            &mut dyn Allocator<Buffer = Dmabuf, Error = AnyError>,
        )>,
        target_node: &DrmNode,
        state: &mut Common,
        screencopy: Option<&[(ScreencopySession, BufferParams)]>,
    ) -> Result<()> {
        if self.surface.is_none() {
            return Ok(());
        }

        let surface = self.surface.as_mut().unwrap();
        let (render_node, mut renderer) = match render_node {
            Some((render_node, allocator)) => (
                render_node,
                api.renderer(&render_node, &target_node, allocator, surface.format())
                    .unwrap(),
            ),
            None => (target_node, api.single_renderer(&target_node).unwrap()),
        };

        let (buffer, age) = surface
            .next_buffer()
            .with_context(|| "Failed to allocate buffer")?;

        match render::render_output::<GlMultiRenderer, _, Gles2Renderbuffer, _>(
            Some(&render_node),
            &mut renderer,
            buffer.clone(),
            &mut self.damage_tracker,
            age as usize,
            state,
            &self.output,
            CursorMode::All,
            screencopy.map(|sessions| (buffer, sessions)),
            Some(&mut self.fps),
        ) {
            Ok((damage, states)) => {
                let feedback = if damage.is_some() {
                    Some(state.take_presentation_feedback(&self.output, &states))
                } else {
                    None
                };
                state.send_frames(&self.output, &states);
                surface
                    .queue_buffer(damage, feedback)
                    .with_context(|| "Failed to submit buffer for display")?;
            }
            Err(err) => {
                surface.reset_buffers();
                anyhow::bail!("Rendering failed: {}", err);
            }
        };

        Ok(())
    }
}

impl KmsState {
    pub fn switch_vt(&mut self, num: i32) -> Result<(), anyhow::Error> {
        self.session.change_vt(num).map_err(Into::into)
    }

    pub fn apply_config_for_output(
        &mut self,
        output: &Output,
        seats: impl Iterator<Item = Seat<State>>,
        shell: &mut Shell,
        test_only: bool,
        loop_handle: &LoopHandle<'_, Data>,
    ) -> Result<(), anyhow::Error> {
        let recreated = if let Some(device) = self
            .devices
            .values_mut()
            .find(|dev| dev.surfaces.values().any(|s| s.output == *output))
        {
            let (crtc, mut surface) = device
                .surfaces
                .iter_mut()
                .find(|(_, s)| s.output == *output)
                .unwrap();
            let output_config = output
                .user_data()
                .get::<RefCell<OutputConfig>>()
                .unwrap()
                .borrow();

            if !output_config.enabled {
                if !test_only {
                    shell.remove_output(output, seats);
                    if surface.surface.take().is_some() {
                        // just drop it
                        surface.pending = false;
                        surface.dirty = false;
                    }
                }
                false
            } else {
                let drm = &mut *device.drm.as_source_mut();
                let conn = surface.connector;
                let conn_info = drm.get_connector(conn, false)?;
                let mode = conn_info
                    .modes()
                    .iter()
                    // match the size
                    .filter(|mode| {
                        let (x, y) = mode.size();
                        Size::from((x as i32, y as i32)) == output_config.mode_size()
                    })
                    // and then select the closest refresh rate (e.g. to match 59.98 as 60)
                    .min_by_key(|mode| {
                        let refresh_rate = drm_helpers::calculate_refresh_rate(**mode);
                        (output_config.mode.1.unwrap() as i32 - refresh_rate as i32).abs()
                    })
                    .ok_or(anyhow::anyhow!("Unknown mode"))?;

                if !test_only {
                    let res = if let Some(gbm_surface) = surface.surface.as_mut() {
                        if output_config.vrr != surface.vrr {
                            surface.vrr = drm_helpers::set_vrr(
                                drm,
                                *crtc,
                                conn_info.handle(),
                                output_config.vrr,
                            )?;
                        }
                        gbm_surface.use_mode(*mode).unwrap();
                        false
                    } else {
                        surface.vrr = drm_helpers::set_vrr(drm, *crtc, conn, output_config.vrr)
                            .unwrap_or(false);
                        surface.refresh_rate = drm_helpers::calculate_refresh_rate(*mode);

                        let drm_surface = drm.create_surface(*crtc, *mode, &[conn])?;
                        let target = GbmBufferedSurface::new(
                            drm_surface,
                            GbmAllocator::new(
                                device.gbm.clone(),
                                GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
                            ),
                            device.formats.clone(),
                        )
                        .with_context(|| {
                            format!(
                                "Failed to initialize Gbm surface for {}",
                                drm_helpers::interface_name(drm, conn)
                                    .unwrap_or_else(|_| String::from("Unknown"))
                            )
                        })?;
                        surface.surface = Some(target);
                        true
                    };
                    shell.add_output(output);
                    res
                } else {
                    false
                }
            }
        } else {
            false
        };

        shell.refresh_outputs();
        if recreated {
            let sessions = output.pending_buffers().collect::<Vec<_>>();
            if let Err(err) = self.schedule_render(
                loop_handle,
                output,
                None,
                if !sessions.is_empty() {
                    Some(sessions)
                } else {
                    None
                },
            ) {
                error!(
                    ?err,
                    "Error scheduling event loop for output {}.",
                    output.name(),
                );
            }
        }
        Ok(())
    }
    pub fn target_node_for_output(&self, output: &Output) -> Option<DrmNode> {
        self.devices
            .values()
            .find(|dev| dev.surfaces.values().any(|s| s.output == *output))
            .map(|dev| &dev.render_node)
            .copied()
    }

    pub fn try_early_import(
        &mut self,
        dh: &DisplayHandle,
        surface: &WlSurface,
        output: &Output,
        target: DrmNode,
        shell: &Shell,
    ) {
        let render = render_node_for_output(dh, &output, target, &shell);
        if let Err(err) = self.api.early_import(
            if let Some(client) = dh.get_client(surface.id()).ok() {
                if let Some(normal_client) = client.get_data::<ClientState>() {
                    normal_client.drm_node.clone()
                } else if let Some(xwayland_client) = client.get_data::<XWaylandClientData>() {
                    xwayland_client.user_data().get::<DrmNode>().cloned()
                } else {
                    None
                }
            } else {
                None
            },
            render,
            surface,
        ) {
            debug!(?err, "Early import failed.");
        }
    }

    pub fn dmabuf_imported(&mut self, global: &DmabufGlobal, dmabuf: Dmabuf) -> Result<()> {
        use smithay::backend::renderer::ImportDma;

        for device in self.devices.values() {
            if device
                .socket
                .as_ref()
                .map(|s| &s.dmabuf_global == global)
                .unwrap_or(false)
            {
                return self
                    .api
                    .single_renderer(&device.render_node)?
                    .import_dmabuf(&dmabuf, None)
                    .map(|_| ())
                    .map_err(Into::into);
            }
        }
        unreachable!()
    }

    pub fn schedule_render(
        &mut self,
        loop_handle: &LoopHandle<'_, Data>,
        output: &Output,
        delay: Option<Duration>,
        mut screencopy_sessions: Option<Vec<(ScreencopySession, BufferParams)>>,
    ) -> Result<(), InsertError<Timer>> {
        if let Some((device, crtc, surface)) = self
            .devices
            .iter_mut()
            .flat_map(|(node, d)| d.surfaces.iter_mut().map(move |(c, s)| (node, c, s)))
            .find(|(_, _, s)| s.output == *output)
        {
            if surface.surface.is_none() {
                if let Some(sessions) = screencopy_sessions {
                    loop_handle.insert_idle(move |data| {
                        for (session, params) in sessions.into_iter() {
                            data.state.common.still_pending(session, params);
                        }
                    });
                }
                return Ok(());
            }
            if !surface.pending {
                let device = *device;
                let crtc = *crtc;
                if let Some(token) = surface.render_timer_token.take() {
                    loop_handle.remove(token);
                }
                surface.render_timer_token = Some(loop_handle.insert_source(
                    if surface.vrr || delay.is_none() {
                        Timer::immediate()
                    } else {
                        Timer::from_duration(delay.unwrap())
                    },
                    move |_time, _, data| {
                        let backend = data.state.backend.kms();
                        let (mut device, mut other) = backend
                            .devices
                            .iter_mut()
                            .partition::<Vec<_>, _>(|(key, _val)| *key == &device);
                        let target_device = &mut device[0].1;

                        if let Some(surface) = target_device.surfaces.get_mut(&crtc) {
                            let target_node = target_device.render_node;
                            let render_node = render_node_for_output(
                                &data.display.handle(),
                                &surface.output,
                                target_node,
                                &data.state.common.shell,
                            );
                            let state = &mut data.state.common;

                            let result = if render_node != target_node {
                                let render_device = &mut other
                                    .iter_mut()
                                    .find(|(_, val)| val.render_node == render_node)
                                    .unwrap()
                                    .1;
                                surface.render_output(
                                    &mut backend.api,
                                    Some((
                                        &render_device.render_node,
                                        render_device.allocator.as_mut(),
                                    )),
                                    &target_node,
                                    state,
                                    screencopy_sessions.as_deref(),
                                )
                            } else {
                                surface.render_output(
                                    &mut backend.api,
                                    None,
                                    &target_node,
                                    state,
                                    screencopy_sessions.as_deref(),
                                )
                            };

                            match result {
                                Ok(_) => {
                                    surface.dirty = false;
                                    surface.pending = true;
                                    return TimeoutAction::Drop;
                                }
                                Err(err) => {
                                    if backend.session.is_active() {
                                        error!(?err, "Error rendering.");
                                        return TimeoutAction::ToDuration(Duration::from_secs_f64(
                                            (1000.0 / surface.refresh_rate as f64) - 0.003,
                                        ));
                                    }
                                }
                            };
                        }

                        if let Some(sessions) = screencopy_sessions.as_mut() {
                            for (session, params) in sessions.drain(..) {
                                data.state.common.still_pending(session, params);
                            }
                        }
                        TimeoutAction::Drop
                    },
                )?);
            } else {
                if let Some(sessions) = screencopy_sessions {
                    loop_handle.insert_idle(|data| {
                        for (session, params) in sessions.into_iter() {
                            data.state.common.still_pending(session, params);
                        }
                    });
                }
                surface.dirty = true;
            }
        }
        Ok(())
    }
}
