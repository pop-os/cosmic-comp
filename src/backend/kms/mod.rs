// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use crate::state::Fps;

use crate::{
    backend::render,
    config::OutputConfig,
    shell::Shell,
    state::{BackendData, ClientState, Common, Data},
    utils::prelude::*,
    wayland::{
        handlers::screencopy::{PendingScreencopyBuffers, UserdataExt},
        protocols::screencopy::{BufferParams, Session as ScreencopySession},
    },
};

use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::{dmabuf::Dmabuf, gbm::GbmDevice, Format},
        drm::{DrmDevice, DrmEvent, DrmEventTime, DrmNode, GbmBufferedSurface, NodeType},
        egl::{EGLContext, EGLDevice, EGLDisplay},
        input::InputEvent,
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{
            damage::DamageTrackedRenderer,
            gles2::Gles2Renderbuffer,
            glow::GlowRenderer,
            multigpu::{egl::EglGlesBackend, GpuManager},
        },
        session::{auto::AutoSession, Session, Signal},
        udev::{all_gpus, primary_gpu, UdevBackend, UdevEvent},
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
    utils::{
        signaling::{Linkable, SignalToken, Signaler},
        Size, Transform,
    },
    wayland::dmabuf::DmabufGlobal,
};

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    os::unix::io::{FromRawFd, OwnedFd},
    path::PathBuf,
    rc::Rc,
    time::Duration,
};

mod drm_helpers;
mod session_fd;
mod socket;
use session_fd::*;
use socket::*;

use super::render::{CursorMode, GlMultiRenderer};

pub struct KmsState {
    devices: HashMap<DrmNode, Device>,
    pub api: GpuManager<EglGlesBackend<GlowRenderer>>,
    pub primary: DrmNode,
    session: AutoSession,
    signaler: Signaler<Signal>,
    _restart_token: SignalToken,
    _tokens: Vec<RegistrationToken>,
}

pub struct Device {
    render_node: DrmNode,
    surfaces: HashMap<crtc::Handle, Surface>,
    allocator: Rc<RefCell<GbmDevice<SessionFd>>>,
    drm: Dispatcher<'static, DrmDevice<SessionFd>, Data>,
    formats: HashSet<Format>,
    supports_atomic: bool,
    event_token: Option<RegistrationToken>,
    socket: Option<Socket>,
}

pub struct Surface {
    surface: Option<
        GbmBufferedSurface<
            Rc<RefCell<GbmDevice<SessionFd>>>,
            SessionFd,
            Option<OutputPresentationFeedback>,
        >,
    >,
    damage_tracker: DamageTrackedRenderer,
    connector: connector::Handle,
    output: Output,
    refresh_rate: u32,
    vrr: bool,
    pending: bool,
    dirty: bool,
    render_timer_token: Option<RegistrationToken>,
    #[cfg(feature = "debug")]
    fps: Fps,
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<'static, Data>,
    state: &mut State,
) -> Result<()> {
    let (session, notifier) = AutoSession::new(None).context("Failed to acquire session")?;
    let signaler = notifier.signaler();

    let udev_backend = UdevBackend::new(session.seat(), None)?;
    let mut libinput_context =
        Libinput::new_with_udev::<LibinputSessionInterface<AutoSession>>(session.clone().into());
    libinput_context
        .udev_assign_seat(&session.seat())
        .map_err(|_| anyhow::anyhow!("Failed to assign seat to libinput"))?;
    let mut libinput_backend = LibinputInputBackend::new(libinput_context, None);
    libinput_backend.link(signaler.clone());

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
                    slog_scope::crit!(
                        "Error scheduling event loop for output {}: {:?}",
                        output.name(),
                        err
                    );
                }
            }
        })
        .map_err(|err| err.error)
        .context("Failed to initialize libinput event source")?;
    let session_event_source = event_loop
        .handle()
        .insert_source(notifier, |(), &mut (), _state| {})
        .map_err(|err| err.error)
        .context("Failed to initialize session event source")?;

    let api = GpuManager::new(EglGlesBackend::<GlowRenderer>::default(), None)
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
    slog_scope::info!("Using {} as primary gpu for rendering", primary);

    let udev_dispatcher = Dispatcher::new(udev_backend, move |event, _, data: &mut Data| {
        match match event {
            UdevEvent::Added { device_id, path } => data
                .state
                .device_added(device_id, path, &data.display.handle())
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
                slog_scope::debug!("Successfully handled udev event")
            }
            Err(err) => {
                slog_scope::error!("Error while handling udev event: {}", err)
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
    let _restart_token = signaler.register(move |signal| {
        if let Signal::ActivateSession = signal {
            let dispatcher = dispatcher.clone();
            handle.insert_idle(move |data| {
                for (dev, path) in dispatcher.as_source_ref().device_list() {
                    let drm_node = match DrmNode::from_dev_id(dev) {
                        Ok(node) => node,
                        Err(err) => {
                            slog_scope::error!(
                                "Failed to read drm device {}: {}",
                                path.display(),
                                err
                            );
                            continue;
                        }
                    };
                    if data.state.backend.kms().devices.contains_key(&drm_node) {
                        if let Err(err) = data.state.device_changed(dev) {
                            slog_scope::error!(
                                "Failed to update drm device {}: {}",
                                path.display(),
                                err
                            );
                        }
                    } else {
                        if let Err(err) =
                            data.state
                                .device_added(dev, path.into(), &data.display.handle())
                        {
                            slog_scope::error!(
                                "Failed to add drm device {}: {}",
                                path.display(),
                                err
                            );
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
                        slog_scope::crit!(
                            "Error scheduling event loop for output {}: {:?}",
                            output.name(),
                            err
                        );
                    }
                }
            });
            loop_signal.wakeup();
        }
    });

    state.backend = BackendData::Kms(KmsState {
        api,
        _tokens: vec![
            libinput_event_source,
            session_event_source,
            udev_event_source,
        ],
        primary,
        session,
        signaler,
        _restart_token,
        devices: HashMap::new(),
    });

    for (dev, path) in udev_dispatcher.as_source_ref().device_list() {
        state
            .device_added(dev, path.into(), dh)
            .with_context(|| format!("Failed to add drm device: {}", path.display()))?;
    }
    Ok(())
}

impl State {
    fn device_added(&mut self, dev: dev_t, path: PathBuf, dh: &DisplayHandle) -> Result<()> {
        if !self.backend.kms().session.is_active() {
            return Ok(());
        }

        let fd = SessionFd::new(unsafe {
            OwnedFd::from_raw_fd(
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
        let mut drm = DrmDevice::new(fd.clone(), false, None)
            .with_context(|| format!("Failed to initialize drm device for: {}", path.display()))?;
        let drm_node = DrmNode::from_dev_id(dev)?;
        let supports_atomic = drm.is_atomic();

        let gbm = GbmDevice::new(fd)
            .with_context(|| format!("Failed to initialize GBM device for {}", path.display()))?;
        let egl_display = unsafe {
            EGLDisplay::new(&gbm, None).with_context(|| {
                format!("Failed to create EGLDisplay for device: {}", path.display())
            })?
        };
        let egl_device = EGLDevice::device_for_display(&egl_display).with_context(|| {
            format!("Unable to find matching egl device for {}", path.display())
        })?;
        let render_node = egl_device
            .try_get_render_node()
            .ok()
            .and_then(std::convert::identity)
            .with_context(|| {
                format!(
                    "Failed to determine path of egl device for {}",
                    path.display()
                )
            })?;
        let egl_context = EGLContext::new(&egl_display, None).with_context(|| {
            format!(
                "Failed to create EGLContext for device {:?}:{}",
                egl_device,
                path.display()
            )
        })?;
        let formats = egl_context.dmabuf_render_formats().clone();

        drm.link(self.backend.kms().signaler.clone());
        let dispatcher =
            Dispatcher::new(drm, move |event, metadata, data: &mut Data| match event {
                DrmEvent::VBlank(crtc) => {
                    let rescheduled_output =
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
                                        surface.dirty.then(|| surface.output.clone())
                                    }
                                    Some(Err(err)) => {
                                        slog_scope::warn!("Failed to submit frame: {}", err);
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

                    if let Some(output) = rescheduled_output {
                        let mut scheduled_sessions =
                            data.state.workspace_session_for_output(&output);
                        if let Some(sessions) = output.user_data().get::<PendingScreencopyBuffers>()
                        {
                            scheduled_sessions
                                .get_or_insert_with(Vec::new)
                                .extend(sessions.borrow_mut().drain(..));
                        }

                        let output_refresh = match output.current_mode() {
                            Some(mode) => mode.refresh,
                            None => return,
                        };
                        // TODO: Record rendering times and use sliding window to estimate duration for next render
                        let repaint_delay = Duration::from_secs_f64(
                            ((1000.0 / output_refresh as f64) * 0.6).max(0.003), // for now we assume we need at least 3ms
                        );

                        if let Err(err) = data.state.backend.kms().schedule_render(
                            &data.state.common.event_loop_handle,
                            &output,
                            Some(repaint_delay),
                            scheduled_sessions,
                        ) {
                            slog_scope::warn!("Failed to schedule render: {}", err);
                        }
                    }
                }
                DrmEvent::Error(err) => {
                    slog_scope::warn!("Failed to read events of device {:?}: {}", dev, err);
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
                slog_scope::warn!(
                    "Failed to initialize hardware-acceleration for clients on {}: {}",
                    render_node,
                    err
                );
                None
            }
        };

        let mut device = Device {
            render_node,
            surfaces: HashMap::new(),
            allocator: Rc::new(RefCell::new(gbm)),
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
            for (crtc, conn) in outputs {
                let mut renderer = match backend.api.renderer(&render_node, &render_node) {
                    Ok(renderer) => renderer,
                    Err(err) => {
                        slog_scope::warn!("Failed to initialize output: {}", err);
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
                    Err(err) => slog_scope::warn!("Failed to initialize output: {}", err),
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

        Ok(())
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
                    let mut renderer = match backend
                        .api
                        .renderer(&device.render_node, &device.render_node)
                    {
                        Ok(renderer) => renderer,
                        Err(err) => {
                            slog_scope::warn!("Failed to initialize output: {}", err);
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
                        Err(err) => slog_scope::warn!("Failed to initialize output: {}", err),
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

        Ok(())
    }

    fn device_removed(&mut self, dev: dev_t, dh: &DisplayHandle) -> Result<()> {
        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        if let Some(mut device) = self.backend.kms().devices.remove(&drm_node) {
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
        renderer: &mut GlMultiRenderer<'_>,
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
            None,
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
            #[cfg(feature = "debug")]
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
            dh.get_client(w.toplevel().wl_surface().id())
                .ok()?
                .get_data::<ClientState>()
                .unwrap()
                .drm_node
                .clone()
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
        dh: &DisplayHandle,
        api: &mut GpuManager<EglGlesBackend<GlowRenderer>>,
        target_node: &DrmNode,
        state: &mut Common,
        screencopy: Option<&[(ScreencopySession, BufferParams)]>,
    ) -> Result<()> {
        if self.surface.is_none() {
            return Ok(());
        }

        let render_node = render_node_for_output(dh, &self.output, *target_node, &state.shell);
        let mut renderer: GlMultiRenderer = api.renderer(&render_node, &target_node).unwrap();

        let surface = self.surface.as_mut().unwrap();
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
            #[cfg(feature = "debug")]
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
                    .queue_buffer(feedback)
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
                        let mut drm_surface = drm.create_surface(*crtc, *mode, &[conn])?;
                        drm_surface.link(self.signaler.clone());

                        let target = GbmBufferedSurface::new(
                            drm_surface,
                            device.allocator.clone(),
                            device.formats.clone(),
                            None,
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
                slog_scope::crit!(
                    "Error scheduling event loop for output {}: {:?}",
                    output.name(),
                    err
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
            dh.get_client(surface.id())
                .ok()
                .and_then(|c| c.get_data::<ClientState>().unwrap().drm_node.clone()),
            render,
            surface,
        ) {
            slog_scope::debug!("Early import failed: {}", err);
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
                    .renderer::<Gles2Renderbuffer>(&device.render_node, &device.render_node)?
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
                return Ok(());
            }
            if !surface.pending {
                surface.dirty = false;
                surface.pending = true;

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
                        if let Some(device) = backend.devices.get_mut(&device) {
                            if let Some(surface) = device.surfaces.get_mut(&crtc) {
                                if let Err(err) = surface.render_output(
                                    &data.display.handle(),
                                    &mut backend.api,
                                    &device.render_node,
                                    &mut data.state.common,
                                    screencopy_sessions.as_deref(),
                                ) {
                                    if let Some(sessions) = screencopy_sessions.as_mut() {
                                        for (session, params) in sessions.drain(..) {
                                            data.state.common.still_pending(session, params);
                                        }
                                    }
                                    if backend.session.is_active() {
                                        slog_scope::error!("Error rendering: {}", err);
                                        return TimeoutAction::ToDuration(Duration::from_secs_f64(
                                            (1000.0 / surface.refresh_rate as f64) - 0.003,
                                        ));
                                    }
                                }
                            }
                        }
                        TimeoutAction::Drop
                    },
                )?);
            } else {
                surface.dirty = true;
            }
        }
        Ok(())
    }
}
