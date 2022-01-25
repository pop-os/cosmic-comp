// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{BackendData, Common, State},
    utils::GlobalDrop,
};
use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::{gbm::GbmDevice, Format},
        drm::{DrmDevice, DrmEvent, GbmBufferedSurface, DrmEventTime},
        egl::{EGLDevice, EGLDisplay, EGLContext},
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        session::{Session, Signal, auto::AutoSession, AsErrno},
        udev::{UdevBackend, UdevEvent, primary_gpu},
        renderer::{Bind, gles2::Gles2Renderer},
    },
    reexports::{
        drm::{
            control::{
                Device as ControlDevice,
                connector,
                crtc,
            },
        },
        calloop::{EventLoop, Dispatcher, RegistrationToken, LoopHandle, timer::{Timer, TimerHandle}},
        input::Libinput,
        nix::{
            fcntl::OFlag,
            sys::stat::dev_t,
        },
        wayland_server::{
            Display,
            protocol::wl_output,
        }
    },
    wayland::output::{Output, Mode as OutputMode, PhysicalProperties},
    utils::signaling::{Signaler, Linkable},
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
    time::{Duration, Instant, SystemTime},
};

mod crtc_mapping;
mod session_fd;
use session_fd::*;

pub struct KmsState {
    devices: HashMap<dev_t, Device>,
    session: AutoSession,
    signaler: Signaler<Signal>,
    tokens: Vec<RegistrationToken>,
}

pub struct Device {
    renderer: Gles2Renderer,
    surfaces: HashMap<crtc::Handle, Surface>,
    allocator: Rc<RefCell<GbmDevice<SessionFd>>>,
    drm: Dispatcher<'static, DrmDevice<SessionFd>, State>,
    formats: HashSet<Format>,
    supports_atomic: bool,
    event_token: Option<RegistrationToken>,
}

pub struct Surface {
    surface: GbmBufferedSurface<Rc<RefCell<GbmDevice<SessionFd>>>, SessionFd>,
    output: Output,
    _global: GlobalDrop<wl_output::WlOutput>,
    last_submit: Option<DrmEventTime>,
    refresh_rate: u32,
    vrr: bool,
    pending: bool,
    render_timer: TimerHandle<(dev_t, crtc::Handle)>,
    render_timer_token: Option<RegistrationToken>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct DevId(pub dev_t);

pub fn init_backend(event_loop: &mut EventLoop<State>, state: &mut State) -> Result<()> {
    let (session, notifier) = AutoSession::new(None).context("Failed to acquire session")?;
    let signaler = notifier.signaler();

    let udev_backend = UdevBackend::new(session.seat(), None)?;
    let mut libinput_context = Libinput::new_with_udev::<LibinputSessionInterface<AutoSession>>(session.clone().into());
    libinput_context.udev_assign_seat(&session.seat()).map_err(|_| anyhow::anyhow!("Failed to assign seat to libinput"))?;
    let mut libinput_backend = LibinputInputBackend::new(libinput_context, None);
    libinput_backend.link(signaler.clone());

    let libinput_event_source = event_loop
        .handle()
        .insert_source(libinput_backend, move |event, _, state| {
            state.common.process_input_event(event);
            for output in state.common.spaces.outputs() {
                state.backend.kms().schedule_render(output);
            }
        })
        .map_err(|err| err.error)
        .context("Failed to initialize libinput event source")?;
    let session_event_source = event_loop
        .handle()
        .insert_source(notifier, |(), &mut (), _state| {})
        .map_err(|err| err.error)
        .context("Failed to initialize session event source")?;

    state.backend = BackendData::Kms(KmsState {
        tokens: vec![
            libinput_event_source,
            session_event_source,
        ],
        session,
        signaler,
        devices: HashMap::new(),
    });

    for (dev, path) in udev_backend.device_list() {
        state.device_added(dev, path.into())
            .with_context(|| format!("Failed to add drm device: {}", path.display()))?;
    }

    let udev_event_source = event_loop
        .handle()
        .insert_source(udev_backend, move |event, _, state| match match event {
            UdevEvent::Added { device_id, path } => state.device_added(device_id, path)
                    .with_context(|| format!("Failed to add drm device: {}", device_id)),
            UdevEvent::Changed { device_id } => state.device_changed(device_id)
                    .with_context(|| format!("Failed to update drm device: {}", device_id)),
            UdevEvent::Removed { device_id } => state.device_removed(device_id)
                    .with_context(|| format!("Failed to remove drm device: {}", device_id)),
        } {
            Ok(()) => { slog_scope::debug!("Successfully handled udev event") },
            Err(err) => { slog_scope::error!("Error while handling udev event: {}", err) },
        }).unwrap();
    state.backend.kms().tokens.push(udev_event_source);

    Ok(())
}

impl State {
    fn device_added(&mut self, dev: dev_t, path: PathBuf) -> Result<()> {
        let fd = SessionFd::new(self.backend.kms().session.open(&path, OFlag::O_RDWR | OFlag::O_CLOEXEC | OFlag::O_NOCTTY | OFlag::O_NONBLOCK)
            .with_context(|| format!("Failed to optain file descriptor for drm device: {}", path.display()))?);
        let mut drm = DrmDevice::new(fd.clone(), false, None)
            .with_context(|| format!("Failed to initialize drm device for: {}", path.display()))?;
        let supports_atomic = drm.is_atomic();

        let egl_device = EGLDevice::enumerate().context("Failed to enumerate EGLDevices")?
            // TODO: this check compares the primary node path.
            // On split display controller setups however this *may* return the render node
            // *or* if `EGL_EXT_device_drm_render_node` is supported nothing and we need to
            // query `EGL_DRM_RENDER_NODE_FILE_EXT` instead for comparision.
            .find(|dev| dev.drm_device_path().map(|p| p == path).unwrap_or(false))
            .with_context(|| format!("Unable to find matching egl device for {}", path.display()))?;
        let egl_display = EGLDisplay::new(&egl_device, None)
            .with_context(|| format!("Failed to open EGLDisplay for device {:?}:{}", egl_device, path.display()))?;
        let egl_context = EGLContext::new(&egl_display, None)
            .with_context(|| format!("Failed to create EGLContext for device {:?}:{}", egl_device, path.display()))?;
        let formats = egl_context.dmabuf_render_formats().clone();
        let renderer = unsafe { Gles2Renderer::new(egl_context, None) }
            .with_context(|| format!("Failed to create OpenGLES renderer for device {:?}:{}", egl_device, path.display()))?;

        let gbm = GbmDevice::new(fd).with_context(|| format!("Failed to initialize GBM device for {}", path.display()))?;
        drm.link(self.backend.kms().signaler.clone());
        let dispatcher = Dispatcher::new(drm, move |event, metadata, state: &mut State| {
            match event {
                DrmEvent::VBlank(crtc) => {
                    if let Some(device) = state.backend.kms().devices.get_mut(&dev) {
                        if let Some(surface) = device.surfaces.get_mut(&crtc) {
                            match surface.surface.frame_submitted() {
                                Ok(_) => {
                                    surface.last_submit = metadata.take().map(|data| data.time);
                                    surface.pending = false;
                                    state.common.spaces.active_space_mut(&surface.output)
                                        .send_frames(true, state.common.start_time.elapsed().as_millis() as u32);
                                },
                                Err(err) => slog_scope::warn!("Failed to submit frame: {}", err),
                            };
                        }
                    }
                },
                DrmEvent::Error(err) => {
                    slog_scope::warn!("Failed to read events of device {:?}: {}", dev, err);
                }
            }
        });
        let token = self.common.event_loop_handle.register_dispatcher(dispatcher.clone())
            .with_context(|| format!("Failed to add drm device to event loop: {}", dev))?;

        let mut device = Device {
            renderer,
            surfaces: HashMap::new(),
            allocator: Rc::new(RefCell::new(gbm)),
            drm: dispatcher,
            formats,
            supports_atomic,
            event_token: Some(token),
        };

        let outputs = device.enumerate_surfaces()?.added; // There are no removed outputs on newly added devices
        for (crtc, conn) in outputs {
            match device.setup_surface(crtc, conn, self.backend.kms().signaler.clone(), &mut self.common.display.borrow_mut(), &mut self.common.event_loop_handle) {
                Ok(output) => self.common.spaces.map_output(&output),
                Err(err) => slog_scope::warn!("Failed to initialize output: {}", err),
            };
        }

        self.backend.kms().devices.insert(dev, device);
        Ok(())
    }

    fn device_changed(&mut self, dev: dev_t) -> Result<()> {
        let signaler = self.backend.kms().signaler.clone();
        if let Some(device) = self.backend.kms().devices.get_mut(&dev) {
            let changes = device.enumerate_surfaces()?;
            for (crtc, _) in changes.removed {
                if let Some(surface) = device.surfaces.get_mut(&crtc) {
                    if let Some(token) = surface.render_timer_token.take() {
                        self.common.event_loop_handle.remove(token);
                    }
                    self.common.spaces.unmap_output(&surface.output);
                }
            }
            for (crtc, conn) in changes.added {
                match device.setup_surface(crtc, conn, signaler.clone(), &mut self.common.display.borrow_mut(), &mut self.common.event_loop_handle) {
                    Ok(output) => self.common.spaces.map_output(&output),
                    Err(err) => slog_scope::warn!("Failed to initialize output: {}", err),
                };
            }
        }
        Ok(())
    }

    fn device_removed(&mut self, dev: dev_t) -> Result<()> {
        if let Some(device) = self.backend.kms().devices.get_mut(&dev) {
            for surface in device.surfaces.values_mut() {
                if let Some(token) = surface.render_timer_token.take() {
                    self.common.event_loop_handle.remove(token);
                }
                self.common.spaces.unmap_output(&surface.output);
            }
            if let Some(token) = device.event_token.take() {
                self.common.event_loop_handle.remove(token);
            }
        }
        Ok(())
    }
}

pub struct OutputChanges {
    pub added: Vec<(crtc::Handle, connector::Handle)>,
    pub removed: Vec<(crtc::Handle, connector::Handle)>,
}

impl Device {
    pub fn enumerate_surfaces(&mut self) -> Result<OutputChanges> {
        let drm = &mut *self.drm.as_source_mut();

        // enumerate our outputs
        let config = crtc_mapping::display_configuration(drm, self.supports_atomic)?;

        let surfaces = self.surfaces.iter()
            .map(|(c, s)| (*c, s.surface.current_connectors().into_iter().next().unwrap()))
            .collect::<HashMap<crtc::Handle, connector::Handle>>();
        
        let added = config.iter()
            .filter(|(conn, crtc)| surfaces.get(&crtc).map(|c| c != *conn).unwrap_or(true))
            .map(|(conn, crtc)| (crtc, conn))
            .map(|(crtc, conn)| (*crtc, *conn))
            .collect::<Vec<_>>();
        let removed = surfaces.iter()
            .filter(|(crtc, conn)| config.get(&conn).map(|c| c != *crtc).unwrap_or(true))
            .map(|(crtc, conn)| (*crtc, *conn))
            .collect::<Vec<_>>();

        Ok(OutputChanges {
            added,
            removed,
        })
    }

    fn setup_surface(
        &mut self,
        crtc: crtc::Handle,
        conn: connector::Handle,
        signaler: Signaler<Signal>,
        display: &mut Display,
        loop_handle: &mut LoopHandle<'static, State>,
    ) -> Result<Output> {
        let drm = &mut *self.drm.as_source_mut();
        let crtc_info = drm.get_crtc(crtc)?;
        let conn_info = drm.get_connector(conn)?;
        let vrr = crtc_mapping::set_vrr(drm, crtc, conn, true)?;
        let interface = crtc_mapping::interface_name(drm, conn)?;
        let edid_info = crtc_mapping::edid_info(drm, conn)?;
        let mode = crtc_info.mode().unwrap_or(conn_info.modes()[0]);
        let mut surface = drm.create_surface(crtc, mode, &[conn])?;
        surface.link(signaler);

        let target = GbmBufferedSurface::new(surface, self.allocator.clone(), self.formats.clone(), None)
            .with_context(|| format!("Failed to initialize Gbm surface for {}", interface))?;

        let output_mode = OutputMode {
            size: (mode.size().0 as i32, mode.size().1 as i32).into(),
            refresh: (mode.vrefresh() * 1000) as i32,
        };
        let (phys_w, phys_h) = conn_info.size().unwrap_or((0, 0));
        let (output, output_global) = Output::new(
            display,
            interface,
            PhysicalProperties {
                size: (phys_w as i32, phys_h as i32).into(),
                // TODO: We need to read that from the connector properties
                subpixel: wl_output::Subpixel::Unknown,
                make: edid_info.manufacturer,
                model: edid_info.model,
            },
            None
        );
        output.set_preferred(output_mode.clone());
        output.change_current_state(
            Some(output_mode),
            None, // TODO: Readout property for monitor rotation
            None,
            None,
        );

        let timer = Timer::new()?;
        let timer_handle = timer.handle();
        // render timer
        let timer_token = loop_handle
            .insert_source(timer, |(dev_id, crtc), _, state| {
                if let Some(device) = state.backend.kms().devices.get_mut(&dev_id) {
                    let renderer = &mut device.renderer;
                    if let Some(surface) = device.surfaces.get_mut(&crtc) {
                        if let Err(err) = surface.render_output(renderer, &mut state.common) {
                            slog_scope::error!("Error rendering: {}", err);
                            // TODO re-schedule?
                        }
                    }
                }
            })
            .unwrap();
        timer_handle.add_timeout(Duration::ZERO, (drm.device_id(), crtc));

        let data = Surface {
            output: output.clone(),
            _global: output_global.into(),
            surface: target,
            vrr,
            refresh_rate: crtc_mapping::calculate_refresh_rate(mode),
            last_submit: None,
            pending: true,
            render_timer: timer_handle,
            render_timer_token: Some(timer_token),
        };
        self.surfaces.insert(crtc, data);

        Ok(output)
    }
}

impl Surface {
    pub fn render_output(
        &mut self,
        renderer: &mut Gles2Renderer,
        state: &mut Common,
    ) -> Result<()> {
        let custom_elements = Vec::new();

        let space = state.spaces.active_space_mut(&self.output);
        let (buffer, age) = self
            .surface
            .next_buffer()
            .with_context(|| "Failed to allocate buffer")?;
        renderer
            .bind(buffer)
            .with_context(|| "Failed to bind buffer")?;
        match space.render_output(
            renderer,
            &self.output,
            age as usize,
            [0.153, 0.161, 0.165, 1.0],
            &*custom_elements,
        ) {
            Ok(_) => {
                self.surface
                    .queue_buffer()
                    .with_context(|| "Failed to submit buffer for display")?;
            }
            Err(err) => {
                self.surface.reset_buffers();
                anyhow::bail!("Rendering failed: {}", err);
            }
        };
        Ok(())
    }
}

impl KmsState {
    pub fn schedule_render(&mut self, output: &Output) {
        if let Some((device, surface)) = self.devices.values_mut()
            .flat_map(|d| {
                let dev_id = d.drm.as_source_ref().device_id();
                d.surfaces.values_mut().map(move |s| (dev_id, s))
            })
            .find(|(_, s)| s.output == *output)
        {
            if !surface.pending {
                surface.pending = true;
                let duration = match surface.last_submit {
                    Some(DrmEventTime::Monotonic(instant)) => Instant::now().duration_since(instant),
                    Some(DrmEventTime::Realtime(time)) => SystemTime::now().duration_since(time).unwrap_or(Duration::new(100, 0)),
                    None => Duration::new(100, 0), // Just do an insane amount
                };
                let data = (device, surface.surface.crtc());
                if surface.vrr || 1.0 / (surface.refresh_rate as f64) < duration.as_secs_f64() / 1000.0 {
                    surface.render_timer.add_timeout(Duration::ZERO, data);
                } else {
                    surface.render_timer.add_timeout(duration.saturating_sub(Duration::from_millis(5)), data);
                }
            }
        }
    }
}