// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{CompOutputConfig, ScreenFilter},
    shell::Shell,
    state::BackendData,
    utils::{env::dev_var, prelude::*},
};

use anyhow::{Context, Result};
use calloop::LoopSignal;
use calloop::timer::{TimeoutAction, Timer};
use cosmic_comp_config::output::comp::{AdaptiveSync, OutputState};
use indexmap::IndexMap;
use render::gles::GbmGlowBackend;
use smithay::{
    backend::{
        allocator::{Buffer, dmabuf::Dmabuf, format::FormatSet},
        drm::{DrmDeviceFd, DrmNode, NodeType, VrrSupport, output::DrmOutputRenderElements},
        egl::{EGLContext, EGLDevice, EGLDisplay},
        input::InputEvent,
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{glow::GlowRenderer, multigpu::GpuManager},
        session::{Event as SessionEvent, Session, libseat::LibSeatSession},
        udev::{UdevBackend, UdevEvent, primary_gpu},
    },
    output::Output,
    reexports::{
        calloop::{Dispatcher, EventLoop, LoopHandle},
        drm::{
            Device as _,
            control::{Device as _, connector::Interface, crtc},
        },
        input::{self, Libinput},
        wayland_server::{Client, DisplayHandle},
    },
    utils::{Clock, DevPath, Monotonic, Size},
    wayland::{
        dmabuf::DmabufGlobal,
        drm_syncobj::{DrmSyncobjState, supports_syncobj_eventfd},
        relative_pointer::RelativePointerManagerState,
    },
};
use surface::GbmDrmOutput;
use tracing::{debug, error, info, warn};

use std::{
    collections::{HashMap, HashSet},
    path::Path,
    sync::{Arc, RwLock, atomic::AtomicBool},
    time::{Duration, Instant},
};

mod device;
mod drm_helpers;
pub mod render;
mod socket;
mod surface;
use device::*;
pub(crate) use surface::Surface;
pub use surface::Timings;

use super::render::{CLEAR_COLOR, CursorMode, output_elements};

#[derive(Debug)]
pub struct KmsState {
    pub drm_devices: IndexMap<DrmNode, Device>,
    pub input_devices: HashMap<String, input::Device>,
    pub primary_node: Arc<RwLock<Option<DrmNode>>>,
    // Mesa llvmpipe renderer, if supported and there are no render nodes
    pub software_renderer: Option<GlowRenderer>,
    pub api: GpuManager<GbmGlowBackend<DrmDeviceFd>>,

    pub session: LibSeatSession,
    libinput: Libinput,

    pub syncobj_state: Option<DrmSyncobjState>,
    pub udev_dispatcher: Dispatcher<'static, UdevBackend, State>,
}

pub struct KmsGuard<'a> {
    pub drm_devices: IndexMap<DrmNode, LockedDevice<'a>>,
    pub primary_node: Arc<RwLock<Option<DrmNode>>>,
    api: &'a mut GpuManager<GbmGlowBackend<DrmDeviceFd>>,
    session: &'a LibSeatSession,
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<'static, State>,
    state: &mut State,
) -> Result<()> {
    // establish session
    let (session, notifier) = LibSeatSession::new().context("Failed to acquire session")?;

    // setup input
    let libinput_context = init_libinput(dh, &session, &event_loop.handle())
        .context("Failed to initialize libinput backend")?;

    // watch for gpu events
    let udev_dispatcher = init_udev(session.seat(), &event_loop.handle())
        .context("Failed to initialize udev connection")?;

    // handle session events
    let loop_signal = event_loop.get_signal();
    let dispatcher = udev_dispatcher.clone();
    event_loop
        .handle()
        .insert_source(notifier, move |event, &mut (), state| match event {
            SessionEvent::ActivateSession => {
                state.resume_session(
                    dispatcher.clone(),
                    state.common.event_loop_handle.clone(),
                    loop_signal.clone(),
                );
            }
            SessionEvent::PauseSession => {
                state.pause_session();
            }
        })
        .map_err(|err| err.error)
        .context("Failed to initialize session event source")?;

    // finish backend initialization
    state.backend = BackendData::Kms(KmsState {
        drm_devices: IndexMap::new(),
        input_devices: HashMap::new(),
        primary_node: Arc::new(RwLock::new(None)),
        software_renderer: None,
        api: GpuManager::new(GbmGlowBackend::new()).context("Failed to initialize gpu backend")?,

        session,
        libinput: libinput_context,

        syncobj_state: None,
        udev_dispatcher: udev_dispatcher.clone(),
    });

    // manually add already present gpus
    let mut outputs = Vec::new();
    for (dev, path) in udev_dispatcher.as_source_ref().device_list() {
        match state.device_added(dev, path, dh) {
            Ok(added) => outputs.extend(added),
            Err(err) => warn!("Failed to add device {}: {:?}", path.display(), err),
        }
    }

    if let Err(err) = state.backend.kms().select_primary_gpu(dh) {
        warn!("Failed to determine primary gpu: {}", err);
    }

    if let Err(err) = state.refresh_output_config() {
        info!(
            ?err,
            "Couldn't enable all found outputs, trying to disable outputs."
        );
        if let Some(pos) = outputs
            .iter()
            .position(|o| o.is_internal())
            .or((!outputs.is_empty()).then_some(0))
        {
            for (i, output) in outputs.iter().enumerate() {
                output.config_mut().enabled = if i == pos {
                    OutputState::Enabled
                } else {
                    OutputState::Disabled
                };
            }
            if let Err(err) = state.refresh_output_config() {
                error!("Couldn't enable any output: {}", err);
            }
        }
    }

    // start x11
    let primary = *state.backend.kms().primary_node.read().unwrap();
    state.launch_xwayland(primary);

    Ok(())
}

fn init_libinput(
    dh: &DisplayHandle,
    session: &LibSeatSession,
    evlh: &LoopHandle<'static, State>,
) -> Result<Libinput> {
    let mut libinput_context =
        Libinput::new_with_udev::<LibinputSessionInterface<LibSeatSession>>(session.clone().into());
    libinput_context
        .udev_assign_seat(&session.seat())
        .map_err(|_| anyhow::anyhow!("Failed to assign seat to libinput"))?;
    let libinput_backend = LibinputInputBackend::new(libinput_context.clone());

    evlh.insert_source(libinput_backend, move |mut event, _, state| {
        if let InputEvent::DeviceAdded { device } = &mut event {
            state.common.config.read_device(device);
            state
                .backend
                .kms()
                .input_devices
                .insert(device.name().into(), device.clone());
        } else if let InputEvent::DeviceRemoved { device } = &event {
            state.backend.kms().input_devices.remove(device.name());
        }

        state.process_input_event(event);

        for output in state.common.shell.read().outputs() {
            state.backend.kms().schedule_render(output);
        }
    })
    .map_err(|err| err.error)
    .context("Failed to initialize libinput event source")?;

    // Create relative pointer global
    RelativePointerManagerState::new::<State>(dh);

    Ok(libinput_context)
}

fn determine_boot_gpu(seat: String) -> Option<DrmNode> {
    let primary_node = primary_gpu(&seat)
        .ok()
        .flatten()
        .and_then(|x| DrmNode::from_path(x).ok());
    primary_node.and_then(|x| x.node_with_type(NodeType::Render).and_then(Result::ok))
}

fn determine_primary_gpu(
    drm_devices: &IndexMap<DrmNode, Device>,
    seat: String,
) -> Result<Option<DrmNode>> {
    if let Some(device) = dev_var("COSMIC_RENDER_DEVICE") {
        if let Some(node) = drm_devices.values().find_map(|dev| {
            device
                .matches(&dev.inner.render_node)
                .then_some(dev.inner.render_node)
        }) {
            return Ok(Some(node));
        }
    }

    // try to find builtin display
    for dev in drm_devices.values() {
        if dev.inner.surfaces.values().any(|s| {
            if let Ok(conn_info) = dev.drm.device().get_connector(s.connector, false) {
                let i = conn_info.interface();
                i == Interface::EmbeddedDisplayPort || i == Interface::LVDS || i == Interface::DSI
            } else {
                false
            }
        }) {
            return Ok(Some(dev.inner.render_node));
        }
    }

    // else try to find the boot gpu
    let boot = determine_boot_gpu(seat);
    if let Some(boot) = boot {
        if drm_devices
            .values()
            .any(|dev| dev.inner.render_node == boot)
        {
            return Ok(Some(boot));
        }
    }

    // else just take the first
    Ok(drm_devices
        .values()
        .next()
        .filter(|dev| !dev.inner.is_software)
        .map(|dev| dev.inner.render_node))
}

/// Create `GlowRenderer` for `EGL_MESA_device_software` device, if present
fn software_renderer() -> anyhow::Result<GlowRenderer> {
    let mut devices = EGLDevice::enumerate()?;
    let device = devices
        .find(|device| {
            device
                .extensions()
                .iter()
                .any(|ext| ext == "EGL_MESA_device_software")
        })
        .ok_or_else(|| anyhow::anyhow!("no EGL device found with `EGL_MESA_device_software`"))?;
    let display = unsafe { EGLDisplay::new(device)? };
    let context = EGLContext::new(&display)?;
    unsafe { Ok(GlowRenderer::new(context)?) }
}

fn init_udev(
    seat: String,
    evlh: &LoopHandle<'static, State>,
) -> Result<Dispatcher<'static, UdevBackend, State>> {
    let udev_backend = UdevBackend::new(&seat)?;

    let dispatcher = Dispatcher::new(udev_backend, move |event, _, state: &mut State| {
        let dh = state.common.display_handle.clone();
        match match event {
            UdevEvent::Added {
                device_id,
                ref path,
            } => state
                .device_added(device_id, path, &dh)
                .with_context(|| format!("Failed to add drm device: {}", device_id)),
            UdevEvent::Changed { device_id } => state
                .device_changed(device_id)
                .with_context(|| format!("Failed to update drm device: {}", device_id)),
            UdevEvent::Removed { device_id } => state
                .device_removed(device_id, &dh)
                .with_context(|| format!("Failed to remove drm device: {}", device_id))
                .map(|_| Vec::new()),
        } {
            Ok(added) => {
                debug!("Successfully handled udev event.");

                {
                    let backend = state.backend.kms();
                    if matches!(event, UdevEvent::Added { .. } | UdevEvent::Removed { .. })
                        && backend.primary_node.read().unwrap().is_none()
                    {
                        if let Err(err) = state.backend.kms().select_primary_gpu(&dh) {
                            warn!("Failed to determine a new primary gpu: {}", err);
                        }
                    }
                }

                if let Err(err) = state.refresh_output_config() {
                    warn!("Unable to load output config: {}", err);
                    if !added.is_empty() {
                        for output in added {
                            output.config_mut().enabled = OutputState::Disabled;
                        }
                        if let Err(err) = state.refresh_output_config() {
                            error!("Unrecoverable config error: {}", err);
                        }
                    }
                }
            }
            Err(err) => {
                error!(?err, "Error while handling udev event.")
            }
        }
    });

    evlh.register_dispatcher(dispatcher.clone())
        .context("Failed to register udev event source")?;

    Ok(dispatcher)
}

impl State {
    pub(crate) fn resume_session(
        &mut self,
        dispatcher: Dispatcher<'static, UdevBackend, Self>,
        loop_handle: LoopHandle<'static, State>,
        loop_signal: LoopSignal,
    ) {
        let backend = self.backend.kms();

        // resume input
        if let Err(err) = backend.libinput.resume() {
            error!(?err, "Failed to resume libinput context.");
        }
        // active drm, resume leases
        for device in backend.drm_devices.values_mut() {
            if let Err(err) = device.drm.lock().activate(true) {
                error!(?err, "Failed to resume drm device");
            }
            if let Some(lease_state) = device.inner.leasing_global.as_mut() {
                lease_state.resume::<State>();
            }
        }

        // update state and schedule new render,
        // after processing the rest of the pending event loop events
        let dispatcher = dispatcher.clone();
        loop_handle.insert_idle(move |state| {
            // add new devices, update devices now
            let mut added = Vec::new();
            for (dev, path) in dispatcher.as_source_ref().device_list() {
                let drm_node = match DrmNode::from_dev_id(dev) {
                    Ok(node) => node,
                    Err(err) => {
                        error!(?err, "Failed to read drm device {}.", path.display(),);
                        continue;
                    }
                };
                if state.backend.kms().drm_devices.contains_key(&drm_node) {
                    match state.device_changed(dev) {
                        Ok(outputs) => added.extend(outputs),
                        Err(err) => {
                            error!(?err, "Failed to update drm device {}.", path.display(),)
                        }
                    }
                } else {
                    let dh = state.common.display_handle.clone();
                    match state.device_added(dev, path, &dh) {
                        Ok(outputs) => added.extend(outputs),
                        Err(err) => error!(?err, "Failed to add drm device {}.", path.display(),),
                    }
                }
            }

            // update outputs
            if let Err(err) = state.refresh_output_config() {
                warn!("Unable to load output config: {}", err);
                if !added.is_empty() {
                    for output in added {
                        output.config_mut().enabled = OutputState::Disabled;
                    }
                    if let Err(err) = state.refresh_output_config() {
                        error!("Unrecoverable config error: {}", err);
                    }
                }
            }
            state.common.refresh();

            // Re-configure lock surfaces to prompt the client to re-commit
            // fresh buffers. During S3 suspend, the GPU context may change
            // (new EGL context / DRM device reset), invalidating cached textures
            // in RendererSurfaceState. The lock client was frozen during suspend
            // and never re-committed, so its old buffer may be unimportable.
            // Without this, a deadlock occurs: no valid buffer → 0 render
            // elements → primary scanout never set → frame callbacks suppressed
            // → client never learns it needs to re-commit.
            //
            // Smithay deduplicates send_configure() when the size hasn't changed,
            // so we bump height by 1, send, then correct and send again. The
            // client processes both in one dispatch, acks the correct size, and
            // re-commits.
            {
                let shell = state.common.shell.read();
                if let Some(session_lock) = &shell.session_lock {
                    for (output, lock_surface) in &session_lock.surfaces {
                        let size = output.geometry().size;
                        let (w, h) = (size.w as u32, size.h as u32);
                        lock_surface.with_pending_state(|states| {
                            states.size = Some(Size::from((w, h.saturating_add(1))));
                        });
                        lock_surface.send_configure();
                        lock_surface.with_pending_state(|states| {
                            states.size = Some(Size::from((w, h)));
                        });
                        lock_surface.send_configure();
                    }
                }
            }
        });

        // Schedule delayed re-probes for connectors that need link training
        // after resume (e.g., DisplayPort takes 1-3s to re-establish).
        let resume_start = Instant::now();
        if let Err(err) = loop_handle.insert_source(
            Timer::from_duration(Duration::from_secs(1)),
            move |_, _, state| {
                let nodes = match &mut state.backend {
                    BackendData::Kms(kms) => kms.drm_devices.keys().cloned().collect::<Vec<_>>(),
                    _ => Vec::new(),
                };
                let mut added = Vec::new();
                for node in nodes {
                    match state.device_changed(node.dev_id()) {
                        Ok(outputs) => added.extend(outputs),
                        Err(err) => {
                            error!(?err, "Failed to re-probe drm device {}.", node);
                        }
                    }
                }
                if let Err(err) = state.refresh_output_config() {
                    warn!("Unable to load output config during re-probe: {}", err);
                    if !added.is_empty() {
                        for output in added {
                            output.config_mut().enabled = OutputState::Disabled;
                        }
                        if let Err(err) = state.refresh_output_config() {
                            error!("Unrecoverable config error during re-probe: {}", err);
                        }
                    }
                }
                state.common.refresh();

                // Safety net: if any lock surface still has no renderer buffer,
                // re-send configure. Handles the case where the initial
                // re-configure was sent before the client processed its events.
                {
                    let shell = state.common.shell.read();
                    if let Some(session_lock) = &shell.session_lock {
                        for (output, lock_surface) in &session_lock.surfaces {
                            let has_buffer =
                                smithay::backend::renderer::utils::with_renderer_surface_state(
                                    lock_surface.wl_surface(),
                                    |rs| rs.buffer().is_some(),
                                )
                                .unwrap_or(false);
                            if !has_buffer {
                                let size = output.geometry().size;
                                let (w, h) = (size.w as u32, size.h as u32);
                                lock_surface.with_pending_state(|states| {
                                    states.size =
                                        Some(Size::from((w, h.saturating_add(1))));
                                });
                                lock_surface.send_configure();
                                lock_surface.with_pending_state(|states| {
                                    states.size = Some(Size::from((w, h)));
                                });
                                lock_surface.send_configure();
                                warn!(
                                    "Lock surface for {} had no buffer after resume, re-sent configure",
                                    output.name(),
                                );
                            }
                        }
                    }
                }

                if resume_start.elapsed() < Duration::from_secs(3) {
                    TimeoutAction::ToDuration(Duration::from_secs(1))
                } else {
                    TimeoutAction::Drop
                }
            },
        ) {
            error!(?err, "Failed to schedule post-resume re-probe timer");
        }

        loop_signal.wakeup();
    }

    pub(crate) fn pause_session(&mut self) {
        let backend = self.backend.kms();
        backend.libinput.suspend();
        for device in backend.drm_devices.values_mut() {
            if let Some(lease_state) = device.inner.leasing_global.as_mut() {
                lease_state.suspend();
            }
            for surface in device.inner.surfaces.values_mut() {
                surface.suspend();
            }
            device.drm.pause();
        }
    }
}

impl KmsState {
    fn select_primary_gpu(&mut self, dh: &DisplayHandle) -> Result<()> {
        // We don't have to check the allow/blocklist here,
        // as any disallowed devices won't be in `self.drm_devices`.

        let mut primary_node = self.primary_node.write().unwrap();
        let _ = primary_node.take(); // if we error don't leave an old node in place
        *primary_node = determine_primary_gpu(&self.drm_devices, self.session.seat())?;

        if let Some(node) = *primary_node {
            info!("Using {} as primary gpu for rendering.", node);
            self.software_renderer.take();
        } else if self.software_renderer.is_none() {
            info!("Failed to find a suitable gpu, using software renderingr");
            self.software_renderer = match software_renderer() {
                Ok(renderer) => Some(renderer),
                Err(err) => {
                    error!(?err, "Failed to initialize software EGL renderer.");
                    None
                }
            };
        }

        if !crate::utils::env::bool_var("COSMIC_DISABLE_SYNCOBJ").unwrap_or(false) {
            if let Some(primary_node) = primary_node
                .as_ref()
                .and_then(|node| node.node_with_type(NodeType::Primary).and_then(|x| x.ok()))
            {
                if let Some(device) = self.drm_devices.get(&primary_node) {
                    let import_device = device.drm.device().device_fd().clone();
                    if supports_syncobj_eventfd(&import_device) {
                        if let Some(state) = self.syncobj_state.as_mut() {
                            state.update_device(import_device);
                        } else {
                            let syncobj_state = DrmSyncobjState::new::<State>(dh, import_device);
                            self.syncobj_state = Some(syncobj_state);
                        }
                        return Ok(());
                    }
                }
            }

            if let Some(old_state) = self.syncobj_state.take() {
                dh.remove_global::<State>(old_state.into_global());
            }
        }

        Ok(())
    }

    pub fn switch_vt(&mut self, num: i32) -> Result<(), anyhow::Error> {
        self.session.change_vt(num).map_err(Into::into)
    }

    pub fn dmabuf_imported(
        &mut self,
        client: Option<Client>,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
    ) -> Result<DrmNode> {
        let mut device = self
            .drm_devices
            .values_mut()
            .find(|device| {
                device
                    .socket
                    .as_ref()
                    .map(|s| &s.dmabuf_global == global)
                    .unwrap_or(false)
            })
            .context("Couldn't find gpu for dmabuf global")?;

        // If device advertised to client doesn't support format/modifier, select
        // first device that does. This is needed for image-copy from
        // output/toplevel on a different node.
        //
        // TODO: After
        // https://gitlab.freedesktop.org/wayland/wayland-protocols/-/merge_requests/268,
        // only try the device specified explicitly by the client, if set.
        if !device.texture_formats.contains(&dmabuf.format()) {
            device = self
                .drm_devices
                .values_mut()
                .find(|device| device.texture_formats.contains(&dmabuf.format()))
                .context("Dmabuf cannot be imported on any gpu")?;
        }

        let new_client = if let Some(client) = client {
            let new = device.inner.active_clients.insert(client.id());
            device.inner.update_egl(
                self.primary_node.read().unwrap().as_ref(),
                self.api.as_mut(),
            )? && new
        } else {
            false
        };

        let egl = device
            .inner
            .egl
            .as_ref()
            .context("EGL initialization Error")?;
        egl.display
            .create_image_from_dmabuf(&dmabuf)
            .inspect(|image| unsafe {
                smithay::backend::egl::ffi::egl::DestroyImageKHR(
                    **egl.display.get_display_handle(),
                    *image,
                );
            })
            .context("Failed to create EGLImage from dmabuf")?;

        let node = device.inner.render_node;
        dmabuf.set_node(node);

        if new_client {
            self.refresh_used_devices()?;
        }

        Ok(node)
    }

    pub fn schedule_render(&mut self, output: &Output) {
        for surface in self
            .drm_devices
            .values()
            .flat_map(|d| d.inner.surfaces.values())
            .filter(|s| s.output == *output || s.output.mirroring().is_some_and(|o| &o == output))
        {
            surface.schedule_render();
        }
    }

    pub fn target_node_for_output(&self, output: &Output) -> Option<DrmNode> {
        self.drm_devices
            .values()
            .find(|dev| dev.inner.surfaces.values().any(|s| s.output == *output))
            .map(|dev| &dev.inner.render_node)
            .copied()
    }

    pub fn update_screen_filter(&mut self, screen_filter: &ScreenFilter) -> Result<()> {
        for device in self.drm_devices.values_mut() {
            for surface in device.inner.surfaces.values_mut() {
                surface.set_screen_filter(screen_filter.clone());
            }
        }

        // We don't expect this to fail in a meaningful way.
        // The shader is already compiled at this point and we don't rely on any features,
        // that might not be available for any filters we currently expose.
        //
        // But we might conditionally fail here in the future.
        Ok(())
    }

    pub fn refresh_used_devices(&mut self) -> Result<()> {
        let primary_node = self.primary_node.read().unwrap();
        let mut used_devices = HashSet::new();

        for device in self.drm_devices.values_mut() {
            if device
                .inner
                .update_egl(primary_node.as_ref(), self.api.as_mut())?
            {
                used_devices.insert(device.inner.render_node);
            }
        }

        // trigger re-evaluation... urgh
        if let Some(primary_node) = primary_node.as_ref() {
            let _ = self.api.single_renderer(primary_node);
        }

        // I hate this. I want partial borrows of hashmap values
        let all_devices = self
            .drm_devices
            .values()
            .map(|d| d.inner.render_node)
            .collect::<Vec<_>>();
        for node in all_devices {
            let (mut device, others) = self
                .drm_devices
                .values_mut()
                .map(|d| &mut d.inner)
                .partition::<Vec<_>, _>(|d| d.render_node == node);
            device[0].update_surface_nodes(&used_devices, &others)?;
        }

        Ok(())
    }

    pub fn lock_devices(&mut self) -> KmsGuard<'_> {
        KmsGuard {
            drm_devices: self
                .drm_devices
                .iter_mut()
                .map(|(node, device)| (*node, device.lock()))
                .collect(),
            primary_node: self.primary_node.clone(),
            api: &mut self.api,
            session: &self.session,
        }
    }
}

impl KmsGuard<'_> {
    pub fn schedule_render(&mut self, output: &Output) {
        for surface in self
            .drm_devices
            .values()
            .flat_map(|d| d.inner.surfaces.values())
            .filter(|s| s.output == *output || s.output.mirroring().is_some_and(|o| &o == output))
        {
            surface.schedule_render();
        }
    }

    pub fn refresh_used_devices(&mut self) -> Result<()> {
        let primary_node = self.primary_node.read().unwrap();
        let mut used_devices = HashSet::new();

        for device in self.drm_devices.values_mut() {
            if device
                .inner
                .update_egl(primary_node.as_ref(), self.api.as_mut())?
            {
                used_devices.insert(device.inner.render_node);
            }
        }

        // trigger re-evaluation... urgh
        if let Some(primary_node) = primary_node.as_ref() {
            let _ = self.api.single_renderer(primary_node);
        }

        // I hate this. I want partial borrows of hashmap values
        let all_devices = self
            .drm_devices
            .values()
            .map(|d| d.inner.render_node)
            .collect::<Vec<_>>();
        for node in all_devices {
            let (mut device, others) = self
                .drm_devices
                .values_mut()
                .map(|d| &mut *d.inner)
                .partition::<Vec<_>, _>(|d| d.render_node == node);
            device[0].update_surface_nodes(&used_devices, &others)?;
        }

        Ok(())
    }

    pub fn all_outputs(&self) -> Vec<Output> {
        self.drm_devices
            .values()
            .flat_map(|device| {
                device
                    .inner
                    .outputs
                    .iter()
                    .filter(|(conn, _)| {
                        !device
                            .inner
                            .leased_connectors
                            .iter()
                            .any(|(leased_conn, _)| *conn == leased_conn)
                    })
                    .map(|(_, output)| output.clone())
            })
            .collect()
    }

    pub fn apply_config_for_outputs(
        &mut self,
        test_only: bool,
        loop_handle: &LoopHandle<'static, State>,
        screen_filter: &ScreenFilter,
        shell: Arc<parking_lot::RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
        clock: &Clock<Monotonic>,
    ) -> Result<(), anyhow::Error> {
        if !self.session.is_active() {
            return Ok(());
        }

        for device in self.drm_devices.values_mut() {
            // we only want outputs exposed to wayland - not leased ones
            // but that is also not all surface, because that doesn't contain all detected, but unmapped outputs
            let outputs = device
                .inner
                .outputs
                .iter()
                .filter(|(conn, _)| {
                    !device
                        .inner
                        .leased_connectors
                        .iter()
                        .any(|(leased_conn, _)| *conn == leased_conn)
                })
                .map(|(_, output)| output.clone())
                .collect::<Vec<_>>();

            let mut new_pairings = HashMap::new();

            // figure out potential new crtcs
            // TODO: Right now we always keep crtcs of already enabled outputs,
            //  even if another configuration could potentially enable more outputs

            let res_handles = device.drm.device().resource_handles()?;
            let free_crtcs = res_handles
                .crtcs()
                .iter()
                .filter(|crtc| {
                    device.inner.surfaces.get(crtc).is_none()
                    // TODO: We can't do this. See https://github.com/Smithay/smithay/pull/1820
                    //.is_some_and(|surface| surface.output.is_enabled())
                })
                .copied()
                .collect::<HashSet<crtc::Handle>>();
            let open_conns = outputs
                .iter()
                .filter(|output| {
                    output.is_enabled()
                        && !device.inner.surfaces.values().any(|s| &s.output == *output)
                })
                .flat_map(|output| {
                    device
                        .inner
                        .outputs
                        .iter()
                        .find_map(|(conn, o)| (output == o).then_some(*conn))
                });

            for conn in open_conns {
                let conn_info = device
                    .drm
                    .device()
                    .get_connector(conn, false)
                    .with_context(|| {
                        format!(
                            "Failed to query drm device: {:?}",
                            device.drm.device().dev_path().as_deref().map(Path::display),
                        )
                    })?;
                'outer: for encoder_info in conn_info
                    .encoders()
                    .iter()
                    .flat_map(|encoder_handle| device.drm.device().get_encoder(*encoder_handle))
                {
                    for crtc in res_handles.filter_crtcs(encoder_info.possible_crtcs()) {
                        if free_crtcs.contains(&crtc) {
                            new_pairings.insert(conn, crtc);
                            break 'outer;
                        }
                    }
                }

                if !new_pairings.contains_key(&conn) {
                    // test failed, we don't have a crtc for conn
                    anyhow::bail!("Missing crtc for {conn:?}, gpu doesn't have enough resources.");
                }
            }

            // first drop old surfaces
            if !test_only {
                for output in outputs.iter().filter(|o| !o.is_enabled()) {
                    device
                        .inner
                        .surfaces
                        .retain(|_, surface| surface.output != *output);
                }
            }

            // add new ones
            let mut w = shell.read().global_space().size.w as u32;
            if !test_only {
                for (conn, crtc) in new_pairings {
                    let (output, _) = device.inner.connector_added(
                        device.drm.device_mut(),
                        self.primary_node.clone(),
                        conn,
                        Some(crtc),
                        (w, 0),
                        loop_handle,
                        screen_filter.clone(),
                        shell.clone(),
                        startup_done.clone(),
                    )?;
                    if output.mirroring().is_none() {
                        w += output.geometry().size.w as u32;
                    }
                }
            }
        }

        if !test_only {
            self.refresh_used_devices()
                .context("Failed to enable devices")?;
        }

        for device in self.drm_devices.values_mut() {
            let now = clock.now();
            let output_map = device
                .inner
                .surfaces
                .iter()
                .filter(|(_, s)| s.is_active())
                .map(|(crtc, surface)| (*crtc, surface.output.clone()))
                .collect::<HashMap<_, _>>();

            // reconfigure existing
            for (crtc, surface) in device.inner.surfaces.iter_mut() {
                let output_config = CompOutputConfig(surface.output.config());

                let drm = &mut device.drm;
                let conn = surface.connector;
                let conn_info = drm.device().get_connector(conn, false)?;
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
                        (output_config.0.mode.1.unwrap() as i32 - refresh_rate as i32).abs()
                    })
                    .ok_or(anyhow::anyhow!("Unable to find matching mode"))?;

                if !test_only {
                    if !surface.is_active() {
                        let mut planes = drm
                            .device()
                            .planes(crtc)
                            .with_context(|| "Failed to enumerate planes")?;

                        let driver = drm.device().get_driver().ok();

                        // QUIRK: Using an overlay plane on a nvidia card breaks the display controller (wtf...)
                        if driver.as_ref().is_some_and(|driver| {
                            driver
                                .name()
                                .to_string_lossy()
                                .to_lowercase()
                                .contains("nvidia")
                        }) {
                            planes.overlay = vec![];
                        }
                        // QUIRK: Cursor planes on evdi sometimes don't disappear correctly.
                        // TODO: Debug and figure out, as they can be a nice improvement.
                        if driver.as_ref().is_some_and(|driver| {
                            driver
                                .name()
                                .to_string_lossy()
                                .to_lowercase()
                                .contains("evdi")
                        }) {
                            planes.cursor = vec![];
                        }

                        let compositor: GbmDrmOutput = {
                            let mut renderer = self
                                .api
                                .single_renderer(&device.inner.render_node)
                                .with_context(|| "Failed to create renderer")?;

                            let mut elements = DrmOutputRenderElements::default();
                            for (crtc, output) in output_map.iter() {
                                let output_elements = output_elements(
                                    Some(&device.inner.render_node),
                                    &mut renderer,
                                    &shell,
                                    now,
                                    output,
                                    CursorMode::All,
                                    None,
                                )
                                .with_context(|| "Failed to render outputs")?;

                                elements.add_output(crtc, CLEAR_COLOR, output_elements);
                            }

                            let compositor = drm
                                .initialize_output(
                                    *crtc,
                                    *mode,
                                    &[conn],
                                    &surface.output,
                                    Some(planes.clone()),
                                    &mut renderer,
                                    &elements,
                                )
                                .with_context(|| "Failed to create drm surface")?;

                            let _ = renderer;

                            compositor
                        };

                        if let Some(bpc) = output_config.0.max_bpc {
                            if let Err(err) = drm_helpers::set_max_bpc(drm.device(), conn, bpc) {
                                warn!(
                                    ?bpc,
                                    ?err,
                                    "Failed to set max_bpc on connector: {}",
                                    surface.output.name()
                                );
                            }
                        }

                        let vrr = output_config.0.vrr;
                        std::mem::drop(output_config);

                        let compositor_ref = drm.compositors().get(crtc).unwrap().lock().unwrap();
                        let vrr_support = compositor_ref
                            .vrr_supported(
                                compositor_ref
                                    .pending_connectors()
                                    .into_iter()
                                    .next()
                                    .unwrap(),
                            )
                            .ok();

                        let primary_formats = compositor_ref.surface().plane_info().formats.clone();
                        let overlay_formats = planes
                            .overlay
                            .iter()
                            .flat_map(|p| p.formats.iter().cloned())
                            .collect::<FormatSet>();
                        surface.resume(
                            compositor,
                            primary_formats,
                            Some(overlay_formats).filter(|f| !f.indexset().is_empty()),
                        );

                        surface.output.set_adaptive_sync_support(vrr_support);
                        if match vrr_support {
                            Some(VrrSupport::RequiresModeset) if vrr == AdaptiveSync::Enabled => {
                                false
                            }
                            Some(VrrSupport::NotSupported) => false,
                            _ => true,
                        } {
                            surface.use_adaptive_sync(vrr);
                            surface.output.set_adaptive_sync(vrr);
                        } else {
                            surface.use_adaptive_sync(AdaptiveSync::Disabled);
                            surface.output.config_mut().vrr = AdaptiveSync::Disabled;
                            surface.output.set_adaptive_sync(AdaptiveSync::Disabled);
                        }
                    } else {
                        let vrr = output_config.0.vrr;
                        std::mem::drop(output_config);
                        if vrr != surface.output.adaptive_sync() {
                            if match surface.output.adaptive_sync_support() {
                                Some(VrrSupport::RequiresModeset)
                                    if vrr == AdaptiveSync::Enabled =>
                                {
                                    true
                                }
                                Some(VrrSupport::NotSupported) => true,
                                _ => false,
                            } {
                                anyhow::bail!("Requested VRR mode unsupported");
                            }

                            surface.use_adaptive_sync(vrr);
                            surface.output.set_adaptive_sync(vrr);
                        }

                        let mut renderer = self
                            .api
                            .single_renderer(&device.inner.render_node)
                            .with_context(|| "Failed to create renderer")?;

                        let mut elements = DrmOutputRenderElements::default();
                        for (crtc, output) in output_map.iter() {
                            let output_elements = output_elements(
                                Some(&device.inner.render_node),
                                &mut renderer,
                                &shell,
                                now,
                                output,
                                CursorMode::All,
                                None,
                            )
                            .with_context(|| "Failed to render outputs")?;

                            elements.add_output(crtc, CLEAR_COLOR, output_elements);
                        }

                        drm.use_mode(&surface.crtc, *mode, &mut renderer, &elements)
                            .context("Failed to apply new mode")?;
                    }
                }
            }

            // configure primary scanout allowance
            if !device.inner.surfaces.is_empty() {
                let mut renderer = self
                    .api
                    .single_renderer(&device.inner.render_node)
                    .with_context(|| "Failed to create renderer")?;

                device
                    .allow_primary_scanout_any(
                        device
                            .inner
                            .surfaces
                            .values()
                            .filter(|s| s.output.is_enabled() && s.output.mirroring().is_none())
                            .count()
                            <= 1,
                        &mut renderer,
                        clock,
                        &shell,
                    )
                    .context("Failed to switch primary-plane scanout flags")?;

                let mut elements = DrmOutputRenderElements::default();
                for (crtc, output) in output_map.iter() {
                    let output_elements = output_elements(
                        Some(&device.inner.render_node),
                        &mut renderer,
                        &shell,
                        now,
                        output,
                        CursorMode::All,
                        None,
                    )
                    .with_context(|| "Failed to render outputs")?;

                    elements.add_output(crtc, CLEAR_COLOR, output_elements);
                }

                if let Err(err) = device
                    .drm
                    .try_to_restore_modifiers(&mut renderer, &elements)
                {
                    warn!(?err, "Failed to restore modifiers");
                }
            }
        }

        // we need to handle mirroring, after all outputs have been enabled
        let all_outputs = self.all_outputs();
        for device in self.drm_devices.values_mut() {
            for surface in device.inner.surfaces.values_mut() {
                let mirrored_output =
                    if let OutputState::Mirroring(conn) = &surface.output.config().enabled {
                        Some(
                            all_outputs
                                .iter()
                                .find(|output| output.name() == *conn)
                                .cloned()
                                .ok_or(anyhow::anyhow!("Unable to find mirroring output"))?,
                        )
                    } else {
                        None
                    };

                if !test_only && mirrored_output != surface.output.mirroring() {
                    surface.set_mirroring(mirrored_output.clone());
                }
            }
        }

        Ok(())
    }
}
