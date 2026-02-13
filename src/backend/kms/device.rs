// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::{
        kms::render::gles::GbmGlowBackend,
        render::{CLEAR_COLOR, CursorMode, GlMultiRenderer, init_shaders, output_elements},
    },
    config::{CompTransformDef, EdidProduct, ScreenFilter},
    shell::Shell,
    utils::{env::dev_list_var, prelude::*},
    wayland::handlers::image_copy_capture::PendingImageCopyData,
};

use anyhow::{Context, Result};
use cosmic_comp_config::output::comp::{AdaptiveSync, OutputConfig, OutputState};
use libc::dev_t;
use smithay::{
    backend::{
        allocator::{
            Format, Fourcc,
            format::FormatSet,
            gbm::{GbmAllocator, GbmDevice},
        },
        drm::{
            DrmDevice, DrmDeviceFd, DrmEvent, DrmNode, NodeType,
            compositor::{FrameError, FrameFlags},
            exporter::gbm::GbmFramebufferExporter,
            output::{DrmOutputManager, LockedDrmOutputManager},
        },
        egl::{EGLContext, EGLDevice, EGLDisplay, context::ContextPriority},
        renderer::glow::GlowRenderer,
        session::Session,
    },
    desktop::utils::OutputPresentationFeedback,
    output::{Mode as OutputMode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{LoopHandle, RegistrationToken},
        drm::control::{Device as ControlDevice, ModeTypeFlags, connector, crtc},
        gbm::BufferObjectFlags as GbmBufferFlags,
        rustix::fs::OFlags,
        wayland_server::DisplayHandle,
    },
    utils::{Clock, DevPath, DeviceFd, Monotonic, Point, Transform},
    wayland::drm_lease::{DrmLease, DrmLeaseState},
};
use tracing::{error, info, warn};
use wayland_backend::server::ClientId;

use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    os::fd::OwnedFd,
    path::Path,
    sync::{Arc, RwLock, atomic::AtomicBool, mpsc::Receiver},
    time::Duration,
};

use super::{drm_helpers, socket::Socket, surface::Surface};

#[derive(Debug)]
pub struct EGLInternals {
    pub display: EGLDisplay,
    pub device: EGLDevice,
    pub context: EGLContext,
}

pub type LockedGbmDrmOutputManager<'a> = LockedDrmOutputManager<
    'a,
    GbmAllocator<DrmDeviceFd>,
    GbmFramebufferExporter<DrmDeviceFd>,
    Option<(
        OutputPresentationFeedback,
        Receiver<PendingImageCopyData>,
        Duration,
    )>,
    DrmDeviceFd,
>;

pub type GbmDrmOutputManager = DrmOutputManager<
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
pub struct Device {
    pub inner: InnerDevice,
    pub drm: GbmDrmOutputManager,

    supports_atomic: bool,
    pub texture_formats: FormatSet,
    event_token: Option<RegistrationToken>,
    pub socket: Option<Socket>,
}

#[derive(Debug)]
pub struct LockedDevice<'a> {
    pub inner: &'a mut InnerDevice,
    pub drm: LockedGbmDrmOutputManager<'a>,
}

pub struct InnerDevice {
    pub dev_node: DrmNode,
    pub render_node: DrmNode,
    pub is_software: bool,
    pub egl: Option<EGLInternals>,

    pub outputs: HashMap<connector::Handle, Output>,
    pub surfaces: HashMap<crtc::Handle, Surface>,
    pub gbm: GbmDevice<DrmDeviceFd>,

    pub leased_connectors: Vec<(connector::Handle, crtc::Handle)>,
    pub leasing_global: Option<DrmLeaseState>,
    pub active_leases: Vec<DrmLease>,
    pub active_clients: HashSet<ClientId>,
}

impl fmt::Debug for InnerDevice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Device")
            .field("dev_node", &self.dev_node)
            .field("render_node", &self.render_node)
            .field("is_software", &self.is_software)
            .field("egl", &self.egl)
            .field("outputs", &self.outputs)
            .field("surfaces", &self.surfaces)
            .field("gbm", &"..")
            .field("leased_connectors", &self.leased_connectors)
            .field("leasing_global", &self.leasing_global)
            .field("active_leases", &self.active_leases)
            .field("active_clients", &self.active_clients.len())
            .finish()
    }
}

pub fn init_egl(gbm: &GbmDevice<DrmDeviceFd>) -> Result<EGLInternals> {
    let path = gbm.dev_path();

    let display = unsafe { EGLDisplay::new(gbm.clone()) }.with_context(|| {
        format!(
            "Failed to create EGLDisplay for device: {:?}",
            path.as_deref().map(Path::display)
        )
    })?;
    let device = EGLDevice::device_for_display(&display).with_context(|| {
        format!(
            "Unable to find matching egl device for {:?}",
            path.as_deref().map(Path::display)
        )
    })?;

    let context =
        EGLContext::new_with_priority(&display, ContextPriority::High).with_context(|| {
            format!(
                "Failed to create EGLContext for device {:?}:{:?}",
                device,
                path.as_deref().map(Path::display),
            )
        })?;

    Ok(EGLInternals {
        display,
        device,
        context,
    })
}

impl State {
    pub fn device_added(
        &mut self,
        dev: dev_t,
        path: &Path,
        dh: &DisplayHandle,
    ) -> Result<Vec<Output>> {
        if !self.backend.kms().session.is_active() {
            return Ok(Vec::new());
        }

        if let Some(allowlist) = dev_list_var("COSMIC_DRM_ALLOW_DEVICES") {
            let mut matched = false;
            if let Ok(node) = DrmNode::from_dev_id(dev) {
                let node = node
                    .node_with_type(NodeType::Render)
                    .and_then(|res| res.ok())
                    .unwrap_or(node);
                for ident in allowlist {
                    if ident.matches(&node) {
                        matched = true;
                        break;
                    }
                }
                if !matched {
                    info!(
                        "Skipping device {} due to COSMIC_DRM_ALLOW_DEVICE list.",
                        path.display()
                    );
                    return Ok(Vec::new());
                }
            }
        }
        if let Some(blocklist) = dev_list_var("COSMIC_DRM_BLOCK_DEVICES") {
            if let Ok(node) = DrmNode::from_dev_id(dev) {
                let node = node
                    .node_with_type(NodeType::Render)
                    .and_then(|res| res.ok())
                    .unwrap_or(node);
                for ident in blocklist {
                    if ident.matches(&node) {
                        info!(
                            "Skipping device {} due to COSMIC_DRM_BLOCK_DEVICE list.",
                            path.display()
                        );
                        return Ok(Vec::new());
                    }
                }
            }
        }

        let fd = DrmDeviceFd::new(DeviceFd::from(
            self.backend
                .kms()
                .session
                .open(
                    path,
                    OFlags::RDWR | OFlags::CLOEXEC | OFlags::NOCTTY | OFlags::NONBLOCK,
                )
                .with_context(|| {
                    format!(
                        "Failed to optain file descriptor for drm device: {}",
                        path.display()
                    )
                })?,
        ));
        let (drm, notifier) = DrmDevice::new(fd.clone(), false)
            .with_context(|| format!("Failed to initialize drm device for: {}", path.display()))?;
        let drm_node = DrmNode::from_dev_id(dev)?;
        let supports_atomic = drm.is_atomic();

        let gbm = GbmDevice::new(fd)
            .with_context(|| format!("Failed to initialize GBM device for {}", path.display()))?;
        let (render_node, render_formats, texture_formats, is_software) = {
            let egl = init_egl(&gbm)?;

            let render_node = egl
                .device
                .try_get_render_node()
                .ok()
                .and_then(std::convert::identity)
                .unwrap_or(drm_node);
            let render_formats = egl.context.dmabuf_render_formats().clone();
            let texture_formats = egl.context.dmabuf_texture_formats().clone();

            (
                render_node,
                render_formats,
                texture_formats,
                egl.device.is_software(),
            )
        };

        let token = self
            .common
            .event_loop_handle
            .insert_source(
                notifier,
                move |event, metadata, state: &mut State| match event {
                    DrmEvent::VBlank(crtc) => {
                        if let Some(device) = state.backend.kms().drm_devices.get_mut(&drm_node) {
                            if let Some(surface) = device.inner.surfaces.get_mut(&crtc) {
                                surface.on_vblank(metadata.take());
                            }
                        }
                    }
                    DrmEvent::Error(err) => {
                        warn!(?err, "Failed to read events of device {:?}.", dev);
                    }
                },
            )
            .with_context(|| format!("Failed to add drm device to event loop: {}", dev))?;

        let socket = match (!is_software)
            .then(|| self.create_socket(dh, render_node, texture_formats.clone()))
            .transpose()
        {
            Ok(socket) => socket,
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to initialize hardware-acceleration for clients on {}.", render_node,
                );
                None
            }
        };

        let leasing_global = match (!is_software)
            .then(|| DrmLeaseState::new::<State>(dh, &drm_node))
            .transpose()
        {
            Ok(global) => global,
            Err(err) => {
                // TODO: replace with inspect_err, once stable
                warn!(
                    ?err,
                    "Failed to initialize drm lease global for: {}", drm_node
                );
                None
            }
        };

        let drm = GbmDrmOutputManager::new(
            drm,
            GbmAllocator::new(
                gbm.clone(),
                GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
            ),
            GbmFramebufferExporter::new(gbm.clone(), render_node.into()),
            Some(gbm.clone()),
            [
                Fourcc::Abgr2101010,
                Fourcc::Argb2101010,
                Fourcc::Abgr8888,
                Fourcc::Argb8888,
            ],
            render_formats,
        );

        let mut device = Device {
            drm,
            inner: InnerDevice {
                dev_node: drm_node,
                render_node,
                is_software,
                egl: None,

                outputs: HashMap::new(),
                surfaces: HashMap::new(),
                gbm,

                leased_connectors: Vec::new(),
                leasing_global,
                active_leases: Vec::new(),
                active_clients: HashSet::new(),
            },

            supports_atomic,
            texture_formats,
            event_token: Some(token),
            socket,
        };

        let connectors = device.enumerate_surfaces()?.added; // There are no removed outputs on newly added devices
        let mut wl_outputs = Vec::new();
        let mut w = self.common.shell.read().global_space().size.w as u32;

        {
            for (conn, maybe_crtc) in connectors {
                match device.inner.connector_added(
                    device.drm.device_mut(),
                    self.backend.kms().primary_node.clone(),
                    conn,
                    maybe_crtc,
                    (w, 0),
                    &self.common.event_loop_handle,
                    self.common.config.dynamic_conf.screen_filter().clone(),
                    self.common.shell.clone(),
                    self.common.startup_done.clone(),
                ) {
                    Ok((output, should_expose)) => {
                        if should_expose {
                            w += output.geometry().size.w as u32;
                            wl_outputs.push(output.clone());
                        }
                        device.inner.outputs.insert(conn, output);
                    }
                    Err(err) => {
                        warn!(?err, "Failed to initialize output, skipping");
                    }
                }
            }

            // TODO atomic commit all surfaces together and drop surfaces, if it fails due to bandwidth

            let kms = self.backend.kms();
            kms.drm_devices.insert(drm_node, device);
        }

        self.common
            .output_configuration_state
            .add_heads(wl_outputs.iter());

        self.backend.kms().refresh_used_devices()?;
        Ok(wl_outputs)
    }

    pub fn device_changed(&mut self, dev: dev_t) -> Result<Vec<Output>> {
        if !self.backend.kms().session.is_active() {
            return Ok(Vec::new());
        }

        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        let mut outputs_added = Vec::new();

        {
            let backend = self.backend.kms();
            if let Some(device) = backend.drm_devices.get_mut(&drm_node) {
                let changes = device.enumerate_surfaces()?;

                let mut w = self.common.shell.read().global_space().size.w as u32;
                for conn in changes.removed {
                    // contains conns with updated crtcs, just drop the surface and re-create
                    if let Some(pos) = device
                        .inner
                        .leased_connectors
                        .iter()
                        .position(|(handle, _)| *handle == conn)
                    {
                        let _ = device.inner.leased_connectors.remove(pos);
                        if let Some(leasing_state) = device.inner.leasing_global.as_mut() {
                            leasing_state.withdraw_connector(conn);
                        }
                    } else if let Some(crtc) = device
                        .inner
                        .surfaces
                        .iter()
                        .find_map(|(crtc, surface)| (surface.connector == conn).then_some(crtc))
                        .cloned()
                    {
                        device.inner.surfaces.remove(&crtc).unwrap();
                    }

                    if !changes.added.iter().any(|(c, _)| c == &conn) {
                        outputs_removed.push(
                            device
                                .inner
                                .outputs
                                .remove(&conn)
                                .expect("Connector without output?"),
                        );
                    }
                }

                for (conn, maybe_crtc) in changes.added {
                    match device.inner.connector_added(
                        device.drm.device_mut(),
                        backend.primary_node.clone(),
                        conn,
                        maybe_crtc,
                        (w, 0),
                        &self.common.event_loop_handle,
                        self.common.config.dynamic_conf.screen_filter().clone(),
                        self.common.shell.clone(),
                        self.common.startup_done.clone(),
                    ) {
                        Ok((output, should_expose)) => {
                            if should_expose {
                                w += output.geometry().size.w as u32;
                                outputs_added.push(output.clone());
                            }

                            device.inner.outputs.insert(conn, output);
                        }
                        Err(err) => {
                            warn!(?err, "Failed to initialize output, skipping");
                        }
                    }
                }
            }
        }

        self.common
            .output_configuration_state
            .remove_heads(outputs_removed.iter());
        self.common
            .output_configuration_state
            .add_heads(outputs_added.iter());

        for output in outputs_removed {
            self.common.remove_output(&output);
        }

        self.backend.kms().refresh_used_devices()?;
        Ok(outputs_added)
    }

    pub fn device_removed(&mut self, dev: dev_t, dh: &DisplayHandle) -> Result<()> {
        let backend = self.backend.kms();
        // we can't use DrmNode::from_node_id, because that assumes the node is still on sysfs
        let drm_node = backend
            .drm_devices
            .values()
            .find_map(|device| {
                (device.inner.dev_node.dev_id() == dev).then_some(device.inner.dev_node)
            })
            .with_context(|| format!("Couldn't find drm node for {}", dev))?;

        let mut outputs_removed = Vec::new();
        let device_fd = if let Some(mut device) = backend.drm_devices.shift_remove(&drm_node) {
            if let Some(mut leasing_global) = device.inner.leasing_global.take() {
                leasing_global.disable_global::<State>();
            }
            for (_, surface) in device.inner.surfaces.drain() {
                outputs_removed.push(surface.output.clone());
                surface.drop_and_join();
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
            backend.api.as_mut().remove_node(&device.inner.render_node);
            backend
                .primary_node
                .write()
                .unwrap()
                .take_if(|node| node == &device.inner.render_node);

            Some(device.drm.device().device_fd().device_fd())
        } else {
            None
        };
        self.common
            .output_configuration_state
            .remove_heads(outputs_removed.iter());

        if backend.session.is_active() {
            for output in outputs_removed {
                self.common.remove_output(&output);
            }
        } else {
            self.common.output_configuration_state.update();
        }

        backend.refresh_used_devices()?;

        if let Some(fd) = device_fd {
            match TryInto::<OwnedFd>::try_into(fd) {
                Ok(fd) => {
                    if let Err(err) = backend.session.close(fd) {
                        warn!("Failed to close drm device fd: {}", err);
                    }
                }
                Err(_) => {
                    warn!(?drm_node, "Unable to close drm device fd cleanly.");
                }
            };
        }
        Ok(())
    }

    pub fn refresh_output_config(&mut self) -> Result<()> {
        self.common.config.read_outputs(
            &mut self.common.output_configuration_state,
            &mut self.backend,
            &self.common.shell,
            &self.common.event_loop_handle,
            &mut self.common.workspace_state.update(),
            &self.common.xdg_activation_state,
            self.common.startup_done.clone(),
            &self.common.clock,
        )?;
        self.common.refresh();
        Ok(())
    }
}

pub struct OutputChanges {
    pub added: Vec<(connector::Handle, Option<crtc::Handle>)>,
    pub removed: Vec<connector::Handle>,
}

impl Device {
    pub fn enumerate_surfaces(&mut self) -> Result<OutputChanges> {
        // enumerate our outputs
        let config =
            drm_helpers::display_configuration(self.drm.device_mut(), self.supports_atomic)?;

        let surfaces = self
            .inner
            .surfaces
            .iter()
            .map(|(c, s)| (s.connector, *c))
            .chain(
                self.inner
                    .leased_connectors
                    .iter()
                    .map(|(conn, crtc)| (*conn, *crtc)),
            )
            .collect::<HashMap<connector::Handle, crtc::Handle>>();

        let added = config
            .iter()
            .filter(|(conn, maybe)| match (surfaces.get(conn), maybe) {
                (Some(current_crtc), Some(new_crtc)) => current_crtc != new_crtc,
                // see `removed`
                (Some(_), None) => true,
                // if we already know about it, we don't consider it added
                (None, _) => !self.inner.outputs.contains_key(conn),
            })
            .map(|(conn, crtc)| (*conn, *crtc))
            .collect::<Vec<_>>();

        let removed = self
            .inner
            .outputs
            .iter()
            .filter(|(conn, _)| match config.get(conn) {
                Some(Some(c)) => surfaces.get(conn).is_some_and(|crtc| c != crtc),
                // if don't have a crtc, we need to drop the surface if it exists.
                // so it needs to be in both `removed` AND `added`.
                Some(None) => surfaces.contains_key(conn),
                _ => true,
            })
            .map(|(conn, _)| *conn)
            .collect::<Vec<_>>();

        Ok(OutputChanges { added, removed })
    }

    pub fn lock(&mut self) -> LockedDevice<'_> {
        LockedDevice {
            inner: &mut self.inner,
            drm: self.drm.lock(),
        }
    }
}

impl LockedDevice<'_> {
    fn allow_frame_flags(
        &mut self,
        flag: bool,
        flags: FrameFlags,
        renderer: &mut GlMultiRenderer,
        clock: &Clock<Monotonic>,
        shell: &Arc<parking_lot::RwLock<Shell>>,
    ) -> Result<()> {
        for surface in self.inner.surfaces.values_mut() {
            surface.allow_frame_flags(flag, flags);
        }

        if !flag {
            let now = clock.now();
            let output_map = self
                .inner
                .surfaces
                .iter()
                .filter(|(_, s)| s.is_active())
                .map(|(crtc, surface)| (*crtc, surface.output.clone()))
                .collect::<HashMap<_, _>>();

            for (crtc, compositor) in self.drm.compositors().iter() {
                let elements = match output_map.get(crtc) {
                    Some(output) => output_elements(
                        Some(&self.inner.render_node),
                        renderer,
                        shell,
                        now,
                        output,
                        CursorMode::All,
                        None,
                    )
                    .with_context(|| "Failed to render outputs")?,
                    None => Vec::new(),
                };

                let mut compositor = compositor.lock().unwrap();
                compositor.render_frame(renderer, &elements, CLEAR_COLOR, FrameFlags::empty())?;
                if let Err(err) = compositor.commit_frame() {
                    if !matches!(err, FrameError::EmptyFrame) {
                        return Err(err.into());
                    }
                }
            }
        }

        Ok(())
    }

    pub fn allow_overlay_scanout(
        &mut self,
        flag: bool,
        renderer: &mut GlMultiRenderer,
        clock: &Clock<Monotonic>,
        shell: &Arc<parking_lot::RwLock<Shell>>,
    ) -> Result<()> {
        self.allow_frame_flags(
            flag,
            FrameFlags::ALLOW_OVERLAY_PLANE_SCANOUT,
            renderer,
            clock,
            shell,
        )
    }

    pub fn allow_primary_scanout_any(
        &mut self,
        flag: bool,
        renderer: &mut GlMultiRenderer,
        clock: &Clock<Monotonic>,
        shell: &Arc<parking_lot::RwLock<Shell>>,
    ) -> Result<()> {
        self.allow_frame_flags(
            flag,
            FrameFlags::ALLOW_PRIMARY_PLANE_SCANOUT_ANY,
            renderer,
            clock,
            shell,
        )?;

        for (crtc, comp) in self.drm.compositors() {
            let Some(surface) = self.inner.surfaces.get_mut(crtc) else {
                continue;
            };
            let comp = comp.lock().unwrap();
            surface.primary_plane_formats = if flag {
                comp.surface().plane_info().formats.clone()
            } else {
                // This certainly isn't perfect and might still miss the happy path,
                // but it is surprisingly difficult to hack an api into smithay,
                // to get the actual framebuffer format
                let code = comp.format();
                FormatSet::from_iter(comp.modifiers().iter().map(|mo| Format {
                    code,
                    modifier: *mo,
                }))
            };
            surface.feedback.clear();
        }

        Ok(())
    }
}

impl InnerDevice {
    pub fn in_use(&self, primary: Option<&DrmNode>) -> bool {
        Some(&self.render_node) == primary
            || !self.surfaces.is_empty()
            || !self.active_clients.is_empty()
    }

    pub fn connector_added(
        &mut self,
        drm: &mut DrmDevice,
        primary_node: Arc<RwLock<Option<DrmNode>>>,
        conn: connector::Handle,
        maybe_crtc: Option<crtc::Handle>,
        position: (u32, u32),
        evlh: &LoopHandle<'static, State>,
        screen_filter: ScreenFilter,
        shell: Arc<parking_lot::RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
    ) -> Result<(Output, bool)> {
        let output = self
            .outputs
            .get(&conn)
            .cloned()
            .map(Ok)
            .unwrap_or_else(|| create_output_for_conn(drm, conn))
            .context("Failed to create `Output`")?;

        let non_desktop = match drm_helpers::get_property_val(drm, conn, "non-desktop") {
            Ok((val_type, value)) => val_type.convert_value(value).as_boolean().unwrap(),
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to determine if connector is meant desktop usage, assuming so."
                );
                false
            }
        };

        if non_desktop {
            if let Some(crtc) = maybe_crtc {
                self.leased_connectors.push((conn, crtc));
                info!(
                    "Connector {} is non-desktop, setting up for leasing",
                    output.name()
                );
                if let Some(lease_state) = self.leasing_global.as_mut() {
                    let physical = output.physical_properties();
                    lease_state.add_connector::<State>(
                        conn,
                        output.name(),
                        format!("{} {}", physical.make, physical.model),
                    );
                }
            } else {
                warn!(
                    "Connector {} is non-desktop, but we don't have a free crtc: not leasing",
                    output.name()
                );
            }

            Ok((output, false))
        } else {
            let new_config = output
                .user_data()
                .insert_if_missing(|| RefCell::new(OutputConfig::default()));

            populate_modes(drm, &output, conn, new_config, position)
                .with_context(|| "Failed to enumerate connector modes")?;

            let has_surface = if let Some(crtc) = maybe_crtc {
                match Surface::new(
                    &output,
                    crtc,
                    conn,
                    primary_node,
                    self.dev_node,
                    self.render_node,
                    evlh,
                    screen_filter,
                    shell,
                    startup_done,
                ) {
                    Ok(data) => {
                        self.surfaces.insert(crtc, data);
                        true
                    }
                    Err(err) => {
                        error!(?crtc, "Failed to initialize surface: {}", err);
                        false
                    }
                }
            } else {
                false
            };

            if !has_surface {
                output
                    .user_data()
                    .get::<RefCell<OutputConfig>>()
                    .unwrap()
                    .borrow_mut()
                    .enabled = OutputState::Disabled;
            }

            Ok((output, true))
        }
    }

    pub fn update_egl(
        &mut self,
        primary_node: Option<&DrmNode>,
        api: &mut GbmGlowBackend<DrmDeviceFd>,
    ) -> Result<bool> {
        if self.in_use(primary_node) {
            if self.egl.is_none() {
                let egl = init_egl(&self.gbm).context("Failed to create EGL context")?;
                let mut renderer = unsafe {
                    GlowRenderer::new(
                        EGLContext::new_shared_with_priority(
                            &egl.display,
                            &egl.context,
                            ContextPriority::High,
                        )
                        .context("Failed to create shared EGL context")?,
                    )
                    .context("Failed to create GL renderer")?
                };
                init_shaders(renderer.borrow_mut()).context("Failed to compile shaders")?;
                api.add_node(
                    self.render_node,
                    GbmAllocator::new(
                        self.gbm.clone(),
                        // SCANOUT because stride bugs
                        GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
                    ),
                    renderer,
                );
                self.egl = Some(egl);
            }
            Ok(true)
        } else {
            if self.egl.is_some() {
                let _ = self.egl.take();
                api.remove_node(&self.render_node);
            }
            Ok(false)
        }
    }

    pub fn update_surface_nodes<'b>(
        &mut self,
        used_devices: &HashSet<DrmNode>,
        others: &[&'b mut Self],
    ) -> Result<()>
    where
        Self: 'b,
    {
        for surface in self.surfaces.values_mut() {
            let known_nodes = surface.known_nodes().clone();
            for gone_device in known_nodes.difference(used_devices) {
                surface.remove_node(*gone_device);
            }
            for new_device in used_devices.difference(&known_nodes) {
                let (render_node, egl, gbm) = if self.render_node == *new_device {
                    // we need to make sure to do partial borrows here, as device.surfaces is borrowed mutable
                    (
                        self.render_node,
                        self.egl.as_ref().unwrap(),
                        self.gbm.clone(),
                    )
                } else {
                    let device = others
                        .iter()
                        .find(|d| d.render_node == *new_device)
                        .unwrap();
                    (
                        device.render_node,
                        device.egl.as_ref().unwrap(),
                        device.gbm.clone(),
                    )
                };

                surface.add_node(
                    render_node,
                    GbmAllocator::new(gbm, GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT),
                    EGLContext::new_shared_with_priority(
                        &egl.display,
                        &egl.context,
                        ContextPriority::High,
                    )
                    .context("Failed to create shared EGL context")?,
                );
            }
        }

        Ok(())
    }
}

fn create_output_for_conn(drm: &mut DrmDevice, conn: connector::Handle) -> Result<Output> {
    let conn_info = drm
        .get_connector(conn, false)
        .with_context(|| "Failed to query connector info")?;
    let interface = drm_helpers::interface_name(drm, conn)?;
    let edid_info = drm_helpers::edid_info(drm, conn)
        .inspect_err(|err| warn!(?err, "failed to get EDID for {}", interface))
        .ok();
    let (phys_w, phys_h) = conn_info.size().unwrap_or((0, 0));

    let output = Output::new(
        interface,
        PhysicalProperties {
            size: (phys_w as i32, phys_h as i32).into(),
            subpixel: match conn_info.subpixel() {
                connector::SubPixel::HorizontalRgb => Subpixel::HorizontalRgb,
                connector::SubPixel::HorizontalBgr => Subpixel::HorizontalBgr,
                connector::SubPixel::VerticalRgb => Subpixel::VerticalRgb,
                connector::SubPixel::VerticalBgr => Subpixel::VerticalBgr,
                connector::SubPixel::None => Subpixel::None,
                _ => Subpixel::Unknown,
            },
            make: edid_info
                .as_ref()
                .and_then(|info| info.make())
                .unwrap_or_else(|| String::from("Unknown")),
            model: edid_info
                .as_ref()
                .and_then(|info| info.model())
                .unwrap_or_else(|| String::from("Unknown")),
            serial_number: edid_info
                .as_ref()
                .and_then(|info| info.serial())
                .unwrap_or_else(|| String::from("Unknown")),
        },
    );
    if let Some(edid) = edid_info.as_ref().and_then(|x| x.edid()) {
        output
            .user_data()
            .insert_if_missing(|| EdidProduct::from(edid.vendor_product()));
    }
    Ok(output)
}

fn populate_modes(
    drm: &mut DrmDevice,
    output: &Output,
    conn: connector::Handle,
    new_config: bool,
    position: (u32, u32),
) -> Result<()> {
    let conn_info = drm.get_connector(conn, false)?;
    let max_bpc = drm_helpers::get_max_bpc(drm, conn)?.map(|(_val, range)| range.end.min(16));
    let Some(mode) = conn_info
        .modes()
        .iter()
        .find(|mode| mode.mode_type().contains(ModeTypeFlags::PREFERRED))
        .copied()
        .or(conn_info.modes().first().copied())
    else {
        anyhow::bail!("No mode found");
    };

    let refresh_rate = drm_helpers::calculate_refresh_rate(mode);
    let output_mode = OutputMode {
        size: (mode.size().0 as i32, mode.size().1 as i32).into(),
        refresh: refresh_rate as i32,
    };

    let mut modes = Vec::new();
    for mode in conn_info.modes() {
        let refresh_rate = drm_helpers::calculate_refresh_rate(*mode);
        let mode = OutputMode {
            size: (mode.size().0 as i32, mode.size().1 as i32).into(),
            refresh: refresh_rate as i32,
        };
        modes.push(mode);
        output.add_mode(mode);
    }
    for mode in output
        .modes()
        .into_iter()
        .filter(|mode| !modes.contains(mode))
    {
        output.delete_mode(mode);
    }
    output.set_preferred(output_mode);

    if new_config {
        let scale = conn_info
            .size()
            .map(|size| calculate_scale(conn_info.interface(), size, mode.size()))
            .unwrap_or(1.0);
        let transform = drm_helpers::panel_orientation(drm, conn).unwrap_or(Transform::Normal);
        output.change_current_state(
            Some(output_mode),
            Some(transform),
            Some(Scale::Fractional(scale)),
            Some(Point::from((position.0 as i32, position.1 as i32))),
        );

        let mut output_config = output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow_mut();
        *output_config = OutputConfig {
            mode: ((output_mode.size.w, output_mode.size.h), Some(refresh_rate)),
            position,
            max_bpc,
            scale,
            transform: CompTransformDef::from(transform).0,
            // Try opportunistic VRR by default,
            // if not supported this will be turned off on `resume`,
            // when we have the `Surface` to actually check for support.
            vrr: AdaptiveSync::Enabled,
            ..std::mem::take(&mut *output_config)
        };
    }

    Ok(())
}

pub fn calculate_scale(
    interface: connector::Interface,
    monitor_size_mm: (u32, u32),
    resolution: (u16, u16),
) -> f64 {
    let (w_mm, h_mm) = monitor_size_mm;
    let (w_px, h_px) = resolution;
    let shorter_res = w_px.min(h_px);
    let fallback_scale = match shorter_res {
        px if px <= 1600 => 1.0,
        _ => 2.0,
    };
    if w_mm == 0 || h_mm == 0 {
        // possibly projector, but could just be some no-brand display
        return fallback_scale;
    }

    let (w_in, h_in) = (w_mm as f64 / 25.4, h_mm as f64 / 25.4);
    // due to edid's setting non-sensicle values,
    // lets be really careful we don't devide by 0 (or non-normal values) when deriving values from size.
    // (also the size should be positive, but the u16 arguments already force that.)
    if !w_in.is_normal() || !h_in.is_normal() {
        return fallback_scale;
    }

    let diag = (w_in.powf(2.) * h_in.powf(2.)).sqrt();
    let dpi = (w_px as f64 / w_in + h_px as f64 / h_in) / 2.0;

    match diag {
        _diag if diag < 20. || interface == connector::Interface::EmbeddedDisplayPort => {
            // likely laptop
            scale_from_dpi(dpi, 144, shorter_res)
        }
        // 16:10 has a height of 19.6, anything lower is most likely an ultrawide
        _diag if diag >= 40. && h_in >= 19. => {
            // likely TV
            match shorter_res {
                px if px <= 1200 => 1.0,
                _ => 2.0,
            }
        }
        _ => {
            // likely desktop
            scale_from_dpi(dpi, 120, shorter_res)
        }
    }
}

pub fn scale_from_dpi(dpi: f64, step_size: u32, shorter_px: u16) -> f64 {
    let scale = (dpi / step_size as f64) * 100.0;

    // snap to 50%
    let scale = ((scale.round() as u32).next_multiple_of(50) as f64) / 100.0;

    // max values
    let max = match shorter_px {
        px if px <= 1200 => 1.0, // 125% is usually a worse experience than 100% atm
        // for 1440p and weird variants we let the algorithm take over, but limit the max, this is inspired by what windows does
        px if px <= 1600 => 1.5,
        _ => 2.0, // never go higher than 200% by default though (Note for the future: Tablets? Phones?)
    };

    let min = match shorter_px {
        px if px >= 2000 => 2.0, // 4k should default to 200%, as we prefer integer scales
        _ => 1.0,                // never go lower than 100%
    };

    scale.min(max).max(min)
}

#[cfg(test)]
mod test {
    use super::{calculate_scale, connector::Interface};

    #[test]
    fn test_scale() {
        fn scale(interface: Interface, diag_in: f64, w_px: u16, h_px: u16) -> i32 {
            let aspect = (w_px as f64) / (h_px as f64);
            let diag_mm = diag_in * 25.4;
            let h_mm = diag_mm / (aspect.powf(2.0) + 1.0).sqrt();
            let w_mm = h_mm * aspect;
            (calculate_scale(interface, (w_mm as u32, h_mm as u32), (w_px, h_px)) * 100.0) as i32
        }

        // Laptops
        for &iface in &[Interface::EmbeddedDisplayPort, Interface::LVDS] {
            // 14 inch 3:2
            assert_eq!(scale(iface, 14.0, 3000, 2000), 200);

            // Various sizes 16:9
            for &diag_in in &[14.0, 15.6, 17.3] {
                assert_eq!(scale(iface, diag_in, 1920, 1080), 100);
                assert_eq!(scale(iface, diag_in, 2560, 1440), 150);
                assert_eq!(scale(iface, diag_in, 3840, 2160), 200);
            }

            // Various sizes 16:10
            for &diag_in in &[14.0, 16.0] {
                assert_eq!(scale(iface, diag_in, 1920, 1200), 100);
                assert_eq!(scale(iface, diag_in, 2560, 1600), 150);
                assert_eq!(scale(iface, diag_in, 3840, 2400), 200);
            }
        }

        // Desktops
        for &iface in &[Interface::DisplayPort, Interface::HDMIA, Interface::HDMIB] {
            // 24 inch 16:9
            assert_eq!(scale(iface, 24.0, 1920, 1080), 100);
            assert_eq!(scale(iface, 24.0, 2560, 1440), 150);
            assert_eq!(scale(iface, 24.0, 3840, 2160), 200);

            // Larger sizes 16:9
            for &diag_in in &[27.0, 32.0, 38.0] {
                assert_eq!(scale(iface, diag_in, 1920, 1080), 100);
                assert_eq!(scale(iface, diag_in, 2560, 1440), 100);
                assert_eq!(scale(iface, diag_in, 3840, 2160), 200);
            }

            // Smaller sizes 21:9
            for &diag_in in &[26.0, 29.0] {
                assert_eq!(scale(iface, diag_in, 2560, 1080), 100);
                assert_eq!(scale(iface, diag_in, 3440, 1440), 150);
                assert_eq!(scale(iface, diag_in, 5120, 2160), 200);
            }

            // Larger sizes 21:9
            for &diag_in in &[34.0, 45.0] {
                assert_eq!(scale(iface, diag_in, 2560, 1080), 100);
                assert_eq!(scale(iface, diag_in, 3440, 1440), 100);
                assert_eq!(scale(iface, diag_in, 5120, 2160), 200);
            }

            // Various sizes 32:9
            for &diag_in in &[45.0, 49.0, 57.0] {
                assert_eq!(scale(iface, diag_in, 3840, 1080), 100);
                assert_eq!(scale(iface, diag_in, 5120, 1440), 100);
                assert_eq!(scale(iface, diag_in, 7680, 2160), 200);
            }
        }

        // TVs
        for &iface in &[Interface::DisplayPort, Interface::HDMIA, Interface::HDMIB] {
            // Various sizes 16:9
            for &diag_in in &[40.0, 42.0, 48.0, 55.0, 65.0, 77.0, 83.0] {
                assert_eq!(scale(iface, diag_in, 1920, 1080), 100);
                assert_eq!(scale(iface, diag_in, 2560, 1440), 200);
                assert_eq!(scale(iface, diag_in, 3840, 2160), 200);
            }
        }

        // Zero sized displays (projectors, invalid EDID)
        for &iface in &[Interface::DisplayPort, Interface::HDMIA, Interface::HDMIB] {
            assert_eq!(scale(iface, 0.0, 1920, 1080), 100);
            assert_eq!(scale(iface, 0.0, 2560, 1440), 100);
            assert_eq!(scale(iface, 0.0, 3840, 2160), 200);
        }
    }
}
