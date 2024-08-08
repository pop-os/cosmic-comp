// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    config::{OutputConfig, OutputState},
    shell::Shell,
    utils::prelude::*,
};

use anyhow::{Context, Result};
use libc::dev_t;
use smithay::{
    backend::{
        allocator::gbm::GbmDevice,
        drm::{DrmDevice, DrmDeviceFd, DrmEvent, DrmNode},
        egl::{context::ContextPriority, EGLContext, EGLDevice, EGLDisplay},
        session::Session,
    },
    output::{Mode as OutputMode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{LoopHandle, RegistrationToken},
        drm::control::{connector, crtc, Device as ControlDevice, ModeTypeFlags},
        rustix::fs::OFlags,
        wayland_server::{protocol::wl_buffer::WlBuffer, DisplayHandle, Weak},
    },
    utils::{DevPath, DeviceFd, Point, Transform},
    wayland::drm_lease::{DrmLease, DrmLeaseState},
};
use tracing::{error, info, warn};

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    path::{Path, PathBuf},
    sync::{atomic::AtomicBool, Arc, RwLock},
};

use super::{drm_helpers, socket::Socket, surface::Surface};

#[derive(Debug)]
pub struct EGLInternals {
    pub display: EGLDisplay,
    pub device: EGLDevice,
    pub context: EGLContext,
}

pub struct Device {
    pub dev_node: DrmNode,
    pub render_node: DrmNode,
    pub egl: Option<EGLInternals>,

    pub outputs: HashMap<connector::Handle, Output>,
    pub surfaces: HashMap<crtc::Handle, Surface>,
    pub gbm: GbmDevice<DrmDeviceFd>,
    pub drm: DrmDevice,

    supports_atomic: bool,

    pub leased_connectors: Vec<(connector::Handle, crtc::Handle)>,
    pub leasing_global: Option<DrmLeaseState>,
    pub active_leases: Vec<DrmLease>,
    pub active_buffers: HashSet<Weak<WlBuffer>>,

    event_token: Option<RegistrationToken>,
    pub socket: Option<Socket>,
}

impl fmt::Debug for Device {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Device")
            .field("dev_node", &self.render_node)
            .field("render_node", &self.render_node)
            .field("outputs", &self.outputs)
            .field("surfaces", &self.surfaces)
            .field("drm", &self.drm)
            .field("gbm", &self.gbm)
            .field("egl", &self.egl)
            .field("supports_atomic", &self.supports_atomic)
            .field("leased_connectors", &self.leased_connectors)
            .field("leasing_global", &self.leasing_global)
            .field("active_leases", &self.active_leases)
            .field("event_token", &self.event_token)
            .field("socket", &self.socket)
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
    pub fn device_added(&mut self, dev: dev_t, path: PathBuf, dh: &DisplayHandle) -> Result<()> {
        if !self.backend.kms().session.is_active() {
            return Ok(());
        }

        let fd = DrmDeviceFd::new(DeviceFd::from(
            self.backend
                .kms()
                .session
                .open(
                    &path,
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
        let (render_node, render_formats) = {
            let egl = init_egl(&gbm)?;

            let render_node = egl
                .device
                .try_get_render_node()
                .ok()
                .and_then(std::convert::identity)
                .unwrap_or(drm_node);
            let render_formats = egl.context.dmabuf_texture_formats().clone();

            (render_node, render_formats)
        };

        let token = self
            .common
            .event_loop_handle
            .insert_source(
                notifier,
                move |event, metadata, state: &mut State| match event {
                    DrmEvent::VBlank(crtc) => {
                        if let Some(device) = state.backend.kms().drm_devices.get_mut(&drm_node) {
                            if let Some(surface) = device.surfaces.get_mut(&crtc) {
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

        let socket = match self.create_socket(dh, render_node, render_formats.clone()) {
            Ok(socket) => Some(socket),
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to initialize hardware-acceleration for clients on {}.", render_node,
                );
                None
            }
        };

        let mut device = Device {
            dev_node: drm_node,
            render_node,
            egl: None,

            outputs: HashMap::new(),
            surfaces: HashMap::new(),
            gbm: gbm.clone(),
            drm,

            supports_atomic,

            leased_connectors: Vec::new(),
            leasing_global: DrmLeaseState::new::<State>(dh, &drm_node)
                .map_err(|err| {
                    // TODO: replace with inspect_err, once stable
                    warn!(
                        ?err,
                        "Failed to initialize drm lease global for: {}", drm_node
                    );
                    err
                })
                .ok(),
            active_leases: Vec::new(),
            active_buffers: HashSet::new(),

            event_token: Some(token),
            socket,
        };

        let connectors = device.enumerate_surfaces()?.added; // There are no removed outputs on newly added devices
        let mut wl_outputs = Vec::new();
        let mut w = self.common.shell.read().unwrap().global_space().size.w as u32;

        {
            for (conn, maybe_crtc) in connectors {
                match device.connector_added(
                    self.backend.kms().primary_node.as_ref(),
                    conn,
                    maybe_crtc,
                    (w, 0),
                    &self.common.event_loop_handle,
                    self.common.shell.clone(),
                    self.common.startup_done.clone(),
                ) {
                    Ok((output, should_expose)) => {
                        if should_expose {
                            w += output.config().mode_size().w as u32;
                            wl_outputs.push(output.clone());
                        }
                        device.outputs.insert(conn, output);
                    }
                    Err(err) => {
                        warn!(?err, "Failed to initialize output, skipping");
                    }
                }
            }

            // TODO atomic commit all surfaces together and drop surfaces, if it fails due to bandwidth

            self.backend.kms().drm_devices.insert(drm_node, device);
        }

        self.backend.kms().refresh_used_devices()?;

        self.common
            .output_configuration_state
            .add_heads(wl_outputs.iter());
        self.common.config.read_outputs(
            &mut self.common.output_configuration_state,
            &mut self.backend,
            &self.common.shell,
            &self.common.event_loop_handle,
            &mut self.common.workspace_state.update(),
            &self.common.xdg_activation_state,
            self.common.startup_done.clone(),
        );
        self.common.refresh();

        Ok(())
    }

    pub fn device_changed(&mut self, dev: dev_t) -> Result<()> {
        if !self.backend.kms().session.is_active() {
            return Ok(());
        }

        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        let mut outputs_added = Vec::new();

        {
            let backend = self.backend.kms();
            if let Some(device) = backend.drm_devices.get_mut(&drm_node) {
                let changes = device.enumerate_surfaces()?;

                let mut w = self.common.shell.read().unwrap().global_space().size.w as u32;
                for conn in changes.removed {
                    // contains conns with updated crtcs, just drop the surface and re-create
                    if let Some(pos) = device
                        .leased_connectors
                        .iter()
                        .position(|(handle, _)| *handle == conn)
                    {
                        let _ = device.leased_connectors.remove(pos);
                        if let Some(leasing_state) = device.leasing_global.as_mut() {
                            leasing_state.withdraw_connector(conn);
                        }
                    } else if let Some(crtc) = device
                        .surfaces
                        .iter()
                        .find_map(|(crtc, surface)| (surface.connector == conn).then_some(crtc))
                        .cloned()
                    {
                        let surface = device.surfaces.remove(&crtc).unwrap();
                        // TODO: move up later outputs?
                        w -= surface
                            .output
                            .current_mode()
                            .map(|m| m.size.w as u32)
                            .unwrap_or(0);
                    }

                    if !changes.added.iter().any(|(c, _)| c == &conn) {
                        outputs_removed.push(
                            device
                                .outputs
                                .remove(&conn)
                                .expect("Connector without output?"),
                        );
                    }
                }

                for (conn, maybe_crtc) in changes.added {
                    match device.connector_added(
                        backend.primary_node.as_ref(),
                        conn,
                        maybe_crtc,
                        (w, 0),
                        &self.common.event_loop_handle,
                        self.common.shell.clone(),
                        self.common.startup_done.clone(),
                    ) {
                        Ok((output, should_expose)) => {
                            if should_expose {
                                w += output.config().mode_size().w as u32;
                                outputs_added.push(output.clone());
                            }

                            device.outputs.insert(conn, output);
                        }
                        Err(err) => {
                            warn!(?err, "Failed to initialize output, skipping");
                        }
                    }
                }
            }
        }

        self.backend.kms().refresh_used_devices()?;

        self.common
            .output_configuration_state
            .remove_heads(outputs_removed.iter());
        self.common
            .output_configuration_state
            .add_heads(outputs_added.iter());
        {
            self.common.config.read_outputs(
                &mut self.common.output_configuration_state,
                &mut self.backend,
                &self.common.shell,
                &self.common.event_loop_handle,
                &mut self.common.workspace_state.update(),
                &self.common.xdg_activation_state,
                self.common.startup_done.clone(),
            );
            // Don't remove the outputs, before potentially new ones have been initialized.
            // reading a new config means outputs might become enabled, that were previously disabled.
            // This gives the shell more information on how to move outputs.
            for output in outputs_removed {
                self.common.remove_output(&output);
            }
            self.common.refresh();
        }

        Ok(())
    }

    pub fn device_removed(&mut self, dev: dev_t, dh: &DisplayHandle) -> Result<()> {
        let drm_node = DrmNode::from_dev_id(dev)?;
        let mut outputs_removed = Vec::new();
        let backend = self.backend.kms();
        if let Some(mut device) = backend.drm_devices.remove(&drm_node) {
            if let Some(mut leasing_global) = device.leasing_global.take() {
                leasing_global.disable_global::<State>();
            }
            for surface in device.surfaces.values_mut() {
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

        if self.backend.kms().session.is_active() {
            for output in outputs_removed {
                self.common.remove_output(&output);
            }
            self.common.config.read_outputs(
                &mut self.common.output_configuration_state,
                &mut self.backend,
                &self.common.shell,
                &self.common.event_loop_handle,
                &mut self.common.workspace_state.update(),
                &self.common.xdg_activation_state,
                self.common.startup_done.clone(),
            );
            self.common.refresh();
        } else {
            self.common.output_configuration_state.update();
        }

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
        let config = drm_helpers::display_configuration(&mut self.drm, self.supports_atomic)?;

        let surfaces = self
            .surfaces
            .iter()
            .map(|(c, s)| (s.connector, *c))
            .chain(
                self.leased_connectors
                    .iter()
                    .map(|(conn, crtc)| (*conn, *crtc)),
            )
            .collect::<HashMap<connector::Handle, crtc::Handle>>();

        let added = config
            .iter()
            .filter(|(conn, maybe)| match (surfaces.get(&conn), maybe) {
                (Some(current_crtc), Some(new_crtc)) => current_crtc != new_crtc,
                (None, _) => true,
                _ => false,
            })
            .map(|(conn, crtc)| (*conn, *crtc))
            .collect::<Vec<_>>();

        let removed = self
            .outputs
            .iter()
            .filter(|(conn, _)| match config.get(conn) {
                Some(Some(c)) => surfaces.get(&conn).is_some_and(|crtc| c != crtc),
                _ => true,
            })
            .map(|(conn, _)| *conn)
            .collect::<Vec<_>>();

        Ok(OutputChanges { added, removed })
    }

    pub fn connector_added(
        &mut self,
        primary_node: Option<&DrmNode>,
        conn: connector::Handle,
        maybe_crtc: Option<crtc::Handle>,
        position: (u32, u32),
        evlh: &LoopHandle<'static, State>,
        shell: Arc<RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
    ) -> Result<(Output, bool)> {
        let output = self
            .outputs
            .get(&conn)
            .cloned()
            .map(|output| Ok(output))
            .unwrap_or_else(|| create_output_for_conn(&mut self.drm, conn))
            .context("Failed to create `Output`")?;

        let non_desktop = match drm_helpers::get_property_val(&self.drm, conn, "non-desktop") {
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
            output
                .user_data()
                .insert_if_missing(|| RefCell::new(OutputConfig::default()));

            populate_modes(&mut self.drm, &output, conn, position)
                .with_context(|| "Failed to enumerate connector modes")?;

            let has_surface = if let Some(crtc) = maybe_crtc {
                match Surface::new(
                    &output,
                    crtc,
                    conn,
                    primary_node.copied().unwrap_or(self.render_node),
                    self.dev_node,
                    self.render_node,
                    evlh,
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

    pub fn in_use(&self, primary: Option<&DrmNode>) -> bool {
        Some(&self.render_node) == primary
            || !self.surfaces.is_empty()
            || !self.active_buffers.is_empty()
    }
}

fn create_output_for_conn(drm: &mut DrmDevice, conn: connector::Handle) -> Result<Output> {
    let conn_info = drm
        .get_connector(conn, false)
        .with_context(|| "Failed to query connector info")?;
    let interface = drm_helpers::interface_name(drm, conn)?;
    let edid_info = drm_helpers::edid_info(drm, conn);
    let (phys_w, phys_h) = conn_info.size().unwrap_or((0, 0));

    Ok(Output::new(
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
                .map(|info| info.manufacturer.clone())
                .unwrap_or_else(|_| String::from("Unknown")),
            model: edid_info
                .as_ref()
                .map(|info| info.model.clone())
                .unwrap_or_else(|_| String::from("Unknown")),
        },
    ))
}

fn populate_modes(
    drm: &mut DrmDevice,
    output: &Output,
    conn: connector::Handle,
    position: (u32, u32),
) -> Result<()> {
    let conn_info = drm.get_connector(conn, false)?;
    let max_bpc = drm_helpers::get_max_bpc(drm, conn)?.map(|(_val, range)| range.end.min(16));
    let Some(mode) = conn_info
        .modes()
        .iter()
        .find(|mode| mode.mode_type().contains(ModeTypeFlags::PREFERRED))
        .copied()
        .or(conn_info.modes().get(0).copied())
    else {
        anyhow::bail!("No mode found");
    };
    let scale = conn_info
        .size()
        .map(|size| calculate_scale(conn_info.interface(), size, mode.size()))
        .unwrap_or(1.0);

    let refresh_rate = drm_helpers::calculate_refresh_rate(mode);
    let output_mode = OutputMode {
        size: (mode.size().0 as i32, mode.size().1 as i32).into(),
        refresh: refresh_rate as i32,
    };

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
        ..std::mem::take(&mut *output_config)
    };

    Ok(())
}

pub fn calculate_scale(
    interface: connector::Interface,
    monitor_size_mm: (u32, u32),
    resolution: (u16, u16),
) -> f64 {
    let (w_mm, h_mm) = monitor_size_mm;
    let (w_px, h_px) = resolution;
    if w_mm == 0 || h_mm == 0 {
        // possibly projector, but could just be some no-brand display
        return 1.0;
    }

    let (w_in, h_in) = (w_mm as f64 / 25.4, h_mm as f64 / 25.4);
    // due to edid's setting non-sensicle values,
    // lets be really careful we don't devide by 0 (or non-normal values) when deriving values from size.
    // (also the size should be positive, but the u16 arguments already force that.)
    if !w_in.is_normal() || !h_in.is_normal() {
        return 1.0;
    }

    let diag = (w_in.powf(2.) * h_in.powf(2.)).sqrt();
    let dpi = (w_px as f64 / w_in + h_px as f64 / h_in) / 2.0;

    match diag {
        _diag if diag < 20. || interface == connector::Interface::EmbeddedDisplayPort => {
            // likely laptop
            scale_from_dpi(dpi, 144, w_px)
        }
        _diag
            if diag >= 40. && h_in >= 19. // 16:10 has a height of 19.6, anything lower is most likely an ultrawide
                && (interface == connector::Interface::HDMIA
                    || interface == connector::Interface::HDMIB) =>
        {
            // likely TV
            match w_px {
                x if x <= 720 => 1.0,
                _ => 2.0,
            }
        }
        _ => {
            // likely desktop
            scale_from_dpi(dpi, 120, w_px)
        }
    }
}

pub fn scale_from_dpi(dpi: f64, step_size: u32, width_px: u16) -> f64 {
    let scale = (dpi / step_size as f64) * 100.0;

    // snap to 25%
    let scale = (scale.round() as u32)
        .next_multiple_of(25)
        .saturating_sub(25) as f64
        / 100.0;

    // max values
    let max = match width_px {
        w if w <= 1080 => 1.0, // 125% is usually a worse experience than 100% atm
        // for 1440p and weird variants we let the algorithm take over, but limit the max, this is inspired by what windows does
        w if w <= 1440 => 1.5,
        w if w <= 1600 => 1.75,
        _ => 2.0, // never go higher than 200% by default though (Note for the future: Tablets? Phones?)
    };

    let min = match width_px {
        w if w >= 2160 => 2.0, // 4k should default to 200%, as we prefer integer scales
        _ => 1.0,              // never go lower than 100%
    };

    scale.min(max).max(min)
}
