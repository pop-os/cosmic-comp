// SPDX-License-Identifier: GPL-3.0-only

use crate::{config::OutputState, shell::Shell, state::BackendData, utils::prelude::*};

use anyhow::{Context, Result};
use calloop::LoopSignal;
use render::gles::GbmGlowBackend;
use smithay::{
    backend::{
        allocator::{
            dmabuf::Dmabuf,
            gbm::{GbmAllocator, GbmBufferFlags},
        },
        drm::{DrmDeviceFd, DrmNode, NodeType},
        egl::{context::ContextPriority, EGLContext, EGLDevice, EGLDisplay},
        input::InputEvent,
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{glow::GlowRenderer, multigpu::GpuManager},
        session::{libseat::LibSeatSession, Event as SessionEvent, Session},
        udev::{all_gpus, primary_gpu, UdevBackend, UdevEvent},
    },
    output::Output,
    reexports::{
        calloop::{Dispatcher, EventLoop, LoopHandle},
        drm::control::{crtc, Device as _},
        input::{self, Libinput},
        wayland_server::{Client, DisplayHandle},
    },
    utils::{DevPath, Size},
    wayland::{
        dmabuf::DmabufGlobal, drm_syncobj::{DrmSyncobjState, supports_syncobj_eventfd},
        relative_pointer::RelativePointerManagerState,
    },
};
use tracing::{error, info, trace, warn};

use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
    path::Path,
    sync::{atomic::AtomicBool, Arc, RwLock},
};

mod device;
mod drm_helpers;
pub mod render;
mod socket;
mod surface;

use device::*;
pub use surface::Timings;

use super::render::init_shaders;

#[derive(Debug)]
pub struct KmsState {
    pub drm_devices: HashMap<DrmNode, Device>,
    pub input_devices: HashMap<String, input::Device>,
    pub primary_node: Option<DrmNode>,
    // Mesa llvmpipe renderer, if supported and there are no render nodes
    pub software_renderer: Option<GlowRenderer>,
    pub api: GpuManager<GbmGlowBackend<DrmDeviceFd>>,

    session: LibSeatSession,
    libinput: Libinput,

    pub syncobj_state: Option<DrmSyncobjState>,
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

    // get our primary gpu
    let primary = determine_primary_gpu(session.seat());
    if let Some(primary) = primary.as_ref() {
        info!("Using {} as primary gpu for rendering.", primary);
    }

    let software_renderer = if primary.is_none() {
        match software_renderer() {
            Ok(renderer) => Some(renderer),
            Err(err) => {
                error!(?err, "Failed to initialize software EGL renderer.");
                None
            }
        }
    } else {
        None
    };

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
        drm_devices: HashMap::new(),
        input_devices: HashMap::new(),
        primary_node: primary,
        software_renderer,
        api: GpuManager::new(GbmGlowBackend::new()).context("Failed to initialize gpu backend")?,

        session,
        libinput: libinput_context,

        syncobj_state: None,
    });

    // start x11
    state.launch_xwayland(primary);

    // manually add already present gpus
    for (dev, path) in udev_dispatcher.as_source_ref().device_list() {
        if let Err(err) = state.device_added(dev, path.into(), dh) {
            warn!("Failed to add device {}: {:?}", path.display(), err);
        }
    }

    // TODO check if main device supports syncobj eventfd?
    let kms = match &mut state.backend {
        BackendData::Kms(kms) => kms,
        _ => unreachable!(),
    };
    if let Some(primary_node) = kms
        .primary_node
        .and_then(|node| node.node_with_type(NodeType::Primary).and_then(|x| x.ok()))
    {
        if let Some(device) = kms.drm_devices.get(&primary_node) {
            let import_device = device.drm.device_fd().clone();
            if supports_syncobj_eventfd(&import_device) {
                let syncobj_state = DrmSyncobjState::new::<State>(&dh, import_device);
                kms.syncobj_state = Some(syncobj_state);
            }
        }
    }

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
        if let InputEvent::DeviceAdded { ref mut device } = &mut event {
            state.common.config.read_device(device);
            state
                .backend
                .kms()
                .input_devices
                .insert(device.name().into(), device.clone());
        } else if let InputEvent::DeviceRemoved { device } = &event {
            state.backend.kms().input_devices.remove(device.name());
        }

        state.process_input_event(event, true);

        for output in state.common.shell.read().unwrap().outputs() {
            state.backend.kms().schedule_render(output);
        }
    })
    .map_err(|err| err.error)
    .context("Failed to initialize libinput event source")?;

    // Create relative pointer global
    RelativePointerManagerState::new::<State>(&dh);

    Ok(libinput_context)
}

fn determine_primary_gpu(seat: String) -> Option<DrmNode> {
    if let Some(node) = std::env::var("COSMIC_RENDER_DEVICE")
        .ok()
        .and_then(|x| DrmNode::from_path(x).ok())
    {
        Some(node)
    } else {
        let primary_node = primary_gpu(&seat)
            .ok()
            .flatten()
            .and_then(|x| DrmNode::from_path(x).ok());
        primary_node
            .and_then(|x| x.node_with_type(NodeType::Render).and_then(Result::ok))
            .or_else(|| {
                for dev in all_gpus(&seat).expect("Failed to query gpus") {
                    if let Some(node) = DrmNode::from_path(dev)
                        .ok()
                        .and_then(|x| x.node_with_type(NodeType::Render).and_then(Result::ok))
                    {
                        return Some(node);
                    }
                }

                None
            })
    }
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
            UdevEvent::Added { device_id, path } => state
                .device_added(device_id, path, &dh)
                .with_context(|| format!("Failed to add drm device: {}", device_id)),
            UdevEvent::Changed { device_id } => state
                .device_changed(device_id)
                .with_context(|| format!("Failed to update drm device: {}", device_id)),
            UdevEvent::Removed { device_id } => state
                .device_removed(device_id, &dh)
                .with_context(|| format!("Failed to remove drm device: {}", device_id)),
        } {
            Ok(()) => {
                trace!("Successfully handled udev event.")
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
    fn resume_session(
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
            if let Err(err) = device.drm.activate(true) {
                error!(?err, "Failed to resume drm device");
            }
            if let Some(lease_state) = device.leasing_global.as_mut() {
                lease_state.resume::<State>();
            }
        }

        // update state and schedule new render,
        // after processing the rest of the pending event loop events
        let dispatcher = dispatcher.clone();
        loop_handle.insert_idle(move |state| {
            // add new devices, update devices now
            for (dev, path) in dispatcher.as_source_ref().device_list() {
                let drm_node = match DrmNode::from_dev_id(dev) {
                    Ok(node) => node,
                    Err(err) => {
                        error!(?err, "Failed to read drm device {}.", path.display(),);
                        continue;
                    }
                };
                if state.backend.kms().drm_devices.contains_key(&drm_node) {
                    if let Err(err) = state.device_changed(dev) {
                        error!(?err, "Failed to update drm device {}.", path.display(),);
                    }
                } else {
                    let dh = state.common.display_handle.clone();
                    if let Err(err) = state.device_added(dev, path.into(), &dh) {
                        error!(?err, "Failed to add drm device {}.", path.display(),);
                    }
                }
            }

            // update outputs
            state.common.config.read_outputs(
                &mut state.common.output_configuration_state,
                &mut state.backend,
                &state.common.shell,
                &state.common.event_loop_handle,
                &mut state.common.workspace_state.update(),
                &state.common.xdg_activation_state,
                state.common.startup_done.clone(),
            );
            state.common.refresh();
        });
        loop_signal.wakeup();
    }

    fn pause_session(&mut self) {
        let backend = self.backend.kms();
        backend.libinput.suspend();
        for device in backend.drm_devices.values_mut() {
            device.drm.pause();
            if let Some(lease_state) = device.leasing_global.as_mut() {
                lease_state.suspend();
            }
            for surface in device.surfaces.values_mut() {
                surface.suspend();
            }
        }
    }
}

impl KmsState {
    pub fn switch_vt(&mut self, num: i32) -> Result<(), anyhow::Error> {
        self.session.change_vt(num).map_err(Into::into)
    }

    pub fn dmabuf_imported(
        &mut self,
        _client: Option<Client>,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
    ) -> Result<DrmNode> {
        let (expected_node, other_nodes) =
            self.drm_devices
                .values_mut()
                .partition::<Vec<_>, _>(|device| {
                    device
                        .socket
                        .as_ref()
                        .map(|s| &s.dmabuf_global == global)
                        .unwrap_or(false)
                });

        let mut last_err = anyhow::anyhow!("Dmabuf cannot be imported on any gpu");
        for device in expected_node.into_iter().chain(other_nodes.into_iter()) {
            let mut _egl = None;
            let egl_display = if let Some(egl_display) =
                device.egl.as_ref().map(|internals| &internals.display)
            {
                egl_display
            } else {
                _egl = Some(init_egl(&device.gbm).context("Failed to initialize egl context")?);
                &_egl.as_ref().unwrap().display
            };

            let result = egl_display
                .create_image_from_dmabuf(&dmabuf)
                .map(|image| {
                    unsafe {
                        smithay::backend::egl::ffi::egl::DestroyImageKHR(
                            **egl_display.get_display_handle(),
                            image,
                        );
                    };
                    device.render_node
                })
                .map_err(Into::into);

            match result {
                Ok(node) => {
                    dmabuf.set_node(node); // so the MultiRenderer knows what node to use
                    return Ok(node);
                }
                Err(err) => {
                    trace!(?err, "Failed to import dmabuf on {:?}", device.render_node);
                    last_err = err;
                }
            }
        }

        Err(last_err)
    }

    pub fn schedule_render(&mut self, output: &Output) {
        for surface in self
            .drm_devices
            .values()
            .flat_map(|d| d.surfaces.values())
            .filter(|s| s.output == *output || s.output.mirroring().is_some_and(|o| &o == output))
        {
            surface.schedule_render();
        }
    }

    pub fn target_node_for_output(&self, output: &Output) -> Option<DrmNode> {
        self.drm_devices
            .values()
            .find(|dev| dev.surfaces.values().any(|s| s.output == *output))
            .map(|dev| &dev.render_node)
            .copied()
    }

    pub fn refresh_used_devices(&mut self) -> Result<()> {
        let mut used_devices = HashSet::new();

        for device in self.drm_devices.values_mut() {
            if device.in_use(self.primary_node.as_ref()) {
                if device.egl.is_none() {
                    let egl = init_egl(&device.gbm).context("Failed to create EGL context")?;
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
                    self.api.as_mut().add_node(
                        device.render_node,
                        GbmAllocator::new(
                            device.gbm.clone(),
                            // SCANOUT because stride bugs
                            GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
                        ),
                        renderer,
                    );
                    device.egl = Some(egl);
                }
                used_devices.insert(device.render_node);
            } else {
                if device.egl.is_some() {
                    let _ = device.egl.take();
                    self.api.as_mut().remove_node(&device.render_node);
                }
            }
        }

        // trigger re-evaluation... urgh
        if let Some(primary_node) = self.primary_node.as_ref() {
            let _ = self.api.single_renderer(primary_node);
        }

        // I hate this. I want partial borrows of hashmap values
        let all_devices = self
            .drm_devices
            .values()
            .map(|d| d.render_node)
            .collect::<Vec<_>>();
        for node in all_devices {
            let (mut device, mut others) = self
                .drm_devices
                .values_mut()
                .partition::<Vec<_>, _>(|d| d.render_node == node);
            let device = &mut device[0];

            for surface in device.surfaces.values_mut() {
                let known_nodes = surface.known_nodes().clone();
                for gone_device in known_nodes.difference(&used_devices) {
                    surface.remove_node(*gone_device);
                }
                for new_device in used_devices.difference(&known_nodes) {
                    let (render_node, egl, gbm) = if node == *new_device {
                        // we need to make sure to do partial borrows here, as device.surfaces is borrowed mutable
                        (
                            device.render_node,
                            device.egl.as_ref().unwrap(),
                            device.gbm.clone(),
                        )
                    } else {
                        let device = others
                            .iter_mut()
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
        }

        Ok(())
    }

    pub fn apply_config_for_outputs(
        &mut self,
        test_only: bool,
        loop_handle: &LoopHandle<'static, State>,
        shell: Arc<RwLock<Shell>>,
        startup_done: Arc<AtomicBool>,
    ) -> Result<Vec<Output>, anyhow::Error> {
        if !self.session.is_active() {
            return Ok(Vec::new());
        }

        let mut all_outputs = Vec::new();
        for device in self.drm_devices.values_mut() {
            // we only want outputs exposed to wayland - not leased ones
            // but that is also not all surface, because that doesn't contain all detected, but unmapped outputs
            let outputs = device
                .outputs
                .iter()
                .filter(|(conn, _)| {
                    !device
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

            let res_handles = device.drm.resource_handles()?;
            let free_crtcs = res_handles
                .crtcs()
                .iter()
                .filter(|crtc| {
                    !device
                        .surfaces
                        .get(crtc)
                        .is_some_and(|surface| surface.output.is_enabled())
                })
                .copied()
                .collect::<HashSet<crtc::Handle>>();
            let open_conns = outputs
                .iter()
                .filter(|output| {
                    output.is_enabled() && !device.surfaces.values().any(|s| &s.output == *output)
                })
                .flat_map(|output| {
                    device
                        .outputs
                        .iter()
                        .find_map(|(conn, o)| (output == o).then_some(*conn))
                });

            for conn in open_conns {
                let conn_info = device.drm.get_connector(conn, false).with_context(|| {
                    format!(
                        "Failed to query drm device: {:?}",
                        device.drm.dev_path().as_deref().map(Path::display),
                    )
                })?;
                'outer: for encoder_info in conn_info
                    .encoders()
                    .iter()
                    .flat_map(|encoder_handle| device.drm.get_encoder(*encoder_handle))
                {
                    for crtc in res_handles.filter_crtcs(encoder_info.possible_crtcs()) {
                        if !free_crtcs.contains(&crtc) {
                            new_pairings.insert(conn, crtc);
                            break 'outer;
                        }
                    }

                    // test failed, we don't have a crtc for conn
                    anyhow::bail!("Missing crtc for {conn:?}, gpu doesn't have enough resources.");
                }
            }

            // first drop old surfaces
            if !test_only {
                for output in outputs.iter().filter(|o| !o.is_enabled()) {
                    device
                        .surfaces
                        .retain(|_, surface| surface.output != *output);
                }
            }

            // reconfigure existing
            for (crtc, surface) in device.surfaces.iter_mut() {
                let output_config = surface.output.config();

                let drm = &mut device.drm;
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
                    .ok_or(anyhow::anyhow!("Unable to find matching mode"))?;

                if !test_only {
                    if !surface.is_active() {
                        let drm_surface = drm
                            .create_surface(*crtc, *mode, &[conn])
                            .with_context(|| "Failed to create drm surface")?;
                        let gbm = device.gbm.clone();
                        let cursor_size = drm.cursor_size();

                        let vrr = drm_helpers::set_vrr(drm, *crtc, conn, output_config.vrr)
                            .unwrap_or(false);
                        surface.output.set_adaptive_sync(vrr);

                        if let Some(bpc) = output_config.max_bpc {
                            if let Err(err) = drm_helpers::set_max_bpc(drm, conn, bpc) {
                                warn!(
                                    ?bpc,
                                    ?err,
                                    "Failed to set max_bpc on connector: {}",
                                    surface.output.name()
                                );
                            }
                        }

                        std::mem::drop(output_config);
                        surface
                            .resume(drm_surface, gbm, cursor_size, vrr)
                            .context("Failed to create surface")?;
                    } else {
                        if output_config.vrr != surface.output.adaptive_sync() {
                            surface.output.set_adaptive_sync(drm_helpers::set_vrr(
                                drm,
                                surface.crtc,
                                surface.connector,
                                output_config.vrr,
                            )?);
                        }
                        std::mem::drop(output_config);
                        surface
                            .set_mode(*mode)
                            .context("Failed to apply new mode")?;
                    }
                }
            }

            // add new ones
            let mut w = shell.read().unwrap().global_space().size.w as u32;
            if !test_only {
                for (conn, crtc) in new_pairings {
                    let (output, _) = device.connector_added(
                        self.primary_node.as_ref(),
                        conn,
                        Some(crtc),
                        (0, w),
                        loop_handle,
                        shell.clone(),
                        startup_done.clone(),
                    )?;
                    if output.mirroring().is_none() {
                        w += output.config().mode_size().w as u32;
                    }
                    all_outputs.push(output);
                }
            }

            all_outputs.extend(outputs);
        }

        // we need to handle mirroring, after all outputs have been enabled
        for device in self.drm_devices.values_mut() {
            for surface in device.surfaces.values_mut() {
                let mirrored_output =
                    if let OutputState::Mirroring(conn) = &surface.output.config().enabled {
                        Some(
                            all_outputs
                                .iter()
                                .find(|output| &output.name() == conn)
                                .cloned()
                                .ok_or(anyhow::anyhow!("Unable to find mirroring output"))?,
                        )
                    } else {
                        None
                    };

                if !test_only {
                    if mirrored_output != surface.output.mirroring() {
                        surface.set_mirroring(mirrored_output.clone());
                    }
                }
            }
        }

        Ok(all_outputs)
    }
}
