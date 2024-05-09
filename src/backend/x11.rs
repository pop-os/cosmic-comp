// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::OutputConfig,
    shell::{Devices, SeatExt},
    state::{BackendData, Common},
    utils::prelude::*,
};
use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::{
            dmabuf::{Dmabuf, DmabufAllocator},
            gbm::{GbmAllocator, GbmBufferFlags},
            vulkan::{ImageUsageFlags, VulkanAllocator},
        },
        drm::{DrmDeviceFd, DrmNode},
        egl::{EGLContext, EGLDevice, EGLDisplay},
        input::{Event, InputEvent},
        renderer::{
            damage::{OutputDamageTracker, RenderOutputResult},
            gles::GlesRenderbuffer,
            glow::GlowRenderer,
            Bind, ImportDma, ImportEgl,
        },
        vulkan::{version::Version, Instance, PhysicalDevice},
        x11::{Window, WindowBuilder, X11Backend, X11Event, X11Handle, X11Input, X11Surface},
    },
    desktop::layer_map_for_output,
    output::{Mode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{ping, EventLoop, LoopHandle},
        gbm::Device as GbmDevice,
        wayland_protocols::wp::presentation_time::server::wp_presentation_feedback,
        wayland_server::DisplayHandle,
    },
    utils::{DeviceFd, Transform},
    wayland::dmabuf::DmabufFeedbackBuilder,
};
use std::{cell::RefCell, os::unix::io::OwnedFd, time::Duration};
use tracing::{debug, error, info, warn};

#[cfg(feature = "debug")]
use crate::state::Fps;

use super::render::init_shaders;

#[derive(Debug)]
enum Allocator {
    Gbm(GbmAllocator<DrmDeviceFd>),
    Vulkan(PhysicalDevice),
}

#[derive(Debug)]
pub struct X11State {
    allocator: Allocator,
    _egl: EGLDisplay,
    pub renderer: GlowRenderer,
    surfaces: Vec<Surface>,
    handle: X11Handle,
}

impl X11State {
    pub fn add_window(&mut self, handle: LoopHandle<'_, State>) -> Result<Output> {
        let window = WindowBuilder::new()
            .title("COSMIC")
            .build(&self.handle)
            .with_context(|| "Failed to create window")?;
        let fourcc = window.format();
        let modifiers = Bind::<Dmabuf>::supported_formats(&self.renderer).unwrap();
        let filtered_modifiers = modifiers
            .iter()
            .filter(|format| format.code == fourcc)
            .map(|format| format.modifier);
        let surface = match &self.allocator {
            Allocator::Gbm(gbm) => self
                .handle
                .create_surface(&window, DmabufAllocator(gbm.clone()), filtered_modifiers)
                .with_context(|| "Failed to create surface")?,
            Allocator::Vulkan(vulkan) => self
                .handle
                .create_surface(
                    &window,
                    DmabufAllocator(
                        VulkanAllocator::new(vulkan, ImageUsageFlags::COLOR_ATTACHMENT)
                            .with_context(|| "Failed to create vulkan allocator for window")?,
                    ),
                    filtered_modifiers,
                )
                .with_context(|| "Failed to create surface")?,
        };

        let name = format!("X11-{}", self.surfaces.len());
        let size = window.size();
        let props = PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "COSMIC".to_string(),
            model: name.clone(),
        };
        let mode = Mode {
            size: (size.w as i32, size.h as i32).into(),
            refresh: 60_000,
        };
        let output = Output::new(name, props);
        output.add_mode(mode);
        output.set_preferred(mode);
        output.change_current_state(
            Some(mode),
            Some(Transform::Normal),
            Some(Scale::Integer(1)),
            Some((0, 0).into()),
        );
        output.user_data().insert_if_missing(|| {
            RefCell::new(OutputConfig {
                mode: ((size.w as i32, size.h as i32), None),
                ..Default::default()
            })
        });

        let output_ref = output.clone();
        let (ping, source) =
            ping::make_ping().with_context(|| "Failed to create output event loop source")?;
        let _token = handle
            .insert_source(source, move |_, _, state| {
                let x11_state = state.backend.x11();
                if let Some(surface) = x11_state
                    .surfaces
                    .iter_mut()
                    .find(|s| s.output == output_ref)
                {
                    if let Err(err) =
                        surface.render_output(&mut x11_state.renderer, &mut state.common)
                    {
                        error!(?err, "Error rendering.");
                    }
                    surface.dirty = false;
                    surface.pending = true;
                }
            })
            .with_context(|| "Failed to add output to event loop")?;

        self.surfaces.push(Surface {
            window,
            surface,
            damage_tracker: OutputDamageTracker::from_output(&output),
            output: output.clone(),
            render: ping.clone(),
            dirty: false,
            pending: true,
            #[cfg(feature = "debug")]
            fps: Fps::new(&mut self.renderer),
        });

        // schedule first render
        ping.ping();
        Ok(output)
    }

    pub fn schedule_render(&mut self, output: &Output) {
        if let Some(surface) = self.surfaces.iter_mut().find(|s| s.output == *output) {
            surface.dirty = true;
            if !surface.pending {
                surface.render.ping();
            }
        }
    }

    pub fn apply_config_for_output(
        &mut self,
        output: &Output,
        test_only: bool,
    ) -> Result<(), anyhow::Error> {
        // TODO: if we ever have multiple winit outputs, don't ignore config.enabled
        // reset size
        let size = self
            .surfaces
            .iter()
            .find(|s| s.output == *output)
            .unwrap()
            .window
            .size();
        let mut config = output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow_mut();
        if config.mode.0 != (size.w as i32, size.h as i32) {
            if !test_only {
                config.mode = ((size.w as i32, size.h as i32), None);
            }
            Err(anyhow::anyhow!("Cannot set window size"))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Surface {
    window: Window,
    damage_tracker: OutputDamageTracker,
    surface: X11Surface,
    output: Output,
    render: ping::Ping,
    dirty: bool,
    pending: bool,
    #[cfg(feature = "debug")]
    fps: Fps,
}

impl Surface {
    pub fn render_output(&mut self, renderer: &mut GlowRenderer, state: &mut Common) -> Result<()> {
        let (buffer, age) = self
            .surface
            .buffer()
            .with_context(|| "Failed to allocate buffer")?;
        match render::render_output::<_, _, GlesRenderbuffer>(
            None,
            renderer,
            buffer.clone(),
            &mut self.damage_tracker,
            age as usize,
            &*state.shell.read().unwrap(),
            &state.config,
            &state.theme,
            state.clock.now(),
            &self.output,
            render::CursorMode::NotDefault,
            #[cfg(not(feature = "debug"))]
            None,
            #[cfg(feature = "debug")]
            Some(&mut self.fps),
        ) {
            Ok(RenderOutputResult { damage, states, .. }) => {
                self.surface
                    .submit()
                    .with_context(|| "Failed to submit buffer for display")?;
                #[cfg(feature = "debug")]
                self.fps.displayed();
                state.send_frames(&self.output, &states, |_| None);
                if damage.is_some() {
                    let mut output_presentation_feedback =
                        state.take_presentation_feedback(&self.output, &states);
                    output_presentation_feedback.presented(
                        state.clock.now(),
                        self.output
                            .current_mode()
                            .map(|mode| Duration::from_secs_f64(1_000.0 / mode.refresh as f64))
                            .unwrap_or_default(),
                        0,
                        wp_presentation_feedback::Kind::Vsync,
                    )
                }
            }
            Err(err) => {
                self.surface.reset_buffers();
                anyhow::bail!("Rendering failed: {}", err);
            }
        };

        Ok(())
    }
}

fn try_vulkan_allocator(node: &DrmNode) -> Option<Allocator> {
    let instance = match Instance::new(Version::VERSION_1_2, None) {
        Ok(instance) => instance,
        Err(err) => {
            warn!(
                ?err,
                "Failed to instanciate vulkan, falling back to gbm allocator.",
            );
            return None;
        }
    };

    let devices = match PhysicalDevice::enumerate(&instance) {
        Ok(devices) => devices,
        Err(err) => {
            debug!(?err, "No vulkan devices, falling back to gbm.");
            return None;
        }
    };

    let Some(device) = devices
        .filter(|phd| {
            phd.has_device_extension(
                smithay::reexports::ash::extensions::ext::PhysicalDeviceDrm::name(),
            )
        })
        .find(|phd| {
            phd.primary_node().unwrap() == Some(*node) || phd.render_node().unwrap() == Some(*node)
        })
    else {
        debug!(?node, "No vulkan device for node, falling back to gbm.");
        return None;
    };

    Some(Allocator::Vulkan(device))
}

fn try_gbm_allocator(fd: OwnedFd) -> Option<Allocator> {
    // Create the gbm device for buffer allocation.
    let device = match GbmDevice::new(DrmDeviceFd::new(DeviceFd::from(fd))) {
        Ok(gbm) => gbm,
        Err(err) => {
            error!(?err, "Failed to create GBM device.");
            return None;
        }
    };

    Some(Allocator::Gbm(GbmAllocator::new(
        device,
        GbmBufferFlags::RENDERING,
    )))
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<State>,
    state: &mut State,
) -> Result<()> {
    let backend = X11Backend::new().with_context(|| "Failed to initilize X11 backend")?;
    let handle = backend.handle();

    // Obtain the DRM node the X server uses for direct rendering.
    let (drm_node, fd) = handle
        .drm_node()
        .with_context(|| "Could not get DRM node used by X server")?;

    let device = EGLDevice::enumerate()
        .with_context(|| "Failed to enumerate EGL devices")?
        .find(|device| device.try_get_render_node().ok().flatten() == Some(drm_node))
        .with_context(|| format!("Failed to find EGLDevice for node {}", drm_node))?;
    // Initialize EGL
    let egl = unsafe { EGLDisplay::new(device) }.with_context(|| "Failed to create EGL display")?;
    // Create the OpenGL context
    let context = EGLContext::new(&egl).with_context(|| "Failed to create EGL context")?;
    // Create a renderer
    let mut renderer =
        unsafe { GlowRenderer::new(context) }.with_context(|| "Failed to initialize renderer")?;

    init_shaders(&mut renderer).context("Failed to initialize renderer")?;
    init_egl_client_side(dh, state, &drm_node, &mut renderer)?;

    state.backend = BackendData::X11(X11State {
        handle,
        allocator: try_vulkan_allocator(&drm_node)
            .or_else(|| try_gbm_allocator(fd))
            .context("Failed to create allocator for x11")?,
        _egl: egl,
        renderer,
        surfaces: Vec::new(),
    });

    let output = state
        .backend
        .x11()
        .add_window(event_loop.handle())
        .with_context(|| "Failed to create wl_output")?;
    state
        .common
        .output_configuration_state
        .add_heads(std::iter::once(&output));
    {
        state.common.add_output(&output);
        let mut shell = state.common.shell.write().unwrap();
        state.common.config.read_outputs(
            &mut state.common.output_configuration_state,
            &mut state.backend,
            &mut *shell,
            &state.common.event_loop_handle,
            &mut state.common.workspace_state.update(),
            &state.common.xdg_activation_state,
        );

        std::mem::drop(shell);
        state.common.refresh();
    }
    state.launch_xwayland(None);

    event_loop
        .handle()
        .insert_source(backend, move |event, _, state| match event {
            X11Event::CloseRequested { window_id } => {
                // TODO: drain_filter
                let mut outputs_removed = Vec::new();
                for surface in state
                    .backend
                    .x11()
                    .surfaces
                    .iter()
                    .filter(|s| s.window.id() == window_id)
                {
                    surface.window.unmap();
                    outputs_removed.push(surface.output.clone());
                }
                state
                    .backend
                    .x11()
                    .surfaces
                    .retain(|s| s.window.id() != window_id);
                for output in outputs_removed.into_iter() {
                    state.common.remove_output(&output);
                }
            }
            X11Event::Resized {
                new_size,
                window_id,
            } => {
                let size = { (new_size.w as i32, new_size.h as i32).into() };
                let mode = Mode {
                    size,
                    refresh: 60_000,
                };
                if let Some(surface) = state
                    .backend
                    .x11()
                    .surfaces
                    .iter_mut()
                    .find(|s| s.window.id() == window_id)
                {
                    let output = &surface.output;
                    {
                        let mut config = output
                            .user_data()
                            .get::<RefCell<OutputConfig>>()
                            .unwrap()
                            .borrow_mut();
                        config.mode.0 = size.into();
                    }

                    output.delete_mode(output.current_mode().unwrap());
                    output.change_current_state(Some(mode), None, None, None);
                    output.set_preferred(mode);
                    layer_map_for_output(output).arrange();
                    state.common.output_configuration_state.update();
                    surface.dirty = true;
                    if !surface.pending {
                        surface.render.ping();
                    }
                }
            }
            X11Event::Refresh { window_id } | X11Event::PresentCompleted { window_id } => {
                if let Some(surface) = state
                    .backend
                    .x11()
                    .surfaces
                    .iter_mut()
                    .find(|s| s.window.id() == window_id)
                {
                    if surface.dirty {
                        surface.render.ping();
                    } else {
                        surface.pending = false;
                    }
                }
            }
            X11Event::Input {
                event,
                window_id: _,
            } => state.process_x11_event(event),
            X11Event::Focus { .. } => {} // TODO: release all keys when losing focus and make sure to go through our keyboard filter code
        })
        .map_err(|_| anyhow::anyhow!("Failed to insert X11 Backend into event loop"))?;

    Ok(())
}

fn init_egl_client_side<R>(
    dh: &DisplayHandle,
    state: &mut State,
    render_node: &DrmNode,
    renderer: &mut R,
) -> Result<()>
where
    R: ImportEgl + ImportDma,
{
    if let Err(err) = renderer.bind_wl_display(dh) {
        warn!(
            ?err,
            "Unable to initialize bind display to EGL. Some older clients may not work correctly."
        )
    }

    let default_feedback =
        DmabufFeedbackBuilder::new(render_node.dev_id(), renderer.dmabuf_formats())
            .build()
            .unwrap();
    state
        .common
        .dmabuf_state
        .create_global_with_default_feedback::<State>(dh, &default_feedback);

    info!("EGL hardware-acceleration enabled.");

    Ok(())
}

impl State {
    pub fn process_x11_event(&mut self, event: InputEvent<X11Input>) {
        // here we can handle special cases for x11 inputs, like mapping them to windows
        match &event {
            InputEvent::PointerMotionAbsolute { event } => {
                if let Some(window) = event.window() {
                    let output = self
                        .backend
                        .x11()
                        .surfaces
                        .iter()
                        .find(|surface| &surface.window == window.as_ref())
                        .map(|surface| surface.output.clone())
                        .unwrap();

                    let device = event.device();
                    for seat in self.common.shell.read().unwrap().seats.iter() {
                        let devices = seat.user_data().get::<Devices>().unwrap();
                        if devices.has_device(&device) {
                            seat.set_active_output(&output);
                            break;
                        }
                    }
                }
            }
            _ => {}
        };

        self.process_input_event(event, false);
        // TODO actually figure out the output
        for output in self.common.shell.read().unwrap().outputs() {
            self.backend.x11().schedule_render(output);
        }
    }
}
