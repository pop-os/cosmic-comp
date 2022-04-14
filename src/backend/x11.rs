// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::OutputConfig,
    input::{set_active_output, Devices},
    state::{BackendData, Common, State},
};
use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        egl::{EGLContext, EGLDisplay},
        input::{Event, InputEvent},
        renderer::{gles2::Gles2Renderer, Bind, ImportDma, ImportEgl},
        x11::{Window, WindowBuilder, X11Backend, X11Event, X11Handle, X11Input, X11Surface},
    },
    desktop::layer_map_for_output,
    reexports::{
        calloop::{ping, EventLoop, LoopHandle},
        gbm::{Device as GbmDevice, FdWrapper},
        wayland_server::{protocol::wl_output::Subpixel, Display},
    },
    wayland::{
        dmabuf::init_dmabuf_global,
        output::{Mode, Output, PhysicalProperties},
    },
};
use std::{
    cell::RefCell,
    rc::Rc,
    sync::{Arc, Mutex},
};

#[cfg(feature = "debug")]
use crate::state::Fps;

pub struct X11State {
    allocator: Arc<Mutex<GbmDevice<FdWrapper>>>,
    _egl: EGLDisplay,
    renderer: Rc<RefCell<Gles2Renderer>>,
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
        let surface = self
            .handle
            .create_surface(
                &window,
                self.allocator.clone(),
                Bind::<Dmabuf>::supported_formats(&*self.renderer.borrow())
                    .unwrap()
                    .iter()
                    .filter(|format| format.code == fourcc)
                    .map(|format| format.modifier),
            )
            .with_context(|| "Failed to create surface")?;

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
        let output = Output::new(name, props, None);
        output.change_current_state(Some(mode), None, None, Some((0, 0).into()));
        output.set_preferred(mode);
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
                    if let Err(err) = surface
                        .render_output(&mut *x11_state.renderer.borrow_mut(), &mut state.common)
                    {
                        slog_scope::error!("Error rendering: {}", err);
                    }
                    surface.dirty = false;
                    surface.pending = true;
                }
            })
            .with_context(|| "Failed to add output to event loop")?;

        self.surfaces.push(Surface {
            window,
            surface,
            output: output.clone(),
            render: ping.clone(),
            dirty: false,
            pending: true,
            #[cfg(feature = "debug")]
            fps: Fps::default(),
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

pub struct Surface {
    window: Window,
    surface: X11Surface,
    output: Output,
    render: ping::Ping,
    dirty: bool,
    pending: bool,
    #[cfg(feature = "debug")]
    fps: Fps,
}

impl Surface {
    pub fn render_output(
        &mut self,
        renderer: &mut Gles2Renderer,
        state: &mut Common,
    ) -> Result<()> {
        let (buffer, age) = self
            .surface
            .buffer()
            .with_context(|| "Failed to allocate buffer")?;
        renderer
            .bind(buffer)
            .with_context(|| "Failed to bind buffer")?;

        match render::render_output(
            None,
            renderer,
            age as u8,
            state,
            &self.output,
            true,
            #[cfg(feature = "debug")]
            &mut self.fps,
        ) {
            Ok(_) => {
                state
                    .shell
                    .active_space_mut(&self.output)
                    .space
                    .send_frames(state.start_time.elapsed().as_millis() as u32);
                self.surface
                    .submit()
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

pub fn init_backend(event_loop: &mut EventLoop<State>, state: &mut State) -> Result<()> {
    let backend = X11Backend::new(None).with_context(|| "Failed to initilize X11 backend")?;
    let handle = backend.handle();

    // Obtain the DRM node the X server uses for direct rendering.
    let (_drm_node, fd) = handle
        .drm_node()
        .with_context(|| "Could not get DRM node used by X server")?;

    // Create the gbm device for buffer allocation.
    let device =
        unsafe { GbmDevice::new_from_fd(fd) }.with_context(|| "Failed to create GBM device")?;
    // Initialize EGL using the GBM device.
    let egl = EGLDisplay::new(&device, None).with_context(|| "Failed to create EGL display")?;
    // Create the OpenGL context
    let context = EGLContext::new(&egl, None).with_context(|| "Failed to create EGL context")?;
    // Create a renderer
    let renderer = Rc::new(RefCell::new(
        unsafe { Gles2Renderer::new(context, None) }
            .with_context(|| "Failed to initialize renderer")?,
    ));

    init_egl_client_side(&mut *state.common.display.borrow_mut(), renderer.clone())?;

    state.backend = BackendData::X11(X11State {
        handle,
        allocator: Arc::new(Mutex::new(device)),
        _egl: egl,
        renderer,
        surfaces: Vec::new(),
    });

    let output = state
        .backend
        .x11()
        .add_window(event_loop.handle())
        .with_context(|| "Failed to create wl_output")?;
    state.common.output_conf.add_heads(std::iter::once(&output));
    state
        .common
        .output_conf
        .update(&mut *state.common.display.borrow_mut());
    state
        .common
        .shell
        .add_output(&output);
    state.common.config.read_outputs(std::iter::once(&output), &mut state.backend, &mut state.common.shell);
    state.common.shell.refresh_outputs();
    state.common.config.write_outputs(std::iter::once(&output));



    event_loop
        .handle()
        .insert_source(backend, |event, _, state| match event {
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
                    state.common.shell.remove_output(
                        &output,
                    );
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
                    state
                        .common
                        .output_conf
                        .update(&mut *state.common.display.borrow_mut());
                    state.common.shell.refresh_outputs();
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
            X11Event::Input(event) => state.process_x11_event(event),
        })
        .map_err(|_| anyhow::anyhow!("Failed to insert X11 Backend into event loop"))?;

    Ok(())
}

fn init_egl_client_side(display: &mut Display, renderer: Rc<RefCell<Gles2Renderer>>) -> Result<()> {
    let bind_result = renderer.borrow_mut().bind_wl_display(display);
    match bind_result {
        Ok(_) => {
            slog_scope::info!("EGL hardware-acceleration enabled");
            let dmabuf_formats = renderer
                .borrow()
                .dmabuf_formats()
                .cloned()
                .collect::<Vec<_>>();
            init_dmabuf_global(
                display,
                dmabuf_formats,
                move |buffer, _| renderer.borrow_mut().import_dmabuf(buffer, None).is_ok(),
                None,
            );
        }
        Err(err) => slog_scope::warn!("Unable to initialize bind display to EGL: {}", err),
    };

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
                    for seat in self.common.seats.clone().iter() {
                        let devices = seat.user_data().get::<Devices>().unwrap();
                        if devices.has_device(&device) {
                            set_active_output(seat, &output);
                            break;
                        }
                    }
                }
            }
            _ => {}
        };

        self.common.process_input_event(event);
        // TODO actually figure out the output
        for output in self.common.shell.outputs() {
            self.backend.schedule_render(output);
        }
    }
}
