// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    state::{BackendData, State},
    utils::GlobalDrop,
};
use anyhow::{Context, Result};
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        egl::{EGLContext, EGLDisplay},
        renderer::{gles2::Gles2Renderer, Bind, ImportDma, ImportEgl, Unbind},
        x11::{Window, WindowBuilder, X11Backend, X11Event, X11Handle, X11Surface},
    },
    desktop::Space,
    reexports::{
        calloop::{ping, EventLoop, LoopHandle},
        gbm::Device as GbmDevice,
        wayland_server::{
            protocol::wl_output::{Subpixel, WlOutput},
            Display,
        },
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

pub struct X11State {
    handle: X11Handle,
    allocator: Arc<Mutex<GbmDevice<DrmNode>>>,
    _egl: EGLDisplay,
    renderer: Rc<RefCell<Gles2Renderer>>,
    surfaces: Vec<Surface>,
}

impl X11State {
    pub fn add_window(
        &mut self,
        display: &mut Display,
        handle: LoopHandle<'_, State>,
    ) -> Result<Output> {
        let window = WindowBuilder::new()
            .title("COSMIC")
            .build(&self.handle)
            .with_context(|| "Failed to create window")?;
        let fourcc = window.format().unwrap();
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
        let (output, global) = Output::new(display, name, props, None);
        let _global = global.into();
        output.change_current_state(Some(mode), None, None, Some((0, 0).into()));
        output.set_preferred(mode);

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
                        .render_from_space(&mut *x11_state.renderer.borrow_mut(), &mut state.spaces)
                    {
                        slog_scope::error!("Error rendering: {}", err);
                    }
                }
            })
            .with_context(|| "Failed to add output to event loop")?;

        self.surfaces.push(Surface {
            window,
            surface,
            output: output.clone(),
            render: ping.clone(),
            _global,
        });

        // schedule first render
        ping.ping();
        Ok(output)
    }
}

pub struct Surface {
    window: Window,
    surface: X11Surface,
    output: Output,
    render: ping::Ping,
    _global: GlobalDrop<WlOutput>,
}

impl Surface {
    pub fn render_from_space(
        &mut self,
        renderer: &mut Gles2Renderer,
        space: &mut Space,
    ) -> Result<()> {
        let (buffer, age) = self
            .surface
            .buffer()
            .with_context(|| "Failed to allocate buffer")?;
        renderer
            .bind(buffer)
            .with_context(|| "Failed to bind buffer")?;
        match space.render_output(
            renderer,
            &self.output,
            age as usize,
            [0.153, 0.161, 0.165, 1.0],
        ) {
            Ok(true) => {
                slog_scope::trace!("Finished rendering");
                self.surface
                    .submit()
                    .with_context(|| "Failed to submit buffer for display")?;
            }
            Ok(false) => {
                let _ = renderer.unbind();
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
    let drm_node = handle
        .drm_node()
        .with_context(|| "Could not get DRM node used by X server")?;

    // Create the gbm device for buffer allocation.
    let device = GbmDevice::new(drm_node).with_context(|| "Failed to create GBM device")?;
    // Initialize EGL using the GBM device.
    let egl = EGLDisplay::new(&device, None).with_context(|| "Failed to create EGL display")?;
    // Create the OpenGL context
    let context = EGLContext::new(&egl, None).with_context(|| "Failed to create EGL context")?;
    // Create a renderer
    let renderer = Rc::new(RefCell::new(
        unsafe { Gles2Renderer::new(context, None) }
            .with_context(|| "Failed to initialize renderer")?,
    ));

    init_egl_client_side(&mut *state.display.borrow_mut(), renderer.clone())?;

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
        .add_window(&mut *state.display.borrow_mut(), event_loop.handle())
        .with_context(|| "Failed to create wl_output")?;
    state
        .spaces
        .map_output(&output, 1.0, (0, 0).into() /* TODO */);

    event_loop
        .handle()
        .insert_source(backend, |event, window, state| match event {
            X11Event::CloseRequested => {
                window.unmap();
                // TODO: drain_filter
                for output in state
                    .backend
                    .x11()
                    .surfaces
                    .iter()
                    .filter(|s| &s.window == window)
                    .map(|s| &s.output)
                {
                    state.spaces.unmap_output(output);
                }
                state.backend.x11().surfaces.retain(|s| &s.window != window);
            }
            X11Event::Resized(size) => {
                let size = { (size.w as i32, size.h as i32).into() };
                let mode = Mode {
                    size,
                    refresh: 60_000,
                };
                let surface = state
                    .backend
                    .x11()
                    .surfaces
                    .iter_mut()
                    .find(|s| &s.window == window)
                    .expect("Unmanaged X11 surface?");
                surface.render.ping();

                let output = &surface.output;
                output.delete_mode(output.current_mode().unwrap());
                output.change_current_state(Some(mode), None, None, None);
                output.set_preferred(mode);
            }

            X11Event::PresentCompleted | X11Event::Refresh => {
                state
                    .backend
                    .x11()
                    .surfaces
                    .iter_mut()
                    .find(|s| &s.window == window)
                    .expect("Unmanaged X11 surface?")
                    .render
                    .ping();
            }

            X11Event::Input(event) => { /*TODO*/ }
        })
        .expect("Failed to insert X11 Backend into event loop");

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
                move |buffer, _| renderer.borrow_mut().import_dmabuf(buffer).is_ok(),
                None,
            );
        }
        Err(err) => slog_scope::warn!("Unable to initialize bind display to EGL: {}", err),
    };

    Ok(())
}
