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
        renderer::{ImportDma, ImportEgl},
        winit::{self, WinitEvent, WinitGraphicsBackend, WinitVirtualDevice},
    },
    desktop::layer_map_for_output,
    reexports::{
        calloop::{ping, EventLoop},
        wayland_server::{
            protocol::wl_output::{Subpixel, Transform},
            Display,
        },
    },
    wayland::{
        dmabuf::init_dmabuf_global,
        output::{Mode, Output, PhysicalProperties},
    },
};
use std::{cell::RefCell, rc::Rc};

#[cfg(feature = "debug")]
use crate::state::Fps;

pub struct WinitState {
    // The winit backend currently has no notion of multiple windows
    backend: Rc<RefCell<WinitGraphicsBackend>>,
    //_global: GlobalDrop<WlOutput>,
    output: Output,
    #[cfg(feature = "debug")]
    fps: Fps,
}

impl WinitState {
    pub fn render_output(&mut self, state: &mut Common) -> Result<()> {
        let backend = &mut *self.backend.borrow_mut();
        backend.bind().with_context(|| "Failed to bind buffer")?;
        let age = backend.buffer_age().unwrap_or(0);

        match render::render_output(
            None,
            backend.renderer(),
            age as u8,
            state,
            &self.output,
            true,
            #[cfg(feature = "debug")]
            &mut self.fps,
        ) {
            Ok(damage) => {
                state
                    .shell
                    .active_space_mut(&self.output)
                    .space
                    .send_frames(state.start_time.elapsed().as_millis() as u32);
                backend
                    .submit(damage.as_ref().map(|x| &**x), 1.0)
                    .with_context(|| "Failed to submit buffer for display")?;
            }
            Err(err) => {
                anyhow::bail!("Rendering failed: {}", err);
            }
        };

        Ok(())
    }
}

pub fn init_backend(event_loop: &mut EventLoop<State>, state: &mut State) -> Result<()> {
    let (backend, mut input) =
        winit::init(None).with_context(|| "Failed to initilize winit backend")?;
    let backend = Rc::new(RefCell::new(backend));

    init_egl_client_side(&mut *state.common.display.borrow_mut(), backend.clone())?;

    let name = format!("WINIT-0");
    let size = backend.borrow().window_size();
    let props = PhysicalProperties {
        size: (0, 0).into(),
        subpixel: Subpixel::Unknown,
        make: "COSMIC".to_string(),
        model: name.clone(),
    };
    let mode = Mode {
        size: (size.physical_size.w as i32, size.physical_size.h as i32).into(),
        refresh: 60_000,
    };
    let (output, _global) = Output::new(&mut *state.common.display.borrow_mut(), name, props, None);
    //let _global = global.into();
    output.change_current_state(
        Some(mode),
        Some(Transform::Flipped180),
        None,
        Some((0, 0).into()),
    );
    output.set_preferred(mode);
    output.user_data().insert_if_missing(|| {
        RefCell::new(OutputConfig {
            mode: (
                (size.physical_size.w as i32, size.physical_size.h as i32),
                None,
            ),
            transform: Transform::Flipped180.into(),
            ..Default::default()
        })
    });

    let (event_ping, event_source) =
        ping::make_ping().with_context(|| "Failed to init eventloop timer for winit")?;
    let (render_ping, render_source) =
        ping::make_ping().with_context(|| "Failed to init eventloop timer for winit")?;
    let event_ping_handle = event_ping.clone();
    let render_ping_handle = render_ping.clone();
    let mut token = Some(
        event_loop
            .handle()
            .insert_source(render_source, move |_, _, state| {
                if let Err(err) = state.backend.winit().render_output(&mut state.common) {
                    slog_scope::error!("Failed to render frame: {}", err);
                    render_ping.ping();
                }
            })
            .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?,
    );
    let event_loop_handle = event_loop.handle();
    event_loop
        .handle()
        .insert_source(event_source, move |_, _, state| {
            match input
                .dispatch_new_events(|event| state.process_winit_event(event, &render_ping_handle))
            {
                Ok(_) => {
                    event_ping_handle.ping();
                    render_ping_handle.ping();
                }
                Err(winit::WinitError::WindowClosed) => {
                    let output = state.backend.winit().output.clone();
                    state.common.shell.unmap_output(
                        &output,
                        &mut state.backend,
                        &state.common.config,
                    );
                    if let Some(token) = token.take() {
                        event_loop_handle.remove(token);
                    }
                }
            };
        })
        .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?;
    event_ping.ping();

    state.backend = BackendData::Winit(WinitState {
        backend,
        output: output.clone(),
        #[cfg(feature = "debug")]
        fps: Fps::default(),
    });
    state
        .common
        .shell
        .map_output(&output, &mut state.backend, &mut state.common.config);

    Ok(())
}

fn init_egl_client_side(
    display: &mut Display,
    renderer: Rc<RefCell<WinitGraphicsBackend>>,
) -> Result<()> {
    let bind_result = renderer.borrow_mut().renderer().bind_wl_display(display);
    match bind_result {
        Ok(_) => {
            slog_scope::info!("EGL hardware-acceleration enabled");
            let dmabuf_formats = renderer
                .borrow_mut()
                .renderer()
                .dmabuf_formats()
                .cloned()
                .collect::<Vec<_>>();
            init_dmabuf_global(
                display,
                dmabuf_formats,
                move |buffer, _| {
                    renderer
                        .borrow_mut()
                        .renderer()
                        .import_dmabuf(buffer, None)
                        .is_ok()
                },
                None,
            );
        }
        Err(err) => slog_scope::warn!("Unable to initialize bind display to EGL: {}", err),
    };

    Ok(())
}

impl State {
    pub fn process_winit_event(&mut self, event: WinitEvent, render_ping: &ping::Ping) {
        // here we can handle special cases for winit inputs
        match event {
            WinitEvent::Focus(true) => {
                for seat in self.common.seats.clone().iter() {
                    let devices = seat.user_data().get::<Devices>().unwrap();
                    if devices.has_device(&WinitVirtualDevice) {
                        set_active_output(seat, &self.backend.winit().output);
                        break;
                    }
                }
            }
            WinitEvent::Resized { size, .. } => {
                let winit_state = self.backend.winit();
                let output = &winit_state.output;
                let mode = Mode {
                    size,
                    refresh: 60_000,
                };
                output.delete_mode(output.current_mode().unwrap());
                output.change_current_state(Some(mode), None, None, None);
                output.set_preferred(mode);
                layer_map_for_output(output).arrange();
                render_ping.ping();
            }
            WinitEvent::Refresh => render_ping.ping(),
            WinitEvent::Input(event) => self.common.process_input_event(event),
            _ => {}
        };
    }
}
