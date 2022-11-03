// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::OutputConfig,
    input::Devices,
    state::{BackendData, Common, Data},
    utils::prelude::*,
    wayland::protocols::screencopy::{BufferParams, Session as ScreencopySession},
};
use anyhow::{anyhow, Context, Result};
use smithay::{
    backend::{
        renderer::{damage::DamageTrackedRenderer, gles2::Gles2Renderbuffer, ImportDma, ImportEgl},
        winit::{self, WinitEvent, WinitGraphicsBackend, WinitVirtualDevice},
    },
    desktop::layer_map_for_output,
    output::{Mode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{ping, EventLoop},
        wayland_server::DisplayHandle,
    },
    utils::Transform,
};
use std::cell::RefCell;

#[cfg(feature = "debug")]
use crate::state::Fps;

use super::render::CursorMode;

pub struct WinitState {
    // The winit backend currently has no notion of multiple windows
    pub backend: WinitGraphicsBackend,
    output: Output,
    damage_tracker: DamageTrackedRenderer,
    screencopy: Vec<(ScreencopySession, BufferParams)>,
    #[cfg(feature = "debug")]
    fps: Fps,
}

impl WinitState {
    pub fn render_output(&mut self, state: &mut Common) -> Result<()> {
        self.backend
            .bind()
            .with_context(|| "Failed to bind buffer")?;
        let age = self.backend.buffer_age().unwrap_or(0);

        let surface = self.backend.egl_surface();
        match render::render_output::<_, Gles2Renderbuffer, _>(
            None,
            self.backend.renderer(),
            &mut self.damage_tracker,
            age,
            state,
            &self.output,
            CursorMode::NotDefault,
            if !self.screencopy.is_empty() {
                Some((surface, &self.screencopy))
            } else {
                None
            },
            #[cfg(feature = "debug")]
            Some(&mut self.fps),
        ) {
            Ok((damage, states)) => {
                self.screencopy.clear();
                self.backend
                    .submit(damage.as_deref())
                    .with_context(|| "Failed to submit buffer for display")?;
                state.send_frames(&self.output, &states);
            }
            Err(err) => {
                for (session, params) in self.screencopy.drain(..) {
                    state.still_pending(session, params)
                }
                anyhow::bail!("Rendering failed: {}", err);
            }
        };

        Ok(())
    }

    pub fn apply_config_for_output(
        &mut self,
        output: &Output,
        test_only: bool,
    ) -> Result<(), anyhow::Error> {
        // TODO: if we ever have multiple winit outputs, don't ignore config.enabled
        // reset size
        let size = self.backend.window_size();
        let mut config = output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow_mut();
        if dbg!(config.mode.0) != dbg!((size.physical_size.w as i32, size.physical_size.h as i32)) {
            if !test_only {
                config.mode = (
                    (size.physical_size.w as i32, size.physical_size.h as i32),
                    None,
                );
            }
            Err(anyhow::anyhow!("Cannot set window size"))
        } else {
            Ok(())
        }
    }

    pub fn pending_screencopy(&mut self, new: Option<Vec<(ScreencopySession, BufferParams)>>) {
        if let Some(sessions) = new {
            self.screencopy.extend(sessions);
        }
    }
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<Data>,
    state: &mut State,
) -> Result<()> {
    let (mut backend, mut input) =
        winit::init(None).map_err(|_| anyhow!("Failed to initilize winit backend"))?;

    init_egl_client_side(dh, state, &mut backend)?;

    let name = format!("WINIT-0");
    let size = backend.window_size();
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
    let output = Output::new(name, props, None);
    output.add_mode(mode);
    output.set_preferred(mode);
    output.change_current_state(
        Some(mode),
        Some(Transform::Flipped180),
        Some(Scale::Integer(1)),
        Some((0, 0).into()),
    );
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
            .insert_source(render_source, move |_, _, data| {
                if let Err(err) = data
                    .state
                    .backend
                    .winit()
                    .render_output(&mut data.state.common)
                {
                    slog_scope::error!("Failed to render frame: {}", err);
                    render_ping.ping();
                }
            })
            .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?,
    );
    let event_loop_handle = event_loop.handle();
    event_loop
        .handle()
        .insert_source(event_source, move |_, _, data| {
            match input.dispatch_new_events(|event| {
                data.state.process_winit_event(event, &render_ping_handle)
            }) {
                Ok(_) => {
                    event_ping_handle.ping();
                    render_ping_handle.ping();
                }
                Err(winit::WinitError::WindowClosed) => {
                    let output = data.state.backend.winit().output.clone();
                    data.state.common.shell.remove_output(&output);
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
        damage_tracker: DamageTrackedRenderer::from_output(&output),
        screencopy: Vec::new(),
        #[cfg(feature = "debug")]
        fps: Fps::default(),
    });
    state
        .common
        .output_configuration_state
        .add_heads(std::iter::once(&output));
    state.common.output_configuration_state.update();
    state.common.shell.add_output(&output);
    state.common.config.read_outputs(
        std::iter::once(&output),
        &mut state.backend,
        &mut state.common.shell,
        &state.common.event_loop_handle,
    );
    state.common.shell.refresh_outputs();
    state.common.config.write_outputs(std::iter::once(&output));

    Ok(())
}

fn init_egl_client_side(
    dh: &DisplayHandle,
    state: &mut State,
    renderer: &mut WinitGraphicsBackend,
) -> Result<()> {
    let bind_result = renderer.renderer().bind_wl_display(dh);
    match bind_result {
        Ok(_) => {
            slog_scope::info!("EGL hardware-acceleration enabled");
            let dmabuf_formats = renderer
                .renderer()
                .dmabuf_formats()
                .cloned()
                .collect::<Vec<_>>();
            state
                .common
                .dmabuf_state
                .create_global::<State, _>(dh, dmabuf_formats, None);
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
                for seat in self.common.seats().cloned().collect::<Vec<_>>().iter() {
                    let devices = seat.user_data().get::<Devices>().unwrap();
                    if devices.has_device(&WinitVirtualDevice) {
                        seat.set_active_output(&self.backend.winit().output);
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

                {
                    let mut config = output
                        .user_data()
                        .get::<RefCell<OutputConfig>>()
                        .unwrap()
                        .borrow_mut();
                    config.mode.0 = size.into();
                }
                output.delete_mode(output.current_mode().unwrap());
                output.set_preferred(mode);
                output.change_current_state(Some(mode), None, None, None);
                layer_map_for_output(output).arrange();
                self.common.output_configuration_state.update();
                self.common.shell.refresh_outputs();
                render_ping.ping();
            }
            WinitEvent::Refresh => render_ping.ping(),
            WinitEvent::Input(event) => self.process_input_event(event),
            _ => {}
        };
    }
}
