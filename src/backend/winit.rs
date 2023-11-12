// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::OutputConfig,
    input::Devices,
    state::{BackendData, Common},
    utils::prelude::*,
    wayland::protocols::screencopy::{BufferParams, Session as ScreencopySession},
};
use anyhow::{anyhow, Context, Result};
use smithay::{
    backend::{
        egl::EGLDevice,
        renderer::{
            damage::{OutputDamageTracker, RenderOutputResult},
            gles::GlesRenderbuffer,
            glow::GlowRenderer,
            ImportDma, ImportEgl,
        },
        winit::{self, WinitEvent, WinitGraphicsBackend, WinitVirtualDevice},
    },
    desktop::layer_map_for_output,
    output::{Mode, Output, PhysicalProperties, Scale, Subpixel},
    reexports::{
        calloop::{ping, EventLoop},
        wayland_protocols::wp::presentation_time::server::wp_presentation_feedback,
        wayland_server::DisplayHandle,
        winit::platform::pump_events::PumpStatus,
    },
    utils::Transform,
    wayland::dmabuf::DmabufFeedbackBuilder,
};
use std::{cell::RefCell, time::Duration};
use tracing::{error, info, warn};

#[cfg(feature = "debug")]
use crate::state::Fps;

use super::render::{init_shaders, CursorMode};

#[derive(Debug)]
pub struct WinitState {
    // The winit backend currently has no notion of multiple windows
    pub backend: WinitGraphicsBackend<GlowRenderer>,
    output: Output,
    damage_tracker: OutputDamageTracker,
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
        match render::render_output::<_, _, GlesRenderbuffer, _>(
            None,
            self.backend.renderer(),
            surface.clone(),
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
            #[cfg(not(feature = "debug"))]
            None,
            #[cfg(feature = "debug")]
            Some(&mut self.fps),
        ) {
            Ok(RenderOutputResult { damage, states, .. }) => {
                self.backend
                    .bind()
                    .with_context(|| "Failed to bind display")?;
                self.backend
                    .submit(damage.as_deref())
                    .with_context(|| "Failed to submit buffer for display")?;
                self.screencopy.clear();
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
                    );
                }
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
        if dbg!(config.mode.0) != dbg!((size.w as i32, size.h as i32)) {
            if !test_only {
                config.mode = ((size.w as i32, size.h as i32), None);
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
    event_loop: &mut EventLoop<State>,
    state: &mut State,
) -> Result<()> {
    let (mut backend, mut input) =
        winit::init().map_err(|_| anyhow!("Failed to initilize winit backend"))?;
    init_shaders(backend.renderer()).expect("Failed to initialize renderer");

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
        size: (size.w as i32, size.h as i32).into(),
        refresh: 60_000,
    };
    let output = Output::new(name, props);
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
            mode: ((size.w as i32, size.h as i32), None),
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
                    error!(?err, "Failed to render frame.");
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
                PumpStatus::Continue => {
                    event_ping_handle.ping();
                    render_ping_handle.ping();
                }
                PumpStatus::Exit(_) => {
                    let output = state.backend.winit().output.clone();
                    let seats = state.common.seats().cloned().collect::<Vec<_>>();
                    state.common.shell.remove_output(&output, seats.into_iter());
                    if let Some(token) = token.take() {
                        event_loop_handle.remove(token);
                    }
                }
            };
        })
        .map_err(|_| anyhow::anyhow!("Failed to init eventloop timer for winit"))?;
    event_ping.ping();

    #[cfg(feature = "debug")]
    let fps = Fps::new(backend.renderer());

    state.backend = BackendData::Winit(WinitState {
        backend,
        output: output.clone(),
        damage_tracker: OutputDamageTracker::from_output(&output),
        screencopy: Vec::new(),
        #[cfg(feature = "debug")]
        fps,
    });

    state
        .common
        .output_configuration_state
        .add_heads(std::iter::once(&output));
    state.common.shell.add_output(&output);
    let seats = state.common.seats().cloned().collect::<Vec<_>>();
    state.common.config.read_outputs(
        &mut state.common.output_configuration_state,
        &mut state.backend,
        &mut state.common.shell,
        seats.iter().cloned(),
        &state.common.event_loop_handle,
    );
    state.launch_xwayland(None);

    Ok(())
}

fn init_egl_client_side(
    dh: &DisplayHandle,
    state: &mut State,
    renderer: &mut WinitGraphicsBackend<GlowRenderer>,
) -> Result<()> {
    let bind_result = renderer.renderer().bind_wl_display(dh);
    let render_node = EGLDevice::device_for_display(renderer.renderer().egl_context().display())
        .and_then(|device| device.try_get_render_node());

    let dmabuf_formats = renderer.renderer().dmabuf_formats().collect::<Vec<_>>();
    let dmabuf_default_feedback = match render_node {
        Ok(Some(node)) => {
            let dmabuf_default_feedback =
                DmabufFeedbackBuilder::new(node.dev_id(), dmabuf_formats.clone())
                    .build()
                    .unwrap();
            Some(dmabuf_default_feedback)
        }
        Ok(None) => {
            warn!("Failed to query render node, dmabuf protocol will only advertise v3");
            None
        }
        Err(err) => {
            warn!(
                ?err,
                "Failed to egl device for display, dmabuf protocol will only advertise v3"
            );
            None
        }
    };

    match dmabuf_default_feedback {
        Some(feedback) => {
            state
                .common
                .dmabuf_state
                .create_global_with_default_feedback::<State>(dh, &feedback);

            info!("EGL hardware-acceleration enabled.");
        }
        None if bind_result.is_ok() => {
            state
                .common
                .dmabuf_state
                .create_global::<State>(dh, dmabuf_formats);
            info!("EGL hardware-acceleration enabled.");
        }
        None => {
            let err = bind_result.unwrap_err();
            warn!(?err, "Unable to initialize bind display to EGL.")
        }
    }

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
                render_ping.ping();
            }
            WinitEvent::Redraw => render_ping.ping(),
            WinitEvent::Input(event) => self.process_input_event(event, false),
            _ => {}
        };
    }
}
