// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render,
    config::ScreenFilter,
    shell::{Devices, SeatExt},
    state::{BackendData, Common},
    utils::prelude::*,
};
use anyhow::{anyhow, Context, Result};
use cosmic_comp_config::output::comp::{OutputConfig, TransformDef};
use smithay::{
    backend::{
        drm::NodeType,
        egl::EGLDevice,
        renderer::{
            damage::{OutputDamageTracker, RenderOutputResult},
            glow::GlowRenderer,
            ImportDma,
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
    wayland::{dmabuf::DmabufFeedbackBuilder, presentation::Refresh},
};
use std::{borrow::BorrowMut, cell::RefCell, time::Duration};
use tracing::{error, info, warn};

use super::render::{init_shaders, CursorMode, ScreenFilterStorage};

#[derive(Debug)]
pub struct WinitState {
    // The winit backend currently has no notion of multiple windows
    pub backend: WinitGraphicsBackend<GlowRenderer>,
    output: Output,
    damage_tracker: OutputDamageTracker,
    screen_filter_state: ScreenFilterStorage,
}

impl WinitState {
    #[profiling::function]
    pub fn render_output(&mut self, state: &mut Common) -> Result<()> {
        let age = self.backend.buffer_age().unwrap_or(0);
        let (renderer, mut fb) = self
            .backend
            .bind()
            .with_context(|| "Failed to bind buffer")?;
        match render::render_output(
            None,
            renderer,
            &mut fb,
            &mut self.damage_tracker,
            age,
            &state.shell,
            state.clock.now(),
            &self.output,
            CursorMode::NotDefault,
            &mut self.screen_filter_state,
            &state.event_loop_handle,
        ) {
            Ok(RenderOutputResult { damage, states, .. }) => {
                std::mem::drop(fb);
                self.backend
                    .submit(damage.map(|x| x.as_slice()))
                    .with_context(|| "Failed to submit buffer for display")?;
                state.send_frames(&self.output, None);
                state.update_primary_output(&self.output, &states);
                state.send_dmabuf_feedback(&self.output, &states, |_| None);
                if damage.is_some() {
                    let mut output_presentation_feedback = state
                        .shell
                        .read()
                        .take_presentation_feedback(&self.output, &states);
                    output_presentation_feedback.presented(
                        state.clock.now(),
                        self.output
                            .current_mode()
                            .map(|mode| {
                                Refresh::Fixed(Duration::from_secs_f64(
                                    1_000.0 / mode.refresh as f64,
                                ))
                            })
                            .unwrap_or(Refresh::Unknown),
                        0,
                        wp_presentation_feedback::Kind::Vsync,
                    );
                }
            }
            Err(err) => {
                anyhow::bail!("Rendering failed: {}", err);
            }
        };

        Ok(())
    }

    pub fn all_outputs(&self) -> Vec<Output> {
        vec![self.output.clone()]
    }

    pub fn apply_config_for_outputs(&mut self, test_only: bool) -> Result<(), anyhow::Error> {
        // TODO: if we ever have multiple winit outputs, don't ignore config.enabled
        // reset size
        let size = self.backend.window_size();
        let mut config = self
            .output
            .user_data()
            .get::<RefCell<OutputConfig>>()
            .unwrap()
            .borrow_mut();
        if config.mode.0 != (size.w, size.h) {
            if !test_only {
                config.mode = ((size.w, size.h), None);
            }
            Err(anyhow::anyhow!("Cannot set window size"))
        } else {
            Ok(())
        }
    }

    pub fn update_screen_filter(&mut self, screen_filter: &ScreenFilter) -> Result<()> {
        self.screen_filter_state.filter = screen_filter.clone();
        Ok(())
    }
}

pub fn init_backend(
    dh: &DisplayHandle,
    event_loop: &mut EventLoop<State>,
    state: &mut State,
) -> Result<()> {
    let (mut backend, mut input): (WinitGraphicsBackend<GlowRenderer>, _) =
        winit::init().map_err(|e| anyhow!("Failed to initilize winit backend: {e:?}"))?;
    init_shaders(backend.renderer().borrow_mut()).context("Failed to initialize renderer")?;

    init_egl_client_side(dh, state, &mut backend)?;

    let name = format!("WINIT-0");
    let size = backend.window_size();
    let props = PhysicalProperties {
        size: (0, 0).into(),
        subpixel: Subpixel::Unknown,
        make: "COSMIC".to_string(),
        model: name.clone(),
        serial_number: "Unknown".to_string(),
    };
    let mode = Mode {
        size: (size.w, size.h).into(),
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
            mode: ((size.w, size.h), None),
            transform: TransformDef::Flipped180,
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
                profiling::finish_frame!();
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
                    state.common.remove_output(&output);
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
        damage_tracker: OutputDamageTracker::from_output(&output),
        screen_filter_state: ScreenFilterStorage::default(),
    });

    state
        .common
        .output_configuration_state
        .add_heads(std::iter::once(&output));
    {
        state.common.add_output(&output);
        if let Err(err) = state.common.config.read_outputs(
            &mut state.common.output_configuration_state,
            &mut state.backend,
            &state.common.shell,
            &state.common.event_loop_handle,
            &mut state.common.workspace_state.update(),
            &state.common.xdg_activation_state,
            state.common.startup_done.clone(),
            &state.common.clock,
        ) {
            error!("Unrecoverable output config error: {}", err);
        }
        state.common.refresh();
    }
    state.launch_xwayland(None);

    Ok(())
}

fn init_egl_client_side(
    dh: &DisplayHandle,
    state: &mut State,
    renderer: &mut WinitGraphicsBackend<GlowRenderer>,
) -> Result<()> {
    let render_node = EGLDevice::device_for_display(renderer.renderer().egl_context().display())
        .and_then(|device| device.try_get_render_node());

    let dmabuf_formats = renderer.renderer().dmabuf_formats();

    match render_node {
        Ok(Some(node)) => {
            let feedback = DmabufFeedbackBuilder::new(node.dev_id(), dmabuf_formats.clone())
                .build()
                .unwrap();

            let dmabuf_global = state
                .common
                .dmabuf_state
                .create_global_with_default_feedback::<State>(dh, &feedback);

            let render_node = render_node.unwrap().unwrap();
            let _drm_global_id = state.common.wl_drm_state.create_global::<State>(
                dh,
                render_node
                    .dev_path_with_type(NodeType::Render)
                    .or_else(|| render_node.dev_path())
                    .ok_or(anyhow!(
                        "Could not determine path for gpu node: {}",
                        render_node
                    ))?,
                dmabuf_formats,
                &dmabuf_global,
            );

            info!("EGL hardware-acceleration enabled.");
        }
        Ok(None) => {
            warn!("Failed to query render node. Unable to initialize bind display to EGL.")
        }
        Err(err) => {
            warn!(
                ?err,
                "Failed to egl device for display. Unable to initialize bind display to EGL."
            )
        }
    }

    Ok(())
}

impl State {
    pub fn process_winit_event(&mut self, event: WinitEvent, render_ping: &ping::Ping) {
        // here we can handle special cases for winit inputs
        match event {
            WinitEvent::Focus(true) => {
                for seat in self.common.shell.read().seats.iter() {
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
            WinitEvent::Input(event) => self.process_input_event(event),
            WinitEvent::CloseRequested => {
                self.common.should_stop = true;
            }
            _ => {}
        };
    }
}
