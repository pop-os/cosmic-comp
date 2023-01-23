// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use crate::{debug::fps_ui, utils::prelude::*};
use crate::{
    shell::{layout::floating::SeatMoveGrabState, CosmicMappedRenderElement},
    state::{Common, Fps},
    wayland::{
        handlers::{data_device::get_dnd_icon, screencopy::render_session},
        protocols::{
            screencopy::{
                BufferParams, CursorMode as ScreencopyCursorMode, Session as ScreencopySession,
            },
            workspace::WorkspaceHandle,
        },
    },
};

use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::FailureReason;
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmNode,
        renderer::{
            buffer_dimensions,
            damage::{
                DamageTrackedRenderer, DamageTrackedRendererError as RenderError, OutputNoMode,
            },
            element::{RenderElement, RenderElementStates},
            gles2::{Gles2Error, Gles2Renderbuffer},
            glow::GlowRenderer,
            multigpu::{egl::EglGlesBackend, MultiFrame, MultiRenderer},
            Bind, Blit, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, TextureFilter,
        },
    },
    output::Output,
    utils::{Physical, Rectangle},
    wayland::dmabuf::get_dmabuf,
};

pub mod cursor;
use self::cursor::CursorRenderElement;
pub mod element;
use self::element::{AsGlowRenderer, CosmicElement};

pub type GlMultiRenderer<'a> = MultiRenderer<
    'a,
    'a,
    EglGlesBackend<GlowRenderer>,
    EglGlesBackend<GlowRenderer>,
    Gles2Renderbuffer,
>;
pub type GlMultiFrame<'a, 'frame> = MultiFrame<
    'a,
    'a,
    'frame,
    EglGlesBackend<GlowRenderer>,
    EglGlesBackend<GlowRenderer>,
    Gles2Renderbuffer,
>;

pub static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    None,
    NotDefault,
    All,
}

pub fn cursor_elements<'frame, E, R>(
    renderer: &mut R,
    state: &Common,
    output: &Output,
    mode: CursorMode,
) -> Vec<E>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    E: From<CursorRenderElement<R>> + From<CosmicMappedRenderElement<R>>,
{
    let scale = output.current_scale().fractional_scale();
    let mut elements = Vec::new();

    for seat in state.seats() {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = pointer.current_location() - output.current_location().to_f64();

        if mode != CursorMode::None {
            elements.extend(
                cursor::draw_cursor(
                    renderer,
                    seat,
                    location,
                    scale.into(),
                    state.clock.now(),
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(E::from),
            );
        }

        if let Some(wl_surface) = get_dnd_icon(seat) {
            elements.extend(
                cursor::draw_dnd_icon(renderer, &wl_surface, location.to_i32_round(), scale)
                    .into_iter()
                    .map(E::from),
            );
        }

        if let Some(grab_elements) = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .borrow()
            .as_ref()
            .map(|state| state.render::<E, R>(renderer, seat, output))
        {
            elements.extend(grab_elements);
        }
    }

    elements
}

pub fn render_output<'frame, R, Target, OffTarget, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    fps: Option<&mut Fps>,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + Blit<Source>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<Gles2Error>,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    Source: Clone,
{
    let handle = state.shell.workspaces.active(output).handle;
    let result = render_workspace(
        gpu,
        renderer,
        target,
        damage_tracker,
        age,
        state,
        output,
        &handle,
        cursor_mode,
        screencopy,
        fps,
    );

    /*
    if let Ok((_, states)) = result.as_ref() {
        for xwm in state
            .xwayland_state
            .values_mut()
            .flat_map(|state| state.xwm.as_mut())
        {
            if let Err(err) = xwm.update_stacking_order_upwards(states.states.keys()) {
                slog_scope::warn!(
                    "Failed to update Xwm ({:?}) stacking order: {}",
                    xwm.id(),
                    err
                );
            }
        }
    }
    */

    result
}

pub fn render_workspace<'frame, R, Target, OffTarget, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    target: Target,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    handle: &WorkspaceHandle,
    mut cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    mut fps: Option<&mut Fps>,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Bind<Target>
        + Offscreen<OffTarget>
        + Blit<Source>
        + AsGlowRenderer,
    <R as Renderer>::TextureId: Clone + 'static,
    <R as Renderer>::Error: From<Gles2Error>,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    Source: Clone,
{
    if let Some(ref mut fps) = fps {
        fps.start();
        #[cfg(feature = "debug")]
        if screencopy.is_some() {
            if let Some(rd) = fps.rd.as_mut() {
                rd.start_frame_capture(
                    renderer.glow_renderer().egl_context().get_context_handle(),
                    std::ptr::null(),
                );
            }
        }
    }

    let workspace = state.shell.space_for_handle(&handle).ok_or(OutputNoMode)?;

    let screencopy_contains_embedded = screencopy.as_ref().map_or(false, |(_, sessions)| {
        sessions
            .iter()
            .any(|(s, _)| s.cursor_mode() == ScreencopyCursorMode::Embedded)
    });
    // cursor handling without a cursor_plane in this case is horrible.
    // because what if some session disagree and/or the backend wants to render with a different mode?
    // It seems we would need to render to an offscreen buffer in those cases (and do multiple renders, which messes with damage tracking).
    // So for now, we just pick the worst mode (embedded), if any requires it.
    //
    // Once we move to a cursor_plane, the default framebuffer will never contain a cursor and we can just composite the cursor for each session separately on top (or not).
    if screencopy_contains_embedded {
        cursor_mode = CursorMode::All;
    };

    let mut elements: Vec<CosmicElement<R>> = cursor_elements(renderer, state, output, cursor_mode);

    #[cfg(feature = "debug")]
    {
        let output_geo = output.geometry();
        let scale = output.current_scale().fractional_scale();

        if let Some(fps) = fps.as_mut() {
            let fps_overlay = fps_ui(
                gpu,
                state,
                renderer.glow_renderer_mut(),
                fps,
                Rectangle::from_loc_and_size(
                    (0, 0),
                    (output_geo.size.w.min(400), output_geo.size.h.min(800)),
                ),
                scale,
            )
            .map_err(<R as Renderer>::Error::from)
            .map_err(RenderError::Rendering)?;
            elements.push(fps_overlay.into());
        }
    }

    elements.extend(
        workspace
            .render_output::<R>(renderer, output, &state.shell.override_redirect_windows)
            .map_err(|_| OutputNoMode)?
            .into_iter()
            .map(Into::into),
    );

    if let Some(fps) = fps.as_mut() {
        fps.elements();
    }

    renderer.bind(target).map_err(RenderError::Rendering)?;
    let res = damage_tracker.render_output(renderer, age, &elements, CLEAR_COLOR, None);

    if let Some(fps) = fps.as_mut() {
        fps.render();
    }

    if let Some((source, buffers)) = screencopy {
        if res.is_ok() {
            for (session, params) in buffers {
                match render_session(
                    gpu.cloned(),
                    renderer,
                    &session,
                    params,
                    output.current_transform(),
                    |_node, buffer, renderer, dtr, age| {
                        let res = dtr.damage_output(age, &elements, slog_scope::logger())?;

                        if let (Some(ref damage), _) = &res {
                            if let Ok(dmabuf) = get_dmabuf(buffer) {
                                renderer.bind(dmabuf).map_err(RenderError::Rendering)?;
                            } else {
                                let size = buffer_dimensions(buffer).unwrap();
                                let render_buffer = renderer
                                    .create_buffer(size)
                                    .map_err(RenderError::Rendering)?;
                                renderer
                                    .bind(render_buffer)
                                    .map_err(RenderError::Rendering)?;
                            }
                            for rect in damage {
                                renderer
                                    .blit_from(source.clone(), *rect, *rect, TextureFilter::Nearest)
                                    .map_err(RenderError::Rendering)?;
                            }
                        }

                        Ok(res)
                    },
                ) {
                    Ok(true) => {} // success
                    Ok(false) => state.still_pending(session.clone(), params.clone()),
                    Err(err) => {
                        slog_scope::warn!("Error rendering to screencopy session: {}", err);
                        session.failed(FailureReason::Unspec);
                    }
                }
            }
        }
        if let Some(fps) = fps.as_mut() {
            fps.screencopy();
            #[cfg(feature = "debug")]
            if let Some(rd) = fps.rd.as_mut() {
                rd.end_frame_capture(
                    renderer.glow_renderer().egl_context().get_context_handle(),
                    std::ptr::null(),
                );
            }
        }
    }

    res
}
