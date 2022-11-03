// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui, EguiFrame},
    state::Fps,
    utils::prelude::*,
};
use crate::{
    shell::{
        layout::floating::SeatMoveGrabState, CosmicMappedRenderElement, WorkspaceRenderElement,
    },
    state::Common,
    wayland::{
        handlers::{data_device::get_dnd_icon, screencopy::render_to_buffer},
        protocols::{
            screencopy::{BufferParams, Session as ScreencopySession},
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
            damage::{
                DamageTrackedRenderer, DamageTrackedRendererError as RenderError, OutputNoMode,
            },
            element::RenderElementStates,
            gles2::{Gles2Renderbuffer, Gles2Renderer},
            multigpu::{egl::EglGlesBackend, MultiFrame, MultiRenderer},
            Bind, Blit, ExportMem, ImportAll, ImportMem, Offscreen, Renderer, TextureFilter,
        },
    },
    output::Output,
    utils::{Physical, Rectangle},
};

pub mod cursor;
use self::cursor::CursorRenderElement;

pub type GlMultiRenderer<'a> = MultiRenderer<
    'a,
    'a,
    EglGlesBackend<Gles2Renderer>,
    EglGlesBackend<Gles2Renderer>,
    Gles2Renderbuffer,
>;
pub type GlMultiFrame = MultiFrame<EglGlesBackend<Gles2Renderer>, EglGlesBackend<Gles2Renderer>>;

pub static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];

smithay::render_elements! {
    pub CosmicElement<R> where R: ImportAll;
    WorkspaceElement=WorkspaceRenderElement<R>,
    CursorElement=CursorRenderElement<R>,
    MoveGrabRenderElement=CosmicMappedRenderElement<R>,
    //#[cfg(feature = "debug")]
    //EguiFrame=EguiFrame,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    None,
    NotDefault,
    All,
}

pub fn cursor_elements<E, R>(
    renderer: &mut R,
    state: &Common,
    output: &Output,
    mode: CursorMode,
) -> Vec<E>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: Clone + 'static,
    E: From<CursorRenderElement<R>> + From<CosmicMappedRenderElement<R>>,
{
    let scale = output.current_scale().fractional_scale();
    let mut elements = Vec::new();

    for seat in state.seats() {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = state
            .shell
            .map_global_to_space(pointer.current_location().to_i32_round(), output);

        if mode != CursorMode::None {
            elements.extend(
                cursor::draw_cursor(
                    renderer,
                    seat,
                    location,
                    scale.into(),
                    &state.start_time,
                    mode != CursorMode::NotDefault,
                )
                .into_iter()
                .map(E::from),
            );
        }

        if let Some(wl_surface) = get_dnd_icon(seat) {
            elements.extend(
                cursor::draw_dnd_icon(&wl_surface, location.to_i32_round(), scale)
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
            .map(|state| state.render::<E, R>(seat, output))
        {
            elements.extend(grab_elements);
        }
    }

    elements
}

pub fn render_output<R, Target, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<Target>
        + Bind<Source>
        + Blit<Source>,
    <R as Renderer>::TextureId: Clone + 'static,
    Source: Clone,
{
    let handle = state.shell.workspaces.active(output).handle;
    render_workspace(
        gpu,
        renderer,
        damage_tracker,
        age,
        state,
        output,
        &handle,
        cursor_mode,
        screencopy,
    )
}

pub fn render_workspace<R, Target, Source>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    handle: &WorkspaceHandle,
    cursor_mode: CursorMode,
    screencopy: Option<(Source, &[(ScreencopySession, BufferParams)])>,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), RenderError<R>>
where
    R: Renderer
        + ImportAll
        + ImportMem
        + ExportMem
        + Bind<Dmabuf>
        + Offscreen<Target>
        + Bind<Source>
        + Blit<Source>,
    Source: Clone,
    <R as Renderer>::TextureId: Clone + 'static,
{
    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        fps.start();
    }

    let workspace = state.shell.space_for_handle(&handle).ok_or(OutputNoMode)?;

    let mut elements: Vec<CosmicElement<R>> = cursor_elements(renderer, state, output, cursor_mode);

    #[cfg(feature = "debug")]
    {
        // TODO add debug elements
        let workspace = &state.shell.spaces[space_idx];
        let output_geo = workspace
            .space
            .output_geometry(output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)));
        let scale = output.current_scale().fractional_scale();

        if let Some(fps) = fps {
            let fps_overlay = fps_ui(
                _gpu,
                state,
                fps,
                output_geo.to_f64().to_physical(scale),
                scale,
            );
            custom_elements.push(fps_overlay.into());
        }

        let area = Rectangle::<f64, smithay::utils::Logical>::from_loc_and_size(
            state
                .shell
                .space_relative_output_geometry((0.0f64, 0.0f64), output),
            state.shell.global_space().to_f64().size,
        )
        .to_physical(scale);
        if let Some(log_ui) = log_ui(state, area, scale, output_geo.size.w as f32 * 0.6) {
            custom_elements.push(log_ui.into());
        }
        if let Some(debug_overlay) = debug_ui(state, area, scale) {
            custom_elements.push(debug_overlay.into());
        }
    }

    elements.extend(
        workspace
            .render_output(output)
            .map_err(|_| OutputNoMode)?
            .into_iter()
            .map(Into::into),
    );

    let res = damage_tracker.render_output(renderer, age, &elements, CLEAR_COLOR, None);

    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        fps.end();
    }

    if let Some((source, buffers)) = screencopy {
        if res.is_ok() {
            for (session, params) in buffers {
                match render_to_buffer(
                    gpu.cloned(),
                    renderer,
                    &session,
                    params,
                    output.current_transform(),
                    |_node, renderer, dtr, age| {
                        let res = dtr.damage_output(age, &elements, slog_scope::logger())?;

                        if let (Some(ref damage), _) = &res {
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
    }

    res
}
