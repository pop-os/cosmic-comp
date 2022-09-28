// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui, EguiFrame},
    state::Fps,
    utils::prelude::*,
};
//grabs::{MoveGrabRenderElement, SeatMoveGrabState},
use crate::{
    shell::WorkspaceRenderElement, state::Common, wayland::handlers::data_device::get_dnd_icon,
};

use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            damage::{
                DamageTrackedRenderer, DamageTrackedRendererError as RenderError, OutputNoMode,
            },
            gles2::{Gles2Renderbuffer, Gles2Renderer},
            multigpu::{egl::EglGlesBackend, MultiFrame, MultiRenderer},
            ImportAll, ImportMem, Renderer,
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

static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];

smithay::render_elements! {
    pub CosmicElement<R> where R: ImportAll;
    WorkspaceElement=WorkspaceRenderElement<R>,
    CursorElement=CursorRenderElement<R>,
    //MoveGrabRenderElement=MoveGrabRenderElement,
    //#[cfg(feature = "debug")]
    //EguiFrame=EguiFrame,
}

pub fn cursor_elements<E, R>(
    renderer: &mut R,
    state: &Common,
    output: &Output,
    hardware_cursor: bool,
) -> Vec<E>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: Clone + 'static,
    E: From<CursorRenderElement<R>>,
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

        /*
        if let Some(grab) = seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .borrow()
            .as_ref()
            .and_then(|state| state.render(seat, output))
        {
            custom_elements.push(grab);
        }
        */

        if let Some(wl_surface) = get_dnd_icon(seat) {
            elements.extend(
                cursor::draw_dnd_icon(&wl_surface, location.to_i32_round(), scale)
                    .into_iter()
                    .map(E::from),
            );
        }

        elements.extend(
            cursor::draw_cursor(
                renderer,
                seat,
                location,
                scale.into(),
                &state.start_time,
                !hardware_cursor,
            )
            .into_iter()
            .map(E::from),
        );
    }

    elements
}

pub fn render_output<R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: Clone + 'static,
{
    let idx = state.shell.workspaces.active_num(output);
    render_workspace(
        gpu,
        renderer,
        damage_tracker,
        age,
        state,
        output,
        idx,
        hardware_cursor,
    )
}

pub fn render_workspace<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    damage_tracker: &mut DamageTrackedRenderer,
    age: usize,
    state: &mut Common,
    output: &Output,
    idx: usize,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: Clone + 'static,
{
    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        fps.start();
    }

    let workspace = &state
        .shell
        .workspaces
        .get(idx, output)
        .ok_or(OutputNoMode)?;

    let mut elements: Vec<CosmicElement<R>> =
        cursor_elements(renderer, state, output, hardware_cursor);

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

    res
}
