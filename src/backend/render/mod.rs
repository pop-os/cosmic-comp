// SPDX-License-Identifier: GPL-3.0-only

#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui, EguiFrame},
    state::Fps,
    utils::prelude::*,
};
use crate::{
    shell::grabs::{MoveGrabRenderElement, SeatMoveGrabState},
    state::Common,
    wayland::handlers::data_device::get_dnd_icon,
};

use slog::Logger;
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            gles2::{Gles2Renderbuffer, Gles2Renderer, Gles2Texture},
            multigpu::{egl::EglGlesBackend, Error as MultiError, MultiFrame, MultiRenderer},
            Frame, ImportAll, Renderer,
        },
    },
    desktop::{
        draw_layer_popups, draw_layer_surface, draw_window, draw_window_popups,
        layer_map_for_output,
        space::{RenderElement, RenderError, SpaceOutputTuple, SurfaceTree},
        utils::damage_from_surface_tree,
        Window,
    },
    output::Output,
    utils::{Physical, Point, Rectangle, Scale, Transform},
    wayland::shell::wlr_layer::Layer as WlrLayer,
};

pub mod cursor;
use self::cursor::PointerElement;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, EglGlesBackend, EglGlesBackend, Gles2Renderbuffer>;
pub type GlMultiFrame = MultiFrame<EglGlesBackend, EglGlesBackend>;

static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];

smithay::custom_elements! {
    pub CustomElem<=Gles2Renderer>;
    SurfaceTree=SurfaceTree,
    PointerElement=PointerElement::<Gles2Texture>,
    MoveGrabRenderElement=MoveGrabRenderElement,
    #[cfg(feature = "debug")]
    EguiFrame=EguiFrame,
}

// TODO: due to the lifetime of MultiRenderer, we cannot be generic over CustomElem's renderer
// util after GATs land. So we generate with the macro for Gles2 and then
// do a manual impl for MultiRenderer.
impl RenderElement<GlMultiRenderer<'_>> for CustomElem {
    fn id(&self) -> usize {
        RenderElement::<Gles2Renderer>::id(self)
    }

    fn location(&self, scale: impl Into<Scale<f64>>) -> Point<f64, Physical> {
        RenderElement::<Gles2Renderer>::location(self, scale)
    }

    fn geometry(&self, scale: impl Into<Scale<f64>>) -> Rectangle<i32, Physical> {
        RenderElement::<Gles2Renderer>::geometry(self, scale)
    }

    fn accumulated_damage(
        &self,
        scale: impl Into<Scale<f64>>,
        for_values: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Physical>> {
        RenderElement::<Gles2Renderer>::accumulated_damage(self, scale, for_values)
    }

    fn opaque_regions(
        &self,
        scale: impl Into<Scale<f64>>,
    ) -> Option<Vec<Rectangle<i32, Physical>>> {
        RenderElement::<Gles2Renderer>::opaque_regions(self, scale)
    }

    fn draw(
        &self,
        renderer: &mut GlMultiRenderer<'_>,
        frame: &mut GlMultiFrame,
        scale: impl Into<Scale<f64>>,
        location: Point<f64, Physical>,
        damage: &[Rectangle<i32, Physical>],
        log: &Logger,
    ) -> Result<(), MultiError<EglGlesBackend, EglGlesBackend>> {
        RenderElement::<Gles2Renderer>::draw(
            self,
            renderer.as_mut(),
            frame.as_mut(),
            scale,
            location,
            damage,
            log,
        )
        .map_err(MultiError::Render)
    }

    fn z_index(&self) -> u8 {
        RenderElement::<Gles2Renderer>::z_index(self)
    }
}

pub trait AsGles2Renderer {
    fn as_gles2(&mut self) -> &mut Gles2Renderer;
}
impl AsGles2Renderer for Gles2Renderer {
    fn as_gles2(&mut self) -> &mut Gles2Renderer {
        self
    }
}
impl AsGles2Renderer for GlMultiRenderer<'_> {
    fn as_gles2(&mut self) -> &mut Gles2Renderer {
        self.as_mut()
    }
}

pub fn needs_buffer_reset(output: &Output, state: &Common) -> bool {
    use std::sync::atomic::{AtomicBool, Ordering};
    struct DidCustomRendering(AtomicBool);

    let will_render_custom = {
        let workspace = state.shell.active_space(output);
        workspace.get_fullscreen(output).is_some()
    };

    let userdata = output.user_data();
    userdata.insert_if_missing(|| DidCustomRendering(AtomicBool::new(false)));
    userdata
        .get::<DidCustomRendering>()
        .unwrap()
        .0
        .swap(will_render_custom, Ordering::AcqRel)
        != will_render_custom
}

pub fn cursor_custom_elements<R>(
    renderer: &mut R,
    state: &Common,
    output: &Output,
    hardware_cursor: bool,
) -> Vec<CustomElem>
where
    R: AsGles2Renderer,
{
    let mut custom_elements = Vec::new();

    for seat in &state.seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = state
            .shell
            .space_relative_output_geometry(pointer.current_location().to_i32_round(), output);

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

        if let Some(wl_surface) = get_dnd_icon(seat) {
            custom_elements.push(cursor::draw_dnd_icon(wl_surface, location.to_i32_round()).into());
        }

        if let Some(cursor) = cursor::draw_cursor(
            renderer.as_gles2(),
            seat,
            location,
            &state.start_time,
            !hardware_cursor,
        ) {
            custom_elements.push(cursor)
        }
    }

    custom_elements
}

pub fn render_output<R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    age: u8,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    let workspace = state.shell.active_space(output).idx;
    render_workspace(
        gpu,
        renderer,
        age,
        state,
        workspace,
        output,
        hardware_cursor,
    )
}

pub fn render_workspace<R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    age: u8,
    state: &mut Common,
    space_idx: u8,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] mut fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        fps.start();
    }

    let space_idx = space_idx as usize;
    let workspace = &mut state.shell.spaces[space_idx];
    let maybe_fullscreen_window = workspace.get_fullscreen(output).cloned();

    let res = if let Some(window) = maybe_fullscreen_window {
        #[cfg(not(feature = "debug"))]
        {
            render_fullscreen(gpu, renderer, window, state, output, hardware_cursor)
        }
        #[cfg(feature = "debug")]
        {
            render_fullscreen(
                gpu,
                renderer,
                window,
                state,
                output,
                hardware_cursor,
                fps.as_deref_mut(),
            )
        }
    } else {
        #[cfg(not(feature = "debug"))]
        {
            render_desktop(
                gpu,
                renderer,
                age,
                state,
                space_idx,
                output,
                hardware_cursor,
            )
        }
        #[cfg(feature = "debug")]
        {
            render_desktop(
                gpu,
                renderer,
                age,
                state,
                space_idx,
                output,
                hardware_cursor,
                fps.as_deref_mut(),
            )
        }
    };

    #[cfg(feature = "debug")]
    if let Some(ref mut fps) = fps {
        fps.end();
    }

    res
}

fn render_desktop<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    age: u8,
    state: &mut Common,
    space_idx: usize,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    let mut custom_elements = Vec::<CustomElem>::new();

    #[cfg(feature = "debug")]
    {
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

    custom_elements.extend(cursor_custom_elements(
        renderer,
        state,
        output,
        hardware_cursor,
    ));

    state.shell.spaces[space_idx].space.render_output(
        renderer,
        &output,
        age as usize,
        CLEAR_COLOR,
        &*custom_elements,
    )
}

fn render_fullscreen<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    window: Window,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] fps: Option<&mut Fps>,
) -> Result<Option<Vec<Rectangle<i32, Physical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    let transform = Transform::from(output.current_transform());
    let mode = output.current_mode().unwrap();
    let scale = output.current_scale().fractional_scale();

    let mut custom_elements = Vec::<CustomElem>::new();

    #[cfg(feature = "debug")]
    if let Some(fps) = fps {
        let output_geo = output.geometry();
        let fps_overlay = fps_ui(
            _gpu,
            state,
            fps,
            Rectangle::from_loc_and_size((0, 0), output_geo.size)
                .to_f64()
                .to_physical(scale),
            scale,
        );
        custom_elements.push(fps_overlay.into());
    }

    custom_elements.extend(cursor_custom_elements(
        renderer,
        state,
        output,
        hardware_cursor,
    ));

    renderer
        .render(mode.size, transform, |renderer, frame| {
            let mut damage = window.accumulated_damage((0.0, 0.0), scale, None);
            frame.clear(
                CLEAR_COLOR,
                &[Rectangle::from_loc_and_size((0, 0), mode.size)],
            )?;
            draw_window(
                renderer,
                frame,
                &window,
                scale,
                (0.0, 0.0),
                &[Rectangle::from_loc_and_size((0, 0), mode.size)],
                &slog_scope::logger(),
            )?;
            draw_window_popups(
                renderer,
                frame,
                &window,
                scale,
                (0.0, 0.0),
                &[Rectangle::from_loc_and_size((0, 0), mode.size)],
                &slog_scope::logger(),
            )?;
            let layer_map = layer_map_for_output(output);
            for layer_surface in layer_map.layers_on(WlrLayer::Overlay) {
                let geo = layer_map.layer_geometry(&layer_surface).unwrap();
                draw_layer_surface(
                    renderer,
                    frame,
                    layer_surface,
                    scale,
                    geo.loc.to_f64().to_physical(scale),
                    &[Rectangle::from_loc_and_size(
                        (0, 0),
                        geo.size.to_physical_precise_round(scale),
                    )],
                    &slog_scope::logger(),
                )?;
                draw_layer_popups(
                    renderer,
                    frame,
                    layer_surface,
                    scale,
                    geo.loc.to_f64().to_physical(scale),
                    &[Rectangle::from_loc_and_size(
                        (0, 0),
                        geo.size.to_physical_precise_round(scale),
                    )],
                    &slog_scope::logger(),
                )?;
                damage.extend(damage_from_surface_tree(
                    layer_surface.wl_surface(),
                    geo.loc.to_f64().to_physical(scale),
                    scale,
                    None,
                ));
            }
            for elem in custom_elements {
                let loc = elem.location(scale);
                let geo = elem.geometry(scale);
                let elem_damage = elem.accumulated_damage(scale, None);
                elem.draw(renderer, frame, scale, loc, &[geo], &slog_scope::logger())?;
                damage.extend(elem_damage)
            }
            Ok(Some(damage))
        })
        .and_then(std::convert::identity)
        .map_err(RenderError::<R>::Rendering)
}
