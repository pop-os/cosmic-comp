// SPDX-License-Identifier: GPL-3.0-only

use crate::state::Common;
#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui, EguiFrame},
    state::Fps,
};

use slog::Logger;
use smithay::{
    backend::{
        drm::DrmNode,
        renderer::{
            gles2::{Gles2Renderbuffer, Gles2Renderer, Gles2Texture},
            multigpu::{egl::EglGlesBackend, Error as MultiError, MultiFrame, MultiRenderer},
            Frame, ImportAll, Renderer, TextureFilter,
        },
    },
    desktop::{
        draw_layer_surface, draw_window, layer_map_for_output,
        space::{RenderElement, RenderError, SpaceOutputTuple, SurfaceTree},
        utils::damage_from_surface_tree,
        Window,
    },
    utils::{Logical, Point, Rectangle, Transform},
    wayland::{output::Output, shell::wlr_layer::Layer as WlrLayer},
};

mod cursor;
use self::cursor::PointerElement;

pub type GlMultiRenderer<'a> =
    MultiRenderer<'a, 'a, EglGlesBackend, EglGlesBackend, Gles2Renderbuffer>;
pub type GlMultiFrame = MultiFrame<EglGlesBackend, EglGlesBackend>;

static CLEAR_COLOR: [f32; 4] = [0.153, 0.161, 0.165, 1.0];

smithay::custom_elements! {
    pub CustomElem<=Gles2Renderer>;
    SurfaceTree=SurfaceTree,
    PointerElement=PointerElement::<Gles2Texture>,
    #[cfg(feature = "debug")]
    EguiFrame=EguiFrame,
}

// TODO: due to the lifetime, we cannot be generic over CustomElem's renderer
// util after GATs land. So we generate with the macro for Gles2 and then
// do a manual impl for MultiRenderer.
impl RenderElement<GlMultiRenderer<'_>> for CustomElem {
    fn id(&self) -> usize {
        RenderElement::<Gles2Renderer>::id(self)
    }

    fn geometry(&self) -> Rectangle<i32, Logical> {
        RenderElement::<Gles2Renderer>::geometry(self)
    }

    fn accumulated_damage(
        &self,
        for_values: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Logical>> {
        RenderElement::<Gles2Renderer>::accumulated_damage(self, for_values)
    }

    fn draw(
        &self,
        renderer: &mut GlMultiRenderer<'_>,
        frame: &mut GlMultiFrame,
        scale: f64,
        location: Point<i32, Logical>,
        damage: &[Rectangle<i32, Logical>],
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

pub fn render_output<R>(
    gpu: Option<&DrmNode>,
    renderer: &mut R,
    age: u8,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] fps: &mut Fps,
) -> Result<Option<Vec<Rectangle<i32, Logical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    renderer
        .downscale_filter(TextureFilter::Linear)
        .map_err(RenderError::Rendering)?;

    #[cfg(feature = "debug")]
    {
        fps.start();
    }

    let workspace = state.shell.active_space(output);
    let maybe_fullscreen_window = workspace.get_fullscreen(output).cloned();
    let res = if let Some(window) = maybe_fullscreen_window {
        #[cfg(not(feature = "debug"))]
        {
            render_fullscreen(gpu, renderer, window, state, output, hardware_cursor)
        }
        #[cfg(feature = "debug")]
        {
            render_fullscreen(gpu, renderer, window, state, output, hardware_cursor, fps)
        }
    } else {
        #[cfg(not(feature = "debug"))]
        {
            render_desktop(gpu, renderer, age, state, output, hardware_cursor)
        }
        #[cfg(feature = "debug")]
        {
            render_desktop(gpu, renderer, age, state, output, hardware_cursor, fps)
        }
    };

    #[cfg(feature = "debug")]
    {
        fps.end();
    }

    res
}

fn render_desktop<R>(
    _gpu: Option<&DrmNode>,
    renderer: &mut R,
    age: u8,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] fps: &mut Fps,
) -> Result<Option<Vec<Rectangle<i32, Logical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    let mut custom_elements = Vec::<CustomElem>::new();

    #[cfg(feature = "debug")]
    {
        let workspace = state.shell.active_space(output);
        let output_geo = workspace
            .space
            .output_geometry(output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)));
        let scale = output.current_scale().fractional_scale();

        let fps_overlay = fps_ui(_gpu, state, fps, output_geo, scale);
        custom_elements.push(fps_overlay.into());

        let mut area = state.shell.global_space();
        area.loc = state.shell.space_relative_output_geometry((0, 0), output);
        if let Some(log_ui) = log_ui(state, area, scale, output_geo.size.w as f32 * 0.6) {
            custom_elements.push(log_ui.into());
        }
        if let Some(debug_overlay) = debug_ui(state, area, scale) {
            custom_elements.push(debug_overlay.into());
        }
    }

    for seat in &state.seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = state
            .shell
            .space_relative_output_geometry(pointer.current_location().to_i32_round(), output);

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

    state.shell.active_space_mut(output).space.render_output(
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
    #[cfg(feature = "debug")] fps: &mut Fps,
) -> Result<Option<Vec<Rectangle<i32, Logical>>>, RenderError<R>>
where
    R: Renderer + ImportAll + AsGles2Renderer,
    <R as Renderer>::TextureId: Clone + 'static,
    CustomElem: RenderElement<R>,
{
    let transform = Transform::from(output.current_transform());
    let mode = output.current_mode().unwrap();
    let scale = output.current_scale().fractional_scale();
    let output_geo = state.shell.output_geometry(output);

    let mut custom_elements = Vec::<CustomElem>::new();

    #[cfg(feature = "debug")]
    {
        let fps_overlay = fps_ui(
            _gpu,
            state,
            fps,
            Rectangle::from_loc_and_size((0, 0), output_geo.size),
            scale,
        );
        custom_elements.push(fps_overlay.into());
    }

    for seat in &state.seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = state
            .shell
            .space_relative_output_geometry(pointer.current_location().to_i32_round(), output);

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

    renderer
        .render(mode.size, transform, |renderer, frame| {
            let mut damage = window.accumulated_damage(None);
            frame.clear(
                CLEAR_COLOR,
                &[Rectangle::from_loc_and_size((0, 0), mode.size).to_f64()],
            )?;
            draw_window(
                renderer,
                frame,
                &window,
                scale,
                (0, 0),
                &[Rectangle::from_loc_and_size(
                    (0, 0),
                    mode.size.to_f64().to_logical(scale).to_i32_round(),
                )],
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
                    geo.loc,
                    &[Rectangle::from_loc_and_size((0, 0), geo.size)],
                    &slog_scope::logger(),
                )?;
                if let Some(surface) = layer_surface.get_surface() {
                    damage.extend(damage_from_surface_tree(surface, geo.loc, None));
                }
            }
            for elem in custom_elements {
                let geo = elem.geometry();
                let elem_damage = elem.accumulated_damage(None);
                elem.draw(
                    renderer,
                    frame,
                    scale,
                    geo.loc,
                    &[Rectangle::from_loc_and_size((0, 0), geo.size)],
                    &slog_scope::logger(),
                )?;
                damage.extend(elem_damage.into_iter().map(|mut rect| {
                    rect.loc += geo.loc;
                    rect
                }))
            }
            Ok(Some(damage))
        })
        .and_then(std::convert::identity)
        .map_err(RenderError::<R>::Rendering)
}
