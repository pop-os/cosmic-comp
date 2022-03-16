// SPDX-License-Identifier: GPL-3.0-only

use crate::state::Common;
#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui, EguiFrame},
    state::Fps,
};

use slog::Logger;
use smithay::{
    backend::renderer::{
        gles2::{Gles2Renderbuffer, Gles2Renderer, Gles2Texture},
        ImportAll, Renderer,
    },
    desktop::space::{RenderElement, RenderError, SpaceOutputTuple, SurfaceTree},
    utils::{Logical, Point, Rectangle},
    wayland::output::Output,
};

mod cursor;
use self::cursor::PointerElement;

smithay::custom_elements! {
    pub CustomElem<=Gles2Renderer>;
    SurfaceTree=SurfaceTree,
    PointerElement=PointerElement::<Gles2Texture>,
    #[cfg(feature = "debug")]
    EguiFrame=EguiFrame,
}

pub trait AsGles2Renderer {
    fn as_gles2(&mut self) -> &mut Gles2Renderer;
}
impl AsGles2Renderer for Gles2Renderer {
    fn as_gles2(&mut self) -> &mut Gles2Renderer {
        self
    }
}

pub fn render_output<R>(
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
    #[cfg(feature = "debug")]
    {
        fps.start();
    }

    #[allow(unused_mut)]
    let mut custom_elements = Vec::<CustomElem>::new();

    #[cfg(feature = "debug")]
    {
        let space = state.spaces.active_space(output);
        let output_geo = space
            .output_geometry(output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)));
        let scale = space.output_scale(output).unwrap();

        let fps_overlay = fps_ui(state, fps, output_geo, scale);
        custom_elements.push(fps_overlay.into());

        let mut area = state.spaces.global_space();
        area.loc = state.spaces.space_relative_output_geometry((0, 0), output);
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
            .spaces
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

    let res = state.spaces.active_space_mut(output).render_output(
        renderer,
        &output,
        age as usize,
        [0.153, 0.161, 0.165, 1.0],
        &*custom_elements,
    );

    #[cfg(feature = "debug")]
    {
        fps.end();
    }

    res
}
