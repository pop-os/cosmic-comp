// SPDX-License-Identifier: GPL-3.0-only

use crate::state::Common;
#[cfg(feature = "debug")]
use crate::{
    debug::{debug_ui, fps_ui, log_ui},
    state::Fps,
};

use smithay::{
    backend::renderer::gles2::Gles2Renderer,
    desktop::space::{DynamicRenderElements, RenderError},
    utils::{Logical, Rectangle},
    wayland::output::Output,
};

mod cursor;

pub fn render_output(
    renderer: &mut Gles2Renderer,
    age: u8,
    state: &mut Common,
    output: &Output,
    hardware_cursor: bool,
    #[cfg(feature = "debug")] fps: &mut Fps,
) -> Result<Option<Vec<Rectangle<i32, Logical>>>, RenderError<Gles2Renderer>> {
    #[cfg(feature = "debug")]
    {
        fps.start();
    }

    #[allow(unused_mut)]
    let mut custom_elements = Vec::<DynamicRenderElements<Gles2Renderer>>::new();

    #[cfg(feature = "debug")]
    {
        let space = state.spaces.active_space(output);
        let output_geo = space.output_geometry(output)
            .unwrap_or(Rectangle::from_loc_and_size((0, 0), (0, 0)));
        let scale = space.output_scale(output).unwrap();

        let fps_overlay = fps_ui(state, fps, output_geo, scale);
        custom_elements.push(Box::new(fps_overlay));

        let mut area = state.spaces.global_space();
        area.loc = state.spaces.space_relative_output_geometry((0, 0), output);
        if let Some(log_ui) = log_ui(state, area, scale, output_geo.size.w as f32 * 0.6) {
            custom_elements.push(Box::new(log_ui));
        }
        if let Some(debug_overlay) = debug_ui(state, area, scale) {
            custom_elements.push(Box::new(debug_overlay));
        }
    }

    for seat in &state.seats {
        let pointer = match seat.get_pointer() {
            Some(ptr) => ptr,
            None => continue,
        };
        let location = state.spaces.space_relative_output_geometry(pointer.current_location().to_i32_round(), output);

        if let Some(cursor) =
            cursor::draw_cursor(renderer, seat, location, &state.start_time, !hardware_cursor)
        {
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
