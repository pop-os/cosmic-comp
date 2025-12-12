use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use cgmath::{Matrix3, Vector2};
use smithay::{
    backend::renderer::{
        element::Kind,
        gles::{
            GlesPixelProgram, GlesRenderer, Uniform, UniformValue, element::PixelShaderElement,
        },
    },
    utils::{Coordinate, IsAlive, Point, Rectangle, Size},
};

use crate::{
    backend::render::element::AsGlowRenderer,
    shell::element::CosmicMappedKey,
    utils::prelude::{Local, RectLocalExt},
};

pub static SHADOW_SHADER: &str = include_str!("./shaders/shadow.frag");
pub struct ShadowShader(pub GlesPixelProgram);

#[derive(Debug, PartialEq)]
pub struct ShadowParameters {
    geo: Rectangle<i32, Local>,
    scale: f64,
    alpha: f32,
    radius: [u8; 4],
}
type ShadowCache = RefCell<HashMap<CosmicMappedKey, (ShadowParameters, PixelShaderElement)>>;

impl ShadowShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesPixelProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<ShadowShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }

    pub fn element<R: AsGlowRenderer>(
        renderer: &R,
        key: CosmicMappedKey,
        geo: Rectangle<i32, Local>,
        radius: [u8; 4],
        alpha: f32,
        scale: f64,
    ) -> PixelShaderElement {
        let params = ShadowParameters {
            geo,
            scale,
            alpha,
            radius,
        };
        let ceil = |logical: f64| (logical * scale).ceil() / scale;

        let mut geo = geo.to_f64();
        geo.loc.x = ceil(geo.loc.x);
        geo.loc.y = ceil(geo.loc.y);
        geo.size.w = ceil(geo.size.w);
        geo.size.h = ceil(geo.size.h);

        let user_data = Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data();

        user_data.insert_if_missing(|| ShadowCache::new(HashMap::new()));
        let mut cache = user_data.get::<ShadowCache>().unwrap().borrow_mut();
        cache.retain(|k, _| k.alive());

        if cache
            .get(&key)
            .filter(|(old_params, _)| &params == old_params)
            .is_none()
        {
            let shader = Self::get(renderer);

            let softness = 30.;
            let spread = 5.;
            let offset = [0., 5.];
            let color = [0., 0., 0., 0.45];
            let radius = radius.map(|r| ceil(r as f64));
            let radius = [
                radius[3], // top_left
                radius[1], // top_right
                radius[0], // bottom_right
                radius[2], // bottom_left
            ];

            let width = softness;
            let sigma = width / 2.;
            let width = ceil(sigma * 3.);

            let offset = Point::new(ceil(offset[0]), ceil(offset[1]));
            let spread = ceil(spread.abs()).copysign(spread);
            let offset = offset - Point::new(spread, spread);

            let box_size = if spread >= 0. {
                geo.size + Size::new(spread, spread).upscale(2.)
            } else {
                geo.size - Size::new(-spread, -spread).upscale(2.)
            };

            let win_radius = radius;
            let radius = radius.map(|r| if r > 0. { r.saturating_add(spread) } else { 0. });
            let shader_size = box_size + Size::from((width, width)).upscale(2.);
            let mut shader_geo = Rectangle::new(Point::from((-width, -width)), shader_size);

            let window_geo = Rectangle::new(Point::new(0., 0.) - offset - shader_geo.loc, geo.size);
            let area_size = Vector2::new(shader_geo.size.w, shader_geo.size.h);
            let geo_loc = Vector2::new(-shader_geo.loc.x, -shader_geo.loc.y);
            shader_geo.loc += offset + geo.loc;

            let input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -geo_loc.x / area_size.x,
                    -geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            let window_geo_loc = Vector2::new(window_geo.loc.x as f64, window_geo.loc.y as f64);
            let window_input_to_geo = (Matrix3::from_nonuniform_scale(area_size.x, area_size.y)
                * Matrix3::from_translation(Vector2::new(
                    -window_geo_loc.x / area_size.x,
                    -window_geo_loc.y / area_size.y,
                )))
            .cast::<f32>()
            .unwrap();

            let element = PixelShaderElement::new(
                shader,
                shader_geo.to_i32_up().as_logical(),
                None,
                alpha,
                vec![
                    Uniform::new("shadow_color", color),
                    Uniform::new("sigma", sigma as f32),
                    Uniform::new(
                        "input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new("geo_size", [box_size.w as f32, box_size.h as f32]),
                    Uniform::new(
                        "corner_radius",
                        [
                            radius[0] as f32,
                            radius[1] as f32,
                            radius[2] as f32,
                            radius[3] as f32,
                        ],
                    ),
                    Uniform::new(
                        "window_input_to_geo",
                        UniformValue::Matrix3x3 {
                            matrices: vec![*AsRef::<[f32; 9]>::as_ref(&window_input_to_geo)],
                            transpose: false,
                        },
                    ),
                    Uniform::new(
                        "window_geo_size",
                        [window_geo.size.w as f32, window_geo.size.h as f32],
                    ),
                    Uniform::new(
                        "window_corner_radius",
                        [
                            win_radius[0] as f32,
                            win_radius[1] as f32,
                            win_radius[2] as f32,
                            win_radius[3] as f32,
                        ],
                    ),
                ],
                Kind::Unspecified,
            );

            cache.insert(key.clone(), (params, element));
        }

        cache.get(&key).unwrap().1.clone()
    }
}
