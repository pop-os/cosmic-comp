// Taken and modified from niri, licensed GPL-3.

use std::borrow::{Borrow, BorrowMut};

use cgmath::{Matrix3, Vector2};
use smithay::backend::renderer::{
    ImportAll, ImportMem, Renderer,
    element::{
        Element, Id, Kind, RenderElement, UnderlyingStorage, surface::WaylandSurfaceRenderElement,
    },
    gles::{GlesFrame, GlesRenderer, GlesTexProgram, Uniform, UniformValue},
    utils::{CommitCounter, DamageSet, OpaqueRegions},
};
use smithay::utils::{Buffer, Logical, Physical, Point, Rectangle, Scale, Size, Transform};

use crate::backend::render::element::AsGlowRenderer;

pub static CLIPPING_SHADER: &str = include_str!("./shaders/clipped_surface.frag");
pub struct ClippingShader(pub GlesTexProgram);

impl ClippingShader {
    pub fn get<R: AsGlowRenderer>(renderer: &R) -> GlesTexProgram {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<ClippingShader>()
            .expect("Custom Shaders not initialized")
            .0
            .clone()
    }
}

#[derive(Debug)]
pub struct ClippedSurfaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem,
{
    inner: WaylandSurfaceRenderElement<R>,
    program: GlesTexProgram,
    radius: [u8; 4],
    geometry: Rectangle<f64, Logical>,
    uniforms: Vec<Uniform<'static>>,
}

impl<R> ClippedSurfaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem,
{
    pub fn new(
        renderer: &mut R,
        elem: WaylandSurfaceRenderElement<R>,
        scale: Scale<f64>,
        geometry: Rectangle<f64, Logical>,
        radius: [u8; 4],
    ) -> Self
    where
        R: AsGlowRenderer,
    {
        let elem_geo = elem.geometry(scale);
        let geo: Rectangle<i32, Physical> = geometry.to_physical_precise_round(scale);
        let buf_size = elem.buffer_size();
        let view = elem.view();

        let transform = elem.transform();
        let transform_matrix = Matrix3::<f32>::from_translation(Vector2::new(0.5, 0.5))
            * transform.matrix()
            * Matrix3::<f32>::from_translation(-Vector2::new(0.5, 0.5));

        let geo_scale = {
            let Scale { x, y } = elem_geo.size.to_f64() / geo.size.to_f64();
            Matrix3::from_nonuniform_scale(x as f32, y as f32)
        };

        let geo_translation = {
            let offset = (elem_geo.loc - geo.loc).to_f64();
            Matrix3::from_translation(Vector2::new(
                (offset.x / elem_geo.size.w as f64) as f32,
                (offset.y / elem_geo.size.h as f64) as f32,
            ))
        };

        let buf_scale = {
            let Scale { x, y } = buf_size.to_f64() / view.src.size.to_f64();
            Matrix3::from_nonuniform_scale(x as f32, y as f32)
        };

        let buf_translation = Matrix3::from_translation(Vector2::new(
            (view.src.loc.x as f64 / buf_size.w as f64) as f32,
            (view.src.loc.y as f64 / buf_size.h as f64) as f32,
        ));

        let input_to_geo =
            transform_matrix * geo_scale * geo_translation * buf_scale * buf_translation;

        let uniforms = vec![
            Uniform::new("geo_size", (geometry.size.w as f32, geometry.size.h as f32)),
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
                "input_to_geo",
                UniformValue::Matrix3x3 {
                    matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                    transpose: false,
                },
            ),
        ];

        Self {
            inner: elem,
            program: ClippingShader::get(renderer),
            radius,
            geometry,
            uniforms,
        }
    }

    pub fn will_clip(
        elem: &WaylandSurfaceRenderElement<R>,
        scale: Scale<f64>,
        geometry: Rectangle<f64, Logical>,
        radius: [u8; 4],
    ) -> bool {
        let elem_geo = elem.geometry(scale);
        let geo = geometry.to_physical_precise_round(scale);

        let corners = Self::rounded_corners(geometry, radius);
        let corners = corners
            .into_iter()
            .map(|rect| rect.to_physical_precise_up(scale));
        let geo = Rectangle::subtract_rects_many([geo], corners);
        !Rectangle::subtract_rects_many([elem_geo], geo).is_empty()
    }

    fn rounded_corners(
        geo: Rectangle<f64, Logical>,
        radius: [u8; 4],
    ) -> [Rectangle<f64, Logical>; 4] {
        let top_left = radius[0] as f64;
        let top_right = radius[1] as f64;
        let bottom_right = radius[2] as f64;
        let bottom_left = radius[3] as f64;

        [
            Rectangle::new(geo.loc, Size::from((top_left, top_left))),
            Rectangle::new(
                Point::from((geo.loc.x + geo.size.w - top_right, geo.loc.y)),
                Size::from((top_right, top_right)),
            ),
            Rectangle::new(
                Point::from((
                    geo.loc.x + geo.size.w - bottom_right,
                    geo.loc.y + geo.size.h - bottom_right,
                )),
                Size::from((bottom_right, bottom_right)),
            ),
            Rectangle::new(
                Point::from((geo.loc.x, geo.loc.y + geo.size.h - bottom_left)),
                Size::from((bottom_left, bottom_left)),
            ),
        ]
    }
}

impl<R> Element for ClippedSurfaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem,
{
    fn id(&self) -> &Id {
        self.inner.id()
    }

    fn current_commit(&self) -> CommitCounter {
        self.inner.current_commit()
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.inner.geometry(scale)
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        self.inner.src()
    }

    fn transform(&self) -> Transform {
        self.inner.transform()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        // FIXME: radius changes need to cause damage.
        let damage = self.inner.damage_since(scale, commit);

        // Intersect with geometry, since we're clipping by it.
        let mut geo = self.geometry.to_physical_precise_round(scale);
        geo.loc -= self.geometry(scale).loc;
        damage
            .into_iter()
            .filter_map(|rect| rect.intersection(geo))
            .collect()
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        let regions = self.inner.opaque_regions(scale);

        // Intersect with geometry, since we're clipping by it.
        let mut geo = self.geometry.to_physical_precise_round(scale);
        geo.loc -= self.geometry(scale).loc;
        let regions = regions
            .into_iter()
            .filter_map(|rect| rect.intersection(geo));

        // Subtract the rounded corners.
        let corners = Self::rounded_corners(self.geometry, self.radius);

        let elem_loc = self.geometry(scale).loc;
        let corners = corners.into_iter().map(|rect| {
            let mut rect = rect.to_physical_precise_up(scale);
            rect.loc -= elem_loc;
            rect
        });

        OpaqueRegions::from_slice(&Rectangle::subtract_rects_many(regions, corners))
    }

    fn alpha(&self) -> f32 {
        self.inner.alpha()
    }

    fn kind(&self) -> Kind {
        self.inner.kind()
    }
}

impl<R> RenderElement<R> for ClippedSurfaceRenderElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    R::TextureId: 'static,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
    ) -> Result<(), R::Error> {
        BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
            .override_default_tex_program(self.program.clone(), self.uniforms.clone());
        self.inner.draw(frame, src, dst, damage, opaque_regions)?;
        BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
            .clear_tex_program_override();
        Ok(())
    }

    fn underlying_storage(&self, _renderer: &mut R) -> Option<UnderlyingStorage<'_>> {
        None
    }
}
