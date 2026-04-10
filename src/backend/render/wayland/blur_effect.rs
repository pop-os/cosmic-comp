use std::{
    borrow::{Borrow, BorrowMut},
    sync::{LazyLock, Mutex},
};

use cgmath::{Matrix3, SquareMatrix, Vector2};
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            Bind, BlitFrame, Color32F, ContextId, Frame, FrameContext, ImportAll, Offscreen,
            Renderer, Texture, TextureFilter,
            element::{Element, Id, Kind, RenderElement},
            gles::{
                GlesError, GlesFrame, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformName, UniformType, UniformValue, ffi,
            },
            sync::SyncPoint,
            utils::{CommitCounter, DamageSet},
        },
    },
    utils::{
        Buffer, Logical, Physical, Point, Rectangle, Scale, Size, Transform, user_data::UserDataMap,
    },
    wayland::compositor::SurfaceData,
};
use tracing::trace;

use crate::{
    backend::render::{element::AsGlowRenderer, wayland::clipped_surface::ClippingShader},
    wayland::handlers::background_effect::ComputedBlurRegionCachedState,
};

pub static BLUR_DOWNSAMPLE_SHADER: &str = include_str!("../shaders/blur_downsample.frag");
pub static BLUR_UPSAMPLE_SHADER: &str = include_str!("../shaders/blur_upsample.frag");

const NOISE: f32 = 0.03;
const MAX_STEPS: usize = 15;

#[derive(Debug, Clone, Copy, PartialEq)]
struct BlurParameters {
    passes: usize,
    offset: f64,
    extended_radius: i32,
}

static BLUR_PARAMS: LazyLock<Vec<BlurParameters>> = LazyLock::new(|| {
    let mut params = Vec::new();

    let mut remaining_steps = MAX_STEPS as isize;
    let offsets = [
        // min offset, max offset, extended radius to avoid artifacts
        (1.0, 2.0, 10),
        (2.0, 3.0, 20),
        (2.0, 5.0, 50),
        (3.0, 8.0, 150),
    ];

    let sum = offsets.iter().map(|(min, max, _)| *max - *min).sum::<f64>();
    for (i, (min, max, extended_radius)) in offsets.into_iter().enumerate() {
        let mut iter_num = f64::ceil((max - min) / sum * (MAX_STEPS as f64)) as usize;
        remaining_steps -= iter_num as isize;

        if remaining_steps < 0 {
            iter_num = iter_num.saturating_add_signed(remaining_steps);
        }

        let diff = max - min;
        for j in 1..=iter_num {
            params.push(BlurParameters {
                passes: i + 1,
                offset: min + (diff / iter_num as f64) * j as f64,
                extended_radius,
            });
        }
    }

    trace!("Computed blur values: {:#?}", &params);
    params
});

#[derive(Debug, Clone)]
pub struct BlurShaders {
    down: GlesTexProgram,
    up: GlesTexProgram,
}

impl BlurShaders {
    pub fn compile(renderer: &mut GlesRenderer) -> Result<BlurShaders, GlesError> {
        let up = renderer.compile_custom_texture_shader(
            BLUR_UPSAMPLE_SHADER,
            &[
                UniformName::new("half_pixel", UniformType::_2f),
                UniformName::new("offset", UniformType::_1f),
            ],
        )?;
        let down = renderer.compile_custom_texture_shader(
            BLUR_DOWNSAMPLE_SHADER,
            &[
                UniformName::new("half_pixel", UniformType::_2f),
                UniformName::new("offset", UniformType::_1f),
            ],
        )?;

        Ok(BlurShaders { up, down })
    }

    pub fn get<R: AsGlowRenderer>(renderer: &R) -> Self {
        Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<BlurShaders>()
            .expect("Custom Shaders not initialized")
            .clone()
    }
}

type BlurTexture = Mutex<Option<GlesTexture>>;

struct BlurState {
    id: Id,
    renderer_id: Option<ContextId<GlesTexture>>,
    src: Size<f64, Buffer>,
    offset: f64,
    passes: usize,
    region: Vec<Rectangle<i32, Logical>>,
    commit: CommitCounter,
}

unsafe impl Send for BlurState {}
unsafe impl Sync for BlurState {}

impl BlurState {
    fn new() -> Self {
        BlurState {
            id: Id::new(),
            renderer_id: None,
            src: Size::new(0., 0.),
            offset: 0.,
            passes: 0,
            region: Vec::new(),
            commit: CommitCounter::default(),
        }
    }
}

pub struct BlurElement {
    id: Id,
    commit: CommitCounter,
    src: Size<f64, Buffer>,
    extended_offset: Point<f64, Logical>,
    geometry: Rectangle<f64, Logical>,
    scaling_shaders: BlurShaders,
    render_shader: GlesTexProgram,
    region: Vec<Rectangle<i32, Logical>>,
    offset: f64,
    passes: usize,
    uniforms: Vec<Uniform<'static>>,
}

impl BlurElement {
    pub fn from_surface<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        states: &SurfaceData,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
    ) -> Result<Option<Self>, R::Error> {
        let mut blur_region_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
        let Some(region) = blur_region_state.current().blur_region.as_ref() else {
            return Ok(None);
        };

        let geo = geometry.to_physical_precise_round(output_scale);
        let mut extended_geo = geo;
        let radius = BLUR_PARAMS[strength.min(MAX_STEPS - 1)].extended_radius as f64;
        extended_geo.loc -= Point::<f64, Physical>::new(radius, radius);
        extended_geo.size += Size::<f64, Physical>::new(radius, radius).upscale(2.);

        // compute input_to_geo so that it crops the extended capture radius
        let geo_scale = {
            let Scale { x, y } = geo.size / extended_geo.size;
            Matrix3::from_nonuniform_scale(x as f32, y as f32)
                .invert()
                .unwrap()
        };
        let geo_translation = {
            let offset = geo.loc - extended_geo.loc;
            Matrix3::from_translation(Vector2::new(
                (offset.x / extended_geo.size.w) as f32,
                (offset.y / extended_geo.size.h) as f32,
            ))
            .invert()
            .unwrap()
        };
        let input_to_geo = geo_scale * geo_translation;

        let uniforms = vec![
            Uniform::new("geo_size", (geometry.size.w as f32, geometry.size.h as f32)),
            Uniform::new(
                "corner_radius",
                [
                    radii[3] as f32,
                    radii[1] as f32,
                    radii[0] as f32,
                    radii[2] as f32,
                ],
            ),
            Uniform::new(
                "input_to_geo",
                UniformValue::Matrix3x3 {
                    matrices: vec![*AsRef::<[f32; 9]>::as_ref(&input_to_geo)],
                    transpose: false,
                },
            ),
            Uniform::new("noise", UniformValue::_1f(NOISE)),
        ];

        let state = states
            .data_map
            .get_or_insert_threadsafe::<Mutex<BlurState>, _>(|| Mutex::new(BlurState::new()));

        Ok(Some(Self::from_state(
            renderer,
            extended_geo.to_logical(output_scale),
            Point::<f64, Physical>::new(radius, radius).to_logical(output_scale),
            region,
            output_scale,
            &mut state.lock().unwrap(),
            uniforms,
            strength,
        )?))
    }

    fn from_state<R: AsGlowRenderer>(
        renderer: &mut R,
        geometry: Rectangle<f64, Logical>,
        extended_offset: Point<f64, Logical>,
        region: &Vec<Rectangle<i32, Logical>>,
        output_scale: f64,
        state: &mut BlurState,
        uniforms: Vec<Uniform<'static>>,
        strength: usize,
    ) -> Result<Self, R::Error> {
        let renderer_id = renderer.glow_renderer().context_id();
        let src = geometry.size.to_buffer(output_scale, Transform::Normal);
        let params = &BLUR_PARAMS[strength.min(MAX_STEPS - 1)];

        let dirty = !(state
            .renderer_id
            .as_ref()
            .is_some_and(|id| id == &renderer_id)
            && state.offset == params.offset
            && state.passes == params.passes
            && &state.region == region
            && state.src == src);

        state.renderer_id = Some(renderer_id);
        state.offset = params.offset;
        state.passes = params.passes;
        state.region = region.clone();
        state.src = src;
        if dirty {
            state.commit.increment();
        }

        Ok(BlurElement {
            id: state.id.clone(),
            commit: state.commit,
            src,
            geometry,
            extended_offset,
            scaling_shaders: BlurShaders::get(renderer),
            render_shader: ClippingShader::get(renderer),
            offset: state.offset,
            passes: state.passes,
            region: region
                .iter()
                .cloned()
                .map(|mut rect| {
                    rect.loc += extended_offset.to_i32_round();
                    rect
                })
                .collect(),
            uniforms,
        })
    }
}

impl Element for BlurElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        self.commit
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        Rectangle::from_size(self.src)
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry.to_physical_precise_round(scale)
    }

    fn transform(&self) -> Transform {
        Transform::Normal
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        if self.commit.distance(commit).is_none_or(|d| d > 0) {
            DamageSet::from_slice(&[Rectangle::new(
                self.extended_offset.to_physical_precise_round(scale),
                self.geometry.size.to_physical_precise_round(scale)
                    - self
                        .extended_offset
                        .to_size()
                        .upscale(2.)
                        .to_physical_precise_round(scale),
            )])
        } else {
            DamageSet::default()
        }
    }

    fn alpha(&self) -> f32 {
        1.0
    }

    fn kind(&self) -> Kind {
        Kind::default()
    }

    fn is_framebuffer_effect(&self) -> bool {
        true
    }
}

impl<R: Renderer + AsGlowRenderer> RenderElement<R> for BlurElement {
    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);
        let gles_frame = BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame);
        let transform = gles_frame.transformation();
        let tex_size = self.src.to_i32_round();

        let texture_ref = cache.get_or_insert_threadsafe(BlurTexture::default);
        let mut texture_entry = texture_ref.lock().unwrap();
        if texture_entry
            .as_ref()
            .is_some_and(|tex| tex.size() != tex_size)
        {
            texture_entry.take();
        }

        let mut renderer = gles_frame.renderer();
        if texture_entry.is_none() {
            *texture_entry = Some(
                renderer
                    .as_mut()
                    .create_buffer(Fourcc::Abgr8888, tex_size)
                    .map_err(R::from_gles_error)?,
            );
        }
        let texture = texture_entry.as_mut().unwrap();
        let mut off_texture = renderer
            .as_mut()
            .create_buffer(Fourcc::Abgr8888, tex_size)
            .map_err(R::from_gles_error)?;
        std::mem::drop(renderer);

        let sync = blit_from_active_fb(gles_frame, src, dst, transform, texture)
            .map_err(R::from_gles_error)?;
        gles_frame.wait(&sync).map_err(R::from_gles_error)?;

        let mut textures = [texture, &mut off_texture];
        render_blur(
            gles_frame.renderer().as_mut(),
            &self.scaling_shaders,
            &mut textures,
            self.offset,
            self.passes,
        )
        .map_err(R::from_gles_error)?;

        Ok(())
    }

    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);
        let src_to_geo = self.geometry.size / self.src;
        let src_log = src
            .upscale(src_to_geo)
            .to_logical(1., Transform::Normal, &Size::default());
        let scale = dst.size.to_f64() / src_log.size;

        let damage = self
            .region
            .iter()
            .flat_map(|rect| {
                let mut rect = rect.to_f64().intersection(src_log)?;
                rect.loc -= src_log.loc;
                Some(rect.to_physical_precise_round(scale))
            })
            .flat_map(|rect| damage.iter().flat_map(move |r| r.intersection(rect)))
            .collect::<Vec<_>>();
        let cache = cache.expect("Framebuffer element without cache?");
        let Some(texture) = cache.get::<BlurTexture>() else {
            return Err(R::from_gles_error(GlesError::BlitError));
        };
        let texture_ref = texture.lock().unwrap();

        BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame)
            .render_texture_from_to(
                texture_ref
                    .as_ref()
                    .ok_or(R::from_gles_error(GlesError::BlitError))?,
                src,
                dst,
                &damage,
                opaque_regions,
                Transform::Normal,
                1.0,
                Some(&self.render_shader),
                &self.uniforms,
            )
            .map_err(R::from_gles_error)
    }
}

fn blit_from_active_fb(
    frame: &mut GlesFrame<'_, '_>,
    src: Rectangle<f64, Buffer>,
    dst: Rectangle<i32, Physical>,
    transform: Transform,
    to_texture: &mut GlesTexture,
) -> Result<SyncPoint, GlesError> {
    if transform != Transform::Normal {
        let tex_size = to_texture
            .size()
            .to_logical(1, Transform::Normal)
            .to_physical(1);
        let mut renderer = frame.renderer();
        let mut tmp_texture = renderer.as_mut().create_buffer(
            Fourcc::Abgr8888,
            dst.size.to_logical(1).to_buffer(1, Transform::Normal),
        )?;
        let mut fb_tmp = renderer.as_mut().bind(&mut tmp_texture)?;
        let mut fb = renderer.as_mut().bind(to_texture)?;
        std::mem::drop(renderer);

        frame.blit_to(
            &mut fb_tmp,
            dst,
            Rectangle::from_size(dst.size),
            TextureFilter::Linear,
        )?;
        std::mem::drop(fb_tmp);

        let mut renderer = frame.renderer();
        let mut frame = renderer
            .as_mut()
            .render(&mut fb, tex_size, Transform::Normal)?;
        Frame::render_texture_from_to(
            &mut frame,
            &tmp_texture,
            Rectangle::from_size(
                dst.size
                    .to_logical(1)
                    .to_buffer(1, Transform::Normal)
                    .to_f64(),
            ),
            src.to_logical(1., Transform::Normal, &src.size)
                .to_physical(1.)
                .to_i32_round(),
            &[Rectangle::from_size(dst.size)],
            &[Rectangle::from_size(dst.size)],
            transform,
            1.0,
        )?;
        std::mem::drop(tmp_texture);
        frame.finish()
    } else {
        let mut fb = frame.renderer().as_mut().bind(to_texture)?;

        frame
            .blit_to(
                &mut fb,
                dst,
                src.to_logical(1., Transform::Normal, &src.size)
                    .to_physical(1.)
                    .to_i32_round(),
                TextureFilter::Linear,
            )
            .map(|_| SyncPoint::signaled())
    }
}

fn render_blur(
    renderer: &mut GlesRenderer,
    shaders: &BlurShaders,
    textures: &mut [&mut GlesTexture; 2],
    offset: f64,
    passes: usize,
) -> Result<(), GlesError> {
    for i in 0..passes {
        let tex_size = textures[0].size();
        let [src_tex, target_tex] = textures;
        let mut fb = renderer.bind(*target_tex)?;

        let adjusted_tex_size = tex_size.downscale(1 << i);
        let target_tex_size = tex_size
            .downscale(1 << (i + 1))
            .to_logical(1, Transform::Normal)
            .to_physical(1);
        let half_pixel = [
            0.5 / (adjusted_tex_size.w as f32),
            0.5 / (adjusted_tex_size.h as f32),
        ];

        let mut frame = renderer.render(
            &mut fb,
            tex_size.to_logical(1, Transform::Normal).to_physical(1),
            Transform::Normal,
        )?;
        frame.clear(
            Color32F::new(0., 0., 0., 0.),
            &[Rectangle::from_size(
                tex_size.to_logical(1, Transform::Normal).to_physical(1),
            )],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_S,
                ffi::CLAMP_TO_EDGE as i32,
            );
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_T,
                ffi::CLAMP_TO_EDGE as i32,
            );
        })?;
        frame.render_texture_from_to(
            src_tex,
            Rectangle::from_size(adjusted_tex_size.to_f64()),
            Rectangle::from_size(target_tex_size),
            &[Rectangle::from_size(target_tex_size)],
            &[Rectangle::from_size(target_tex_size)],
            Transform::Normal,
            1.0,
            Some(&shaders.down),
            &[
                Uniform::new("half_pixel", half_pixel),
                Uniform::new("offset", (offset / (1 << i) as f64) as f32),
            ],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::REPEAT as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::REPEAT as i32);
        })?;
        let sync = frame.finish()?;
        std::mem::drop(fb);
        renderer.wait(&sync)?;

        textures.swap(0, 1);
    }

    for i in 0..passes {
        let tex_size = textures[0].size();
        let [src_tex, target_tex] = textures;
        let mut fb = renderer.bind(*target_tex)?;

        let adjusted_tex_size = tex_size.downscale(1 << (passes - i));
        let target_tex_size = tex_size
            .downscale(1 << (passes - i - 1))
            .to_logical(1, Transform::Normal)
            .to_physical(1);
        let half_pixel = [
            0.5 / (adjusted_tex_size.w as f32),
            0.5 / (adjusted_tex_size.h as f32),
        ];

        let mut frame = renderer.render(
            &mut fb,
            tex_size.to_logical(1, Transform::Normal).to_physical(1),
            Transform::Normal,
        )?;
        frame.clear(
            Color32F::new(0., 0., 0., 0.),
            &[Rectangle::from_size(
                tex_size.to_logical(1, Transform::Normal).to_physical(1),
            )],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_S,
                ffi::CLAMP_TO_EDGE as i32,
            );
            gl.TexParameteri(
                ffi::TEXTURE_2D,
                ffi::TEXTURE_WRAP_T,
                ffi::CLAMP_TO_EDGE as i32,
            );
        })?;
        frame.render_texture_from_to(
            src_tex,
            Rectangle::from_size(adjusted_tex_size.to_f64()),
            Rectangle::from_size(target_tex_size),
            &[Rectangle::from_size(target_tex_size)],
            &[Rectangle::from_size(target_tex_size)],
            Transform::Normal,
            1.0,
            Some(&shaders.up),
            &[
                Uniform::new("half_pixel", half_pixel),
                Uniform::new("offset", (offset / (1 << (passes - i)) as f64) as f32),
            ],
        )?;
        frame.with_context(|gl| unsafe {
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::REPEAT as i32);
            gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::REPEAT as i32);
        })?;
        let sync = frame.finish()?;
        std::mem::drop(fb);
        renderer.wait(&sync)?;

        textures.swap(0, 1);
    }

    // textures always end up the right way around with `self.texture` containing our final render,
    // since we render PASSES * 2 (downscale and upscale), so the number of swaps is always even.
    Ok(())
}
