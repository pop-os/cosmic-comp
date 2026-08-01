use std::{
    borrow::{Borrow, BorrowMut},
    sync::{LazyLock, Mutex},
};

use glam::{Affine2, Mat3, Vec2};
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

/// Blur strength from configuration, as a step index into [`BLUR_PARAMS`].
///
/// Upstream derives strength from a single frosted-glass boolean, which gives
/// only two of the fifteen steps and reads far weaker than a configurable blur.
/// This keeps the `blur_intensity` config value driving it instead.
static BLUR_INTENSITY_BITS: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
/// Whether blur is enabled at all, from configuration.
static BLUR_ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

/// Update the configured blur strength. `intensity` is 0.0..=1.0.
pub fn set_blur_config(enabled: bool, intensity: f32) {
    BLUR_ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);
    BLUR_INTENSITY_BITS.store(
        intensity.clamp(0.0, 1.0).to_bits(),
        std::sync::atomic::Ordering::Relaxed,
    );
}

/// Whether blur is enabled in configuration.
pub fn blur_enabled() -> bool {
    BLUR_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

/// The configured strength as a step index.
///
/// `frosted` is the theme's frosted-glass flag, kept as the floor so a theme
/// that asks for frosting still gets some even at intensity 0.
pub fn configured_blur_strength(frosted: bool) -> usize {
    let intensity = f32::from_bits(BLUR_INTENSITY_BITS.load(std::sync::atomic::Ordering::Relaxed));
    let floor = frosted as usize;
    // Spread the configured intensity across the whole step range rather than
    // the two steps a boolean can express.
    let steps = (intensity * (MAX_STEPS - 1) as f32).round() as usize;
    steps.max(floor).min(MAX_STEPS - 1)
}

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

type BlurTexture<T> = Mutex<Option<T>>;

#[derive(Debug)]
pub struct BlurState {
    pub id: Id,
    pub renderer_id: Option<ContextId<GlesTexture>>,
    pub src: Size<f64, Buffer>,
    pub offset: f64,
    pub passes: usize,
    pub region: Vec<Rectangle<i32, Logical>>,
    pub commit: CommitCounter,
}

unsafe impl Send for BlurState {}
unsafe impl Sync for BlurState {}

impl Default for BlurState {
    fn default() -> Self {
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
    /// The surface's own alpha, so the backdrop fades with it. A surface
    /// animating out otherwise keeps a fully opaque blur until it is destroyed,
    /// and the blur pops instead of fading.
    alpha: f32,
    uniforms: Vec<Uniform<'static>>,
}

impl BlurElement {
    pub fn from_state<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        state: &mut BlurState,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
        alpha: f32,
    ) -> Result<Option<Self>, R::Error> {
        let region = vec![Rectangle::from_size(geometry.size.to_i32_round())];

        Self::internal(
            renderer,
            state,
            geometry,
            &region,
            output_scale,
            radii,
            strength,
            alpha,
        )
    }

    pub fn from_surface<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        states: &SurfaceData,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
        alpha: f32,
    ) -> Result<Vec<Self>, R::Error> {
        // Blur disabled in config means no blur element at all, rather than a
        // zero-strength one that still pays for the blit and the passes.
        if !blur_enabled() {
            return Ok(Vec::new());
        }

        let mut blur_region_state = states.cached_state.get::<ComputedBlurRegionCachedState>();
        let blur = blur_region_state.current().clone();

        // A whole-surface request carries no region: the area is the surface
        // itself, resolved here so it stays correct as the surface resizes.
        let whole = [Rectangle::from_size(geometry.size.to_i32_round())];
        let region = if blur.whole_surface {
            whole.as_slice()
        } else if let Some(region) = blur.blur_region.as_deref() {
            region
        } else {
            return Ok(Vec::new());
        };

        // Version 2 lets the client ask for a strength. A hint: clamped to what
        // the blur can actually render.
        let strength = blur
            .blur_radius
            .map(|r| (r as usize).min(MAX_STEPS))
            .unwrap_or(strength);

        let state = states
            .data_map
            .get_or_insert_threadsafe::<Mutex<BlurState>, _>(Default::default);
        let mut state = state.lock().unwrap();

        // One element per rect, rather than one element clipped to all of them.
        //
        // Corner radii round the element's own geometry, so a single element
        // spanning every rect would round the outer bounds and leave each rect
        // square -- which for a full-screen layer surface means rounding the
        // screen corners and nothing else. A client that rounds each card to its
        // own radius needs an element per card. It also blits only each rect
        // rather than the whole surface.
        let mut elements = Vec::with_capacity(region.len());
        for (idx, rect) in region.iter().enumerate() {
            if rect.size.w <= 0 || rect.size.h <= 0 {
                continue;
            }

            // Per-rect radii if the client sent them, falling back to a single
            // entry applied to every rect, then to the caller's value.
            let rect_radii = blur
                .region_radii
                .get(idx)
                .or_else(|| blur.region_radii.first())
                .map(|r| {
                    // Half the shorter side, so adjacent arcs cannot overlap and
                    // erode the edge between them.
                    let half_min = (rect.size.w.min(rect.size.h) / 2).max(0) as u32;
                    r.map(|v| v.min(half_min).min(u8::MAX as u32) as u8)
                })
                .unwrap_or(radii);

            // The rect is surface-local; the element carries it as its own
            // geometry, so its region is that rect at the origin.
            let mut rect_geo = geometry;
            rect_geo.loc += rect.loc.to_f64();
            rect_geo.size = rect.size.to_f64();

            if let Some(element) = Self::internal(
                renderer,
                &mut state,
                rect_geo,
                &[Rectangle::from_size(rect.size)],
                output_scale,
                rect_radii,
                strength,
                alpha,
            )? {
                elements.push(element);
            }
        }

        Ok(elements)
    }

    pub fn internal<R: ImportAll + AsGlowRenderer>(
        renderer: &mut R,
        state: &mut BlurState,
        geometry: Rectangle<f64, Logical>,
        region: &[Rectangle<i32, Logical>],
        output_scale: f64,
        radii: [u8; 4],
        strength: usize,
        alpha: f32,
    ) -> Result<Option<Self>, R::Error> {
        if strength == 0 || geometry.size.w == 0. || geometry.size.h == 0. {
            return Ok(None);
        }

        let geo = geometry.to_physical_precise_round(output_scale);
        let mut extended_geo = geo;
        let radius = BLUR_PARAMS[(strength + 2).min(MAX_STEPS - 1)].extended_radius as f64;
        extended_geo.loc -= Point::<f64, Physical>::new(radius, radius);
        extended_geo.size += Size::<f64, Physical>::new(radius, radius).upscale(2.);

        // Keep the capture inside the framebuffer's origin.
        //
        // The capture is extended past the element so the blur kernel has
        // something to reach into, but a surface against the top or left edge
        // has no screen there to read. That read gets clamped, so the texture
        // receives content at its own origin while everything derived below
        // still assumes the margin exists -- the whole backdrop samples shifted
        // by the radius, and the far edge samples texels nothing ever wrote.
        //
        // Trimming the margin here instead keeps src, input_to_geo and the
        // texture size agreeing with what the blit can actually produce. The
        // far side needs no such trim: src only ever covers the element's own
        // extent, so an over-long right or bottom margin is read by the kernel
        // alone, where unwritten texels are transparent and feather away.
        for (loc, size) in [
            (&mut extended_geo.loc.x, &mut extended_geo.size.w),
            (&mut extended_geo.loc.y, &mut extended_geo.size.h),
        ] {
            if *loc < 0.0 {
                *size += *loc;
                *loc = 0.0;
            }
        }

        // compute input_to_geo so that it crops the extended capture radius
        let geo_scale = {
            let Scale { x, y } = geo.size / extended_geo.size;
            Affine2::from_scale(Vec2::new(x as f32, y as f32)).inverse()
        };
        let geo_translation = {
            let offset = geo.loc - extended_geo.loc;
            Affine2::from_translation(-Vec2::new(
                (offset.x / extended_geo.size.w) as f32,
                (offset.y / extended_geo.size.h) as f32,
            ))
        };
        let input_to_geo = Mat3::from(geo_scale * geo_translation);

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
            // The backdrop draws through the same clipping program, so it needs
            // `scale` too -- without it the corner mask never rounds the blur.
            Uniform::new("scale", output_scale as f32),
        ];

        let geometry = extended_geo.to_logical(output_scale);
        let extended_offset = Point::<f64, Physical>::new(radius, radius).to_logical(output_scale);

        let renderer_id = renderer.glow_renderer().context_id();
        let src = geometry.size.to_buffer(output_scale, Transform::Normal);
        let params = &BLUR_PARAMS[strength.min(MAX_STEPS - 1)];

        let dirty = !(state
            .renderer_id
            .as_ref()
            .is_some_and(|id| id == &renderer_id)
            && state.offset == params.offset
            && state.passes == params.passes
            && state.region == region
            && state.src == src);

        state.renderer_id = Some(renderer_id);
        state.offset = params.offset;
        state.passes = params.passes;
        state.region = region.to_vec();
        state.src = src;
        if dirty {
            state.commit.increment();
        }

        Ok(Some(BlurElement {
            id: state.id.clone(),
            commit: state.commit,
            src,
            geometry,
            extended_offset,
            scaling_shaders: BlurShaders::get(renderer),
            render_shader: ClippingShader::get(renderer),
            offset: state.offset,
            passes: state.passes,
            alpha,
            region: region
                .iter()
                .cloned()
                .map(|mut rect| {
                    rect.loc += extended_offset.to_i32_round();
                    rect
                })
                .collect(),
            uniforms,
        }))
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
        self.alpha
    }

    fn kind(&self) -> Kind {
        Kind::default()
    }

    fn is_framebuffer_effect(&self) -> bool {
        true
    }
}

impl<R: Renderer + AsGlowRenderer> RenderElement<R> for BlurElement
where
    R::TextureId: Send + 'static,
{
    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        let transform = frame.transformation();
        let tex_size = self.src.to_i32_round();
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);
        let gles_frame = BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame);
        let mut renderer = gles_frame.renderer();

        let texture_ref = cache.get_or_insert_threadsafe(BlurTexture::<R::TextureId>::default);
        let mut texture_entry = texture_ref.lock().unwrap();
        if texture_entry.as_ref().is_some_and(|tex| {
            tex.size() != tex_size
                || R::tex_to_gl(
                    &renderer.as_ref().context_id(),
                    texture_entry.as_ref().unwrap(),
                )
                .is_none()
        }) {
            texture_entry.take();
        }
        if texture_entry.is_none() {
            let gl_texture = renderer
                .as_mut()
                .create_buffer(Fourcc::Abgr8888, tex_size)
                .map_err(R::from_gles_error)?;
            *texture_entry = Some(R::tex_from_gl(&renderer.as_ref().context_id(), gl_texture));
        }

        let mut texture = R::tex_to_gl(
            &renderer.as_ref().context_id(),
            texture_entry.as_ref().unwrap(),
        )
        .unwrap();
        let mut off_texture = renderer
            .as_mut()
            .create_buffer(Fourcc::Abgr8888, tex_size)
            .map_err(R::from_gles_error)?;
        std::mem::drop(renderer);

        let sync = blit_from_active_fb(gles_frame, src, dst, transform, &mut texture)
            .map_err(R::from_gles_error)?;
        gles_frame.wait(&sync).map_err(R::from_gles_error)?;

        let mut textures = [&mut texture, &mut off_texture];
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
        let Some(texture) = cache.get::<BlurTexture<R::TextureId>>() else {
            return Err(R::from_gles_error(GlesError::BlitError));
        };
        let texture_ref = texture.lock().unwrap();

        if let Some(tex) = texture_ref.as_ref() {
            BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
                .override_default_tex_program(self.render_shader.clone(), self.uniforms.clone());
            frame.render_texture_from_to(
                tex,
                src,
                dst,
                &damage,
                opaque_regions,
                Transform::Normal,
                // Fade the backdrop with the surface. A surface animating out
                // otherwise keeps an opaque blur until it is destroyed, so the
                // blur pops rather than fading with everything else.
                self.alpha,
            )?;
            BorrowMut::<GlesFrame>::borrow_mut(<R as AsGlowRenderer>::glow_frame_mut(frame))
                .clear_tex_program_override();
        }
        Ok(())
    }
}

fn blit_from_active_fb(
    frame: &mut GlesFrame<'_, '_>,
    src: Rectangle<f64, Buffer>,
    dst: Rectangle<i32, Physical>,
    transform: Transform,
    to_texture: &mut GlesTexture,
) -> Result<SyncPoint, GlesError> {
    let tex_size = to_texture.size();
    let tex_size_phys = tex_size.to_logical(1, Transform::Normal).to_physical(1);
    let fb_size = frame.output_size();

    let mut renderer = frame.renderer();
    let mut fb = renderer.as_mut().bind(to_texture)?;
    let sync = {
        let mut subframe = renderer
            .as_mut()
            .render(&mut fb, tex_size_phys, Transform::Normal)?;
        subframe.clear(
            Color32F::TRANSPARENT,
            &[Rectangle::from_size(tex_size_phys)],
        )?;
        subframe.finish()?
    };

    if transform != Transform::Normal {
        // We need to copy to a temporary texture to do an actual
        // render pass with `render_texture_from_to` to do the rotation.
        // dst is in screen space, but we just want to do a 1:1 copy in
        // buffer space from dst of the current fb, so we need to undo any
        // transforms for the blit.
        let dst_phys = transform.transform_rect_in(dst, &fb_size);
        let dst_buffer = dst_phys
            .to_logical(1)
            .to_buffer(1, Transform::Normal, &Size::default());
        let mut tmp_texture = renderer
            .as_mut()
            .create_buffer(Fourcc::Abgr8888, dst_buffer.size)?;
        let mut fb_tmp = renderer.as_mut().bind(&mut tmp_texture)?;
        std::mem::drop(renderer);
        frame.wait(&sync)?;

        let sync = frame.blit_to(
            &mut fb_tmp,
            dst_phys,
            Rectangle::from_size(dst_phys.size),
            TextureFilter::Linear,
        )?;
        frame.wait(&sync)?;
        std::mem::drop(fb_tmp);

        // now we bind the target texture with `Transform::Normal`
        // and render the temporary texture with inverse transform
        // into src.
        let mut renderer = frame.renderer();
        let mut frame = renderer
            .as_mut()
            .render(&mut fb, tex_size_phys, Transform::Normal)?;
        frame.wait(&sync)?;
        Frame::render_texture_from_to(
            &mut frame,
            &tmp_texture,
            Rectangle::from_size(dst_buffer.size.to_f64()),
            src.to_logical(1., Transform::Normal, &Size::default())
                .to_physical(1.)
                .to_i32_round(),
            &[Rectangle::from_size(dst.size)],
            &[Rectangle::from_size(dst.size)],
            transform.invert(),
            1.0,
        )?;
        std::mem::drop(tmp_texture);
        frame.finish()
    } else {
        std::mem::drop(renderer);
        frame.wait(&sync)?;
        frame.blit_to(
            &mut fb,
            dst,
            src.to_logical(1., Transform::Normal, &Size::default())
                .to_physical(1.)
                .to_i32_round(),
            TextureFilter::Linear,
        )
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
