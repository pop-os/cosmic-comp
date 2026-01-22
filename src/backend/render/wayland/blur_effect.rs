use std::{
    borrow::{Borrow, BorrowMut},
    collections::HashMap,
    ptr,
    sync::{Arc, Mutex},
};

use cgmath::{Matrix, Matrix3, Vector2};
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            ContextId, Frame, ImportAll, Offscreen, Renderer, Texture,
            element::{Element, Id, Kind, RenderElement},
            gles::{
                GlesError, GlesFrame, GlesRenderer, GlesTexProgram, GlesTexture, Uniform,
                UniformValue, ffi, link_program,
            },
            utils::{CommitCounter, DamageSet},
        },
    },
    reexports::glow::COLOR_BUFFER_BIT,
    utils::{Buffer, Logical, Physical, Point, Rectangle, Scale, Size, Transform},
    wayland::compositor::SurfaceData,
};
use tracing::warn;

use crate::backend::render::{
    element::{AsGlowRenderer, FromGlesError},
    wayland::clipped_surface::ClippingShader,
};

pub static BLUR_DOWNSAMPLE_SHADER: &str = include_str!("../shaders/blur_downsample.frag");
pub static BLUR_UPSAMPLE_SHADER: &str = include_str!("../shaders/blur_upsample.frag");
pub static BLUR_VERTEX_SHADER: &str = include_str!("../shaders/blur.vert");

const PASSES: usize = 4;
const OFFSET: f64 = 2.;
const NOISE: f32 = 0.04;

#[derive(Debug, Clone)]
pub struct BlurShaders {
    down: BlurProgram,
    up: BlurProgram,
}

impl BlurShaders {
    pub fn compile(renderer: &mut GlesRenderer) -> Result<BlurShaders, GlesError> {
        renderer.with_context(move |gl| {
            let up = BlurProgram::new(gl, BLUR_UPSAMPLE_SHADER)?;
            let down = BlurProgram::new(gl, BLUR_DOWNSAMPLE_SHADER)?;

            Ok(BlurShaders { up, down })
        })?
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

#[derive(Debug, Clone)]
pub struct BlurProgram {
    program: u32,
    u_matrix: i32,
    u_tex_matrix: i32,
    u_tex: i32,
    u_half_pixel: i32,
    u_offset: i32,
    a_vert: i32,
}

impl BlurProgram {
    pub fn new(gl: &ffi::Gles2, frag: &str) -> Result<Self, GlesError> {
        let program = unsafe { link_program(gl, BLUR_VERTEX_SHADER, frag)? };

        unsafe {
            Ok(BlurProgram {
                program,
                u_matrix: gl.GetUniformLocation(program, c"matrix".as_ptr() as *const _),
                u_tex_matrix: gl.GetUniformLocation(program, c"tex_matrix".as_ptr() as *const _),
                u_tex: gl.GetUniformLocation(program, c"tex".as_ptr() as *const _),
                u_half_pixel: gl.GetUniformLocation(program, c"half_pixel".as_ptr() as *const _),
                u_offset: gl.GetUniformLocation(program, c"offset".as_ptr() as *const _),
                a_vert: gl.GetAttribLocation(program, c"vert".as_ptr() as *const _),
            })
        }
    }
}

struct BlurState {
    id: Id,
    textures: Arc<Mutex<HashMap<ContextId<GlesTexture>, GlesTexture>>>,
    offset: f64,
    passes: usize,
}

unsafe impl Send for BlurState {}
unsafe impl Sync for BlurState {}

impl BlurState {
    fn new() -> Self {
        BlurState {
            id: Id::new(),
            textures: Arc::new(Mutex::new(HashMap::new())),
            offset: 0.,
            passes: 0,
        }
    }
}

pub struct BlurElement {
    id: Id,
    geometry: Rectangle<f64, Logical>,
    scaling_shaders: BlurShaders,
    render_shader: GlesTexProgram,
    texture: GlesTexture,
    dirty: bool,
    textures: Arc<Mutex<HashMap<ContextId<GlesTexture>, GlesTexture>>>,
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
    ) -> Result<Option<Self>, R::Error>
    where
        R::Error: FromGlesError,
    {
        let mut extended_geometry = geometry;
        let radius = OFFSET * 2.0f64.powf(PASSES as f64);
        extended_geometry.loc -= Point::new(radius, radius);
        extended_geometry.size += Size::new(radius, radius).upscale(2.);

        // compute input_to_geo so that it crops the extended capture radius
        let geo_scale = {
            let Scale { x, y } = extended_geometry.size.to_f64() / geometry.size.to_f64();
            Matrix3::from_nonuniform_scale(x as f32, y as f32)
        };
        let geo_translation = {
            let offset = (extended_geometry.loc - geometry.loc).to_f64();
            Matrix3::from_translation(Vector2::new(
                (offset.x / geometry.size.w as f64) as f32,
                (offset.y / geometry.size.h as f64) as f32,
            ))
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
            extended_geometry,
            output_scale,
            &mut *state.lock().unwrap(),
            uniforms,
        )?))
    }

    fn from_state<R: AsGlowRenderer>(
        renderer: &mut R,
        geometry: Rectangle<f64, Logical>,
        output_scale: f64,
        state: &mut BlurState,
        uniforms: Vec<Uniform<'static>>,
    ) -> Result<Self, R::Error>
    where
        R::Error: FromGlesError,
    {
        let renderer_id = renderer.glow_renderer().context_id();
        let mut textures = state.textures.lock().unwrap();
        let tex_size = geometry
            .size
            .to_buffer(output_scale, Transform::Normal)
            .to_i32_ceil();

        let (texture, dirty) = if let Some(tex) = textures
            .get(&renderer_id)
            .filter(|tex| tex.size() == tex_size)
        {
            (tex.clone(), false)
        } else {
            let tex = renderer
                .glow_renderer_mut()
                .create_buffer(Fourcc::Xbgr8888, tex_size)
                .map_err(FromGlesError::from_gles_error)?;
            textures.insert(renderer_id.clone(), tex);
            (textures.get(&renderer_id).cloned().unwrap(), true)
        };

        state.offset = OFFSET * output_scale; // we want the blur result to be stable across scaling;
        state.passes = PASSES;

        Ok(BlurElement {
            id: state.id.clone(),
            geometry,
            scaling_shaders: BlurShaders::get(renderer),
            render_shader: ClippingShader::get(renderer),
            texture: texture,
            dirty,
            textures: state.textures.clone(),
            offset: state.offset,
            passes: state.passes,
            uniforms,
        })
    }
}

impl Element for BlurElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        Default::default()
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        Rectangle::from_size(self.texture.size().to_f64())
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
        if commit.is_none() || self.dirty {
            DamageSet::from_slice(&[self.geometry.to_physical_precise_round(scale)])
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

impl<R: Renderer + AsGlowRenderer> RenderElement<R> for BlurElement
where
    R::Error: FromGlesError,
{
    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
    ) -> Result<(), <R>::Error> {
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);
        let gles_frame = BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame);

        self.textures
            .lock()
            .unwrap()
            .retain(|id, _| *id == gles_frame.context_id());
        gles_frame
            .with_context(|gl| unsafe {
                let tex_size = self.texture.size();
                let mut current_fbo = 0i32;
                let mut viewport = [0i32; 4];
                gl.GetIntegerv(ffi::FRAMEBUFFER_BINDING, &mut current_fbo as *mut _);
                gl.GetIntegerv(ffi::VIEWPORT, viewport.as_mut_ptr());
                gl.Viewport(0, 0, tex_size.w, tex_size.h);
                gl.Disable(ffi::SCISSOR_TEST);
                gl.Disable(ffi::BLEND);

                let mut textures = [0; 2];
                textures[0] = self.texture.tex_id();
                gl.GenTextures(1, textures.as_mut_ptr().add(1));
                init_off_texture(gl, textures[1], tex_size)?;
                let mut fbo = 0u32;
                gl.GenFramebuffers(1, &mut fbo as *mut _);

                let mut res = blit_from_active_fb(src, dst, gl, fbo, textures[0]);
                if res.is_ok() {
                    res = render_blur(
                        gl,
                        fbo,
                        &self.scaling_shaders,
                        &mut textures,
                        tex_size,
                        self.offset,
                        self.passes,
                    );
                }

                gl.BindFramebuffer(ffi::FRAMEBUFFER, current_fbo as u32);
                gl.Viewport(viewport[0], viewport[1], viewport[2], viewport[3]);
                gl.Enable(ffi::SCISSOR_TEST);
                gl.Enable(ffi::BLEND);

                let status = gl.CheckFramebufferStatus(ffi::FRAMEBUFFER);
                if status != ffi::FRAMEBUFFER_COMPLETE {
                    return Err(GlesError::FramebufferBindingError);
                }

                gl.DeleteFramebuffers(1, &mut fbo as *mut _);
                gl.DeleteTextures(1, textures.as_mut_ptr().add(1));

                res
            })
            .map_err(FromGlesError::from_gles_error)?
            .map_err(FromGlesError::from_gles_error)
    }

    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
    ) -> Result<(), R::Error> {
        let glow_frame = <R as AsGlowRenderer>::glow_frame_mut(frame);

        BorrowMut::<GlesFrame<'_, '_>>::borrow_mut(glow_frame)
            .render_texture_from_to(
                &self.texture,
                src,
                dst,
                damage,
                opaque_regions,
                Transform::Normal,
                1.0,
                Some(&self.render_shader),
                &self.uniforms,
            )
            .map_err(FromGlesError::from_gles_error)
    }
}

fn init_off_texture(
    gl: &ffi::Gles2,
    texture: u32,
    size: Size<i32, Buffer>,
) -> Result<(), GlesError> {
    unsafe {
        gl.ActiveTexture(ffi::TEXTURE0);
        gl.BindTexture(ffi::TEXTURE_2D, texture);
        gl.TexImage2D(
            ffi::TEXTURE_2D,
            0,
            ffi::RGBA8 as _,
            size.w,
            size.h,
            0,
            ffi::RGBA,
            ffi::UNSIGNED_BYTE,
            ptr::null(),
        );
    }

    Ok(())
}

fn blit_from_active_fb(
    src: Rectangle<f64, Buffer>,
    dst: Rectangle<i32, Physical>,
    gl: &ffi::Gles2,
    fbo: u32,
    texture: u32,
) -> Result<(), GlesError> {
    unsafe {
        let src = src.to_i32_round();
        gl.GetError(); // clear error

        gl.BindFramebuffer(ffi::DRAW_FRAMEBUFFER, fbo);
        gl.FramebufferTexture2D(
            ffi::DRAW_FRAMEBUFFER,
            ffi::COLOR_ATTACHMENT0,
            ffi::TEXTURE_2D,
            texture,
            0,
        );

        let status = gl.CheckFramebufferStatus(ffi::FRAMEBUFFER);
        if status != ffi::FRAMEBUFFER_COMPLETE {
            return Err(GlesError::FramebufferBindingError);
        }

        gl.Clear(COLOR_BUFFER_BIT);
        gl.BlitFramebuffer(
            dst.loc.x,
            dst.loc.y,
            dst.loc.x + dst.size.w,
            dst.loc.y + dst.size.h,
            src.loc.x,
            src.loc.y,
            src.loc.x + src.size.w,
            src.loc.y + src.size.h,
            ffi::COLOR_BUFFER_BIT,
            ffi::LINEAR,
        );

        let err = gl.GetError();
        if err != ffi::NO_ERROR {
            return Err(GlesError::BlitError);
        }

        Ok(())
    }
}

fn render_blur(
    gl: &ffi::Gles2,
    fbo: u32,
    shaders: &BlurShaders,
    textures: &mut [u32],
    buffer_size: Size<i32, Buffer>,
    offset: f64,
    passes: usize,
) -> Result<(), GlesError> {
    unsafe {
        gl.BindFramebuffer(ffi::FRAMEBUFFER, fbo);
        let projection =
            // shift from (0, 0) -> (-1, -1)
            Matrix3::from_translation(Vector2::new(-1.0f32, -1.0))
            // fill the whole framebuffer
            * Matrix3::from_scale(2.0);

        for i in 0..passes {
            gl.FramebufferTexture2D(
                ffi::FRAMEBUFFER,
                ffi::COLOR_ATTACHMENT0,
                ffi::TEXTURE_2D,
                textures[1],
                0,
            );
            let status = gl.CheckFramebufferStatus(ffi::FRAMEBUFFER);
            if status != ffi::FRAMEBUFFER_COMPLETE {
                return Err(GlesError::FramebufferBindingError);
            }
            let projection_matrix = projection * Matrix3::from_scale(1.0 / (1 << (i + 1)) as f32);
            let texture_matrix = Matrix3::from_scale(1.0 / (1 << i) as f32);
            let tex_size = buffer_size.downscale(1 << i);
            let half_pixel = [
                1.0 / (tex_size.w as f32 / 2.0),
                1.0 / (tex_size.h as f32 / 2.0),
            ];

            render_blur_pass(
                gl,
                &projection_matrix,
                &texture_matrix,
                textures[0],
                &shaders.down,
                half_pixel,
                offset / (1 << i) as f64,
            )?;
            textures.swap(0, 1);
        }

        for i in 0..passes {
            gl.FramebufferTexture2D(
                ffi::FRAMEBUFFER,
                ffi::COLOR_ATTACHMENT0,
                ffi::TEXTURE_2D,
                textures[1],
                0,
            );
            let status = gl.CheckFramebufferStatus(ffi::FRAMEBUFFER);
            if status != ffi::FRAMEBUFFER_COMPLETE {
                return Err(GlesError::FramebufferBindingError);
            }
            let projection_matrix =
                projection * Matrix3::from_scale(1.0 / (1 << (PASSES - 1 - i)) as f32);
            let texture_matrix = Matrix3::from_scale(1.0 / (1 << (PASSES - i)) as f32);
            let tex_size = buffer_size.downscale(1 << (PASSES - i));
            let half_pixel = [
                1.0 / (tex_size.w as f32 / 2.0),
                1.0 / (tex_size.h as f32 / 2.0),
            ];

            render_blur_pass(
                gl,
                &projection_matrix,
                &texture_matrix,
                textures[0],
                &shaders.up,
                half_pixel,
                offset / (1 << (PASSES - i)) as f64,
            )?;
            textures.swap(0, 1);
        }

        // textures always end up the right way around with `self.texture` containing our final render,
        // since we render PASSES * 2 (downscale and upscale), so the number of swaps is always even.
        Ok(())
    }
}

fn render_blur_pass(
    gl: &ffi::Gles2,
    projection_matrix: &Matrix3<f32>,
    texture_matrix: &Matrix3<f32>,
    texture: u32,
    shader: &BlurProgram,
    half_pixel: [f32; 2],
    offset: f64,
) -> Result<(), GlesError> {
    unsafe {
        gl.GetError(); // clear
        gl.Clear(ffi::COLOR_BUFFER_BIT);

        gl.ActiveTexture(ffi::TEXTURE0);
        gl.BindTexture(ffi::TEXTURE_2D, texture);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MIN_FILTER, ffi::LINEAR as i32);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_MAG_FILTER, ffi::LINEAR as i32);
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

        gl.Disable(ffi::BLEND);
        let vertices = [
            0.0f32, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0,
        ];

        gl.UseProgram(shader.program);
        gl.Uniform1i(shader.u_tex, 0);
        gl.Uniform1f(shader.u_offset, offset as f32);
        gl.Uniform2f(shader.u_half_pixel, half_pixel[0], half_pixel[1]);
        gl.UniformMatrix3fv(shader.u_matrix, 1, ffi::FALSE, projection_matrix.as_ptr());
        gl.UniformMatrix3fv(shader.u_tex_matrix, 1, ffi::FALSE, texture_matrix.as_ptr());

        gl.EnableVertexAttribArray(shader.a_vert as u32);
        gl.BindBuffer(ffi::ARRAY_BUFFER, 0);
        gl.VertexAttribPointer(
            shader.a_vert as u32,
            2,
            ffi::FLOAT,
            ffi::FALSE,
            0,
            vertices.as_ptr() as *const _,
        );

        gl.DrawArrays(ffi::TRIANGLES, 0, 6);

        let err = gl.GetError();

        gl.BindTexture(ffi::TEXTURE_2D, 0);
        gl.DisableVertexAttribArray(shader.a_vert as u32);
        gl.Enable(ffi::BLEND);

        if err != ffi::NO_ERROR {
            warn!("Render error: {}", err);
            return Err(GlesError::ShaderCompileError);
        }
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::REPEAT as i32);
        gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::REPEAT as i32);
        Ok(())
    }
}
