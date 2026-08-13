// SPDX-License-Identifier: GPL-3.0-only

//! FidelityFX Super Resolution 1.0 as a two-pass fragment upscale.
//!
//! EASU resamples the source to the presentation size along estimated edge
//! directions; RCAS then restores high-frequency detail at that size with a
//! limiter derived from local headroom. See `shaders/fsr_easu.frag` and
//! `shaders/fsr_rcas.frag`.
//!
//! Unlike [`super::nis`] this is fragment work, so it needs no GLES 3.1 and has
//! no ratio ceiling — quality falls off past 2x rather than the filter becoming
//! undefined.

use std::borrow::BorrowMut;

use smithay::{
    backend::renderer::{
        Bind, Color32F, Frame, Renderer, Texture,
        gles::{GlesRenderer, GlesTexture, Uniform},
    },
    utils::{Buffer as BufferCoords, Physical, Rectangle, Size, Transform},
};

use super::{FsrEasuShader, FsrRcasShader, element::AsGlowRenderer};

/// Why a frame did not take the FSR path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unavailable {
    /// `COSMIC_FSR=0`.
    Disabled,
    /// A pass failed earlier in this session.
    Failed,
    /// Nothing to upscale — see [`super::nis::Unavailable::NoUpscale`].
    NoUpscale,
    /// A zero or negative dimension.
    DegenerateSize,
}

static FSR_DISABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// `COSMIC_FSR=0` forces the plain scaled draw.
fn fsr_allowed() -> bool {
    static ALLOWED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ALLOWED.get_or_init(|| !matches!(std::env::var("COSMIC_FSR").as_deref(), Ok("0")))
}

/// Whether FSR could run at all here. The shaders are compiled unconditionally
/// by `init_shaders`, so this is only the kill switch and the failure latch.
pub fn available() -> Result<(), Unavailable> {
    if !fsr_allowed() {
        return Err(Unavailable::Disabled);
    }
    if FSR_DISABLED.load(std::sync::atomic::Ordering::Relaxed) {
        return Err(Unavailable::Failed);
    }
    Ok(())
}

/// RCAS takes a linear factor; FSR specifies sharpening as a stop value where 0
/// is the maximum. Maps a 0..=1 slider onto stops 2..=0.
pub fn sharpness_to_linear(sharpness: f32) -> f32 {
    let stop = 2.0 * (1.0 - sharpness.clamp(0.0, 1.0));
    (-stop).exp2()
}

/// Allocates one render target. Both passes draw into a bound framebuffer, so
/// unlike the NIS target these are ordinary `Offscreen` textures.
pub fn create_target<R>(
    renderer: &mut R,
    size: Size<i32, Physical>,
) -> Result<GlesTexture, Unavailable>
where
    R: AsGlowRenderer,
{
    use smithay::backend::allocator::Fourcc;
    use smithay::backend::renderer::Offscreen;

    if size.w <= 0 || size.h <= 0 {
        return Err(Unavailable::DegenerateSize);
    }
    let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
    Offscreen::<GlesTexture>::create_buffer(renderer, Fourcc::Abgr8888, buffer_size).map_err(|_| {
        tracing::warn!("could not allocate an FSR target");
        Unavailable::Failed
    })
}

/// Upscales `src` into `target`, using `intermediate` for the EASU result.
///
/// Both scratch textures must be `dst_size` and come from [`create_target`].
pub fn upscale<R>(
    renderer: &mut R,
    src: &GlesTexture,
    intermediate: &mut GlesTexture,
    target: &mut GlesTexture,
    dst_size: Size<i32, Physical>,
    sharpness: f32,
) -> Result<(), Unavailable>
where
    R: AsGlowRenderer,
{
    available()?;

    let src_buffer = src.size();
    let src_size = Size::<i32, Physical>::from((src_buffer.w, src_buffer.h));
    if src_size.w <= 0 || src_size.h <= 0 || dst_size.w <= 0 || dst_size.h <= 0 {
        return Err(Unavailable::DegenerateSize);
    }
    if src_size.w >= dst_size.w && src_size.h >= dst_size.h {
        return Err(Unavailable::NoUpscale);
    }

    let easu = FsrEasuShader::get(renderer);
    let rcas = FsrRcasShader::get(renderer);

    let result = (|| -> Result<(), smithay::backend::renderer::gles::GlesError> {
        pass(
            renderer,
            &easu,
            src,
            src_size,
            intermediate,
            dst_size,
            &[
                Uniform::new("src_size", (src_size.w as f32, src_size.h as f32)),
                Uniform::new("dst_size", (dst_size.w as f32, dst_size.h as f32)),
            ],
        )?;
        pass(
            renderer,
            &rcas,
            intermediate,
            dst_size,
            target,
            dst_size,
            &[
                Uniform::new(
                    "inv_size",
                    (1.0 / dst_size.w as f32, 1.0 / dst_size.h as f32),
                ),
                Uniform::new("sharpness", sharpness_to_linear(sharpness)),
            ],
        )
    })();

    if let Err(err) = result {
        // A driver that fails one pass keeps failing, so stop retrying.
        FSR_DISABLED.store(true, std::sync::atomic::Ordering::Relaxed);
        tracing::warn!(?err, "FSR pass failed, presenting without upscaling");
        return Err(Unavailable::Failed);
    }
    Ok(())
}

/// One full-target draw of `src` into `dst` through `program`.
fn pass<R>(
    renderer: &mut R,
    program: &smithay::backend::renderer::gles::GlesTexProgram,
    src: &GlesTexture,
    src_size: Size<i32, Physical>,
    dst: &mut GlesTexture,
    dst_size: Size<i32, Physical>,
    uniforms: &[Uniform<'_>],
) -> Result<(), smithay::backend::renderer::gles::GlesError>
where
    R: AsGlowRenderer,
{
    let gles: &mut GlesRenderer = BorrowMut::borrow_mut(renderer.glow_renderer_mut());
    let full = Rectangle::from_size(dst_size);

    let mut fb = gles.bind(dst)?;
    let sync = {
        let mut frame = gles.render(&mut fb, dst_size, Transform::Normal)?;
        frame.clear(Color32F::TRANSPARENT, &[full])?;
        let src_rect: Rectangle<f64, BufferCoords> =
            Rectangle::from_size((src_size.w as f64, src_size.h as f64).into());
        frame.render_texture_from_to(
            src,
            src_rect,
            full,
            &[full],
            &[full],
            Transform::Normal,
            1.0,
            Some(program),
            uniforms,
        )?;
        frame.finish()?
    };
    std::mem::drop(fb);
    gles.wait(&sync)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// 0 is the gentlest setting and 1 the strongest, and RCAS's factor must
    /// rise across that range or the slider reads backwards.
    #[test]
    fn sharpness_maps_onto_rcas_stops() {
        assert!((sharpness_to_linear(1.0) - 1.0).abs() < 1e-6);
        assert!((sharpness_to_linear(0.5) - 0.5).abs() < 1e-6);
        assert!((sharpness_to_linear(0.0) - 0.25).abs() < 1e-6);

        let mut previous = 0.0;
        for step in 0..=20 {
            let v = sharpness_to_linear(step as f32 / 20.0);
            assert!(v > previous, "sharpness fell at step {step}");
            previous = v;
        }
    }

    #[test]
    fn sharpness_is_clamped() {
        assert_eq!(sharpness_to_linear(-1.0), sharpness_to_linear(0.0));
        assert_eq!(sharpness_to_linear(2.0), sharpness_to_linear(1.0));
    }
}
