// SPDX-License-Identifier: GPL-3.0-only

//! NVIDIA Image Scaling (NVScaler) as a GLES 3.1 compute pass.
//!
//! Upscales a game's buffer to its presentation rect with edge-directed
//! filtering instead of a bilinear stretch. Takes an imported texture, writes an
//! upscaled copy; knows nothing of surfaces, workspaces or game mode. See
//! `shaders/nis_scaler.comp` for the algorithm.

use std::{
    borrow::BorrowMut,
    sync::atomic::{AtomicBool, Ordering},
};

use smithay::{
    backend::renderer::{
        Texture,
        gles::{GlesComputeProgram, GlesError, GlesRenderer, GlesTexture, ffi},
    },
    utils::{Physical, Size, Transform},
};

use super::{element::AsGlowRenderer, nis_coefficients};

/// Compute source for the scaler, with `//_COEFFICIENTS_` still to be replaced.
static NIS_SCALER_SHADER: &str = include_str!("./shaders/nis_scaler.comp");

/// Taps per phase the shader uses. The SDK's tables are 8 wide but its scaler is
/// 6-tap and the last two columns are zero; [`coefficients_are_six_tap`] holds
/// the file to that.
const SHADER_FILTER_SIZE: usize = 6;

/// Destination pixels per workgroup axis. Must match `BLOCK` in the shader.
const WORKGROUP: i32 = 8;

/// NIS is a 1x..2x upscaler; past that the 6-tap support cannot reach far
/// enough. Source-over-destination, so 2x is 0.5 and 1:1 is 1.0.
const SCALE_MIN: f64 = 0.5;
const SCALE_MAX: f64 = 1.0;

/// Set once a dispatch has failed, to stop retrying it every frame.
static NIS_DISABLED: AtomicBool = AtomicBool::new(false);

/// `COSMIC_NIS=0` forces the plain scaled draw, so the two paths are comparable
/// on one binary and there is an escape hatch for a bad driver.
fn nis_allowed() -> bool {
    static ALLOWED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ALLOWED.get_or_init(|| !matches!(std::env::var("COSMIC_NIS").as_deref(), Ok("0")))
}

/// Why a frame did not take the NIS path, for logging and the profiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unavailable {
    /// `COSMIC_NIS=0`.
    Disabled,
    /// A dispatch failed earlier in this session.
    Failed,
    /// The context is pre-3.1, or the shader did not compile.
    NoProgram,
    /// The requested ratio is outside NIS's 1x..2x range.
    OutOfRange,
    /// The source or destination has a zero or negative dimension.
    DegenerateSize,
}

/// A ratio NIS is defined for. Constructing one *is* the range check, so no
/// dispatch can be issued for a ratio the filter would garble.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScaleRatio {
    x: f32,
    y: f32,
}

impl ScaleRatio {
    /// Both axes must be in range; an axis that is out of range is the one that
    /// would break, whatever the other does.
    pub fn new(src: Size<i32, Physical>, dst: Size<i32, Physical>) -> Result<Self, Unavailable> {
        if src.w <= 0 || src.h <= 0 || dst.w <= 0 || dst.h <= 0 {
            return Err(Unavailable::DegenerateSize);
        }
        let x = src.w as f64 / dst.w as f64;
        let y = src.h as f64 / dst.h as f64;
        if !(SCALE_MIN..=SCALE_MAX).contains(&x) || !(SCALE_MIN..=SCALE_MAX).contains(&y) {
            return Err(Unavailable::OutOfRange);
        }
        Ok(ScaleRatio {
            x: x as f32,
            y: y as f32,
        })
    }
}

/// The scaler's tunables, from `NVScalerUpdateConfig` (SDR path only). Uploaded
/// as uniforms so sharpness is adjustable without recompiling.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NisConfig {
    detect_ratio: f32,
    detect_thres: f32,
    min_contrast_ratio: f32,
    ratio_norm: f32,
    contrast_boost: f32,
    eps: f32,
    sharp_start_y: f32,
    sharp_scale_y: f32,
    sharp_strength_min: f32,
    sharp_strength_scale: f32,
    sharp_limit_min: f32,
    sharp_limit_scale: f32,
}

impl Default for NisConfig {
    fn default() -> Self {
        Self::new(DEFAULT_SHARPNESS)
    }
}

/// The SDK's slider midpoint, the neutral setting its constants centre on.
pub const DEFAULT_SHARPNESS: f32 = 0.5;

impl NisConfig {
    /// Derive the tunables for `sharpness` in `0.0..=1.0`.
    pub fn new(sharpness: f32) -> Self {
        let sharpness = sharpness.clamp(0.0, 1.0);
        // Remapped to -0.5..=0.5, with different gains per half so 0% is no
        // sharpening and 100% stops short of haloing.
        let slider = sharpness - 0.5;
        let (max_scale, min_scale, limit_scale) = if slider >= 0.0 {
            (1.25f32, 1.25f32, 1.25f32)
        } else {
            (1.75f32, 1.0f32, 1.0f32)
        };

        let min_contrast_ratio = 2.0f32;
        let max_contrast_ratio = 10.0f32;
        let sharp_start_y = 0.45f32;
        let sharp_end_y = 0.9f32;

        let sharp_strength_min = f32::max(0.0, 0.4 + slider * min_scale * 1.2);
        let sharp_strength_max = 1.6 + slider * max_scale * 1.8;
        let sharp_limit_min = f32::max(0.1, 0.14 + slider * limit_scale * 0.32);
        let sharp_limit_max = 0.5 + slider * limit_scale * 0.6;

        NisConfig {
            detect_ratio: 2.0 * 1127.0 / 1024.0,
            detect_thres: 64.0 / 1024.0,
            min_contrast_ratio,
            ratio_norm: 1.0 / (max_contrast_ratio - min_contrast_ratio),
            contrast_boost: 1.0,
            eps: 1.0 / 255.0,
            sharp_start_y,
            sharp_scale_y: 1.0 / (sharp_end_y - sharp_start_y),
            sharp_strength_min,
            sharp_strength_scale: sharp_strength_max - sharp_strength_min,
            sharp_limit_min,
            sharp_limit_scale: sharp_limit_max - sharp_limit_min,
        }
    }
}

/// Renders one row of a baked GLSL array literal.
fn bake_table(out: &mut String, name: &str, table: &[[f32; nis_coefficients::FILTER_SIZE]]) {
    let len = table.len() * SHADER_FILTER_SIZE;
    out.push_str(&format!("const float {name}[{len}] = float[{len}](\n"));
    for (phase, row) in table.iter().enumerate() {
        out.push_str("    ");
        for (tap, value) in row.iter().take(SHADER_FILTER_SIZE).enumerate() {
            let last = phase == table.len() - 1 && tap == SHADER_FILTER_SIZE - 1;
            // Every value needs a decimal point: GLSL will not convert an
            // integer literal in a float[] constructor. `{:?}` on f32 emits one.
            out.push_str(&format!("{value:?}{}", if last { "" } else { ", " }));
        }
        out.push('\n');
    }
    out.push_str(");\n");
}

/// The token the tables replace, split so the template can be searched for it.
const COEFFICIENT_PLACEHOLDER: &str = concat!("//", "_COEFFICIENTS_");

/// The compute source with the coefficient tables substituted in.
///
/// Panics unless the template holds exactly one placeholder — a property of a
/// file in this repo, and the alternative is a shader that only fails on a
/// machine with a GLES 3.1 context.
pub fn shader_source() -> String {
    assert_eq!(
        NIS_SCALER_SHADER.matches(COEFFICIENT_PLACEHOLDER).count(),
        1,
        "nis_scaler.comp must mention the coefficient placeholder exactly once"
    );
    let mut baked = String::with_capacity(16 * 1024);
    bake_table(&mut baked, "kCoefScale", &nis_coefficients::COEF_SCALE);
    baked.push('\n');
    bake_table(&mut baked, "kCoefUsm", &nis_coefficients::COEF_USM);
    NIS_SCALER_SHADER.replace(COEFFICIENT_PLACEHOLDER, &baked)
}

/// The compiled scaler, present only on GLES 3.1+ contexts. Absence means
/// present without NIS.
pub struct NisShader(pub GlesComputeProgram);

impl NisShader {
    /// Failure is logged and dropped rather than failing renderer init: NIS is
    /// an enhancement, and a 2.0 context must still get a working desktop.
    pub fn compile(renderer: &mut GlesRenderer) -> Option<Self> {
        if !renderer.supports_compute() {
            tracing::info!("Context is pre-GLES 3.1, NIS upscaling unavailable");
            return None;
        }
        match renderer.compile_compute_program(&shader_source()) {
            Ok(program) => {
                tracing::info!("NIS upscaler compiled");
                Some(NisShader(program))
            }
            Err(err) => {
                tracing::warn!(?err, "NIS shader failed to compile, upscaling unavailable");
                None
            }
        }
    }

    fn get<R: AsGlowRenderer>(renderer: &R) -> Option<GlesComputeProgram> {
        std::borrow::Borrow::<GlesRenderer>::borrow(renderer.glow_renderer())
            .egl_context()
            .user_data()
            .get::<NisShader>()
            .map(|s| s.0)
    }
}

/// Whether NIS could run at all here, so a caller can decide how to present
/// without allocating a texture it may not use.
pub fn available<R: AsGlowRenderer>(renderer: &R) -> Result<(), Unavailable> {
    if !nis_allowed() {
        return Err(Unavailable::Disabled);
    }
    if NIS_DISABLED.load(Ordering::Relaxed) {
        return Err(Unavailable::Failed);
    }
    NisShader::get(renderer)
        .map(|_| ())
        .ok_or(Unavailable::NoProgram)
}

/// Allocates a destination the scaler can write to.
///
/// `glBindImageTexture` rejects mutable-format textures, hence
/// `create_compute_buffer` rather than the `Offscreen` impl. Immutable storage
/// cannot be resized, so the caller keeps this and reallocates only on a size
/// change.
pub fn create_target<R>(
    renderer: &mut R,
    size: Size<i32, Physical>,
) -> Result<GlesTexture, Unavailable>
where
    R: AsGlowRenderer,
{
    if size.w <= 0 || size.h <= 0 {
        return Err(Unavailable::DegenerateSize);
    }
    let buffer_size = size.to_logical(1).to_buffer(1, Transform::Normal);
    let gles: &mut GlesRenderer = BorrowMut::borrow_mut(renderer.glow_renderer_mut());
    gles.create_compute_buffer(buffer_size).map_err(|err| {
        tracing::warn!(?err, "could not allocate a NIS target");
        Unavailable::NoProgram
    })
}

/// Upscales `src` into `dst`, which must have come from [`create_target`].
/// `flip_y` is the source's `is_y_inverted`, undone while sampling.
///
/// On `Err` the caller presents the source directly. Only a dispatch failure is
/// terminal for the session; a range rejection is re-checked every frame, since
/// the game can be reconfigured to another render resolution at any time.
pub fn upscale<R>(
    renderer: &mut R,
    src: &GlesTexture,
    flip_y: bool,
    dst: &GlesTexture,
    config: NisConfig,
) -> Result<(), Unavailable>
where
    R: AsGlowRenderer,
{
    available(renderer)?;

    let src_buffer = src.size();
    let dst_buffer = dst.size();
    let src_size = Size::<i32, Physical>::from((src_buffer.w, src_buffer.h));
    let dst_size = Size::<i32, Physical>::from((dst_buffer.w, dst_buffer.h));
    let ratio = ScaleRatio::new(src_size, dst_size)?;
    let program = NisShader::get(renderer).ok_or(Unavailable::NoProgram)?;

    if let Err(err) = dispatch(
        renderer, &program, src, flip_y, dst, src_size, dst_size, ratio, config,
    ) {
        // A driver that fails once keeps failing, so stop retrying. This also
        // catches a source imported as an external OES texture: binding it as a
        // sampler2D raises GL_INVALID_OPERATION, which smithay checks after the
        // first dispatch, so that degrades to an unfiltered present rather than
        // drawing nothing.
        NIS_DISABLED.store(true, Ordering::Relaxed);
        tracing::warn!(?err, "NIS dispatch failed, presenting without upscaling");
        return Err(Unavailable::Failed);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn dispatch<R>(
    renderer: &mut R,
    program: &GlesComputeProgram,
    src: &GlesTexture,
    flip_y: bool,
    dst: &GlesTexture,
    src_size: Size<i32, Physical>,
    dst_size: Size<i32, Physical>,
    ratio: ScaleRatio,
    config: NisConfig,
) -> Result<(), GlesError>
where
    R: AsGlowRenderer,
{
    let src_id = src.tex_id();
    let dst_id = dst.tex_id();
    let gles: &mut GlesRenderer = BorrowMut::borrow_mut(renderer.glow_renderer_mut());

    // Round up so the last partial block is covered; the shader discards
    // invocations that land outside the image.
    let groups = (
        ((dst_size.w + WORKGROUP - 1) / WORKGROUP).max(1) as u32,
        ((dst_size.h + WORKGROUP - 1) / WORKGROUP).max(1) as u32,
        1,
    );

    unsafe {
        gles.dispatch_compute(program, groups, |gl, prog| {
            let name = |s: &[u8]| gl.GetUniformLocation(prog, s.as_ptr() as *const _);

            gl.ActiveTexture(ffi::TEXTURE0);
            gl.BindTexture(ffi::TEXTURE_2D, src_id);

            // Compute samples the texture as it finds it, unlike a draw where
            // smithay's render_texture sets this every time. LINEAR is needed
            // for the chroma tap; CLAMP_TO_EDGE pads the tile loader's reads
            // past the edge instead of wrapping them.
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

            gl.Uniform1i(name(b"tex\0"), 0);
            // The shader declares `binding = 0` for the image, so unit 0 here.
            gl.BindImageTexture(0, dst_id, 0, ffi::FALSE, 0, ffi::WRITE_ONLY, ffi::RGBA8);

            gl.Uniform2f(name(b"src_size\0"), src_size.w as f32, src_size.h as f32);
            gl.Uniform2f(
                name(b"inv_src_size\0"),
                1.0 / src_size.w as f32,
                1.0 / src_size.h as f32,
            );
            gl.Uniform1i(name(b"flip_y\0"), i32::from(flip_y));
            gl.Uniform2i(name(b"dst_size\0"), dst_size.w, dst_size.h);
            gl.Uniform2f(name(b"scale\0"), ratio.x, ratio.y);

            gl.Uniform1f(name(b"k_detect_ratio\0"), config.detect_ratio);
            gl.Uniform1f(name(b"k_detect_thres\0"), config.detect_thres);
            gl.Uniform1f(name(b"k_min_contrast_ratio\0"), config.min_contrast_ratio);
            gl.Uniform1f(name(b"k_ratio_norm\0"), config.ratio_norm);
            gl.Uniform1f(name(b"k_contrast_boost\0"), config.contrast_boost);
            gl.Uniform1f(name(b"k_eps\0"), config.eps);
            gl.Uniform1f(name(b"k_sharp_start_y\0"), config.sharp_start_y);
            gl.Uniform1f(name(b"k_sharp_scale_y\0"), config.sharp_scale_y);
            gl.Uniform1f(name(b"k_sharp_strength_min\0"), config.sharp_strength_min);
            gl.Uniform1f(
                name(b"k_sharp_strength_scale\0"),
                config.sharp_strength_scale,
            );
            gl.Uniform1f(name(b"k_sharp_limit_min\0"), config.sharp_limit_min);
            gl.Uniform1f(name(b"k_sharp_limit_scale\0"), config.sharp_limit_scale);
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The shader reads six taps, so taps seven and eight must be zero or it is
    /// silently dropping signal. Holds a future regeneration to that shape.
    #[test]
    fn coefficients_are_six_tap() {
        for (phase, row) in nis_coefficients::COEF_SCALE.iter().enumerate() {
            for (tap, v) in row.iter().enumerate().skip(SHADER_FILTER_SIZE) {
                assert_eq!(*v, 0.0, "coef_scale phase {phase} tap {tap} is not zero");
            }
        }
        for (phase, row) in nis_coefficients::COEF_USM.iter().enumerate() {
            for (tap, v) in row.iter().enumerate().skip(SHADER_FILTER_SIZE) {
                assert_eq!(*v, 0.0, "coef_usm phase {phase} tap {tap} is not zero");
            }
        }
    }

    /// An unnormalised kernel changes brightness with the sub-pixel phase, which
    /// reads as shimmer in motion rather than an obviously wrong still.
    #[test]
    fn scale_kernel_is_normalised() {
        for (phase, row) in nis_coefficients::COEF_SCALE.iter().enumerate() {
            let sum: f32 = row.iter().take(SHADER_FILTER_SIZE).sum();
            assert!(
                (sum - 1.0).abs() < 1e-3,
                "coef_scale phase {phase} sums to {sum}, expected 1.0"
            );
        }
    }

    /// A difference operator, so a non-zero sum adds a DC term and shifts the
    /// whole image's brightness rather than only its detail.
    #[test]
    fn usm_kernel_is_zero_sum() {
        for (phase, row) in nis_coefficients::COEF_USM.iter().enumerate() {
            let sum: f32 = row.iter().take(SHADER_FILTER_SIZE).sum();
            assert!(
                sum.abs() < 1e-3,
                "coef_usm phase {phase} sums to {sum}, expected 0.0"
            );
        }
    }

    /// Dumps the baked shader so it can be compiled without a GPU:
    ///
    ///   NIS_SHADER_DUMP=/tmp/nis.comp cargo test --lib dump_shader
    ///   glslc -fshader-stage=compute --target-env=opengl /tmp/nis.comp -o /dev/null
    ///
    /// glslc targets SPIR-V, which forbids the default uniform block, so it
    /// flags every non-opaque `uniform` as needing a location. Expected — we go
    /// through `glCompileShader`/`glGetUniformLocation`. Prefix each with
    /// `layout(location = N)` to see past them; anything else it reports is real.
    #[test]
    fn dump_shader_for_external_validation() {
        if let Ok(path) = std::env::var("NIS_SHADER_DUMP") {
            std::fs::write(path, shader_source()).unwrap();
        }
    }

    /// A second mention — in prose, say — would also be replaced, baking the
    /// tables into a comment and breaking a shader that only fails on a machine
    /// which can compile compute.
    #[test]
    fn placeholder_appears_exactly_once() {
        assert_eq!(
            NIS_SCALER_SHADER.matches(COEFFICIENT_PLACEHOLDER).count(),
            1
        );
    }

    /// A declaration at column zero means the tables were not spliced into a
    /// comment, which would land mid-line.
    #[test]
    fn tables_are_baked_at_top_level() {
        let src = shader_source();
        assert!(
            src.contains("\nconst float kCoefScale["),
            "kCoefScale is not declared at the start of a line"
        );
        assert!(
            src.contains("\nconst float kCoefUsm["),
            "kCoefUsm is not declared at the start of a line"
        );
    }

    #[test]
    fn baked_shader_has_no_placeholder_and_both_tables() {
        let src = shader_source();
        assert!(
            !src.contains("//_COEFFICIENTS_"),
            "placeholder not replaced"
        );
        let len = nis_coefficients::PHASE_COUNT * SHADER_FILTER_SIZE;
        assert!(src.contains(&format!("const float kCoefScale[{len}]")));
        assert!(src.contains(&format!("const float kCoefUsm[{len}]")));
    }

    /// GLSL will not convert an integer literal in a `float[]` constructor, so a
    /// bare `0` is a compile error only seen at runtime on a 3.1 machine.
    #[test]
    fn baked_values_are_all_float_literals() {
        for (name, table) in [
            ("kCoefScale", &nis_coefficients::COEF_SCALE),
            ("kCoefUsm", &nis_coefficients::COEF_USM),
        ] {
            let mut baked = String::new();
            bake_table(&mut baked, name, table);

            let open = baked.find('(').expect("constructor open paren");
            let close = baked.rfind(')').expect("constructor close paren");
            let values: Vec<_> = baked[open + 1..close]
                .split(',')
                .map(str::trim)
                .filter(|v| !v.is_empty())
                .collect();

            for value in &values {
                assert!(
                    value.contains('.'),
                    "{name}: baked value {value:?} has no decimal point"
                );
                assert!(
                    value.parse::<f32>().is_ok(),
                    "{name}: baked value {value:?} is not a float"
                );
            }
            assert_eq!(
                values.len(),
                nis_coefficients::PHASE_COUNT * SHADER_FILTER_SIZE,
                "{name}: wrong number of baked values"
            );
        }
    }

    /// Anything the range check admits must fit the tile, or a support window
    /// reads past the end of shared memory.
    #[test]
    fn tile_covers_every_in_range_ratio() {
        const TILE_DIM: i32 = 14;
        const BLOCK: i32 = 8;
        for permille in 500..=1000 {
            let scale = permille as f64 / 1000.0;
            // Worst case base index within a block, mirroring the shader.
            let block_start = ((0.5) * scale - 0.5).floor();
            let last = ((BLOCK as f64 - 1.0 + 0.5) * scale - 0.5).floor();
            let base = (last - block_start) as i32;
            assert!(
                base + 5 < TILE_DIM,
                "scale {scale}: support reaches {} of {TILE_DIM}",
                base + 5
            );
        }
    }

    #[test]
    fn ratio_accepts_one_to_two_times_and_rejects_the_rest() {
        let ok = ScaleRatio::new(Size::from((1920, 1080)), Size::from((3840, 2160)));
        assert!(ok.is_ok(), "exactly 2x must be accepted");
        let ok = ScaleRatio::new(Size::from((1920, 1080)), Size::from((1920, 1080)));
        assert!(ok.is_ok(), "1:1 must be accepted");

        // Past 2x the 6-tap support cannot reach; NIS is not defined there.
        assert_eq!(
            ScaleRatio::new(Size::from((640, 480)), Size::from((3840, 2160))),
            Err(Unavailable::OutOfRange)
        );
        // Downscaling is not an upscale.
        assert_eq!(
            ScaleRatio::new(Size::from((3840, 2160)), Size::from((1920, 1080))),
            Err(Unavailable::OutOfRange)
        );
        assert_eq!(
            ScaleRatio::new(Size::from((0, 1080)), Size::from((1920, 1080))),
            Err(Unavailable::DegenerateSize)
        );
    }

    /// One axis being in range does not excuse the other.
    #[test]
    fn ratio_rejects_a_single_out_of_range_axis() {
        assert_eq!(
            ScaleRatio::new(Size::from((1920, 400)), Size::from((2560, 2160))),
            Err(Unavailable::OutOfRange)
        );
    }

    #[test]
    fn sharpness_midpoint_matches_the_sdk_defaults() {
        let c = NisConfig::new(DEFAULT_SHARPNESS);
        // At the slider midpoint the SDK's expressions collapse to these.
        assert!((c.sharp_strength_min - 0.4).abs() < 1e-6);
        assert!((c.sharp_strength_scale - 1.2).abs() < 1e-6);
        assert!((c.sharp_limit_min - 0.14).abs() < 1e-6);
        assert!((c.sharp_limit_scale - 0.36).abs() < 1e-6);
        assert!((c.ratio_norm - 0.125).abs() < 1e-6);
        assert!((c.eps - 1.0 / 255.0).abs() < 1e-6);
    }

    /// Strength must rise with the slider, and 0% must mean no sharpening.
    #[test]
    fn sharpness_is_monotonic_and_bottoms_out_at_zero() {
        assert_eq!(NisConfig::new(0.0).sharp_strength_min, 0.0);
        let mut previous = f32::NEG_INFINITY;
        for step in 0..=20 {
            let c = NisConfig::new(step as f32 / 20.0);
            let max = c.sharp_strength_min + c.sharp_strength_scale;
            assert!(max > previous, "strength fell at slider {step}");
            previous = max;
        }
    }

    #[test]
    fn sharpness_is_clamped_to_its_range() {
        assert_eq!(NisConfig::new(-1.0), NisConfig::new(0.0));
        assert_eq!(NisConfig::new(2.0), NisConfig::new(1.0));
    }
}
