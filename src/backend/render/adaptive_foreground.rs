// SPDX-License-Identifier: GPL-3.0-only

//! Samples the wallpaper behind a client's adaptive-foreground zones.
//!
//! Runs on the main loop rather than in the render pass. The blur effect reads
//! its backdrop mid-pass via `is_framebuffer_effect`/`capture_framebuffer`,
//! which is right for blur -- it must track live content every frame -- and
//! wrong here for two reasons: the backdrop under a desktop widget is the
//! wallpaper, which changes on wallpaper change rather than per frame, and
//! Wayland events cannot be sent from the KMS render thread.
//!
//! So each zone is rendered to a tiny offscreen and read back, on demand, using
//! the same `offscreen_renderer` path `utils::screenshot` uses.

use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            Color32F, ExportMem, ImportAll, Offscreen, Renderer,
            damage::OutputDamageTracker,
            element::{Kind, surface::WaylandSurfaceRenderElement},
            gles::GlesRenderbuffer,
        },
    },
    desktop::layer_map_for_output,
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Logical, Physical, Rectangle, Scale, Size, Transform},
    wayland::shell::wlr_layer::Layer,
};

use crate::{
    backend::render::{RendererRef, element::AsGlowRenderer},
    state::{State, advertised_node_for_surface},
};

/// Downsample target for one zone.
///
/// The mean is all the decision needs, and 256 samples estimate it well past the
/// precision of a light/dark choice. Larger costs a bigger readback for nothing.
const SAMPLE_EDGE: i32 = 16;

/// The offscreen a zone is rendered into. `Size::from` is not const, hence the
/// edge constant above rather than a `const SAMPLE`.
fn sample_size() -> Size<i32, Physical> {
    Size::from((SAMPLE_EDGE, SAMPLE_EDGE))
}

/// Take a fresh reading for every zone on `surface` and send it.
///
/// Silent on failure: a zone that cannot be sampled leaves the client on its
/// last reading, which is the same state it is in before the first sample and
/// renders as the colour scheme's own choice.
pub fn sample_and_send(state: &mut State, surface: &WlSurface) {
    let zones = state.common.adaptive_foreground_state.zones(surface);
    if zones.is_empty() {
        return;
    }

    let shell = state.common.shell.read();
    let Some(output) = shell.visible_output_for_surface(surface).cloned() else {
        return;
    };
    std::mem::drop(shell);

    // Zones are surface-local; the wallpaper is placed in output coordinates.
    let Some(surface_origin) = layer_surface_origin(&output, surface) else {
        return;
    };
    let zones: Vec<Rectangle<i32, Logical>> = zones
        .into_iter()
        .map(|z| Rectangle::new(surface_origin + z.loc, z.size))
        .collect();

    let dh = state.common.display_handle.clone();
    let readings = {
        let renderer = state
            .backend
            .offscreen_renderer(|kms| {
                advertised_node_for_surface(surface, &dh).or(*kms.primary_node.read().unwrap())
            })
            .ok();
        let Some(renderer) = renderer else {
            return;
        };
        match renderer {
            RendererRef::Glow(r) => sample_zones(r, &output, &zones),
            RendererRef::GlMulti(mut r) => sample_zones(&mut r, &output, &zones),
        }
    };

    let Some(readings) = readings else {
        return;
    };
    tracing::debug!(?readings, "adaptive_foreground: sampled backdrop");
    state
        .common
        .adaptive_foreground_state
        .send_luminance(surface, &readings);
}

/// Where `surface` sits on `output`, in output-local logical coordinates.
fn layer_surface_origin(
    output: &Output,
    surface: &WlSurface,
) -> Option<smithay::utils::Point<i32, Logical>> {
    let map = layer_map_for_output(output);
    let layer = map.layers().find(|l| l.wl_surface() == surface)?;
    map.layer_geometry(layer).map(|geo| geo.loc)
}

/// Render each zone's backdrop to a tiny offscreen and reduce it to a luminance.
fn sample_zones<R>(
    renderer: &mut R,
    output: &Output,
    zones: &[Rectangle<i32, Logical>],
) -> Option<Vec<f32>>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Clone + 'static,
    R::Error: Send + Sync + 'static,
{
    zones
        .iter()
        .map(|zone| match sample_zone(renderer, output, *zone) {
            Ok(luminance) => Some(luminance),
            Err(err) => {
                tracing::debug!(?err, ?zone, "adaptive_foreground: zone sample failed");
                None
            }
        })
        .collect()
}

fn sample_zone<R>(
    renderer: &mut R,
    output: &Output,
    zone: Rectangle<i32, Logical>,
) -> anyhow::Result<f32>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Clone + 'static,
    R::Error: Send + Sync + 'static,
{
    if zone.size.w <= 0 || zone.size.h <= 0 {
        anyhow::bail!("degenerate zone");
    }

    // Scale maps the zone onto the offscreen, so rendering is itself the
    // downsample -- no separate reduction pass.
    let scale = Scale {
        x: sample_size().w as f64 / zone.size.w as f64,
        y: sample_size().h as f64 / zone.size.h as f64,
    };

    // Only the Background layer: that is the wallpaper, and it is what sits
    // under a desktop widget. Widening this to everything below the requesting
    // surface is a change of this list and nothing else.
    let map = layer_map_for_output(output);
    let elements = map
        .layers()
        .filter(|l| l.layer() == Layer::Background)
        .flat_map(|l| {
            let geo = map.layer_geometry(l).unwrap_or_default();
            let offset = geo.loc - zone.loc;
            smithay::backend::renderer::element::surface::render_elements_from_surface_tree::<
                R,
                WaylandSurfaceRenderElement<R>,
            >(
                renderer,
                l.wl_surface(),
                offset.to_physical_precise_round(scale),
                scale,
                1.0,
                Kind::Unspecified,
            )
        })
        .collect::<Vec<_>>();

    let format = Fourcc::Abgr8888;
    let mut buffer = Offscreen::<GlesRenderbuffer>::create_buffer(
        renderer,
        format,
        sample_size().to_logical(1).to_buffer(1, Transform::Normal),
    )?;
    let mut fb = renderer.bind(&mut buffer)?;

    // Black where nothing draws. A zone the wallpaper does not cover reads as
    // dark, which is what the compositor actually shows there.
    let mut damage = OutputDamageTracker::new(sample_size(), 1.0, Transform::Normal);
    damage
        .render_output(renderer, &mut fb, 0, &elements, Color32F::BLACK)
        .map_err(|err| match err {
            smithay::backend::renderer::damage::Error::Rendering(err) => anyhow::anyhow!("{err:?}"),
            smithay::backend::renderer::damage::Error::OutputNoMode(_) => {
                anyhow::anyhow!("output has no mode")
            }
        })?;

    let mapping = renderer.copy_framebuffer(
        &fb,
        Rectangle::from_size(sample_size().to_logical(1).to_buffer(1, Transform::Normal)),
        format,
    )?;
    let pixels = renderer.map_texture(&mapping)?;
    Ok(mean_relative_luminance(pixels))
}

/// WCAG relative luminance of the mean colour of an RGBA8 buffer.
///
/// Averages in the encoded space and linearises once rather than linearising
/// each texel. Across a 256-texel mean the difference is far below the protocol
/// quantum, and it is one transfer-function evaluation instead of 768.
fn mean_relative_luminance(rgba: &[u8]) -> f32 {
    if rgba.len() < 4 {
        return 0.0;
    }
    let texels = (rgba.len() / 4) as f32;
    let (mut r, mut g, mut b) = (0.0f32, 0.0f32, 0.0f32);
    for px in rgba.chunks_exact(4) {
        r += px[0] as f32;
        g += px[1] as f32;
        b += px[2] as f32;
    }
    let linear = |sum: f32| {
        let c = sum / texels / 255.0;
        if c <= 0.04045 {
            c / 12.92
        } else {
            ((c + 0.055) / 1.055).powf(2.4)
        }
    };
    (0.2126 * linear(r) + 0.7152 * linear(g) + 0.0722 * linear(b)).clamp(0.0, 1.0)
}

#[cfg(test)]
mod tests {
    use super::mean_relative_luminance;

    fn solid(r: u8, g: u8, b: u8, texels: usize) -> Vec<u8> {
        [r, g, b, 255].repeat(texels)
    }

    #[test]
    fn the_endpoints_match_wcag() {
        assert!(mean_relative_luminance(&solid(0, 0, 0, 256)) < 1e-6);
        assert!((mean_relative_luminance(&solid(255, 255, 255, 256)) - 1.0).abs() < 1e-4);
    }

    #[test]
    fn a_half_black_half_white_zone_reads_as_mid_grey_not_as_either_half() {
        // The zone the greeting sits in often straddles an edge. It must land
        // between the two, not snap to whichever half came first.
        let mut px = solid(0, 0, 0, 128);
        px.extend(solid(255, 255, 255, 128));
        let l = mean_relative_luminance(&px);
        assert!(l > 0.1 && l < 0.9, "{l}");
    }

    #[test]
    fn green_outweighs_red_and_blue() {
        let g = mean_relative_luminance(&solid(0, 255, 0, 16));
        assert!(g > mean_relative_luminance(&solid(255, 0, 0, 16)));
        assert!(g > mean_relative_luminance(&solid(0, 0, 255, 16)));
    }

    #[test]
    fn an_empty_buffer_does_not_divide_by_zero() {
        assert_eq!(mean_relative_luminance(&[]), 0.0);
    }
}
