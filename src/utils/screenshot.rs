use anyhow::Context;
use smithay::{
    backend::{
        allocator::Fourcc,
        renderer::{
            ExportMem, ImportAll, Offscreen, Renderer, damage::OutputDamageTracker,
            gles::GlesRenderbuffer,
        },
    },
    desktop::utils::bbox_from_surface_tree,
    utils::{Scale, Transform},
    wayland::seat::WaylandFocus,
};
use tracing::warn;

use crate::{
    backend::render::{RendererRef, element::AsGlowRenderer},
    shell::element::CosmicSurface,
    state::{State, advertised_node_for_surface},
};

pub fn screenshot_window(state: &mut State, surface: &CosmicSurface) {
    fn render_window<R>(renderer: &mut R, window: &CosmicSurface) -> anyhow::Result<()>
    where
        R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
        R::TextureId: Clone + 'static,
        R::Error: Send + Sync + 'static,
    {
        let bbox = bbox_from_surface_tree(&window.wl_surface().unwrap(), (0, 0));
        let mut elements = Vec::new();
        window.push_render_elements(
            renderer,
            (-bbox.loc.x, -bbox.loc.y).into(),
            Scale::from(1.0),
            1.0,
            None,
            false,
            [0; 4],
            &mut |elem| elements.push(elem),
            None,
        );

        // TODO: 10-bit
        let format = Fourcc::Abgr8888;
        let mut render_buffer = Offscreen::<GlesRenderbuffer>::create_buffer(
            renderer,
            format,
            bbox.size.to_buffer(1, Transform::Normal),
        )?;
        let mut fb = renderer.bind(&mut render_buffer)?;
        let mut output_damage_tracker =
            OutputDamageTracker::new(bbox.size.to_physical(1), 1.0, Transform::Normal);
        output_damage_tracker
            .render_output(renderer, &mut fb, 0, &elements, [0.0, 0.0, 0.0, 0.0])
            .map_err(|err| match err {
                smithay::backend::renderer::damage::Error::Rendering(err) => err,
                smithay::backend::renderer::damage::Error::OutputNoMode(_) => unreachable!(),
            })?;
        let mapping = renderer.copy_framebuffer(
            &fb,
            bbox.to_buffer(1, Transform::Normal, &bbox.size),
            format,
        )?;
        let gl_data = renderer.map_texture(&mapping)?;

        if let Ok(Some(path)) = xdg_user::pictures() {
            let local_timestamp = jiff::Zoned::now();
            let mut title = window.title();
            title.truncate(227); // 255 - time - png
            let name = sanitize_filename::sanitize(format!(
                "{}_{}.png",
                title,
                local_timestamp.strftime("%Y-%m-%d_%H:%M:%S_%4f"),
            ));
            let file = std::fs::File::create(path.join(name))?;

            let writer = &mut std::io::BufWriter::new(file);
            let mut encoder = png::Encoder::new(writer, bbox.size.w as u32, bbox.size.h as u32);
            encoder.set_color(png::ColorType::Rgba);
            encoder.set_depth(png::BitDepth::Eight);
            encoder.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2)); // 1.0 / 2.2, unscaled, but rounded
            let source_chromaticities = png::SourceChromaticities::new(
                // Using unscaled instantiation here
                (0.31270, 0.32900),
                (0.64000, 0.33000),
                (0.30000, 0.60000),
                (0.15000, 0.06000),
            );
            encoder.set_source_chromaticities(source_chromaticities);
            let mut writer = encoder.write_header()?;
            writer.write_image_data(gl_data)?;
        }

        Ok(())
    }

    if let Some(wl_surface) = surface.wl_surface() {
        let res = state
            .backend
            .offscreen_renderer(|kms| {
                advertised_node_for_surface(&wl_surface, &state.common.display_handle)
                    .or(*kms.primary_node.read().unwrap())
            })
            .with_context(|| "Failed to get renderer for screenshot")
            .and_then(|renderer| match renderer {
                RendererRef::Glow(renderer) => render_window(renderer, surface),
                RendererRef::GlMulti(mut renderer) => render_window(&mut renderer, surface),
            });
        if let Err(err) = res {
            warn!(?err, "Failed to take screenshot")
        }
    }
}
