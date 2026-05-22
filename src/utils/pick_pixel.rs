// SPDX-License-Identifier: GPL-3.0-only
//
// Off-screen single-pixel colour sample for the PickColor portal.
//
// Renders the given output's active workspace to a disposable offscreen
// renderbuffer, reads back a single pixel, and returns it as f64 RGB in [0, 1].

use anyhow::{Context, anyhow};
use smithay::{
    backend::{
        allocator::Fourcc,
        drm::DrmNode,
        renderer::{
            ExportMem, ImportAll, Offscreen, Renderer, damage::OutputDamageTracker,
            element::RenderElement, gles::GlesRenderbuffer,
        },
    },
    output::Output,
    utils::{Rectangle, Transform},
};

use crate::{
    backend::render::{
        CursorMode, ElementFilter, RendererRef,
        element::{AsGlowRenderer, CosmicElement, FromGlesError},
        render_workspace,
    },
    shell::{CosmicMappedRenderElement, WorkspaceRenderElement},
    state::{BackendData, State},
    utils::prelude::OutputExt,
};

/// Sample the colour at logical output-local coordinates `(x, y)` on `output`.
pub fn pick_pixel(
    state: &mut State,
    output_name: &str,
    x: i32,
    y: i32,
) -> anyhow::Result<(f64, f64, f64)> {
    let output = state
        .common
        .shell
        .read()
        .outputs()
        .find(|o| o.name() == output_name)
        .cloned()
        .ok_or_else(|| anyhow!("No output named {}", output_name))?;

    let geo = output.geometry();
    if x < 0 || y < 0 || x >= geo.size.w || y >= geo.size.h {
        return Err(anyhow!(
            "Coordinates ({x}, {y}) are outside output bounds {:?}",
            geo.size
        ));
    }

    if matches!(state.backend, BackendData::Unset) {
        return Err(anyhow!("Backend not yet initialised"));
    }

    let gpu: Option<DrmNode> = match &state.backend {
        BackendData::Kms(kms) => *kms.primary_node.read().unwrap(),
        _ => None,
    };

    let now = state.common.clock.now();
    let shell = state.common.shell.clone();

    let (current, zoom_state) = {
        let shell_ref = shell.read();
        let workspace = shell_ref
            .workspaces
            .active(&output)
            .ok_or_else(|| anyhow!("No active workspace for output"))?
            .1;
        let (_, idx) = shell_ref.workspaces.active_num(&output);
        let zoom_state = shell_ref.zoom_state().cloned();
        ((workspace.handle, idx), zoom_state)
    };

    let renderer = state
        .backend
        .offscreen_renderer(|_kms| gpu.map(crate::state::KmsNodes::from))
        .with_context(|| "Failed to acquire offscreen renderer")?;

    match renderer {
        RendererRef::Glow(renderer) => pick_pixel_with(
            renderer,
            &shell,
            zoom_state.as_ref(),
            now,
            &output,
            current,
            gpu.as_ref(),
            x,
            y,
        ),
        RendererRef::GlMulti(mut renderer) => pick_pixel_with(
            &mut renderer,
            &shell,
            zoom_state.as_ref(),
            now,
            &output,
            current,
            gpu.as_ref(),
            x,
            y,
        ),
    }
}

fn pick_pixel_with<R>(
    renderer: &mut R,
    shell: &std::sync::Arc<parking_lot::RwLock<crate::shell::Shell>>,
    zoom_state: Option<&crate::shell::zoom::ZoomState>,
    now: smithay::utils::Time<smithay::utils::Monotonic>,
    output: &Output,
    current: (crate::wayland::protocols::workspace::WorkspaceHandle, usize),
    gpu: Option<&DrmNode>,
    x: i32,
    y: i32,
) -> anyhow::Result<(f64, f64, f64)>
where
    R: Renderer + ImportAll + Offscreen<GlesRenderbuffer> + ExportMem + AsGlowRenderer,
    R::TextureId: Send + Clone + 'static,
    R::Error: FromGlesError + Send + Sync + 'static,
    CosmicElement<R>: RenderElement<R>,
    CosmicMappedRenderElement<R>: RenderElement<R>,
    WorkspaceRenderElement<R>: RenderElement<R>,
{
    let mode = output
        .current_mode()
        .ok_or_else(|| anyhow!("Output has no mode"))?;
    let transform = output.current_transform();
    let scale = output.current_scale().fractional_scale();

    // Buffer is laid out without transform, matching the image_copy_capture convention.
    let _ = transform;
    let buffer_size = mode.size.to_logical(1).to_buffer(1, Transform::Normal);

    let format = Fourcc::Abgr8888;
    let mut render_buffer =
        Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, buffer_size)
            .map_err(|e| anyhow!("Failed to create offscreen buffer: {e}"))?;
    let mut fb = renderer
        .bind(&mut render_buffer)
        .map_err(|e| anyhow!("Failed to bind offscreen buffer: {e}"))?;

    let mut damage_tracker = OutputDamageTracker::from_output(output);

    let res = render_workspace(
        gpu,
        renderer,
        &mut fb,
        &mut damage_tracker,
        0,
        None,
        shell,
        zoom_state,
        now,
        output,
        None,
        current,
        CursorMode::None,
        ElementFilter::All,
    )
    .map_err(|e| anyhow!("render_workspace failed: {e}"))?;
    renderer
        .wait(&res.0.sync)
        .map_err(|e| anyhow!("Failed to wait on render sync: {e}"))?;

    // Logical output-local coord -> buffer coord.
    let px = (x as f64 * scale).floor() as i32;
    let py = (y as f64 * scale).floor() as i32;
    let region = Rectangle::new((px, py).into(), (1, 1).into());

    let mapping = renderer
        .copy_framebuffer(&fb, region, format)
        .map_err(|e| anyhow!("copy_framebuffer failed: {e}"))?;
    let bytes = renderer
        .map_texture(&mapping)
        .map_err(|e| anyhow!("map_texture failed: {e}"))?;
    if bytes.len() < 4 {
        return Err(anyhow!("Unexpected pixel data length: {}", bytes.len()));
    }

    let r = bytes[0] as f64 / 255.0;
    let g = bytes[1] as f64 / 255.0;
    let b = bytes[2] as f64 / 255.0;
    Ok((r, g, b))
}
