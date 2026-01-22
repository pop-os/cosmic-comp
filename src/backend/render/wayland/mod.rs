use smithay::{
    backend::renderer::{
        ImportAll, Renderer,
        element::surface::{KindEvaluation, WaylandSurfaceRenderElement},
        glow::GlowRenderer,
        utils::RendererSurfaceStateUserData,
    },
    reexports::wayland_server::protocol::wl_surface,
    render_elements,
    utils::{Logical, Physical, Point, Rectangle, Scale},
    wayland::compositor::{self, TraversalAction},
};

pub mod blur_effect;
pub mod clipped_surface;

use blur_effect::BlurElement;
use clipped_surface::ClippedSurfaceRenderElement;
use tracing::warn;

use crate::backend::render::element::{AsGlowRenderer, FromGlesError};

render_elements! {
    pub SurfaceRenderElement<=GlowRenderer>;
    Blur=BlurElement,
    Clipped=ClippedSurfaceRenderElement<GlowRenderer>,
    Wayland=WaylandSurfaceRenderElement<GlowRenderer>,
}

pub fn render_elements_from_surface_tree<R, E>(
    renderer: &mut R,
    surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Physical>>,
    geometry: impl Into<Rectangle<f64, Logical>>,
    scale: impl Into<Scale<f64>>,
    alpha: f32,
    should_clip: bool,
    radii: [u8; 4],
    kind: impl Into<KindEvaluation>,
) -> Vec<E>
where
    R: Renderer + ImportAll + AsGlowRenderer,
    R::Error: FromGlesError,
    R::TextureId: Clone + 'static,
    E: From<SurfaceRenderElement>,
{
    let location = location.into().to_f64();
    let geometry = geometry.into();
    let scale = scale.into();
    let kind = kind.into();
    let mut surfaces: Vec<E> = Vec::new();
    let renderer = AsGlowRenderer::glow_renderer_mut(renderer);

    compositor::with_surface_tree_downward(
        surface,
        location,
        |_, states, location| {
            let mut location = *location;
            let data = states.data_map.get::<RendererSurfaceStateUserData>();

            if let Some(data) = data {
                if let Some(view) = data.lock().unwrap().view() {
                    location += view.offset.to_f64().to_physical(scale);
                    TraversalAction::DoChildren(location)
                } else {
                    TraversalAction::SkipChildren
                }
            } else {
                TraversalAction::SkipChildren
            }
        },
        |surface, states, location| {
            let mut location = *location;
            let kind = kind.eval(states);
            let data = states.data_map.get::<RendererSurfaceStateUserData>();

            if let Some(data) = data {
                let has_view = if let Some(view) = data.lock().unwrap().view() {
                    location += view.offset.to_f64().to_physical(scale);
                    true
                } else {
                    false
                };

                if has_view {
                    match WaylandSurfaceRenderElement::from_surface(
                        renderer, surface, states, location, alpha, kind,
                    ) {
                        Ok(Some(surface)) => {
                            let blur = BlurElement::from_surface(
                                renderer, states, geometry, scale.x, radii,
                            );
                            let elem: SurfaceRenderElement = if radii.iter().any(|r| *r != 0)
                                && should_clip
                                && ClippedSurfaceRenderElement::will_clip(
                                    &surface, scale, geometry, radii,
                                ) {
                                ClippedSurfaceRenderElement::new(
                                    renderer, surface, scale, geometry, radii,
                                )
                                .into()
                            } else {
                                surface.into()
                            };
                            surfaces.push(elem.into());

                            if let Ok(Some(elem)) = blur {
                                surfaces.push(SurfaceRenderElement::from(elem).into());
                            }
                        }
                        Ok(None) => {} // surface is not mapped
                        Err(err) => {
                            warn!("Failed to import surface: {}", err);
                        }
                    };
                }
            }
        },
        |_, _, _| true,
    );

    surfaces
}
