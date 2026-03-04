use smithay::{
    backend::renderer::{
        ImportAll, Renderer,
        element::surface::{KindEvaluation, WaylandSurfaceRenderElement},
        utils::RendererSurfaceStateUserData,
    },
    reexports::wayland_server::protocol::wl_surface,
    render_elements,
    utils::{Logical, Physical, Point, Rectangle, Scale},
    wayland::compositor::{self, TraversalAction},
};
use tracing::warn;

use crate::backend::render::{
    element::AsGlowRenderer, wayland::clipped_surface::ClippedSurfaceRenderElement,
};

pub mod clipped_surface;

render_elements! {
    pub SurfaceRenderElement<R> where R: AsGlowRenderer + ImportAll;
    Clipped=ClippedSurfaceRenderElement<R>,
    Wayland=WaylandSurfaceRenderElement<R>,
}

pub fn push_render_elements_from_surface_tree<R>(
    renderer: &mut R,
    main_surface: &wl_surface::WlSurface,
    location: impl Into<Point<i32, Physical>>,
    geometry: impl Into<Rectangle<f64, Logical>>,
    scale: impl Into<Scale<f64>>,
    alpha: f32,
    should_clip: bool,
    radii: [u8; 4],
    kind: impl Into<KindEvaluation>,
    push_above: &mut dyn FnMut(SurfaceRenderElement<R>),
    mut push_below: Option<&mut dyn FnMut(SurfaceRenderElement<R>)>,
) where
    R: Renderer + ImportAll + AsGlowRenderer,
    R::TextureId: Clone + 'static,
{
    let location = location.into().to_f64();
    let geometry = geometry.into().to_f64();
    let scale = scale.into();
    let kind = kind.into();
    let mut passed_main = false;

    compositor::with_surface_tree_downward(
        main_surface,
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
                            let elem: SurfaceRenderElement<R> = if radii.iter().any(|r| *r != 0)
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
                            if let Some(push_below) = push_below.as_mut()
                                && passed_main
                            {
                                push_below(elem);
                            } else {
                                push_above(elem);
                            }
                        }
                        Ok(None) => {} // surface is not mapped
                        Err(err) => {
                            warn!("Failed to import surface: {}", err);
                        }
                    };
                }
            }

            if surface == main_surface {
                passed_main = true;
            }
        },
        |_, _, _| true,
    );
}
