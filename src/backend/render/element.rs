use crate::shell::{CosmicMappedRenderElement, WorkspaceRenderElement};

use smithay::{
    backend::renderer::{
        element::{texture::TextureRenderElement, Element, RenderElement, UnderlyingStorage},
        gles2::{Gles2Frame, Gles2Texture},
        glow::GlowRenderer,
        multigpu::Error as MultiError,
        Frame, ImportAll, Renderer,
    },
    utils::{Physical, Point, Rectangle, Scale},
};

use super::{cursor::CursorRenderElement, GlMultiFrame, GlMultiRenderer};

pub enum CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    Workspace(WorkspaceRenderElement<R>),
    Cursor(CursorRenderElement<R>),
    MoveGrab(CosmicMappedRenderElement<R>),
    #[cfg(feature = "debug")]
    Egui(TextureRenderElement<Gles2Texture>),
}

impl<R> Element for CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    fn id(&self) -> &smithay::backend::renderer::element::Id {
        match self {
            CosmicElement::Workspace(elem) => elem.id(),
            CosmicElement::Cursor(elem) => elem.id(),
            CosmicElement::MoveGrab(elem) => elem.id(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            CosmicElement::Workspace(elem) => elem.current_commit(),
            CosmicElement::Cursor(elem) => elem.current_commit(),
            CosmicElement::MoveGrab(elem) => elem.current_commit(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            CosmicElement::Workspace(elem) => elem.src(),
            CosmicElement::Cursor(elem) => elem.src(),
            CosmicElement::MoveGrab(elem) => elem.src(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.geometry(scale),
            CosmicElement::Cursor(elem) => elem.geometry(scale),
            CosmicElement::MoveGrab(elem) => elem.geometry(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.location(scale),
            CosmicElement::Cursor(elem) => elem.location(scale),
            CosmicElement::MoveGrab(elem) => elem.location(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            CosmicElement::Workspace(elem) => elem.transform(),
            CosmicElement::Cursor(elem) => elem.transform(),
            CosmicElement::MoveGrab(elem) => elem.transform(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<smithay::backend::renderer::utils::CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicElement::Workspace(elem) => elem.damage_since(scale, commit),
            CosmicElement::Cursor(elem) => elem.damage_since(scale, commit),
            CosmicElement::MoveGrab(elem) => elem.damage_since(scale, commit),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicElement::Workspace(elem) => elem.opaque_regions(scale),
            CosmicElement::Cursor(elem) => elem.opaque_regions(scale),
            CosmicElement::MoveGrab(elem) => elem.opaque_regions(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.opaque_regions(scale),
        }
    }
}

impl RenderElement<GlowRenderer> for CosmicElement<GlowRenderer> {
    fn draw(
        &self,
        renderer: &mut GlowRenderer,
        frame: &mut <GlowRenderer as Renderer>::Frame,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        damage: &[Rectangle<i32, Physical>],
        log: &slog::Logger,
    ) -> Result<(), <GlowRenderer as Renderer>::Error> {
        match self {
            CosmicElement::Workspace(elem) => {
                elem.draw(renderer, frame, location, scale, damage, log)
            }
            CosmicElement::Cursor(elem) => elem.draw(renderer, frame, location, scale, damage, log),
            CosmicElement::MoveGrab(elem) => {
                elem.draw(renderer, frame, location, scale, damage, log)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.draw(renderer, frame, location, scale, damage, log),
        }
    }

    fn underlying_storage(
        &self,
        renderer: &GlowRenderer,
    ) -> Option<UnderlyingStorage<'_, GlowRenderer>> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.underlying_storage(renderer),
        }
    }
}

impl<'a> RenderElement<GlMultiRenderer<'a>> for CosmicElement<GlMultiRenderer<'a>> {
    fn draw(
        &self,
        renderer: &mut GlMultiRenderer<'a>,
        frame: &mut <GlMultiRenderer<'a> as Renderer>::Frame,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        damage: &[Rectangle<i32, Physical>],
        log: &slog::Logger,
    ) -> Result<(), <GlMultiRenderer<'_> as Renderer>::Error> {
        match self {
            CosmicElement::Workspace(elem) => {
                elem.draw(renderer, frame, location, scale, damage, log)
            }
            CosmicElement::Cursor(elem) => elem.draw(renderer, frame, location, scale, damage, log),
            CosmicElement::MoveGrab(elem) => {
                elem.draw(renderer, frame, location, scale, damage, log)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_renderer = renderer.glow_renderer_mut();
                let gles2_frame = frame.gles2_frame_mut();
                elem.draw(glow_renderer, gles2_frame, location, scale, damage, log)
                    .map_err(|err| MultiError::Render(err))
            }
        }
    }

    fn underlying_storage(
        &self,
        renderer: &GlMultiRenderer<'a>,
    ) -> Option<UnderlyingStorage<'_, GlMultiRenderer<'a>>> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_renderer = renderer.glow_renderer();
                match elem.underlying_storage(glow_renderer) {
                    Some(UnderlyingStorage::Wayland(buffer)) => {
                        Some(UnderlyingStorage::Wayland(buffer))
                    }
                    _ => None,
                }
            }
        }
    }
}

impl<R> From<WorkspaceRenderElement<R>> for CosmicElement<R>
where
    R: Renderer + ImportAll + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    fn from(elem: WorkspaceRenderElement<R>) -> Self {
        Self::Workspace(elem)
    }
}

impl<R> From<CursorRenderElement<R>> for CosmicElement<R>
where
    R: Renderer + ImportAll + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    fn from(elem: CursorRenderElement<R>) -> Self {
        Self::Cursor(elem)
    }
}

impl<R> From<CosmicMappedRenderElement<R>> for CosmicElement<R>
where
    R: Renderer + ImportAll + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    fn from(elem: CosmicMappedRenderElement<R>) -> Self {
        Self::MoveGrab(elem)
    }
}

#[cfg(feature = "debug")]
impl<R> From<TextureRenderElement<Gles2Texture>> for CosmicElement<R>
where
    R: Renderer + ImportAll + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Frame: AsGles2Frame,
{
    fn from(elem: TextureRenderElement<Gles2Texture>) -> Self {
        Self::Egui(elem)
    }
}

pub trait AsGlowRenderer
where
    Self: Renderer,
    <Self as Renderer>::Frame: AsGles2Frame,
{
    fn glow_renderer(&self) -> &GlowRenderer;
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer;
}

impl AsGlowRenderer for GlowRenderer {
    fn glow_renderer(&self) -> &GlowRenderer {
        self
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self
    }
}

impl<'a> AsGlowRenderer for GlMultiRenderer<'a> {
    fn glow_renderer(&self) -> &GlowRenderer {
        self.as_ref()
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self.as_mut()
    }
}

pub trait AsGles2Frame
where
    Self: Frame,
{
    fn gles2_frame(&self) -> &Gles2Frame;
    fn gles2_frame_mut(&mut self) -> &mut Gles2Frame;
}

impl AsGles2Frame for Gles2Frame {
    fn gles2_frame(&self) -> &Gles2Frame {
        self
    }
    fn gles2_frame_mut(&mut self) -> &mut Gles2Frame {
        self
    }
}

impl AsGles2Frame for GlMultiFrame {
    fn gles2_frame(&self) -> &Gles2Frame {
        self.as_ref()
    }
    fn gles2_frame_mut(&mut self) -> &mut Gles2Frame {
        self.as_mut()
    }
}
