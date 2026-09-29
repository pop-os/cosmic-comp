use crate::{
    backend::{
        kms::render::gles::GbmGlowBackend,
        render::{GlMultiError, wayland::SurfaceRenderElement},
    },
    shell::{CosmicMappedRenderElement, WorkspaceRenderElement},
    utils::iced::IcedRenderElement,
};

#[cfg(feature = "debug")]
use smithay::backend::renderer::element::texture::TextureRenderElement;
use smithay::{
    backend::{
        allocator::dmabuf::Dmabuf,
        drm::DrmDeviceFd,
        renderer::{
            Bind, Blit, ContextId, ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
            element::{
                Element, Id, Kind, RenderElement, UnderlyingStorage,
                utils::{CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement},
            },
            gles::{GlesError, GlesRenderbuffer, GlesTexture, element::TextureShaderElement},
            glow::{GlowFrame, GlowRenderer},
            multigpu::MultiTexture,
            utils::{CommitCounter, DamageSet, OpaqueRegions},
        },
    },
    utils::{
        Buffer as BufferCoords, Logical, Physical, Point, Rectangle, Scale, user_data::UserDataMap,
    },
};

use super::{GlMultiRenderer, cursor::CursorRenderElement};

pub enum CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    Workspace(
        RelocateRenderElement<CropRenderElement<RescaleRenderElement<WorkspaceRenderElement<R>>>>,
    ),
    Cursor(
        RescaleRenderElement<RescaleRenderElement<RelocateRenderElement<CursorRenderElement<R>>>>,
    ),
    Dnd(SurfaceRenderElement<R>),
    MoveGrab(RescaleRenderElement<CosmicMappedRenderElement<R>>),
    Postprocess(
        CropRenderElement<RelocateRenderElement<RescaleRenderElement<TextureShaderElement>>>,
    ),
    Zoom(IcedRenderElement<R>),
    Damage(DamageElement),
    #[cfg(feature = "debug")]
    Egui(TextureRenderElement<GlesTexture>),
}

impl<R> CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn inner_element(&self) -> &dyn Element {
        match self {
            CosmicElement::Workspace(elem) => elem,
            CosmicElement::Cursor(elem) => elem,
            CosmicElement::Dnd(elem) => elem,
            CosmicElement::MoveGrab(elem) => elem,
            CosmicElement::Postprocess(elem) => elem,
            CosmicElement::Zoom(elem) => elem,
            CosmicElement::Damage(elem) => elem,
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem,
        }
    }
}

impl<R> Element for CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn id(&self) -> &Id {
        self.inner_element().id()
    }

    fn current_commit(&self) -> CommitCounter {
        self.inner_element().current_commit()
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        self.inner_element().src()
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.inner_element().geometry(scale)
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        self.inner_element().location(scale)
    }

    fn transform(&self) -> smithay::utils::Transform {
        self.inner_element().transform()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        self.inner_element().damage_since(scale, commit)
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        self.inner_element().opaque_regions(scale)
    }

    fn alpha(&self) -> f32 {
        self.inner_element().alpha()
    }

    fn kind(&self) -> Kind {
        self.inner_element().kind()
    }

    fn is_framebuffer_effect(&self) -> bool {
        self.inner_element().is_framebuffer_effect()
    }
}

impl<R> RenderElement<R> for CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_, '_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
        cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        match self {
            CosmicElement::Workspace(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            CosmicElement::Cursor(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            CosmicElement::Dnd(elem) => elem.draw(frame, src, dst, damage, opaque_regions, cache),
            CosmicElement::MoveGrab(elem) => {
                elem.draw(frame, src, dst, damage, opaque_regions, cache)
            }
            CosmicElement::Postprocess(elem) => {
                let glow_frame = R::glow_frame_mut(frame);
                RenderElement::<GlowRenderer>::draw(
                    elem,
                    glow_frame,
                    src,
                    dst,
                    damage,
                    opaque_regions,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
            CosmicElement::Zoom(elem) => elem.draw(frame, src, dst, damage, opaque_regions, cache),
            CosmicElement::Damage(elem) => {
                RenderElement::<R>::draw(elem, frame, src, dst, damage, opaque_regions, cache)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_frame = R::glow_frame_mut(frame);
                RenderElement::<GlowRenderer>::draw(
                    elem,
                    glow_frame,
                    src,
                    dst,
                    damage,
                    opaque_regions,
                    cache,
                )
                .map_err(R::from_gles_error)
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut R) -> Option<UnderlyingStorage<'_>> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::Dnd(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            CosmicElement::Postprocess(elem) => {
                let glow_renderer = renderer.glow_renderer_mut();
                elem.underlying_storage(glow_renderer)
            }
            CosmicElement::Zoom(elem) => elem.underlying_storage(renderer),
            CosmicElement::Damage(elem) => elem.underlying_storage(renderer),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_renderer = renderer.glow_renderer_mut();
                elem.underlying_storage(glow_renderer)
            }
        }
    }

    fn capture_framebuffer(
        &self,
        frame: &mut <R>::Frame<'_, '_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        cache: &UserDataMap,
    ) -> Result<(), <R>::Error> {
        match self {
            CosmicElement::Workspace(elem) => elem.capture_framebuffer(frame, src, dst, cache),
            CosmicElement::Cursor(elem) => elem.capture_framebuffer(frame, src, dst, cache),
            CosmicElement::Dnd(elem) => elem.capture_framebuffer(frame, src, dst, cache),
            CosmicElement::MoveGrab(elem) => elem.capture_framebuffer(frame, src, dst, cache),
            CosmicElement::Postprocess(elem) => {
                let glow_frame = R::glow_frame_mut(frame);
                RenderElement::<GlowRenderer>::capture_framebuffer(
                    elem, glow_frame, src, dst, cache,
                )
                .map_err(R::from_gles_error)
            }
            CosmicElement::Zoom(elem) => elem.capture_framebuffer(frame, src, dst, cache),
            CosmicElement::Damage(elem) => {
                RenderElement::<R>::capture_framebuffer(elem, frame, src, dst, cache)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_frame = R::glow_frame_mut(frame);
                RenderElement::<GlowRenderer>::capture_framebuffer(
                    elem, glow_frame, src, dst, cache,
                )
                .map_err(R::from_gles_error)
            }
        }
    }
}

impl<R> From<CropRenderElement<RescaleRenderElement<WorkspaceRenderElement<R>>>>
    for CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CropRenderElement<RescaleRenderElement<WorkspaceRenderElement<R>>>) -> Self {
        Self::Workspace(RelocateRenderElement::from_element(
            elem,
            (0, 0),
            Relocate::Relative,
        ))
    }
}

impl<R> From<IcedRenderElement<R>> for CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(value: IcedRenderElement<R>) -> Self {
        Self::Zoom(value)
    }
}

impl<R> From<DamageElement> for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(value: DamageElement) -> Self {
        Self::Damage(value)
    }
}

#[cfg(feature = "debug")]
impl<R> From<TextureRenderElement<GlesTexture>> for CosmicElement<R>
where
    R: AsGlowRenderer,
    R::TextureId: Send + 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        Self::Egui(elem)
    }
}

pub trait AsGlowRenderer:
    Renderer
    + Offscreen<GlesTexture>
    + Offscreen<GlesRenderbuffer>
    + ImportAll
    + ImportMem
    + ExportMem
    + Bind<Dmabuf>
    + Blit
{
    fn glow_renderer(&self) -> &GlowRenderer;
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer;
    fn glow_frame<'a, 'frame, 'buffer>(
        frame: &'a Self::Frame<'frame, 'buffer>,
    ) -> &'a GlowFrame<'frame, 'buffer>;
    fn glow_frame_mut<'a, 'frame, 'buffer>(
        frame: &'a mut Self::Frame<'frame, 'buffer>,
    ) -> &'a mut GlowFrame<'frame, 'buffer>;
    fn tex_from_gl(context: &ContextId<GlesTexture>, texture: GlesTexture) -> Self::TextureId;
    fn tex_to_gl(
        context: &ContextId<GlesTexture>,
        texture: &Self::TextureId,
    ) -> Option<GlesTexture>;
    fn from_gles_error(err: GlesError) -> Self::Error;
}

impl AsGlowRenderer for GlowRenderer {
    fn glow_renderer(&self) -> &GlowRenderer {
        self
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self
    }
    fn glow_frame<'a, 'frame, 'buffer>(
        frame: &'a Self::Frame<'frame, 'buffer>,
    ) -> &'a GlowFrame<'frame, 'buffer> {
        frame
    }
    fn glow_frame_mut<'a, 'frame, 'buffer>(
        frame: &'a mut Self::Frame<'frame, 'buffer>,
    ) -> &'a mut GlowFrame<'frame, 'buffer> {
        frame
    }
    fn tex_from_gl(_context: &ContextId<GlesTexture>, texture: GlesTexture) -> Self::TextureId {
        texture
    }
    fn tex_to_gl(
        _context: &ContextId<GlesTexture>,
        texture: &Self::TextureId,
    ) -> Option<GlesTexture> {
        Some(texture.clone())
    }
    fn from_gles_error(err: GlesError) -> Self::Error {
        err
    }
}

impl AsGlowRenderer for GlMultiRenderer<'_> {
    fn glow_renderer(&self) -> &GlowRenderer {
        self.as_ref()
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self.as_mut()
    }
    fn glow_frame<'b, 'frame, 'buffer>(
        frame: &'b Self::Frame<'frame, 'buffer>,
    ) -> &'b GlowFrame<'frame, 'buffer> {
        frame.as_ref()
    }
    fn glow_frame_mut<'b, 'frame, 'buffer>(
        frame: &'b mut Self::Frame<'frame, 'buffer>,
    ) -> &'b mut GlowFrame<'frame, 'buffer> {
        frame.as_mut()
    }
    fn tex_from_gl(context: &ContextId<GlesTexture>, texture: GlesTexture) -> Self::TextureId {
        MultiTexture::from_native_texture::<GbmGlowBackend<DrmDeviceFd>>(context, texture).unwrap()
    }
    fn tex_to_gl(
        context: &ContextId<GlesTexture>,
        texture: &Self::TextureId,
    ) -> Option<GlesTexture> {
        texture.get::<GbmGlowBackend<DrmDeviceFd>>(context)
    }
    fn from_gles_error(err: GlesError) -> Self::Error {
        GlMultiError::Render(err)
    }
}

pub struct DamageElement {
    id: Id,
    geometry: Rectangle<i32, Logical>,
}

impl DamageElement {
    pub fn new(geometry: Rectangle<i32, Logical>) -> DamageElement {
        DamageElement {
            id: Id::new(),
            geometry,
        }
    }
}

impl Element for DamageElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        CommitCounter::default()
    }

    fn src(&self) -> Rectangle<f64, BufferCoords> {
        Rectangle::from_size((1.0, 1.0).into())
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry.to_f64().to_physical(scale).to_i32_round()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        _commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        DamageSet::from_slice(&[Rectangle::from_size(self.geometry(scale).size)])
    }
}

impl<R: Renderer> RenderElement<R> for DamageElement {
    fn draw(
        &self,
        _frame: &mut R::Frame<'_, '_>,
        _src: Rectangle<f64, BufferCoords>,
        _dst: Rectangle<i32, Physical>,
        _damage: &[Rectangle<i32, Physical>],
        _opaque_regions: &[Rectangle<i32, Physical>],
        _cache: Option<&UserDataMap>,
    ) -> Result<(), R::Error> {
        Ok(())
    }
}
