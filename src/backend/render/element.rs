use crate::shell::{CosmicMappedRenderElement, WorkspaceRenderElement};

use smithay::{
    backend::renderer::{
        element::{
            surface::WaylandSurfaceRenderElement,
            texture::TextureRenderElement,
            utils::{CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement},
            Element, Id, Kind, RenderElement, UnderlyingStorage,
        },
        gles::{GlesError, GlesTexture},
        glow::{GlowFrame, GlowRenderer},
        utils::{CommitCounter, DamageSet, OpaqueRegions},
        ImportAll, ImportMem, Renderer,
    },
    utils::{Buffer as BufferCoords, Logical, Physical, Point, Rectangle, Scale},
};

use super::{cursor::CursorRenderElement, GlMultiRenderer};

pub enum CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    Workspace(
        RelocateRenderElement<CropRenderElement<RescaleRenderElement<WorkspaceRenderElement<R>>>>,
    ),
    Cursor(RescaleRenderElement<RelocateRenderElement<CursorRenderElement<R>>>),
    Dnd(WaylandSurfaceRenderElement<R>),
    MoveGrab(RescaleRenderElement<CosmicMappedRenderElement<R>>),
    AdditionalDamage(DamageElement),
    Mirror(
        CropRenderElement<
            RelocateRenderElement<RescaleRenderElement<TextureRenderElement<GlesTexture>>>,
        >,
    ),
    #[cfg(feature = "debug")]
    Egui(TextureRenderElement<GlesTexture>),
}

impl<R> Element for CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn id(&self) -> &Id {
        match self {
            CosmicElement::Workspace(elem) => elem.id(),
            CosmicElement::Cursor(elem) => elem.id(),
            CosmicElement::Dnd(elem) => elem.id(),
            CosmicElement::MoveGrab(elem) => elem.id(),
            CosmicElement::AdditionalDamage(elem) => elem.id(),
            CosmicElement::Mirror(elem) => elem.id(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> CommitCounter {
        match self {
            CosmicElement::Workspace(elem) => elem.current_commit(),
            CosmicElement::Cursor(elem) => elem.current_commit(),
            CosmicElement::Dnd(elem) => elem.current_commit(),
            CosmicElement::MoveGrab(elem) => elem.current_commit(),
            CosmicElement::AdditionalDamage(elem) => elem.current_commit(),
            CosmicElement::Mirror(elem) => elem.current_commit(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            CosmicElement::Workspace(elem) => elem.src(),
            CosmicElement::Cursor(elem) => elem.src(),
            CosmicElement::Dnd(elem) => elem.src(),
            CosmicElement::MoveGrab(elem) => elem.src(),
            CosmicElement::AdditionalDamage(elem) => elem.src(),
            CosmicElement::Mirror(elem) => elem.src(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.geometry(scale),
            CosmicElement::Cursor(elem) => elem.geometry(scale),
            CosmicElement::Dnd(elem) => elem.geometry(scale),
            CosmicElement::MoveGrab(elem) => elem.geometry(scale),
            CosmicElement::AdditionalDamage(elem) => elem.geometry(scale),
            CosmicElement::Mirror(elem) => elem.geometry(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.location(scale),
            CosmicElement::Cursor(elem) => elem.location(scale),
            CosmicElement::Dnd(elem) => elem.location(scale),
            CosmicElement::MoveGrab(elem) => elem.location(scale),
            CosmicElement::AdditionalDamage(elem) => elem.location(scale),
            CosmicElement::Mirror(elem) => elem.location(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            CosmicElement::Workspace(elem) => elem.transform(),
            CosmicElement::Cursor(elem) => elem.transform(),
            CosmicElement::Dnd(elem) => elem.transform(),
            CosmicElement::MoveGrab(elem) => elem.transform(),
            CosmicElement::AdditionalDamage(elem) => elem.transform(),
            CosmicElement::Mirror(elem) => elem.transform(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.damage_since(scale, commit),
            CosmicElement::Cursor(elem) => elem.damage_since(scale, commit),
            CosmicElement::Dnd(elem) => elem.damage_since(scale, commit),
            CosmicElement::MoveGrab(elem) => elem.damage_since(scale, commit),
            CosmicElement::AdditionalDamage(elem) => elem.damage_since(scale, commit),
            CosmicElement::Mirror(elem) => elem.damage_since(scale, commit),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> OpaqueRegions<i32, Physical> {
        match self {
            CosmicElement::Workspace(elem) => elem.opaque_regions(scale),
            CosmicElement::Cursor(elem) => elem.opaque_regions(scale),
            CosmicElement::Dnd(elem) => elem.opaque_regions(scale),
            CosmicElement::MoveGrab(elem) => elem.opaque_regions(scale),
            CosmicElement::AdditionalDamage(elem) => elem.opaque_regions(scale),
            CosmicElement::Mirror(elem) => elem.opaque_regions(scale),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            CosmicElement::Workspace(elem) => elem.alpha(),
            CosmicElement::Cursor(elem) => elem.alpha(),
            CosmicElement::Dnd(elem) => elem.alpha(),
            CosmicElement::MoveGrab(elem) => elem.alpha(),
            CosmicElement::AdditionalDamage(elem) => elem.alpha(),
            CosmicElement::Mirror(elem) => elem.alpha(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.alpha(),
        }
    }

    fn kind(&self) -> Kind {
        match self {
            CosmicElement::Workspace(elem) => elem.kind(),
            CosmicElement::Cursor(elem) => elem.kind(),
            CosmicElement::Dnd(elem) => elem.kind(),
            CosmicElement::MoveGrab(elem) => elem.kind(),
            CosmicElement::AdditionalDamage(elem) => elem.kind(),
            CosmicElement::Mirror(elem) => elem.kind(),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.kind(),
        }
    }
}

impl<R> RenderElement<R> for CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
    <R as Renderer>::Error: FromGlesError,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn draw(
        &self,
        frame: &mut R::Frame<'_>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        opaque_regions: &[Rectangle<i32, Physical>],
    ) -> Result<(), R::Error> {
        match self {
            CosmicElement::Workspace(elem) => elem.draw(frame, src, dst, damage, opaque_regions),
            CosmicElement::Cursor(elem) => elem.draw(frame, src, dst, damage, opaque_regions),
            CosmicElement::Dnd(elem) => elem.draw(frame, src, dst, damage, opaque_regions),
            CosmicElement::MoveGrab(elem) => elem.draw(frame, src, dst, damage, opaque_regions),
            CosmicElement::AdditionalDamage(elem) => {
                RenderElement::<R>::draw(elem, frame, src, dst, damage, opaque_regions)
            }
            CosmicElement::Mirror(elem) => {
                let elem = {
                    let glow_frame = R::glow_frame_mut(frame);
                    RenderElement::<GlowRenderer>::draw(
                        elem,
                        glow_frame,
                        src,
                        dst,
                        damage,
                        opaque_regions,
                    )
                    .map_err(FromGlesError::from_gles_error)
                };
                elem
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let elem = {
                    let glow_frame = R::glow_frame_mut(frame);
                    RenderElement::<GlowRenderer>::draw(
                        elem,
                        glow_frame,
                        src,
                        dst,
                        damage,
                        opaque_regions,
                    )
                    .map_err(FromGlesError::from_gles_error)
                };
                elem
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut R) -> Option<UnderlyingStorage> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::Dnd(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            CosmicElement::AdditionalDamage(elem) => elem.underlying_storage(renderer),
            CosmicElement::Mirror(elem) => {
                let glow_renderer = renderer.glow_renderer_mut();
                match elem.underlying_storage(glow_renderer) {
                    Some(UnderlyingStorage::Wayland(buffer)) => {
                        Some(UnderlyingStorage::Wayland(buffer))
                    }
                    _ => None,
                }
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let glow_renderer = renderer.glow_renderer_mut();
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

impl<R> From<CropRenderElement<RescaleRenderElement<WorkspaceRenderElement<R>>>>
    for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
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

impl<R> From<DamageElement> for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: DamageElement) -> Self {
        Self::AdditionalDamage(elem)
    }
}

#[cfg(feature = "debug")]
impl<R> From<TextureRenderElement<GlesTexture>> for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        Self::Egui(elem)
    }
}

pub trait AsGlowRenderer
where
    Self: Renderer,
{
    fn glow_renderer(&self) -> &GlowRenderer;
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer;
    fn glow_frame<'a, 'frame>(frame: &'a Self::Frame<'frame>) -> &'a GlowFrame<'frame>;
    fn glow_frame_mut<'a, 'frame>(frame: &'a mut Self::Frame<'frame>) -> &'a mut GlowFrame<'frame>;
}

impl AsGlowRenderer for GlowRenderer {
    fn glow_renderer(&self) -> &GlowRenderer {
        self
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self
    }
    fn glow_frame<'a, 'frame>(frame: &'a Self::Frame<'frame>) -> &'a GlowFrame<'frame> {
        frame
    }
    fn glow_frame_mut<'a, 'frame>(frame: &'a mut Self::Frame<'frame>) -> &'a mut GlowFrame<'frame> {
        frame
    }
}

impl<'a> AsGlowRenderer for GlMultiRenderer<'a> {
    fn glow_renderer(&self) -> &GlowRenderer {
        self.as_ref()
    }
    fn glow_renderer_mut(&mut self) -> &mut GlowRenderer {
        self.as_mut()
    }
    fn glow_frame<'b, 'frame>(frame: &'b Self::Frame<'frame>) -> &'b GlowFrame<'frame> {
        frame.as_ref()
    }
    fn glow_frame_mut<'b, 'frame>(frame: &'b mut Self::Frame<'frame>) -> &'b mut GlowFrame<'frame> {
        frame.as_mut()
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
        DamageSet::from_slice(&[Rectangle::from_size(self.geometry(scale).size).into()])
    }
}

impl<R: Renderer> RenderElement<R> for DamageElement {
    fn draw(
        &self,
        _frame: &mut <R as Renderer>::Frame<'_>,
        _src: Rectangle<f64, BufferCoords>,
        _dst: Rectangle<i32, Physical>,
        _damage: &[Rectangle<i32, Physical>],
        _opaque_regions: &[Rectangle<i32, Physical>],
    ) -> Result<(), <R as Renderer>::Error> {
        Ok(())
    }
}

pub trait FromGlesError {
    fn from_gles_error(err: GlesError) -> Self;
}

impl FromGlesError for GlesError {
    fn from_gles_error(err: GlesError) -> Self {
        err
    }
}
