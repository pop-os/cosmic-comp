use crate::shell::{CosmicMappedRenderElement, WorkspaceRenderElement};

use smithay::{
    backend::renderer::{
        element::{
            surface::WaylandSurfaceRenderElement,
            utils::{Relocate, RelocateRenderElement},
            Element, Id, RenderElement, UnderlyingStorage,
        },
        glow::{GlowFrame, GlowRenderer},
        utils::CommitCounter,
        Frame, ImportAll, ImportMem, Renderer,
    },
    utils::{Buffer as BufferCoords, Logical, Physical, Point, Rectangle, Scale},
};

#[cfg(feature = "debug")]
use smithay::backend::renderer::{element::texture::TextureRenderElement, gles::GlesTexture};

use super::{cursor::CursorRenderElement, GlMultiError, GlMultiFrame, GlMultiRenderer};

pub enum CosmicElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    Workspace(RelocateRenderElement<WorkspaceRenderElement<R>>),
    Cursor(RelocateRenderElement<CursorRenderElement<R>>),
    Dnd(WaylandSurfaceRenderElement<R>),
    MoveGrab(CosmicMappedRenderElement<R>),
    AdditionalDamage(DamageElement),
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
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicElement::Workspace(elem) => elem.damage_since(scale, commit),
            CosmicElement::Cursor(elem) => elem.damage_since(scale, commit),
            CosmicElement::Dnd(elem) => elem.damage_since(scale, commit),
            CosmicElement::MoveGrab(elem) => elem.damage_since(scale, commit),
            CosmicElement::AdditionalDamage(elem) => elem.damage_since(scale, commit),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicElement::Workspace(elem) => elem.opaque_regions(scale),
            CosmicElement::Cursor(elem) => elem.opaque_regions(scale),
            CosmicElement::Dnd(elem) => elem.opaque_regions(scale),
            CosmicElement::MoveGrab(elem) => elem.opaque_regions(scale),
            CosmicElement::AdditionalDamage(elem) => elem.opaque_regions(scale),
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
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.alpha(),
        }
    }
}

impl RenderElement<GlowRenderer> for CosmicElement<GlowRenderer> {
    fn draw<'frame>(
        &self,
        frame: &mut <GlowRenderer as Renderer>::Frame<'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlowRenderer as Renderer>::Error> {
        match self {
            CosmicElement::Workspace(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::Cursor(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::Dnd(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::MoveGrab(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::AdditionalDamage(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame, src, dst, damage)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame, src, dst, damage)
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut GlowRenderer) -> Option<UnderlyingStorage> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::Dnd(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            CosmicElement::AdditionalDamage(elem) => elem.underlying_storage(renderer),
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => elem.underlying_storage(renderer),
        }
    }
}

impl<'a> RenderElement<GlMultiRenderer<'a>> for CosmicElement<GlMultiRenderer<'a>> {
    fn draw<'frame>(
        &self,
        frame: &mut GlMultiFrame<'a, 'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), GlMultiError> {
        match self {
            CosmicElement::Workspace(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::Cursor(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::Dnd(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::MoveGrab(elem) => elem.draw(frame, src, dst, damage),
            CosmicElement::AdditionalDamage(elem) => {
                RenderElement::<GlMultiRenderer<'a>>::draw(elem, frame, src, dst, damage)
            }
            #[cfg(feature = "debug")]
            CosmicElement::Egui(elem) => {
                let elem = {
                    let glow_frame = frame.glow_frame_mut();
                    RenderElement::<GlowRenderer>::draw(elem, glow_frame, src, dst, damage)
                        .map_err(|err| GlMultiError::Render(err))
                };
                elem
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut GlMultiRenderer<'a>) -> Option<UnderlyingStorage> {
        match self {
            CosmicElement::Workspace(elem) => elem.underlying_storage(renderer),
            CosmicElement::Cursor(elem) => elem.underlying_storage(renderer),
            CosmicElement::Dnd(elem) => elem.underlying_storage(renderer),
            CosmicElement::MoveGrab(elem) => elem.underlying_storage(renderer),
            CosmicElement::AdditionalDamage(elem) => elem.underlying_storage(renderer),
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

impl<R> From<WorkspaceRenderElement<R>> for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: WorkspaceRenderElement<R>) -> Self {
        Self::Workspace(RelocateRenderElement::from_element(
            elem,
            (0, 0),
            Relocate::Relative,
        ))
    }
}

impl<R> From<CosmicMappedRenderElement<R>> for CosmicElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicMappedRenderElement<R>) -> Self {
        Self::MoveGrab(elem)
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

pub trait AsGlowFrame<'a>
where
    Self: Frame,
{
    fn glow_frame(&self) -> &GlowFrame<'a>;
    fn glow_frame_mut(&mut self) -> &mut GlowFrame<'a>;
}

impl<'frame> AsGlowFrame<'frame> for GlowFrame<'frame> {
    fn glow_frame(&self) -> &GlowFrame<'frame> {
        self
    }
    fn glow_frame_mut(&mut self) -> &mut GlowFrame<'frame> {
        self
    }
}

impl<'renderer, 'frame> AsGlowFrame<'frame> for GlMultiFrame<'renderer, 'frame> {
    fn glow_frame(&self) -> &GlowFrame<'frame> {
        self.as_ref()
    }
    fn glow_frame_mut(&mut self) -> &mut GlowFrame<'frame> {
        self.as_mut()
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
        Rectangle::from_loc_and_size((0.0, 0.0), (1.0, 1.0))
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry.to_f64().to_physical(scale).to_i32_round()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        _commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        vec![Rectangle::from_loc_and_size(
            (0, 0),
            self.geometry(scale).size,
        )]
    }
}

impl<R: Renderer> RenderElement<R> for DamageElement {
    fn draw(
        &self,
        _frame: &mut <R as Renderer>::Frame<'_>,
        _src: Rectangle<f64, BufferCoords>,
        _dst: Rectangle<i32, Physical>,
        _damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <R as Renderer>::Error> {
        Ok(())
    }
}
