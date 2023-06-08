use crate::{
    backend::render::{
        element::{AsGlowFrame, AsGlowRenderer},
        BackdropShader, GlMultiError, GlMultiFrame, GlMultiRenderer,
    },
    shell::{
        layout::{
            floating::{FloatingLayout, MoveSurfaceGrab},
            tiling::{TilingLayout, ANIMATION_DURATION},
        },
        OverviewMode,
    },
    state::State,
    utils::prelude::*,
    wayland::{
        handlers::screencopy::DropableSession,
        protocols::{
            screencopy::{BufferParams, Session as ScreencopySession},
            toplevel_info::ToplevelInfoState,
            workspace::WorkspaceHandle,
        },
    },
    xwayland::XWaylandState,
};

use calloop::LoopHandle;
use indexmap::IndexSet;
use smithay::{
    backend::renderer::{
        element::{
            surface::WaylandSurfaceRenderElement, texture::TextureRenderElement, AsRenderElements,
            Element, Id, RenderElement,
        },
        gles::{GlesError, GlesTexture},
        glow::{GlowFrame, GlowRenderer},
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Size},
    wayland::seat::WaylandFocus,
    xwayland::X11Surface,
};
use std::{collections::HashMap, time::Instant};
use tracing::warn;

use super::{
    element::{stack::CosmicStackRenderElement, window::CosmicWindowRenderElement, CosmicMapped},
    focus::{FocusStack, FocusStackMut},
    grabs::{ResizeEdge, ResizeGrab},
    CosmicMappedRenderElement, CosmicSurface,
};

#[derive(Debug)]
pub struct Workspace {
    pub tiling_layer: TilingLayout,
    pub floating_layer: FloatingLayout,
    pub tiling_enabled: bool,
    pub fullscreen: HashMap<Output, CosmicSurface>,
    pub handle: WorkspaceHandle,
    pub focus_stack: FocusStacks,
    pub pending_buffers: Vec<(ScreencopySession, BufferParams)>,
    pub screencopy_sessions: Vec<DropableSession>,
    pub(super) backdrop_id: Id,
}

#[derive(Debug, Default)]
pub struct FocusStacks(HashMap<Seat<State>, IndexSet<CosmicMapped>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ManagedState {
    Tiling,
    Floating,
}

impl Workspace {
    pub fn new(handle: WorkspaceHandle, tiling_enabled: bool, gaps: (u8, u8)) -> Workspace {
        Workspace {
            tiling_layer: TilingLayout::new(gaps),
            floating_layer: FloatingLayout::new(),
            tiling_enabled,
            fullscreen: HashMap::new(),
            handle,
            focus_stack: FocusStacks::default(),
            pending_buffers: Vec::new(),
            screencopy_sessions: Vec::new(),
            backdrop_id: Id::new(),
        }
    }

    pub fn refresh(&mut self) {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        self.fullscreen.retain(|_, w| w.alive());
        self.floating_layer.refresh();
        self.tiling_layer.refresh();
    }

    pub fn animations_going(&self) -> bool {
        self.tiling_layer.animations_going()
    }

    pub fn update_animations(&mut self, handle: &LoopHandle<'static, crate::state::Data>) {
        self.tiling_layer.update_animation_state(handle)
    }

    pub fn commit(&mut self, surface: &WlSurface) {
        if let Some(mapped) = self.element_for_wl_surface(surface) {
            mapped
                .windows()
                .find(|(w, _)| w.wl_surface().as_ref() == Some(surface))
                .unwrap()
                .0
                .on_commit();
        }
    }

    pub fn map_output(&mut self, output: &Output, position: Point<i32, Logical>) {
        self.tiling_layer.map_output(output, position);
        self.floating_layer.map_output(output, position);
    }

    pub fn unmap_output(
        &mut self,
        output: &Output,
        toplevel_info: &mut ToplevelInfoState<State, CosmicSurface>,
    ) {
        if let Some(dead_output_window) = self.fullscreen.remove(output) {
            self.unfullscreen_request(&dead_output_window);
        }
        self.tiling_layer.unmap_output(output, toplevel_info);
        self.floating_layer.unmap_output(output, toplevel_info);
        self.refresh();
    }

    pub fn unmap(&mut self, mapped: &CosmicMapped) -> Option<ManagedState> {
        let was_floating = self.floating_layer.unmap(&mapped);
        let was_tiling = self.tiling_layer.unmap(&mapped).is_some();
        if was_floating || was_tiling {
            assert!(was_floating != was_tiling);
        }

        if mapped.is_maximized(true) || mapped.is_fullscreen(true) {
            self.unmaximize_request(&mapped.active_window());
        }

        self.focus_stack
            .0
            .values_mut()
            .for_each(|set| set.retain(|m| m != mapped));
        if was_floating {
            Some(ManagedState::Floating)
        } else if was_tiling {
            Some(ManagedState::Tiling)
        } else {
            None
        }
    }

    pub fn element_for_surface(&self, surface: &CosmicSurface) -> Option<&CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(_, w, _)| w))
            .find(|e| e.windows().any(|(w, _)| &w == surface))
    }

    pub fn element_for_wl_surface(&self, surface: &WlSurface) -> Option<&CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(_, w, _)| w))
            .find(|e| {
                e.windows()
                    .any(|(w, _)| w.wl_surface().as_ref() == Some(surface))
            })
    }

    pub fn outputs_for_element(&self, elem: &CosmicMapped) -> impl Iterator<Item = Output> {
        self.floating_layer
            .space
            .outputs_for_element(elem)
            .into_iter()
            .chain(self.tiling_layer.output_for_element(elem).cloned())
    }

    pub fn output_under(&self, point: Point<i32, Logical>) -> Option<&Output> {
        let space = &self.floating_layer.space;
        space.outputs().find(|o| {
            let internal_output_geo = space.output_geometry(o).unwrap();
            let external_output_geo = o.geometry();
            internal_output_geo.contains(point - external_output_geo.loc + internal_output_geo.loc)
        })
    }

    pub fn element_under(
        &self,
        location: Point<f64, Logical>,
    ) -> Option<(&CosmicMapped, Point<i32, Logical>)> {
        self.floating_layer
            .space
            .element_under(location)
            .or_else(|| {
                self.tiling_layer.mapped().find_map(|(_, mapped, geo)| {
                    geo.contains(location.to_i32_round())
                        .then(|| {
                            let test_point =
                                location - geo.loc.to_f64() + mapped.geometry().loc.to_f64();
                            mapped
                                .is_in_input_region(&test_point)
                                .then_some((mapped, geo.loc - mapped.geometry().loc))
                        })
                        .flatten()
                })
            })
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Logical>> {
        let space = &self.floating_layer.space;
        let outputs = space.outputs().collect::<Vec<_>>();
        let offset = if outputs.len() == 1
            && space.output_geometry(&outputs[0]).unwrap().loc == Point::from((0, 0))
        {
            outputs[0].geometry().loc
        } else {
            (0, 0).into()
        };

        self.floating_layer
            .space
            .element_geometry(elem)
            .or_else(|| self.tiling_layer.element_geometry(elem))
            .map(|mut geo| {
                geo.loc += offset;
                geo
            })
    }

    pub fn maximize_request(&mut self, window: &CosmicSurface, output: &Output) {
        if self.fullscreen.contains_key(output) {
            return;
        }

        self.floating_layer.maximize_request(window);

        window.set_fullscreen(false);
        window.set_maximized(true);
        self.set_fullscreen(window, output)
    }
    pub fn unmaximize_request(&mut self, window: &CosmicSurface) -> Option<Size<i32, Logical>> {
        if self.fullscreen.values().any(|w| w == window) {
            self.unfullscreen_request(window);
            self.floating_layer.unmaximize_request(window)
        } else {
            None
        }
    }

    pub fn fullscreen_request(&mut self, window: &CosmicSurface, output: &Output) {
        if self.fullscreen.contains_key(output) {
            return;
        }

        window.set_maximized(false);
        window.set_fullscreen(true);
        self.set_fullscreen(window, output)
    }

    pub(super) fn set_fullscreen<'a>(
        &mut self,
        window: impl Into<Option<&'a CosmicSurface>>,
        output: &Output,
    ) {
        match window.into() {
            Some(window) => {
                if let Some(mapped) = self
                    .mapped()
                    .find(|m| m.windows().any(|(w, _)| &w == window))
                {
                    mapped.set_active(window);
                }

                window.set_geometry(output.geometry());
                window.send_configure();
                self.fullscreen.insert(output.clone(), window.clone());
            }
            None => {
                if let Some(surface) = self.fullscreen.get(output).cloned() {
                    self.unfullscreen_request(&surface);
                }
            }
        }
    }

    pub fn unfullscreen_request(&mut self, window: &CosmicSurface) {
        if let Some((output, _)) = self.fullscreen.iter().find(|(_, w)| *w == window) {
            window.set_maximized(false);
            window.set_fullscreen(false);
            self.floating_layer.refresh();
            self.tiling_layer.recalculate(output);
            self.tiling_layer.refresh();
            window.send_configure();
            self.fullscreen.retain(|_, w| w != window);
        }
    }

    pub fn maximize_toggle(&mut self, window: &CosmicSurface, output: &Output) {
        if self.fullscreen.contains_key(output) {
            self.unmaximize_request(window);
        } else {
            self.maximize_request(window, output);
        }
    }

    pub fn get_fullscreen(&self, output: &Output) -> Option<&CosmicSurface> {
        self.fullscreen.get(output).filter(|w| w.alive())
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeGrab> {
        if mapped.is_fullscreen(true) || mapped.is_maximized(true) {
            return None;
        }

        if self.floating_layer.mapped().any(|m| m == mapped) {
            self.floating_layer
                .resize_request(mapped, seat, start_data.clone(), edges)
                .map(Into::into)
        } else if self.tiling_layer.mapped().any(|(_, m, _)| m == mapped) {
            self.tiling_layer
                .resize_request(mapped, seat, start_data, edges)
                .map(Into::into)
        } else {
            None
        }
    }

    pub fn move_request(
        &mut self,
        window: &CosmicSurface,
        seat: &Seat<State>,
        output: &Output,
        start_data: PointerGrabStartData<State>,
        indicator_thickness: u8,
    ) -> Option<MoveSurfaceGrab> {
        let pointer = seat.get_pointer().unwrap();
        let pos = pointer.current_location();

        let mapped = self.element_for_surface(&window)?.clone();
        let mut initial_window_location = self.element_geometry(&mapped).unwrap().loc;

        if mapped.is_fullscreen(true) || mapped.is_maximized(true) {
            // If surface is maximized then unmaximize it
            let new_size = self.unmaximize_request(window);
            let ratio = pos.x / output.geometry().size.w as f64;

            initial_window_location = new_size
                .map(|size| (pos.x - (size.w as f64 * ratio), pos.y).into())
                .unwrap_or_else(|| pos)
                .to_i32_round();
        }

        let was_floating = self.floating_layer.unmap(&mapped);
        //let was_tiled = self.tiling_layer.unmap(&mapped);
        //assert!(was_floating != was_tiled);

        if was_floating {
            Some(MoveSurfaceGrab::new(
                start_data,
                mapped,
                seat,
                pos,
                initial_window_location,
                indicator_thickness,
            ))
        } else {
            None // TODO
        }
    }

    pub fn toggle_tiling(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            for window in self
                .tiling_layer
                .mapped()
                .map(|(_, m, _)| m.clone())
                .collect::<Vec<_>>()
                .into_iter()
            {
                self.tiling_layer.unmap(&window);
                self.floating_layer.map(window, seat, None);
            }
            self.tiling_enabled = false;
        } else {
            let focus_stack = self.focus_stack.get(seat);
            for window in self
                .floating_layer
                .mapped()
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                self.floating_layer.unmap(&window);
                self.tiling_layer
                    .map(window, seat, focus_stack.iter(), None)
            }
            self.tiling_enabled = true;
        }
    }

    pub fn toggle_floating_window(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            if let Some(window) = self.focus_stack.get(seat).iter().next().cloned() {
                if self.tiling_layer.mapped().any(|(_, m, _)| m == &window) {
                    self.tiling_layer.unmap(&window);
                    self.floating_layer.map(window, seat, None);
                } else if self.floating_layer.mapped().any(|w| w == &window) {
                    let focus_stack = self.focus_stack.get(seat);
                    self.floating_layer.unmap(&window);
                    self.tiling_layer
                        .map(window, seat, focus_stack.iter(), None)
                }
            }
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(_, w, _)| w))
    }

    pub fn outputs(&self) -> impl Iterator<Item = &Output> {
        self.floating_layer.space.outputs()
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        self.floating_layer
            .windows()
            .chain(self.tiling_layer.windows().map(|(_, w, _)| w))
    }

    pub fn is_fullscreen(&self, mapped: &CosmicMapped) -> bool {
        self.fullscreen
            .values()
            .any(|s| s == &mapped.active_window())
    }

    pub fn is_floating(&self, mapped: &CosmicMapped) -> bool {
        !self
            .fullscreen
            .values()
            .any(|s| s == &mapped.active_window())
            && self.floating_layer.mapped().any(|m| m == mapped)
    }

    pub fn is_tiled(&self, mapped: &CosmicMapped) -> bool {
        !self
            .fullscreen
            .values()
            .any(|s| s == &mapped.active_window())
            && self.tiling_layer.mapped().any(|(_, m, _)| m == mapped)
    }

    pub fn render_output<'a, R>(
        &self,
        renderer: &mut R,
        output: &Output,
        override_redirect_windows: &[X11Surface],
        xwm_state: Option<&'a mut XWaylandState>,
        draw_focus_indicator: Option<&Seat<State>>,
        overview: OverviewMode,
        indicator_thickness: u8,
    ) -> Result<Vec<WorkspaceRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
        WorkspaceRenderElement<R>: RenderElement<R>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let mut render_elements = Vec::new();

        let output_scale = output.current_scale().fractional_scale();
        let layer_map = layer_map_for_output(output);
        let zone = layer_map.non_exclusive_zone();

        if let Some(fullscreen) = self.fullscreen.get(output) {
            render_elements.extend(
                override_redirect_windows
                    .iter()
                    .filter(|or| or.geometry().intersection(output.geometry()).is_some())
                    .flat_map(|or| {
                        AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                            or,
                            renderer,
                            (or.geometry().loc - output.geometry().loc)
                                .to_physical_precise_round(output_scale),
                            Scale::from(output_scale),
                            1.0,
                        )
                    }),
            );

            // fullscreen window
            render_elements.extend(AsRenderElements::<R>::render_elements::<
                WorkspaceRenderElement<R>,
            >(
                fullscreen,
                renderer,
                (0, 0).into(),
                output_scale.into(),
                1.0,
            ));

            if let Some(xwm) = xwm_state.and_then(|state| state.xwm.as_mut()) {
                if let Err(err) =
                    xwm.update_stacking_order_upwards(render_elements.iter().rev().map(|e| e.id()))
                {
                    warn!(
                        wm_id = ?xwm.id(),
                        ?err,
                        "Failed to update Xwm stacking order.",
                    );
                }
            }
        } else {
            // TODO: Handle modes like
            // - keyboard window swapping
            // - resizing in tiling

            // OR windows above all
            render_elements.extend(
                override_redirect_windows
                    .iter()
                    .filter(|or| or.geometry().intersection(output.geometry()).is_some())
                    .flat_map(|or| {
                        AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                            or,
                            renderer,
                            (or.geometry().loc - output.geometry().loc)
                                .to_physical_precise_round(output_scale),
                            Scale::from(output_scale),
                            1.0,
                        )
                    }),
            );

            let focused =
                draw_focus_indicator.and_then(|seat| self.focus_stack.get(seat).last().cloned());

            // floating surfaces
            let alpha = match &overview {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::None => 1.0,
            };
            render_elements.extend(
                self.floating_layer
                    .render_output::<R>(
                        renderer,
                        output,
                        focused.as_ref(),
                        indicator_thickness,
                        alpha,
                    )
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            //tiling surfaces
            render_elements.extend(
                self.tiling_layer
                    .render_output::<R>(
                        renderer,
                        output,
                        draw_focus_indicator,
                        layer_map.non_exclusive_zone(),
                        overview.clone(),
                        indicator_thickness,
                    )?
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            if let Some(xwm) = xwm_state.and_then(|state| state.xwm.as_mut()) {
                if let Err(err) =
                    xwm.update_stacking_order_upwards(render_elements.iter().rev().map(|e| e.id()))
                {
                    warn!(
                        wm_id = ?xwm.id(),
                        ?err,
                        "Failed to update Xwm stacking order.",
                    );
                }
            }

            let alpha = match overview {
                OverviewMode::Started(_, start) => Some(
                    (Instant::now().duration_since(start).as_millis() as f64 / 100.0).min(1.0)
                        as f32,
                ),
                OverviewMode::Ended(ended) => Some(
                    1.0 - (Instant::now().duration_since(ended).as_millis() as f64 / 100.0).min(1.0)
                        as f32,
                ),
                _ => None,
            };

            if let Some(alpha) = alpha {
                render_elements.push(
                    Into::<CosmicMappedRenderElement<R>>::into(BackdropShader::element(
                        renderer,
                        self.backdrop_id.clone(),
                        zone,
                        0.,
                        alpha * 0.85,
                        [0.0, 0.0, 0.0],
                    ))
                    .into(),
                )
            }
        }

        Ok(render_elements)
    }
}

impl FocusStacks {
    pub fn get<'a>(&'a self, seat: &Seat<State>) -> FocusStack<'a> {
        FocusStack(self.0.get(seat))
    }

    pub fn get_mut<'a>(&'a mut self, seat: &Seat<State>) -> FocusStackMut<'a> {
        FocusStackMut(self.0.entry(seat.clone()).or_default())
    }
}

pub struct OutputNotMapped;

pub enum WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
{
    Wayland(WaylandSurfaceRenderElement<R>),
    Window(CosmicMappedRenderElement<R>),
    Backdrop(TextureRenderElement<GlesTexture>),
}

impl<R> Element for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> &smithay::backend::renderer::element::Id {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.id(),
            WorkspaceRenderElement::Window(elem) => elem.id(),
            WorkspaceRenderElement::Backdrop(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.current_commit(),
            WorkspaceRenderElement::Window(elem) => elem.current_commit(),
            WorkspaceRenderElement::Backdrop(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.src(),
            WorkspaceRenderElement::Window(elem) => elem.src(),
            WorkspaceRenderElement::Backdrop(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Window(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.location(scale),
            WorkspaceRenderElement::Window(elem) => elem.location(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.transform(),
            WorkspaceRenderElement::Window(elem) => elem.transform(),
            WorkspaceRenderElement::Backdrop(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<smithay::backend::renderer::utils::CommitCounter>,
    ) -> Vec<Rectangle<i32, smithay::utils::Physical>> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Window(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Backdrop(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, smithay::utils::Physical>> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Window(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.alpha(),
            WorkspaceRenderElement::Window(elem) => elem.alpha(),
            WorkspaceRenderElement::Backdrop(elem) => elem.alpha(),
        }
    }
}

impl RenderElement<GlowRenderer> for WorkspaceRenderElement<GlowRenderer> {
    fn draw<'frame>(
        &self,
        frame: &mut GlowFrame<'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, smithay::utils::Physical>],
    ) -> Result<(), GlesError> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.draw(frame, src, dst, damage),
            WorkspaceRenderElement::Window(elem) => elem.draw(frame, src, dst, damage),
            WorkspaceRenderElement::Backdrop(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame, src, dst, damage)
            }
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut GlowRenderer,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Window(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Backdrop(elem) => elem.underlying_storage(renderer),
        }
    }
}

impl<'a, 'b> RenderElement<GlMultiRenderer<'a, 'b>>
    for WorkspaceRenderElement<GlMultiRenderer<'a, 'b>>
{
    fn draw<'frame>(
        &self,
        frame: &mut GlMultiFrame<'a, 'b, 'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, smithay::utils::Physical>],
    ) -> Result<(), GlMultiError> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.draw(frame, src, dst, damage),
            WorkspaceRenderElement::Window(elem) => elem.draw(frame, src, dst, damage),
            WorkspaceRenderElement::Backdrop(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame.glow_frame_mut(), src, dst, damage)
                    .map_err(GlMultiError::Render)
            }
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut GlMultiRenderer<'a, 'b>,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage> {
        match self {
            WorkspaceRenderElement::Wayland(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Window(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Backdrop(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
        }
    }
}

impl<R> From<WaylandSurfaceRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: WaylandSurfaceRenderElement<R>) -> Self {
        WorkspaceRenderElement::Wayland(elem)
    }
}

impl<R> From<CosmicMappedRenderElement<R>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: CosmicMappedRenderElement<R>) -> Self {
        WorkspaceRenderElement::Window(elem)
    }
}

impl<R> From<TextureRenderElement<GlesTexture>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        WorkspaceRenderElement::Backdrop(elem)
    }
}
