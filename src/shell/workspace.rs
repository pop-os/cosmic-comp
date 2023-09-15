use crate::{
    backend::render::{
        element::{AsGlowFrame, AsGlowRenderer},
        BackdropShader, GlMultiError, GlMultiFrame, GlMultiRenderer,
    },
    shell::{
        grabs::MoveGrab,
        layout::{floating::FloatingLayout, tiling::TilingLayout},
        OverviewMode, ANIMATION_DURATION,
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
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

use id_tree::Tree;
use indexmap::IndexSet;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::renderer::{
        element::{
            surface::WaylandSurfaceRenderElement, texture::TextureRenderElement,
            utils::RescaleRenderElement, AsRenderElements, Element, Id, RenderElement,
        },
        gles::{GlesError, GlesTexture},
        glow::{GlowFrame, GlowRenderer},
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client, Resource},
    utils::{Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Size},
    wayland::{
        compositor::{add_blocker, Blocker, BlockerState},
        seat::WaylandFocus,
    },
    xwayland::X11Surface,
};
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};
use tracing::warn;
use wayland_backend::server::ClientId;

use super::{
    element::{
        resize_indicator::ResizeIndicator, stack::CosmicStackRenderElement,
        swap_indicator::SwapIndicator, window::CosmicWindowRenderElement, CosmicMapped,
    },
    focus::{
        target::{KeyboardFocusTarget, PointerFocusTarget, WindowGroup},
        FocusStack, FocusStackMut,
    },
    grabs::{ResizeEdge, ResizeGrab},
    layout::tiling::{Data, NodeDesc},
    CosmicMappedRenderElement, CosmicSurface, ResizeDirection, ResizeMode,
};

const FULLSCREEN_ANIMATION_DURATION: Duration = Duration::from_millis(200);

#[derive(Debug)]
pub struct Workspace {
    pub tiling_layer: TilingLayout,
    pub floating_layer: FloatingLayout,
    pub tiling_enabled: bool,
    pub fullscreen: HashMap<Output, FullscreenSurface>,
    pub handle: WorkspaceHandle,
    pub focus_stack: FocusStacks,
    pub pending_buffers: Vec<(ScreencopySession, BufferParams)>,
    pub screencopy_sessions: Vec<DropableSession>,
    pub(super) backdrop_id: Id,
}

#[derive(Debug, Clone)]
pub struct FullscreenSurface {
    pub surface: CosmicSurface,
    pub exclusive: bool,
    original_size: Size<i32, Logical>,
    start_at: Option<Instant>,
    ended_at: Option<Instant>,
    animation_signal: Option<Arc<AtomicBool>>,
}

struct FullscreenBlocker {
    signal: Arc<AtomicBool>,
}

impl Blocker for FullscreenBlocker {
    fn state(&self) -> BlockerState {
        if self.signal.load(Ordering::SeqCst) {
            BlockerState::Released
        } else {
            BlockerState::Pending
        }
    }
}

impl FullscreenSurface {
    pub fn is_animating(&self) -> bool {
        self.start_at.is_some() || self.ended_at.is_some()
    }
}

impl IsAlive for FullscreenSurface {
    fn alive(&self) -> bool {
        self.surface.alive()
    }
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

    pub fn refresh_focus_stack(&mut self) {
        let windows: Vec<CosmicMapped> = self.mapped().cloned().collect();
        for stack in self.focus_stack.0.values_mut() {
            stack.retain(|w| windows.contains(w));
        }
    }

    pub fn animations_going(&self) -> bool {
        self.tiling_layer.animations_going()
            || self
                .fullscreen
                .values()
                .any(|f| f.start_at.is_some() || f.ended_at.is_some())
    }

    pub fn update_animations(&mut self) -> HashMap<ClientId, Client> {
        let mut clients = Vec::new();

        for f in self.fullscreen.values_mut() {
            if let Some(start) = f.start_at.as_ref() {
                let duration_since = Instant::now().duration_since(*start);
                if duration_since > FULLSCREEN_ANIMATION_DURATION {
                    f.start_at.take();
                }
                if duration_since * 2 > FULLSCREEN_ANIMATION_DURATION {
                    if let Some(signal) = f.animation_signal.take() {
                        signal.store(true, Ordering::SeqCst);
                        if let Some(client) =
                            f.surface.wl_surface().as_ref().and_then(Resource::client)
                        {
                            clients.push((client.id(), client));
                        }
                    }
                }
            }
        }
        self.fullscreen.retain(|_, f| match f.ended_at {
            None => true,
            Some(instant) => {
                let duration_since = Instant::now().duration_since(instant);
                if duration_since * 2 > FULLSCREEN_ANIMATION_DURATION {
                    if let Some(signal) = f.animation_signal.take() {
                        signal.store(true, Ordering::SeqCst);
                        if let Some(client) =
                            f.surface.wl_surface().as_ref().and_then(Resource::client)
                        {
                            clients.push((client.id(), client));
                        }
                    }
                }

                duration_since < FULLSCREEN_ANIMATION_DURATION
            }
        });

        let mut updates = self.tiling_layer.update_animation_state();
        updates.extend(clients);
        updates
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
            self.unfullscreen_request(&dead_output_window.surface);
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
        &mut self,
        location: Point<f64, Logical>,
        overview: OverviewMode,
    ) -> Option<(PointerFocusTarget, Point<i32, Logical>)> {
        self.floating_layer
            .space
            .element_under(location)
            .map(|(mapped, p)| (mapped.clone().into(), p))
            .or_else(|| self.tiling_layer.element_under(location, overview))
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

    pub fn recalculate(&mut self, output: &Output) {
        if let Some(f) = self.fullscreen.get(output) {
            if !f.exclusive {
                f.surface
                    .set_geometry(layer_map_for_output(output).non_exclusive_zone());
            }
        }
        self.tiling_layer.recalculate(output);
    }

    pub fn maximize_request(&mut self, window: &CosmicSurface, output: &Output) {
        if self.fullscreen.contains_key(output) {
            return;
        }

        self.floating_layer.maximize_request(window);

        window.set_fullscreen(false);
        window.set_maximized(true);
        self.set_fullscreen(window, output, false)
    }
    pub fn unmaximize_request(&mut self, window: &CosmicSurface) -> Option<Size<i32, Logical>> {
        if self.fullscreen.values().any(|w| &w.surface == window) {
            self.unfullscreen_request(window);
            self.floating_layer.unmaximize_request(window)
        } else {
            None
        }
    }

    pub fn fullscreen_request(&mut self, window: &CosmicSurface, output: &Output) {
        if self
            .fullscreen
            .get(output)
            .map(|w| &w.surface != window)
            .unwrap_or(false)
        {
            return;
        }

        if !window.is_maximized(true) {
            self.floating_layer.maximize_request(window);
        }

        window.set_maximized(false);
        window.set_fullscreen(true);
        self.set_fullscreen(window, output, true)
    }

    fn set_fullscreen<'a>(&mut self, window: &'a CosmicSurface, output: &Output, exclusive: bool) {
        if let Some(mapped) = self
            .mapped()
            .find(|m| m.windows().any(|(w, _)| &w == window))
        {
            mapped.set_active(window);
        }

        window.set_geometry(if exclusive {
            output.geometry()
        } else {
            layer_map_for_output(output).non_exclusive_zone()
        });
        let original_size = window.geometry().size;
        let signal = if let Some(surface) = window.wl_surface() {
            let signal = Arc::new(AtomicBool::new(false));
            add_blocker(
                &surface,
                FullscreenBlocker {
                    signal: signal.clone(),
                },
            );
            Some(signal)
        } else {
            None
        };
        window.send_configure();

        self.fullscreen.insert(
            output.clone(),
            FullscreenSurface {
                surface: window.clone(),
                exclusive,
                original_size,
                start_at: Some(Instant::now()),
                ended_at: None,
                animation_signal: signal,
            },
        );
    }

    pub fn unfullscreen_request(&mut self, window: &CosmicSurface) {
        if let Some((output, f)) = self
            .fullscreen
            .iter_mut()
            .find(|(_, w)| &w.surface == window)
        {
            window.set_maximized(false);
            window.set_fullscreen(false);
            self.floating_layer.unmaximize_request(window);
            self.floating_layer.refresh();
            self.tiling_layer.recalculate(output);
            self.tiling_layer.refresh();
            let signal = if let Some(surface) = window.wl_surface() {
                let signal = Arc::new(AtomicBool::new(false));
                add_blocker(
                    &surface,
                    FullscreenBlocker {
                        signal: signal.clone(),
                    },
                );
                Some(signal)
            } else {
                None
            };
            window.send_configure();

            f.ended_at = Some(
                Instant::now()
                    - (FULLSCREEN_ANIMATION_DURATION
                        - f.start_at
                            .take()
                            .map(|earlier| {
                                Instant::now()
                                    .duration_since(earlier)
                                    .min(FULLSCREEN_ANIMATION_DURATION)
                            })
                            .unwrap_or(FULLSCREEN_ANIMATION_DURATION)),
            );
            if let Some(new_signal) = signal {
                if let Some(old_signal) = f.animation_signal.replace(new_signal) {
                    old_signal.store(true, Ordering::SeqCst);
                }
            }
        }
    }

    pub fn remove_fullscreen(&mut self, output: &Output) {
        if let Some(FullscreenSurface {
            surface,
            ended_at,
            start_at,
            animation_signal,
            ..
        }) = self.fullscreen.get_mut(output)
        {
            surface.set_maximized(false);
            surface.set_fullscreen(false);
            self.floating_layer.unmaximize_request(&surface);
            self.floating_layer.refresh();
            self.tiling_layer.recalculate(output);
            self.tiling_layer.refresh();
            let signal = if let Some(surface) = surface.wl_surface() {
                let signal = Arc::new(AtomicBool::new(false));
                add_blocker(
                    &surface,
                    FullscreenBlocker {
                        signal: signal.clone(),
                    },
                );
                Some(signal)
            } else {
                None
            };
            surface.send_configure();

            *ended_at = Some(
                Instant::now()
                    - (FULLSCREEN_ANIMATION_DURATION
                        - start_at
                            .take()
                            .map(|earlier| {
                                Instant::now()
                                    .duration_since(earlier)
                                    .min(FULLSCREEN_ANIMATION_DURATION)
                            })
                            .unwrap_or(FULLSCREEN_ANIMATION_DURATION)),
            );
            if let Some(new_signal) = signal {
                if let Some(old_signal) = animation_signal.replace(new_signal) {
                    old_signal.store(true, Ordering::SeqCst);
                }
            }
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
        self.fullscreen
            .get(output)
            .filter(|w| w.alive() && w.exclusive)
            .filter(|w| w.ended_at.is_none() && w.start_at.is_none())
            .map(|w| &w.surface)
    }

    pub fn get_maximized(&self, output: &Output) -> Option<&CosmicSurface> {
        self.fullscreen
            .get(output)
            .filter(|w| w.alive() && !w.exclusive)
            .filter(|w| w.ended_at.is_none() && w.start_at.is_none())
            .map(|w| &w.surface)
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
        } else {
            None
        }
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edge: ResizeEdge,
        amount: i32,
    ) -> bool {
        if let Some(toplevel) = focused.toplevel() {
            if self
                .fullscreen
                .values()
                .any(|f| f.surface.wl_surface().as_ref() == Some(&toplevel))
            {
                return false;
            }
        }

        if !self.floating_layer.resize(focused, direction, edge, amount) {
            self.tiling_layer.resize(focused, direction, edge, amount)
        } else {
            true
        }
    }

    pub fn move_request(
        &mut self,
        window: &CosmicSurface,
        seat: &Seat<State>,
        output: &Output,
        start_data: PointerGrabStartData<State>,
        indicator_thickness: u8,
    ) -> Option<MoveGrab> {
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
        let was_tiled = self.tiling_layer.unmap_as_placeholder(&mapped);
        assert!(was_floating != was_tiled.is_some());

        Some(MoveGrab::new(
            start_data,
            mapped,
            seat,
            pos,
            initial_window_location,
            indicator_thickness,
            was_tiled.is_some(),
        ))
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
            .any(|f| f.exclusive && f.surface == mapped.active_window())
    }

    pub fn is_maximized(&self, mapped: &CosmicMapped) -> bool {
        self.fullscreen
            .values()
            .any(|f| !f.exclusive && f.surface == mapped.active_window())
    }

    pub fn is_floating(&self, mapped: &CosmicMapped) -> bool {
        !self
            .fullscreen
            .values()
            .any(|f| f.surface == mapped.active_window())
            && self.floating_layer.mapped().any(|m| m == mapped)
    }

    pub fn is_tiled(&self, mapped: &CosmicMapped) -> bool {
        !self
            .fullscreen
            .values()
            .any(|f| f.surface == mapped.active_window())
            && self.tiling_layer.mapped().any(|(_, m, _)| m == mapped)
    }

    pub fn node_desc(&self, focus: KeyboardFocusTarget) -> Option<NodeDesc> {
        match focus {
            KeyboardFocusTarget::Element(mapped) => {
                self.tiling_layer.mapped().find_map(|(output, m, _)| {
                    (m == &mapped).then_some(output.clone()).and_then(|output| {
                        mapped
                            .tiling_node_id
                            .lock()
                            .unwrap()
                            .clone()
                            .map(|node_id| NodeDesc {
                                handle: self.handle.clone(),
                                output: output.downgrade(),
                                node: node_id,
                                stack_window: if mapped
                                    .stack_ref()
                                    .map(|stack| !stack.whole_stack_focused())
                                    .unwrap_or(false)
                                {
                                    Some(mapped.active_window())
                                } else {
                                    None
                                },
                            })
                    })
                })
            }
            KeyboardFocusTarget::Group(WindowGroup { output, node, .. }) => Some(NodeDesc {
                handle: self.handle.clone(),
                output,
                node,
                stack_window: None,
            }),
            _ => None,
        }
    }

    pub fn render_output<'a, R>(
        &self,
        renderer: &mut R,
        output: &Output,
        override_redirect_windows: &[X11Surface],
        xwm_state: Option<&'a mut XWaylandState>,
        draw_focus_indicator: Option<&Seat<State>>,
        overview: (OverviewMode, Option<(SwapIndicator, Option<&Tree<Data>>)>),
        resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
    ) -> Result<
        (
            Vec<WorkspaceRenderElement<R>>,
            Vec<WorkspaceRenderElement<R>>,
        ),
        OutputNotMapped,
    >
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

        let mut window_elements = Vec::new();
        let mut popup_elements = Vec::new();

        let output_scale = output.current_scale().fractional_scale();
        let layer_map = layer_map_for_output(output);
        let zone = layer_map.non_exclusive_zone();

        // OR windows above all
        popup_elements.extend(
            override_redirect_windows
                .iter()
                .filter(|or| (*or).geometry().intersection(output.geometry()).is_some())
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

        if let Some(fullscreen) = self.fullscreen.get(output) {
            // fullscreen window
            let bbox = fullscreen.surface.bbox();
            let element_geo = Rectangle::from_loc_and_size(
                self.element_for_surface(&fullscreen.surface)
                    .and_then(|elem| {
                        self.floating_layer
                            .space
                            .element_geometry(elem)
                            .or_else(|| self.tiling_layer.element_geometry(elem))
                            .map(|mut geo| {
                                geo.loc -= elem.geometry().loc;
                                geo
                            })
                    })
                    .unwrap_or(bbox)
                    .loc,
                fullscreen.original_size,
            );

            let mut full_geo = if fullscreen.exclusive {
                Rectangle::from_loc_and_size((0, 0), output.geometry().size)
            } else {
                layer_map.non_exclusive_zone()
            };

            if fullscreen.start_at.is_none() {
                if bbox != full_geo {
                    if bbox.size.w < full_geo.size.w {
                        full_geo.loc.x += (full_geo.size.w - bbox.size.w) / 2;
                        full_geo.size.w = bbox.size.w;
                    }
                    if bbox.size.h < full_geo.size.h {
                        full_geo.loc.y += (full_geo.size.h - bbox.size.h) / 2;
                        full_geo.size.h = bbox.size.h;
                    }
                }
            }

            let (target_geo, alpha) = match (fullscreen.start_at, fullscreen.ended_at) {
                (Some(started), _) => {
                    let duration = Instant::now().duration_since(started).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(element_geo),
                            EaseRectangle(full_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 0.0, 1.0, duration),
                    )
                }
                (_, Some(ended)) => {
                    let duration = Instant::now().duration_since(ended).as_secs_f64()
                        / FULLSCREEN_ANIMATION_DURATION.as_secs_f64();
                    (
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(full_geo),
                            EaseRectangle(element_geo),
                            duration,
                        )
                        .0,
                        ease(EaseInOutCubic, 1.0, 0.0, duration),
                    )
                }
                (None, None) => (full_geo, 1.0),
            };

            let render_loc = target_geo.loc.to_physical_precise_round(output_scale);
            let scale = Scale {
                x: target_geo.size.w as f64 / bbox.size.w as f64,
                y: target_geo.size.h as f64 / bbox.size.h as f64,
            };

            window_elements.extend(
                AsRenderElements::<R>::render_elements::<WaylandSurfaceRenderElement<R>>(
                    &fullscreen.surface,
                    renderer,
                    render_loc,
                    output_scale.into(),
                    alpha,
                )
                .into_iter()
                .map(|elem| RescaleRenderElement::from_element(elem, render_loc, scale))
                .map(Into::into),
            );
        }

        if self
            .fullscreen
            .get(output)
            .map(|f| f.start_at.is_some() || f.ended_at.is_some())
            .unwrap_or(true)
        {
            let focused =
                draw_focus_indicator.and_then(|seat| self.focus_stack.get(seat).last().cloned());

            // floating surfaces
            let alpha = match &overview.0 {
                OverviewMode::Started(_, started) => {
                    (1.0 - (Instant::now().duration_since(*started).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        .max(0.0)
                        * 0.4
                        + 0.6
                }
                OverviewMode::Ended(_, ended) => {
                    ((Instant::now().duration_since(*ended).as_millis()
                        / ANIMATION_DURATION.as_millis()) as f32)
                        * 0.4
                        + 0.6
                }
                OverviewMode::None => 1.0,
            };

            let (w_elements, p_elements) = self.floating_layer.render_output::<R>(
                renderer,
                output,
                focused.as_ref(),
                resize_indicator.clone(),
                indicator_thickness,
                alpha,
            );
            popup_elements.extend(p_elements.into_iter().map(WorkspaceRenderElement::from));
            window_elements.extend(w_elements.into_iter().map(WorkspaceRenderElement::from));

            let alpha = match &overview.0 {
                OverviewMode::Started(_, start) => Some(
                    (Instant::now().duration_since(*start).as_millis() as f64 / 100.0).min(1.0)
                        as f32,
                ),
                OverviewMode::Ended(_, ended) => Some(
                    1.0 - (Instant::now().duration_since(*ended).as_millis() as f64 / 100.0)
                        .min(1.0) as f32,
                ),
                _ => None,
            };

            //tiling surfaces
            let (w_elements, p_elements) = self.tiling_layer.render_output::<R>(
                renderer,
                output,
                draw_focus_indicator,
                layer_map.non_exclusive_zone(),
                overview,
                resize_indicator,
                indicator_thickness,
            )?;
            popup_elements.extend(p_elements.into_iter().map(WorkspaceRenderElement::from));
            window_elements.extend(w_elements.into_iter().map(WorkspaceRenderElement::from));

            if let Some(alpha) = alpha {
                window_elements.push(
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

        if let Some(xwm) = xwm_state.and_then(|state| state.xwm.as_mut()) {
            if let Err(err) =
                xwm.update_stacking_order_upwards(window_elements.iter().map(|e| e.id()))
            {
                warn!(
                    wm_id = ?xwm.id(),
                    ?err,
                    "Failed to update Xwm stacking order.",
                );
            }
        }

        Ok((window_elements, popup_elements))
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
    Fullscreen(RescaleRenderElement<WaylandSurfaceRenderElement<R>>),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.id(),
            WorkspaceRenderElement::Wayland(elem) => elem.id(),
            WorkspaceRenderElement::Window(elem) => elem.id(),
            WorkspaceRenderElement::Backdrop(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.current_commit(),
            WorkspaceRenderElement::Wayland(elem) => elem.current_commit(),
            WorkspaceRenderElement::Window(elem) => elem.current_commit(),
            WorkspaceRenderElement::Backdrop(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.src(),
            WorkspaceRenderElement::Wayland(elem) => elem.src(),
            WorkspaceRenderElement::Window(elem) => elem.src(),
            WorkspaceRenderElement::Backdrop(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Wayland(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Window(elem) => elem.geometry(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, smithay::utils::Physical> {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.location(scale),
            WorkspaceRenderElement::Wayland(elem) => elem.location(scale),
            WorkspaceRenderElement::Window(elem) => elem.location(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.transform(),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Wayland(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Window(elem) => elem.damage_since(scale, commit),
            WorkspaceRenderElement::Backdrop(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, smithay::utils::Physical>> {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Wayland(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Window(elem) => elem.opaque_regions(scale),
            WorkspaceRenderElement::Backdrop(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            WorkspaceRenderElement::Fullscreen(elem) => elem.alpha(),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.draw(frame, src, dst, damage),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.underlying_storage(renderer),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.draw(frame, src, dst, damage),
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
            WorkspaceRenderElement::Fullscreen(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Wayland(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Window(elem) => elem.underlying_storage(renderer),
            WorkspaceRenderElement::Backdrop(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
        }
    }
}

impl<R> From<RescaleRenderElement<WaylandSurfaceRenderElement<R>>> for WorkspaceRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: RescaleRenderElement<WaylandSurfaceRenderElement<R>>) -> Self {
        WorkspaceRenderElement::Fullscreen(elem)
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
