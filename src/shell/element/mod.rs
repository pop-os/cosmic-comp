use crate::{
    backend::render::{
        element::{AsGlowFrame, AsGlowRenderer},
        GlMultiError, GlMultiFrame, GlMultiRenderer,
    },
    state::State,
    utils::prelude::SeatExt,
};
use calloop::LoopHandle;
use id_tree::NodeId;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            element::{
                utils::{CropRenderElement, RelocateRenderElement, RescaleRenderElement},
                AsRenderElements, Element, RenderElement, UnderlyingStorage,
            },
            gles::element::PixelShaderElement,
            glow::GlowRenderer,
            ImportAll, ImportMem, Renderer,
        },
    },
    desktop::{space::SpaceElement, PopupManager, WindowSurfaceType},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget, RelativeMotionEvent},
        Seat,
    },
    output::Output,
    reexports::wayland_server::{backend::ObjectId, protocol::wl_surface::WlSurface},
    space_elements,
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size,
    },
    wayland::{
        compositor::{with_surface_tree_downward, TraversalAction},
        seat::WaylandFocus,
    },
    xwayland::{xwm::X11Relatable, X11Surface},
};

use std::{
    collections::HashMap,
    fmt,
    hash::Hash,
    sync::{Arc, Mutex},
};

pub mod surface;
use self::stack::MoveResult;
pub use self::surface::CosmicSurface;
pub mod stack;
pub use self::stack::CosmicStack;
pub mod window;
pub use self::window::CosmicWindow;

#[cfg(feature = "debug")]
use egui::plot::{Corner, Legend, Plot, PlotPoints, Polygon};
#[cfg(feature = "debug")]
use smithay::backend::renderer::{element::texture::TextureRenderElement, gles::GlesTexture};
#[cfg(feature = "debug")]
use tracing::debug;

use super::{
    focus::FocusDirection,
    layout::{floating::ResizeState, tiling::Direction},
};

space_elements! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    CosmicMappedInternal;
    Window=CosmicWindow,
    Stack=CosmicStack,
}

#[derive(Clone)]
pub struct CosmicMapped {
    element: CosmicMappedInternal,

    // associated data
    last_cursor_position: Arc<Mutex<HashMap<usize, Point<f64, Logical>>>>,

    //tiling
    pub(super) tiling_node_id: Arc<Mutex<Option<NodeId>>>,
    //floating
    pub(super) last_geometry: Arc<Mutex<Option<Rectangle<i32, Logical>>>>,
    pub(super) resize_state: Arc<Mutex<Option<ResizeState>>>,

    #[cfg(feature = "debug")]
    debug: Arc<Mutex<Option<smithay_egui::EguiState>>>,
}

impl fmt::Debug for CosmicMapped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicMapped")
            .field("element", &self.element)
            .field("last_cursor_position", &self.last_cursor_position)
            .field("tiling_node_id", &self.tiling_node_id)
            .field("resize_state", &self.resize_state)
            .finish()
    }
}

impl PartialEq for CosmicMapped {
    fn eq(&self, other: &Self) -> bool {
        self.element == other.element
    }
}

impl Eq for CosmicMapped {}

impl Hash for CosmicMapped {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.element.hash(state)
    }
}

impl CosmicMapped {
    pub fn windows(&self) -> impl Iterator<Item = (CosmicSurface, Point<i32, Logical>)> + '_ {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => {
                Box::new(stack.surfaces().map(|w| (w, stack.offset())))
                    as Box<dyn Iterator<Item = (CosmicSurface, Point<i32, Logical>)>>
            }
            CosmicMappedInternal::Window(window) => {
                Box::new(std::iter::once((window.surface(), window.offset())))
            }
            _ => Box::new(std::iter::empty()),
        }
    }

    pub fn active_window(&self) -> CosmicSurface {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.active(),
            CosmicMappedInternal::Window(win) => win.surface(),
            _ => unreachable!(),
        }
    }

    pub fn has_active_window(&self, window: &CosmicSurface) -> bool {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.has_active(window),
            CosmicMappedInternal::Window(win) => win.contains_surface(window),
            _ => unreachable!(),
        }
    }

    pub fn active_window_offset(&self) -> Point<i32, Logical> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.offset(),
            CosmicMappedInternal::Window(window) => window.offset(),
            _ => unreachable!(),
        }
    }

    pub fn active_window_geometry(&self) -> Rectangle<i32, Logical> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => {
                let win = stack.active();
                let location = stack.offset();
                let mut size = win.geometry().size;
                size -= location.to_size();
                Rectangle::from_loc_and_size(location, size)
            }
            CosmicMappedInternal::Window(win) => {
                let location = win.offset();
                let mut size = win.geometry().size;
                size -= location.to_size();
                Rectangle::from_loc_and_size(location, size)
            }
            _ => unreachable!(),
        }
    }

    pub fn cursor_position(&self, seat: &Seat<State>) -> Option<Point<f64, Logical>> {
        self.last_cursor_position
            .lock()
            .unwrap()
            .get(&seat.id())
            .cloned()
    }

    pub fn set_active(&self, window: &CosmicSurface) {
        if let CosmicMappedInternal::Stack(stack) = &self.element {
            stack.set_active(window);
        }
    }

    pub fn focus_window(&self, window: &CosmicSurface) {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.set_active(window),
            _ => {}
        }
    }

    pub fn has_surface(&self, surface: &WlSurface, surface_type: WindowSurfaceType) -> bool {
        self.windows().any(|(w, _)| {
            let Some(toplevel ) = w.wl_surface() else { return false };

            if surface_type.contains(WindowSurfaceType::TOPLEVEL) {
                if toplevel == *surface {
                    return true;
                }
            }

            if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
                use std::sync::atomic::{AtomicBool, Ordering};

                let found = AtomicBool::new(false);
                with_surface_tree_downward(
                    &toplevel,
                    surface,
                    |_, _, search| TraversalAction::DoChildren(search),
                    |s, _, search| {
                        found.fetch_or(s == *search, Ordering::SeqCst);
                    },
                    |_, _, _| !found.load(Ordering::SeqCst),
                );
                if found.load(Ordering::SeqCst) {
                    return true;
                }
            }

            if surface_type.contains(WindowSurfaceType::POPUP) {
                PopupManager::popups_for_surface(&toplevel).any(|(p, _)| p.wl_surface() == surface)
            } else {
                false
            }
        })
    }

    pub fn handle_move(&self, direction: Direction) -> MoveResult {
        if let CosmicMappedInternal::Stack(stack) = &self.element {
            stack.handle_move(direction)
        } else {
            MoveResult::Default
        }
    }

    pub fn handle_focus(&self, direction: FocusDirection) -> bool {
        if let CosmicMappedInternal::Stack(stack) = &self.element {
            stack.handle_focus(direction)
        } else {
            false
        }
    }

    pub fn set_resizing(&self, resizing: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.surfaces()) as Box<dyn Iterator<Item = CosmicSurface>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.surface())),
            _ => unreachable!(),
        } {
            window.set_resizing(resizing);
        }
    }

    pub fn is_resizing(&self, pending: bool) -> Option<bool> {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.is_resizing(pending)
    }

    pub fn set_tiled(&self, tiled: bool) {
        if let Some(window) = match &self.element {
            // we use the tiled state of stack windows anyway to get rid of decorations
            CosmicMappedInternal::Stack(_) => None,
            CosmicMappedInternal::Window(w) => Some(w.surface()),
            _ => unreachable!(),
        } {
            window.set_tiled(tiled)
        }
    }

    pub fn is_tiled(&self, pending: bool) -> Option<bool> {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.is_tiled(pending)
    }

    pub fn set_fullscreen(&self, fullscreen: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.surfaces()) as Box<dyn Iterator<Item = CosmicSurface>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.surface())),
            _ => unreachable!(),
        } {
            window.set_fullscreen(fullscreen);
        }
    }

    pub fn is_fullscreen(&self, pending: bool) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.is_fullscreen(pending)
    }

    pub fn set_maximized(&self, maximized: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.surfaces()) as Box<dyn Iterator<Item = CosmicSurface>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.surface())),
            _ => unreachable!(),
        } {
            window.set_maximized(maximized)
        }
    }

    pub fn is_maximized(&self, pending: bool) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.is_maximized(pending)
    }

    pub fn set_activated(&self, activated: bool) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => s.set_activate(activated),
            CosmicMappedInternal::Window(w) => w.set_activate(activated),
            _ => unreachable!(),
        }
    }

    pub fn is_activated(&self, pending: bool) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.is_activated(pending)
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Logical>) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => s.set_geometry(geo),
            CosmicMappedInternal::Window(w) => w.set_geometry(geo),
            _ => {}
        }
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => {
                stack.surfaces().fold(None, |min_size, window| {
                    let win_min_size = window.min_size();
                    match (min_size, win_min_size) {
                        (None, None) => None,
                        (None, x) | (x, None) => x,
                        (Some(min1), Some(min2)) => {
                            Some((min1.w.max(min2.w), min1.h.max(min2.h)).into())
                        }
                    }
                })
            }
            CosmicMappedInternal::Window(window) => window.surface().min_size(),
            _ => unreachable!(),
        }
    }

    pub fn max_size(&self) -> Option<Size<i32, Logical>> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => {
                let theoretical_max = stack.surfaces().fold(None, |max_size, window| {
                    let win_max_size = window.max_size();
                    match (max_size, win_max_size) {
                        (None, None) => None,
                        (None, x) | (x, None) => x,
                        (Some(max1), Some(max2)) => Some(
                            (
                                if max1.w == 0 {
                                    max2.w
                                } else if max2.w == 0 {
                                    max1.w
                                } else {
                                    max1.w.min(max2.w)
                                },
                                if max1.h == 0 {
                                    max2.h
                                } else if max2.h == 0 {
                                    max1.h
                                } else {
                                    max1.h.min(max2.h)
                                },
                            )
                                .into(),
                        ),
                    }
                });
                // The problem is, with accumulated sizes, the minimum size could be larger than our maximum...
                let min_size = self.min_size();
                match (theoretical_max, min_size) {
                    (None, _) => None,
                    (Some(max), None) => Some(max),
                    (Some(max), Some(min)) => Some((max.w.max(min.w), max.h.max(min.h)).into()),
                }
            }
            CosmicMappedInternal::Window(window) => window.surface().max_size(),
            _ => unreachable!(),
        }
    }

    pub fn set_bounds(&self, size: impl Into<Option<Size<i32, Logical>>>) {
        let size = size.into();
        for (surface, _) in self.windows() {
            surface.set_bounds(size.clone())
        }
    }

    pub fn configure(&self) -> Option<Serial> {
        match &self.element {
            CosmicMappedInternal::Stack(s) => {
                let active = s.active();
                for surface in s.surfaces().filter(|s| s != &active) {
                    surface.send_configure();
                }
                active.send_configure()
            }
            CosmicMappedInternal::Window(w) => w.surface().send_configure(),
            _ => unreachable!(),
        }
    }

    pub fn send_close(&self) {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.surface(),
            _ => unreachable!(),
        };

        window.close();
    }

    pub fn is_window(&self) -> bool {
        match &self.element {
            CosmicMappedInternal::Window(_) => true,
            _ => false,
        }
    }

    pub fn is_stack(&self) -> bool {
        match &self.element {
            CosmicMappedInternal::Stack(_) => true,
            _ => false,
        }
    }

    pub fn stack_ref(&self) -> Option<&CosmicStack> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => Some(stack),
            _ => None,
        }
    }

    pub fn stack_ref_mut(&mut self) -> Option<&mut CosmicStack> {
        match &mut self.element {
            CosmicMappedInternal::Stack(stack) => Some(stack),
            _ => None,
        }
    }

    pub fn convert_to_stack<'a>(
        &mut self,
        outputs: impl Iterator<Item = (&'a Output, Rectangle<i32, Logical>)>,
    ) {
        match &self.element {
            CosmicMappedInternal::Window(window) => {
                let surface = window.surface();
                let activated = surface.is_activated(true);
                let handle = window.loop_handle();

                let stack = CosmicStack::new(std::iter::once(surface), handle);
                if let Some(geo) = self.last_geometry.lock().unwrap().clone() {
                    stack.set_geometry(geo);
                }
                for (output, overlap) in outputs {
                    stack.output_enter(output, overlap);
                }
                stack.set_activate(activated);
                stack.active().send_configure();
                stack.refresh();

                self.element = CosmicMappedInternal::Stack(stack);
            }
            _ => {}
        }
    }

    pub fn convert_to_surface<'a>(
        &mut self,
        surface: CosmicSurface,
        outputs: impl Iterator<Item = (&'a Output, Rectangle<i32, Logical>)>,
    ) {
        let handle = self.loop_handle();
        surface.try_force_undecorated(false);
        surface.set_tiled(false);
        let window = CosmicWindow::new(surface, handle);

        if let Some(geo) = self.last_geometry.lock().unwrap().clone() {
            window.set_geometry(geo);
        }
        for (output, overlap) in outputs {
            window.output_enter(output, overlap);
        }
        window.set_activate(self.is_activated(true));
        window.surface().send_configure();
        window.refresh();

        self.element = CosmicMappedInternal::Window(window);
    }

    pub(super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::Data> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.loop_handle(),
            CosmicMappedInternal::Window(window) => window.loop_handle(),
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "debug")]
    pub fn set_debug(&self, flag: bool) {
        let mut debug = self.debug.lock().unwrap();
        if flag {
            *debug = Some(smithay_egui::EguiState::new(Rectangle::from_loc_and_size(
                (10, 10),
                (100, 100),
            )));
        } else {
            debug.take();
        }
    }
}

impl IsAlive for CosmicMapped {
    fn alive(&self) -> bool {
        self.element.alive()
    }
}

impl SpaceElement for CosmicMapped {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        SpaceElement::bbox(&self.element)
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        SpaceElement::is_in_input_region(&self.element, point)
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.element, activated)
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.element, output, overlap)
    }
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.element, output)
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        SpaceElement::geometry(&self.element)
    }
    fn z_index(&self) -> u8 {
        SpaceElement::z_index(&self.element)
    }
    fn refresh(&self) {
        SpaceElement::refresh(&self.element)
    }
}

impl X11Relatable for CosmicMapped {
    fn is_window(&self, window: &X11Surface) -> bool {
        match self.active_window() {
            CosmicSurface::X11(surface) => &surface == window,
            _ => false,
        }
    }
}

impl KeyboardTarget<State> for CosmicMapped {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => KeyboardTarget::enter(s, seat, data, keys, serial),
            CosmicMappedInternal::Window(w) => KeyboardTarget::enter(w, seat, data, keys, serial),
            _ => {}
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => KeyboardTarget::leave(s, seat, data, serial),
            CosmicMappedInternal::Window(w) => KeyboardTarget::leave(w, seat, data, serial),
            _ => {}
        }
    }
    fn key(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        time: u32,
    ) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => {
                KeyboardTarget::key(s, seat, data, key, state, serial, time)
            }
            CosmicMappedInternal::Window(w) => {
                KeyboardTarget::key(w, seat, data, key, state, serial, time)
            }
            _ => {}
        }
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => {
                KeyboardTarget::modifiers(s, seat, data, modifiers, serial)
            }
            CosmicMappedInternal::Window(w) => {
                KeyboardTarget::modifiers(w, seat, data, modifiers, serial)
            }
            _ => {}
        }
    }
}

impl PointerTarget<State> for CosmicMapped {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        self.last_cursor_position
            .lock()
            .unwrap()
            .insert(seat.id(), event.location);
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::enter(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::enter(w, seat, data, event),
            _ => {}
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        self.last_cursor_position
            .lock()
            .unwrap()
            .insert(seat.id(), event.location);
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::motion(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::motion(w, seat, data, event),
            _ => {}
        }
    }
    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::relative_motion(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::relative_motion(w, seat, data, event),
            _ => {}
        }
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::button(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::button(w, seat, data, event),
            _ => {}
        }
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::axis(s, seat, data, frame),
            CosmicMappedInternal::Window(w) => PointerTarget::axis(w, seat, data, frame),
            _ => {}
        }
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        self.last_cursor_position.lock().unwrap().remove(&seat.id());
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::leave(s, seat, data, serial, time),
            CosmicMappedInternal::Window(w) => PointerTarget::leave(w, seat, data, serial, time),
            _ => {}
        }
    }
}

impl WaylandFocus for CosmicMapped {
    fn wl_surface(&self) -> Option<WlSurface> {
        match &self.element {
            CosmicMappedInternal::Window(w) => w.surface().wl_surface().clone(),
            CosmicMappedInternal::Stack(s) => s.active().wl_surface().clone(),
            _ => None,
        }
    }

    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match &self.element {
            CosmicMappedInternal::Window(w) => w.surface().same_client_as(object_id),
            CosmicMappedInternal::Stack(s) => s.surfaces().any(|w| w.same_client_as(object_id)),
            _ => false,
        }
    }
}

impl From<CosmicWindow> for CosmicMapped {
    fn from(w: CosmicWindow) -> Self {
        CosmicMapped {
            element: CosmicMappedInternal::Window(w),
            last_cursor_position: Arc::new(Mutex::new(HashMap::new())),
            tiling_node_id: Arc::new(Mutex::new(None)),
            last_geometry: Arc::new(Mutex::new(None)),
            resize_state: Arc::new(Mutex::new(None)),
            #[cfg(feature = "debug")]
            debug: Arc::new(Mutex::new(None)),
        }
    }
}

impl From<CosmicStack> for CosmicMapped {
    fn from(s: CosmicStack) -> Self {
        CosmicMapped {
            element: CosmicMappedInternal::Stack(s),
            last_cursor_position: Arc::new(Mutex::new(HashMap::new())),
            tiling_node_id: Arc::new(Mutex::new(None)),
            last_geometry: Arc::new(Mutex::new(None)),
            resize_state: Arc::new(Mutex::new(None)),
            #[cfg(feature = "debug")]
            debug: Arc::new(Mutex::new(None)),
        }
    }
}

pub enum CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    Stack(self::stack::CosmicStackRenderElement<R>),
    Window(self::window::CosmicWindowRenderElement<R>),
    TiledStack(
        RelocateRenderElement<
            RescaleRenderElement<CropRenderElement<self::stack::CosmicStackRenderElement<R>>>,
        >,
    ),
    TiledWindow(
        RelocateRenderElement<
            RescaleRenderElement<CropRenderElement<self::window::CosmicWindowRenderElement<R>>>,
        >,
    ),
    Indicator(PixelShaderElement),
    #[cfg(feature = "debug")]
    Egui(TextureRenderElement<GlesTexture>),
}

impl<R> Element for CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> &smithay::backend::renderer::element::Id {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.id(),
            CosmicMappedRenderElement::Window(elem) => elem.id(),
            CosmicMappedRenderElement::TiledStack(elem) => elem.id(),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.id(),
            CosmicMappedRenderElement::Indicator(elem) => elem.id(),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.id(),
        }
    }

    fn current_commit(&self) -> smithay::backend::renderer::utils::CommitCounter {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.current_commit(),
            CosmicMappedRenderElement::Window(elem) => elem.current_commit(),
            CosmicMappedRenderElement::TiledStack(elem) => elem.current_commit(),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.current_commit(),
            CosmicMappedRenderElement::Indicator(elem) => elem.current_commit(),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.src(),
            CosmicMappedRenderElement::Window(elem) => elem.src(),
            CosmicMappedRenderElement::TiledStack(elem) => elem.src(),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.src(),
            CosmicMappedRenderElement::Indicator(elem) => elem.src(),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.geometry(scale),
            CosmicMappedRenderElement::Window(elem) => elem.geometry(scale),
            CosmicMappedRenderElement::TiledStack(elem) => elem.geometry(scale),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.geometry(scale),
            CosmicMappedRenderElement::Indicator(elem) => elem.geometry(scale),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.location(scale),
            CosmicMappedRenderElement::Window(elem) => elem.location(scale),
            CosmicMappedRenderElement::TiledStack(elem) => elem.location(scale),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.location(scale),
            CosmicMappedRenderElement::Indicator(elem) => elem.location(scale),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.location(scale),
        }
    }

    fn transform(&self) -> smithay::utils::Transform {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.transform(),
            CosmicMappedRenderElement::Window(elem) => elem.transform(),
            CosmicMappedRenderElement::TiledStack(elem) => elem.transform(),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.transform(),
            CosmicMappedRenderElement::Indicator(elem) => elem.transform(),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<smithay::backend::renderer::utils::CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.damage_since(scale, commit),
            CosmicMappedRenderElement::Window(elem) => elem.damage_since(scale, commit),
            CosmicMappedRenderElement::TiledStack(elem) => elem.damage_since(scale, commit),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.damage_since(scale, commit),
            CosmicMappedRenderElement::Indicator(elem) => elem.damage_since(scale, commit),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.opaque_regions(scale),
            CosmicMappedRenderElement::Window(elem) => elem.opaque_regions(scale),
            CosmicMappedRenderElement::TiledStack(elem) => elem.opaque_regions(scale),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.opaque_regions(scale),
            CosmicMappedRenderElement::Indicator(elem) => elem.opaque_regions(scale),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.opaque_regions(scale),
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.alpha(),
            CosmicMappedRenderElement::Window(elem) => elem.alpha(),
            CosmicMappedRenderElement::TiledStack(elem) => elem.alpha(),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.alpha(),
            CosmicMappedRenderElement::Indicator(elem) => elem.alpha(),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.alpha(),
        }
    }
}

impl RenderElement<GlowRenderer> for CosmicMappedRenderElement<GlowRenderer> {
    fn draw<'frame>(
        &self,
        frame: &mut <GlowRenderer as Renderer>::Frame<'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlowRenderer as Renderer>::Error> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::Window(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::TiledStack(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::Indicator(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame, src, dst, damage)
            }
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame, src, dst, damage)
            }
        }
    }

    fn underlying_storage(&self, renderer: &mut GlowRenderer) -> Option<UnderlyingStorage> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::Window(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::TiledStack(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::Indicator(elem) => elem.underlying_storage(renderer),
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => elem.underlying_storage(renderer),
        }
    }
}

impl<'a, 'b> RenderElement<GlMultiRenderer<'a, 'b>>
    for CosmicMappedRenderElement<GlMultiRenderer<'a, 'b>>
{
    fn draw<'frame>(
        &self,
        frame: &mut GlMultiFrame<'a, 'b, 'frame>,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), GlMultiError> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::Window(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::TiledStack(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.draw(frame, src, dst, damage),
            CosmicMappedRenderElement::Indicator(elem) => {
                RenderElement::<GlowRenderer>::draw(elem, frame.glow_frame_mut(), src, dst, damage)
                    .map_err(|err| GlMultiError::Render(err))
            }
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => {
                let glow_frame = frame.glow_frame_mut();
                RenderElement::<GlowRenderer>::draw(elem, glow_frame, src, dst, damage)
                    .map_err(|err| GlMultiError::Render(err))
            }
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut GlMultiRenderer<'a, 'b>,
    ) -> Option<UnderlyingStorage> {
        match self {
            CosmicMappedRenderElement::Stack(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::Window(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::TiledStack(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::TiledWindow(elem) => elem.underlying_storage(renderer),
            CosmicMappedRenderElement::Indicator(elem) => {
                elem.underlying_storage(renderer.glow_renderer_mut())
            }
            #[cfg(feature = "debug")]
            CosmicMappedRenderElement::Egui(elem) => {
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

impl<R> From<stack::CosmicStackRenderElement<R>> for CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: stack::CosmicStackRenderElement<R>) -> Self {
        CosmicMappedRenderElement::Stack(elem)
    }
}
impl<R> From<window::CosmicWindowRenderElement<R>> for CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: window::CosmicWindowRenderElement<R>) -> Self {
        CosmicMappedRenderElement::Window(elem)
    }
}

impl<R> From<PixelShaderElement> for CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: PixelShaderElement) -> Self {
        CosmicMappedRenderElement::Indicator(elem)
    }
}

#[cfg(feature = "debug")]
impl<R> From<TextureRenderElement<GlesTexture>> for CosmicMappedRenderElement<R>
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    fn from(elem: TextureRenderElement<GlesTexture>) -> Self {
        CosmicMappedRenderElement::Egui(elem)
    }
}

impl<R> AsRenderElements<R> for CosmicMapped
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicMappedRenderElement<R>: RenderElement<R>,
{
    type RenderElement = CosmicMappedRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        #[cfg(feature = "debug")]
        let mut elements = if let Some(debug) = self.debug.lock().unwrap().as_mut() {
            let window = self.active_window();
            let window_geo = window.geometry();
            let (min_size, max_size, size) =
                (window.min_size(), window.max_size(), window.geometry().size);

            let area = Rectangle::<i32, Logical>::from_loc_and_size(
                location.to_f64().to_logical(scale).to_i32_round(),
                self.bbox().size,
            );

            let glow_renderer = renderer.glow_renderer_mut();
            match debug.render(
                |ctx| {
                    egui::Area::new("window")
                        .anchor(
                            egui::Align2::RIGHT_TOP,
                            [
                                -window_geo.loc.x as f32 - 10.0,
                                window_geo.loc.y as f32 - 10.0,
                            ],
                        )
                        .show(ctx, |ui| {
                            egui::Frame::none()
                                .fill(egui::Color32::BLACK)
                                .rounding(5.0)
                                .inner_margin(10.0)
                                .show(ui, |ui| {
                                    ui.heading(window.title());
                                    ui.horizontal(|ui| {
                                        ui.label("App ID: ");
                                        ui.label(window.app_id());
                                    });
                                    ui.label(match window {
                                        CosmicSurface::Wayland(_) => "Protocol: Wayland",
                                        CosmicSurface::X11(_) => "Protocol: X11",
                                        _ => unreachable!(),
                                    });
                                    ui.horizontal(|ui| {
                                        ui.label("States: ");
                                        if window.is_maximized() {
                                            ui.label("🗖");
                                        }
                                        if window.is_fullscreen() {
                                            ui.label("⬜");
                                        }
                                        if window.is_activated() {
                                            ui.label("🖱");
                                        }
                                        if window.is_resizing().is_some() {
                                            ui.label("↔");
                                        }
                                    });

                                    let plot = Plot::new("Sizes")
                                        .legend(Legend::default().position(Corner::RightBottom))
                                        .data_aspect(1.0)
                                        .view_aspect(1.0)
                                        .show_x(false)
                                        .show_y(false)
                                        .width(200.0)
                                        .height(200.0);
                                    plot.show(ui, |plot_ui| {
                                        let center = if let Some(max_size) = max_size {
                                            ((max_size.w + 20) / 2, (max_size.h + 20) / 2)
                                        } else {
                                            (100, 100)
                                        };

                                        if let Some(max_size) = max_size {
                                            let max_size_rect =
                                                Polygon::new(PlotPoints::new(vec![
                                                    [10.0, 10.0],
                                                    [max_size.w as f64 + 10.0, 10.0],
                                                    [
                                                        max_size.w as f64 + 10.0,
                                                        max_size.h as f64 + 10.0,
                                                    ],
                                                    [10.0, max_size.h as f64 + 10.0],
                                                    [10.0, 10.0],
                                                ]));
                                            plot_ui.polygon(
                                                max_size_rect
                                                    .name(format!("{}x{}", max_size.w, max_size.h)),
                                            );
                                        }

                                        let size_rect = Polygon::new(PlotPoints::new(vec![
                                            [
                                                (center.0 - size.w / 2) as f64,
                                                (center.1 - size.h / 2) as f64,
                                            ],
                                            [
                                                (center.0 + size.w / 2) as f64,
                                                (center.1 - size.h / 2) as f64,
                                            ],
                                            [
                                                (center.0 + size.w / 2) as f64,
                                                (center.1 + size.h / 2) as f64,
                                            ],
                                            [
                                                (center.0 - size.w / 2) as f64,
                                                (center.1 + size.h / 2) as f64,
                                            ],
                                            [
                                                (center.0 - size.w / 2) as f64,
                                                (center.1 - size.h / 2) as f64,
                                            ],
                                        ]));
                                        plot_ui.polygon(
                                            size_rect.name(format!("{}x{}", size.w, size.h)),
                                        );

                                        if let Some(min_size) = min_size {
                                            let min_size_rect =
                                                Polygon::new(PlotPoints::new(vec![
                                                    [
                                                        (center.0 - min_size.w / 2) as f64,
                                                        (center.1 - min_size.h / 2) as f64,
                                                    ],
                                                    [
                                                        (center.0 + min_size.w / 2) as f64,
                                                        (center.1 - min_size.h / 2) as f64,
                                                    ],
                                                    [
                                                        (center.0 + min_size.w / 2) as f64,
                                                        (center.1 + min_size.h / 2) as f64,
                                                    ],
                                                    [
                                                        (center.0 - min_size.w / 2) as f64,
                                                        (center.1 + min_size.h / 2) as f64,
                                                    ],
                                                    [
                                                        (center.0 - min_size.w / 2) as f64,
                                                        (center.1 - min_size.h / 2) as f64,
                                                    ],
                                                ]));
                                            plot_ui.polygon(
                                                min_size_rect
                                                    .name(format!("{}x{}", min_size.w, min_size.h)),
                                            );
                                        }
                                    })
                                })
                        });
                },
                glow_renderer,
                area,
                scale.x,
                0.8,
            ) {
                Ok(element) => vec![element.into()],
                Err(err) => {
                    debug!(?err, "Error rendering debug overlay.");
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };
        #[cfg(not(feature = "debug"))]
        let mut elements = Vec::new();

        #[cfg_attr(not(feature = "debug"), allow(unused_mut))]
        match &self.element {
            CosmicMappedInternal::Stack(s) => {
                elements.extend(AsRenderElements::<R>::render_elements::<
                    CosmicMappedRenderElement<R>,
                >(s, renderer, location, scale, alpha))
            }
            CosmicMappedInternal::Window(w) => {
                elements.extend(AsRenderElements::<R>::render_elements::<
                    CosmicMappedRenderElement<R>,
                >(w, renderer, location, scale, alpha))
            }
            _ => {}
        };

        elements.into_iter().map(C::from).collect()
    }
}
