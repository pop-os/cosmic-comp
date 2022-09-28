use crate::state::State;
use id_tree::NodeId;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{element::AsRenderElements, ImportAll, Renderer},
    },
    desktop::{space::SpaceElement, Kind, PopupManager, Window, WindowSurfaceType},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget},
        Seat,
    },
    output::Output,
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel::State as XdgState,
        wayland_server::{backend::ObjectId, protocol::wl_surface::WlSurface},
    },
    render_elements, space_elements,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
    wayland::{
        compositor::{with_states, with_surface_tree_downward, TraversalAction},
        seat::WaylandFocus,
        shell::xdg::XdgToplevelSurfaceRoleAttributes,
    },
};
use std::{
    hash::Hash,
    sync::{Arc, Mutex},
};

pub mod stack;
pub use self::stack::CosmicStack;
pub mod window;
pub use self::window::CosmicWindow;

use super::focus::FocusDirection;

space_elements! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    CosmicMappedInternal;
    Window=CosmicWindow,
    Stack=CosmicStack,
}

#[derive(Debug, Clone)]
pub struct CosmicMapped {
    element: CosmicMappedInternal,

    // associated data

    //tiling
    pub(super) tiling_node_id: Arc<Mutex<Option<NodeId>>>,
    //floating
    pub(super) last_geometry: Arc<Mutex<Option<Rectangle<i32, Logical>>>>,
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
    pub fn windows(&self) -> impl Iterator<Item = (Window, Point<i32, Logical>)> + '_ {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => Box::new(stack.windows().map(|w| {
                (
                    w,
                    stack
                        .header
                        .lock()
                        .unwrap()
                        .as_ref()
                        .map(|header| Point::from((0, header.height() as i32)))
                        .unwrap_or(Point::from((0, 0))),
                )
            }))
                as Box<dyn Iterator<Item = (Window, Point<i32, Logical>)>>,
            CosmicMappedInternal::Window(window) => Box::new(std::iter::once((
                window.window.clone(),
                window
                    .header
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map(|header| Point::from((0, header.height() as i32)))
                    .unwrap_or(Point::from((0, 0))),
            ))),
            _ => Box::new(std::iter::empty()),
        }
    }

    pub fn active_window(&self) -> Window {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.active(),
            CosmicMappedInternal::Window(win) => win.window.clone(),
            _ => unreachable!(),
        }
    }

    pub fn focus_window(&self, window: &Window) {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack.set_active(window),
            _ => {}
        }
    }

    pub fn has_surface(&self, surface: &WlSurface, surface_type: WindowSurfaceType) -> bool {
        self.windows().any(|(w, _)| {
            let toplevel = w.toplevel().wl_surface();

            if surface_type.contains(WindowSurfaceType::TOPLEVEL) {
                if toplevel == surface {
                    return true;
                }
            }

            if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
                use std::sync::atomic::{AtomicBool, Ordering};

                let found = AtomicBool::new(false);
                with_surface_tree_downward(
                    toplevel,
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
                PopupManager::popups_for_surface(toplevel).any(|(p, _)| p.wl_surface() == surface)
            } else {
                false
            }
        })
    }

    pub fn handle_focus(&self, direction: FocusDirection) -> bool {
        if let CosmicMappedInternal::Stack(stack) = &self.element {
            //TODO: stack.handle_focus(direction)
            false
        } else {
            false
        }
    }

    pub fn set_tiled(&self, tiled: bool) {
        for toplevel in match &self.element {
            // we use the tiled state of stack windows anyway to get rid of decorations
            CosmicMappedInternal::Stack(s) => None,
            CosmicMappedInternal::Window(w) => Some(w.window.toplevel()),
            _ => unreachable!(),
        } {
            match toplevel {
                Kind::Xdg(xdg) => xdg.with_pending_state(|state| {
                    if tiled {
                        state.states.set(XdgState::TiledLeft);
                        state.states.set(XdgState::TiledRight);
                        state.states.set(XdgState::TiledTop);
                        state.states.set(XdgState::TiledBottom);
                    } else {
                        state.states.unset(XdgState::TiledLeft);
                        state.states.unset(XdgState::TiledRight);
                        state.states.unset(XdgState::TiledTop);
                        state.states.unset(XdgState::TiledBottom);
                    }
                }),
                // Kind::X11?
            };
        }
    }

    pub fn is_tiled(&self) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.window.clone(),
            _ => unreachable!(),
        };

        match window.toplevel() {
            Kind::Xdg(xdg) => xdg.current_state().states.contains(XdgState::TiledLeft),
            // Kind::X11?
        }
    }

    pub fn set_fullscreen(&self, fullscreen: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.windows()) as Box<dyn Iterator<Item = Window>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.window.clone())),
            _ => unreachable!(),
        } {
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.with_pending_state(|state| {
                    if fullscreen {
                        state.states.set(XdgState::Fullscreen);
                    } else {
                        state.states.unset(XdgState::Fullscreen);
                    }
                }),
                // Kind::X11?
            };
        }
    }

    pub fn is_fullscreen(&self) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.window.clone(),
            _ => unreachable!(),
        };

        match window.toplevel() {
            Kind::Xdg(xdg) => xdg.current_state().states.contains(XdgState::Fullscreen),
            // Kind::X11?
        }
    }

    pub fn set_maximized(&self, maximized: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.windows()) as Box<dyn Iterator<Item = Window>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.window.clone())),
            _ => unreachable!(),
        } {
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.with_pending_state(|state| {
                    if maximized {
                        state.states.set(XdgState::Maximized);
                    } else {
                        state.states.unset(XdgState::Maximized);
                    }
                }),
                // Kind::X11?
            };
        }
    }

    pub fn is_maximized(&self) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.window.clone(),
            _ => unreachable!(),
        };

        match window.toplevel() {
            Kind::Xdg(xdg) => xdg.current_state().states.contains(XdgState::Maximized),
            // Kind::X11?
        }
    }

    pub fn set_activated(&self, activated: bool) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.windows()) as Box<dyn Iterator<Item = Window>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.window.clone())),
            _ => unreachable!(),
        } {
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.with_pending_state(|state| {
                    if activated {
                        state.states.set(XdgState::Activated);
                    } else {
                        state.states.unset(XdgState::Activated);
                    }
                }),
                // Kind::X11?
            };
        }
    }

    pub fn is_activated(&self) -> bool {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.window.clone(),
            _ => unreachable!(),
        };

        match window.toplevel() {
            Kind::Xdg(xdg) => xdg.current_state().states.contains(XdgState::Activated),
            // Kind::X11?
        }
    }

    pub fn set_size(&self, size: Size<i32, Logical>) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => s.set_size(size),
            CosmicMappedInternal::Window(w) => w.set_size(size),
            _ => {}
        }
    }

    pub fn min_size(&self) -> Size<i32, Logical> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => stack
                .windows()
                .fold(None, |min_size, window| {
                    let win_min_size = with_states(window.toplevel().wl_surface(), |states| {
                        let attrs = states
                            .data_map
                            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        attrs.min_size
                    });
                    match (min_size, win_min_size) {
                        (None, x) => Some(x),
                        (Some(min1), min2) => Some((min1.w.max(min2.w), min1.h.max(min2.h)).into()),
                    }
                })
                .expect("Empty stack?"),
            CosmicMappedInternal::Window(window) => {
                with_states(window.window.toplevel().wl_surface(), |states| {
                    let attrs = states
                        .data_map
                        .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attrs.min_size
                })
            }
            _ => unreachable!(),
        }
    }

    pub fn max_size(&self) -> Size<i32, Logical> {
        match &self.element {
            CosmicMappedInternal::Stack(stack) => {
                let theoretical_max = stack.windows().fold(None, |max_size, window| {
                    let win_max_size = with_states(window.toplevel().wl_surface(), |states| {
                        let attrs = states
                            .data_map
                            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                            .unwrap()
                            .lock()
                            .unwrap();
                        attrs.max_size
                    });
                    match (max_size, win_max_size) {
                        (None, x) => Some(x),
                        (Some(max1), max2) => Some(
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
                    (None, _) => (0, 0).into(),
                    (Some(max), min) => (max.w.max(min.w), max.h.max(min.h)).into(),
                }
            }
            CosmicMappedInternal::Window(window) => {
                with_states(window.window.toplevel().wl_surface(), |states| {
                    let attrs = states
                        .data_map
                        .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap();
                    attrs.max_size
                })
            }
            _ => unreachable!(),
        }
    }

    pub fn configure(&self) {
        for window in match &self.element {
            CosmicMappedInternal::Stack(s) => {
                Box::new(s.windows()) as Box<dyn Iterator<Item = Window>>
            }
            CosmicMappedInternal::Window(w) => Box::new(std::iter::once(w.window.clone())),
            _ => unreachable!(),
        } {
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.send_configure(),
                // Kind::X11?
            };
        }
    }

    pub fn send_close(&self) {
        let window = match &self.element {
            CosmicMappedInternal::Stack(s) => s.active(),
            CosmicMappedInternal::Window(w) => w.window.clone(),
            _ => unreachable!(),
        };

        match window.toplevel() {
            Kind::Xdg(xdg) => xdg.send_close(),
            // Kind::X11?
        };
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
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::enter(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::enter(w, seat, data, event),
            _ => {}
        }
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        match &self.element {
            CosmicMappedInternal::Stack(s) => PointerTarget::motion(s, seat, data, event),
            CosmicMappedInternal::Window(w) => PointerTarget::motion(w, seat, data, event),
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
            CosmicMappedInternal::Window(w) => w.window.wl_surface().clone(),
            CosmicMappedInternal::Stack(s) => s.active().wl_surface().clone(),
            _ => None,
        }
    }

    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        match &self.element {
            CosmicMappedInternal::Window(w) => w.window.same_client_as(object_id),
            CosmicMappedInternal::Stack(s) => s.windows().any(|w| w.same_client_as(object_id)),
            _ => false,
        }
    }
}

impl From<CosmicWindow> for CosmicMapped {
    fn from(w: CosmicWindow) -> Self {
        CosmicMapped {
            element: CosmicMappedInternal::Window(w),
            tiling_node_id: Arc::new(Mutex::new(None)),
            last_geometry: Arc::new(Mutex::new(None)),
        }
    }
}

impl From<CosmicStack> for CosmicMapped {
    fn from(s: CosmicStack) -> Self {
        CosmicMapped {
            element: CosmicMappedInternal::Stack(s),
            tiling_node_id: Arc::new(Mutex::new(None)),
            last_geometry: Arc::new(Mutex::new(None)),
        }
    }
}

render_elements! {
    pub CosmicMappedRenderElement<R> where R: ImportAll;
    Stack=self::stack::CosmicStackRenderElement<R>,
    Window=self::window::CosmicWindowRenderElement<R>,
}

impl<R> AsRenderElements<R> for CosmicMapped
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = CosmicMappedRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        match &self.element {
            CosmicMappedInternal::Stack(s) => AsRenderElements::<R>::render_elements::<
                CosmicMappedRenderElement<R>,
            >(s, location, scale),
            CosmicMappedInternal::Window(w) => AsRenderElements::<R>::render_elements::<
                CosmicMappedRenderElement<R>,
            >(w, location, scale),
            _ => Vec::new(),
        }
        .into_iter()
        .map(C::from)
        .collect()
    }
}
