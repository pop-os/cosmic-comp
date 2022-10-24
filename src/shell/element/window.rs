use crate::state::State;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            element::{surface::WaylandSurfaceRenderElement, AsRenderElements},
            ImportAll, Renderer,
        },
    },
    desktop::{space::SpaceElement, Kind, Window},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget},
        Seat,
    },
    output::Output,
    reexports::wayland_protocols::xdg::decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode,
    render_elements,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
    wayland::shell::xdg::ToplevelSurface,
};
use std::{
    hash::Hash,
    sync::{Arc, Mutex},
};

#[derive(Debug, Clone)]
pub struct CosmicWindow {
    pub(super) window: Window,
    pub(super) header: Arc<Mutex<Option<HeaderBar>>>,
}

impl PartialEq<Window> for CosmicWindow {
    fn eq(&self, other: &Window) -> bool {
        &self.window == other
    }
}

impl PartialEq<CosmicWindow> for Window {
    fn eq(&self, other: &CosmicWindow) -> bool {
        self == &other.window
    }
}

impl PartialEq for CosmicWindow {
    fn eq(&self, other: &Self) -> bool {
        self.window == other.window
    }
}

impl Eq for CosmicWindow {}

impl Hash for CosmicWindow {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.window.hash(state)
    }
}

impl CosmicWindow {
    pub fn set_size(&self, size: Size<i32, Logical>) {
        let surface_size = (
            size.w,
            size.h
                - self
                    .header
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map(|h| h.height())
                    .unwrap_or(0),
        )
            .into();
        match self.window.toplevel() {
            Kind::Xdg(xdg) => xdg.with_pending_state(|state| state.size = Some(surface_size)),
        };
    }
}

impl From<Window> for CosmicWindow {
    fn from(window: Window) -> Self {
        let is_ssd = matches!(
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.current_state().decoration_mode,
            },
            Some(DecorationMode::ServerSide)
        );
        CosmicWindow {
            window,
            header: Arc::new(Mutex::new(is_ssd.then_some(HeaderBar::default()))),
        }
    }
}

impl From<ToplevelSurface> for CosmicWindow {
    fn from(surf: ToplevelSurface) -> Self {
        let is_ssd = matches!(
            surf.current_state().decoration_mode,
            Some(DecorationMode::ServerSide)
        );
        CosmicWindow {
            window: Window::new(Kind::Xdg(surf)),
            header: Arc::new(Mutex::new(is_ssd.then_some(HeaderBar::default()))),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub(super) struct HeaderBar {
    pointer_loc: Option<Point<f64, Logical>>,
    close_button_hover: bool,
    maximize_button_hover: bool,
}

impl HeaderBar {
    pub fn height(&self) -> i32 {
        0
    }
}

impl IsAlive for CosmicWindow {
    fn alive(&self) -> bool {
        self.window.alive()
    }
}

impl SpaceElement for CosmicWindow {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        SpaceElement::bbox(&self.window)
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        SpaceElement::is_in_input_region(&self.window, point)
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.window, activated)
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.window, output, overlap)
    }
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.window, output)
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        SpaceElement::geometry(&self.window)
    }
    fn z_index(&self) -> u8 {
        SpaceElement::z_index(&self.window)
    }
    fn refresh(&self) {
        SpaceElement::refresh(&self.window)
    }
}

impl KeyboardTarget<State> for CosmicWindow {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        KeyboardTarget::enter(&self.window, seat, data, keys, serial)
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        KeyboardTarget::leave(&self.window, seat, data, serial)
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
        KeyboardTarget::key(&self.window, seat, data, key, state, serial, time)
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        KeyboardTarget::modifiers(&self.window, seat, data, modifiers, serial)
    }
}

impl PointerTarget<State> for CosmicWindow {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        PointerTarget::enter(&self.window, seat, data, event)
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        PointerTarget::motion(&self.window, seat, data, event)
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        PointerTarget::button(&self.window, seat, data, event)
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        PointerTarget::axis(&self.window, seat, data, frame)
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        PointerTarget::leave(&self.window, seat, data, serial, time)
    }
}

render_elements! {
    pub CosmicWindowRenderElement<R> where R: ImportAll;
    Window=WaylandSurfaceRenderElement,
}

impl<R> AsRenderElements<R> for CosmicWindow
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = CosmicWindowRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        AsRenderElements::<R>::render_elements::<CosmicWindowRenderElement<R>>(
            &self.window,
            location,
            scale,
        )
        .into_iter()
        .map(C::from)
        .collect()
    }
}
