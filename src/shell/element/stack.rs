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
    render_elements,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
};
use std::{
    hash::Hash,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

#[derive(Debug, Clone)]
pub struct CosmicStack {
    windows: Arc<Mutex<Vec<Window>>>,
    active: Arc<AtomicUsize>,
    pub(super) header: Arc<Mutex<Option<HeaderBar>>>,
}

impl PartialEq for CosmicStack {
    fn eq(&self, other: &Self) -> bool {
        *self.windows.lock().unwrap() == *other.windows.lock().unwrap()
    }
}

impl Eq for CosmicStack {}

impl Hash for CosmicStack {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.windows).hash(state)
    }
}

#[derive(Debug)]
pub struct HeaderBar {}

impl HeaderBar {
    pub fn height(&self) -> i32 {
        0
    }
}

impl CosmicStack {
    pub fn active(&self) -> Window {
        self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)].clone()
    }

    pub fn set_active(&self, window: &Window) {
        if let Some(val) = self
            .windows
            .lock()
            .unwrap()
            .iter()
            .position(|w| w == window)
        {
            self.active.store(val, Ordering::SeqCst)
        }
    }

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

        for window in self.windows.lock().unwrap().iter() {
            match window.toplevel() {
                Kind::Xdg(xdg) => xdg.with_pending_state(|state| state.size = Some(surface_size)),
            };
        }
    }
}

impl IsAlive for CosmicStack {
    fn alive(&self) -> bool {
        self.windows.lock().unwrap().iter().any(IsAlive::alive)
    }
}

impl SpaceElement for CosmicStack {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        SpaceElement::bbox(&self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)])
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        SpaceElement::is_in_input_region(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            point,
        )
    }
    fn set_activate(&self, activated: bool) {
        self.windows
            .lock()
            .unwrap()
            .iter()
            .for_each(|w| SpaceElement::set_activate(w, activated))
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        self.windows
            .lock()
            .unwrap()
            .iter()
            .for_each(|w| SpaceElement::output_enter(w, output, overlap))
    }
    fn output_leave(&self, output: &Output) {
        self.windows
            .lock()
            .unwrap()
            .iter()
            .for_each(|w| SpaceElement::output_leave(w, output))
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        SpaceElement::geometry(&self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)])
    }
    fn z_index(&self) -> u8 {
        SpaceElement::z_index(&self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)])
    }
    fn refresh(&self) {
        let mut windows = self.windows.lock().unwrap();
        windows.retain(IsAlive::alive);
        let len = windows.len();
        let _ = self
            .active
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |active| {
                (active > len).then_some(len - 1)
            });
        windows.iter().for_each(|w| SpaceElement::refresh(w))
    }
}

impl KeyboardTarget<State> for CosmicStack {
    fn enter(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        KeyboardTarget::enter(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            keys,
            serial,
        )
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        KeyboardTarget::leave(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            serial,
        )
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
        KeyboardTarget::key(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            key,
            state,
            serial,
            time,
        )
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        KeyboardTarget::modifiers(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            modifiers,
            serial,
        )
    }
}

impl PointerTarget<State> for CosmicStack {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        PointerTarget::enter(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            event,
        )
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        PointerTarget::motion(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            event,
        )
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        PointerTarget::button(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            event,
        )
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        PointerTarget::axis(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            frame,
        )
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        PointerTarget::leave(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            serial,
            time,
        )
    }
}

render_elements! {
    pub CosmicStackRenderElement<R> where R: ImportAll;
    Window=WaylandSurfaceRenderElement,
}

impl<R> AsRenderElements<R> for CosmicStack
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = CosmicStackRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        AsRenderElements::<R>::render_elements::<CosmicStackRenderElement<R>>(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            location,
            scale,
        )
        .into_iter()
        .map(C::from)
        .collect()
    }
}
impl CosmicStack {
    pub fn windows(&self) -> impl Iterator<Item = Window> {
        self.windows
            .lock()
            .unwrap()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
    }
}
