use crate::{
    state::State, utils::prelude::SeatExt, wayland::handlers::screencopy::ScreencopySessions,
};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::InputType;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            element::{surface::WaylandSurfaceRenderElement, AsRenderElements},
            ImportAll, Renderer,
        },
    },
    desktop::{space::SpaceElement, Window},
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
    last_location: Arc<Mutex<Option<(Point<f64, Logical>, Serial, u32)>>>,
    previous_keyboard: Arc<AtomicUsize>,
    previous_pointer: Arc<AtomicUsize>,
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
            let old = self.active.swap(val, Ordering::SeqCst);
            self.previous_keyboard.store(old, Ordering::SeqCst);
            self.previous_pointer.store(old, Ordering::SeqCst);
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
            window
                .toplevel()
                .with_pending_state(|state| state.size = Some(surface_size));
        }
    }

    fn keyboard_leave_if_previous(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        serial: Serial,
    ) -> usize {
        let active = self.active.load(Ordering::SeqCst);
        let previous = self.previous_keyboard.swap(active, Ordering::SeqCst);
        if previous != active {
            KeyboardTarget::leave(&self.windows.lock().unwrap()[previous], seat, data, serial);
            // TODO: KeyboardTarget::enter(&self.windows.lock().unwrap()[active], seat, data, serial, seat.keys())
        }
        active
    }

    fn pointer_leave_if_previous(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        serial: Serial,
        time: u32,
        location: Point<f64, Logical>,
    ) -> usize {
        let active = self.active.load(Ordering::SeqCst);
        let previous = self.previous_pointer.swap(active, Ordering::SeqCst);
        if previous != active {
            if let Some(sessions) = self.windows.lock().unwrap()[previous]
                .user_data()
                .get::<ScreencopySessions>()
            {
                for session in &*sessions.0.borrow() {
                    session.cursor_leave(seat, InputType::Pointer)
                }
            }
            PointerTarget::leave(
                &self.windows.lock().unwrap()[previous],
                seat,
                data,
                serial,
                time,
            );
            if let Some(sessions) = self.windows.lock().unwrap()[active]
                .user_data()
                .get::<ScreencopySessions>()
            {
                for session in &*sessions.0.borrow() {
                    session.cursor_enter(seat, InputType::Pointer)
                }
            }
            PointerTarget::enter(
                &self.windows.lock().unwrap()[active],
                seat,
                data,
                &MotionEvent {
                    location,
                    serial,
                    time,
                },
            );
        }
        active
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
        let active = self.active.load(Ordering::SeqCst);
        self.previous_keyboard.store(active, Ordering::SeqCst);
        KeyboardTarget::enter(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            keys,
            serial,
        )
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        let active = self.keyboard_leave_if_previous(seat, data, serial);
        KeyboardTarget::leave(&self.windows.lock().unwrap()[active], seat, data, serial)
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
        let active = self.keyboard_leave_if_previous(seat, data, serial);
        KeyboardTarget::key(
            &self.windows.lock().unwrap()[active],
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
        let active = self.keyboard_leave_if_previous(seat, data, serial);
        KeyboardTarget::modifiers(
            &self.windows.lock().unwrap()[active],
            seat,
            data,
            modifiers,
            serial,
        )
    }
}

impl PointerTarget<State> for CosmicStack {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        if let Some(sessions) = self.active().user_data().get::<ScreencopySessions>() {
            for session in &*sessions.0.borrow() {
                session.cursor_enter(seat, InputType::Pointer)
            }
        }

        *self.last_location.lock().unwrap() = Some((event.location, event.serial, event.time));
        let active = self.active.load(Ordering::SeqCst);
        self.previous_pointer.store(active, Ordering::SeqCst);

        PointerTarget::enter(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            event,
        )
    }
    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let active =
            self.pointer_leave_if_previous(seat, data, event.serial, event.time, event.location);

        if let Some(sessions) = self.active().user_data().get::<ScreencopySessions>() {
            for session in &*sessions.0.borrow() {
                let buffer_loc = (event.location.x, event.location.y); // we always screencast windows at 1x1 scale
                if let Some((geo, hotspot)) =
                    seat.cursor_geometry(buffer_loc, data.common.clock.now())
                {
                    session.cursor_info(seat, InputType::Pointer, geo, hotspot);
                }
            }
        }

        PointerTarget::motion(&self.windows.lock().unwrap()[active], seat, data, event)
    }
    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        if let Some((location, _serial, _time)) = self.last_location.lock().unwrap().clone() {
            self.pointer_leave_if_previous(seat, data, event.serial, event.time, location);
        }

        PointerTarget::button(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            event,
        )
    }
    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        if let Some((location, serial, time)) = self.last_location.lock().unwrap().clone() {
            self.pointer_leave_if_previous(seat, data, serial, time, location);
        }

        PointerTarget::axis(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            seat,
            data,
            frame,
        )
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        if let Some((location, serial, time)) = self.last_location.lock().unwrap().clone() {
            self.pointer_leave_if_previous(seat, data, serial, time, location);
        }
        if let Some(sessions) = self.active().user_data().get::<ScreencopySessions>() {
            for session in &*sessions.0.borrow() {
                session.cursor_leave(seat, InputType::Pointer)
            }
        }

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
    Window=WaylandSurfaceRenderElement<R>,
}

impl<R> AsRenderElements<R> for CosmicStack
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = CosmicStackRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        AsRenderElements::<R>::render_elements::<CosmicStackRenderElement<R>>(
            &self.windows.lock().unwrap()[self.active.load(Ordering::SeqCst)],
            renderer,
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
