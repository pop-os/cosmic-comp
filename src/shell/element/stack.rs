use crate::{
    shell::focus::FocusDirection,
    state::State,
    utils::iced::{IcedElement, Program},
    utils::prelude::SeatExt,
    wayland::handlers::screencopy::ScreencopySessions,
};
use calloop::LoopHandle;
use cosmic::{iced_core::Color, Element};
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::InputType;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            element::{
                memory::MemoryRenderBufferRenderElement, surface::WaylandSurfaceRenderElement,
                AsRenderElements,
            },
            ImportAll, ImportMem, Renderer,
        },
    },
    desktop::space::SpaceElement,
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget, RelativeMotionEvent},
        Seat,
    },
    output::Output,
    render_elements,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
};
use std::{
    fmt,
    hash::Hash,
    sync::{
        atomic::{AtomicU8, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use super::CosmicSurface;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicStack(IcedElement<CosmicStackInternal>);

impl fmt::Debug for CosmicStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.with_program(|stack| {
            f.debug_struct("CosmicStack")
                .field("internal", stack)
                .finish_non_exhaustive()
        })
    }
}

#[derive(Debug, Clone)]
pub struct CosmicStackInternal {
    windows: Arc<Mutex<Vec<CosmicSurface>>>,
    active: Arc<AtomicUsize>,
    previous_keyboard: Arc<AtomicUsize>,
    pointer_entered: Option<Arc<AtomicU8>>,
    previous_pointer: Arc<AtomicUsize>,
    last_location: Arc<Mutex<Option<(Point<f64, Logical>, Serial, u32)>>>,
    mask: Arc<Mutex<Option<tiny_skia::Mask>>>,
}

impl CosmicStackInternal {
    pub fn swap_focus(&self, focus: Focus) -> Focus {
        if let Some(pointer_entered) = self.pointer_entered.as_ref() {
            unsafe {
                std::mem::transmute::<u8, Focus>(
                    pointer_entered.swap(focus as u8, Ordering::SeqCst),
                )
            }
        } else {
            Focus::Window
        }
    }

    pub fn current_focus(&self) -> Focus {
        if let Some(pointer_entered) = self.pointer_entered.as_ref() {
            unsafe { std::mem::transmute::<u8, Focus>(pointer_entered.load(Ordering::SeqCst)) }
        } else {
            Focus::Window
        }
    }
}

const TAB_HEIGHT: i32 = 24;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Focus {
    None,
    Header,
    Window,
}

impl CosmicStack {
    pub fn new<I: Into<CosmicSurface>>(
        windows: impl Iterator<Item = I>,
        handle: LoopHandle<'static, crate::state::Data>,
    ) -> CosmicStack {
        let windows = windows.map(Into::into).collect::<Vec<_>>();
        assert!(!windows.is_empty());
        let width = windows[0].geometry().size.w;
        CosmicStack(IcedElement::new(
            CosmicStackInternal {
                windows: Arc::new(Mutex::new(windows)),
                active: Arc::new(AtomicUsize::new(0)),
                previous_keyboard: Arc::new(AtomicUsize::new(0)),
                pointer_entered: None,
                previous_pointer: Arc::new(AtomicUsize::new(0)),
                last_location: Arc::new(Mutex::new(None)),
                mask: Arc::new(Mutex::new(None)),
            },
            (width, TAB_HEIGHT),
            handle,
        ))
    }

    pub fn add_window(&self, window: CosmicSurface) {
        self.0
            .with_program(|p| p.windows.lock().unwrap().push(window));
    }

    pub fn remove_window(&self, window: &CosmicSurface) {
        self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();
            if windows.len() == 1 {
                return;
            }

            let Some(idx) = windows.iter().position(|w| w == window) else { return };
            windows.remove(idx);
            p.active.fetch_min(windows.len() - 1, Ordering::SeqCst);
        })
    }

    pub fn remove_idx(&self, idx: usize) {
        self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();
            if windows.len() == 1 {
                return;
            }
            if windows.len() >= idx {
                return;
            }
            windows.remove(idx);
            p.active.fetch_min(windows.len() - 1, Ordering::SeqCst);
        })
    }

    pub fn len(&self) -> usize {
        self.0.with_program(|p| p.windows.lock().unwrap().len())
    }

    pub fn handle_focus(&self, direction: FocusDirection) -> bool {
        self.0.with_program(|p| {
            match direction {
                FocusDirection::Left => p
                    .active
                    .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |val| val.checked_sub(1))
                    .is_ok(),
                FocusDirection::Right => {
                    let max = p.windows.lock().unwrap().len();
                    p.active
                        .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |val| {
                            if val < max - 1 {
                                Some(val + 1)
                            } else {
                                None
                            }
                        })
                        .is_ok()
                }
                FocusDirection::Out => false, //TODO
                FocusDirection::In => false,  //TODO
                _ => false,
            }
        })
    }

    pub fn active(&self) -> CosmicSurface {
        self.0
            .with_program(|p| p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].clone())
    }

    pub fn has_active(&self, window: &CosmicSurface) -> bool {
        self.0
            .with_program(|p| &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)] == window)
    }

    pub fn set_active(&self, window: &CosmicSurface) {
        self.0.with_program(|p| {
            if let Some(val) = p.windows.lock().unwrap().iter().position(|w| w == window) {
                let old = p.active.swap(val, Ordering::SeqCst);
                p.previous_keyboard.store(old, Ordering::SeqCst);
                p.previous_pointer.store(old, Ordering::SeqCst);
            }
        })
    }

    pub fn surfaces(&self) -> impl Iterator<Item = CosmicSurface> {
        self.0.with_program(|p| {
            p.windows
                .lock()
                .unwrap()
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
        })
    }

    pub fn offset(&self) -> Point<i32, Logical> {
        Point::from((0, TAB_HEIGHT))
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Logical>) {
        self.0.with_program(|p| {
            let loc = (geo.loc.x, geo.loc.y + TAB_HEIGHT);
            let size = (geo.size.w, geo.size.h - TAB_HEIGHT);

            for window in p.windows.lock().unwrap().iter() {
                window.set_geometry(Rectangle::from_loc_and_size(loc, size));
            }
        });
        self.0.resize(Size::from((geo.size.w, TAB_HEIGHT)));
    }

    fn keyboard_leave_if_previous(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        serial: Serial,
    ) -> usize {
        self.0.with_program(|p| {
            let active = p.active.load(Ordering::SeqCst);
            let previous = p.previous_keyboard.swap(active, Ordering::SeqCst);
            if previous != active {
                KeyboardTarget::leave(&p.windows.lock().unwrap()[previous], seat, data, serial);
                KeyboardTarget::enter(
                    &p.windows.lock().unwrap()[active],
                    seat,
                    data,
                    Vec::new(), //seat.keys(),
                    serial,
                )
            }
            active
        })
    }

    fn pointer_leave_if_previous(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        serial: Serial,
        time: u32,
        location: Point<f64, Logical>,
    ) -> usize {
        self.0.with_program(|p| {
            let active = p.active.load(Ordering::SeqCst);
            let previous = p.previous_pointer.swap(active, Ordering::SeqCst);
            if previous != active {
                if let Some(sessions) = p.windows.lock().unwrap()[previous]
                    .user_data()
                    .get::<ScreencopySessions>()
                {
                    for session in &*sessions.0.borrow() {
                        session.cursor_leave(seat, InputType::Pointer)
                    }
                }
                PointerTarget::leave(
                    &p.windows.lock().unwrap()[previous],
                    seat,
                    data,
                    serial,
                    time,
                );
                if let Some(sessions) = p.windows.lock().unwrap()[active]
                    .user_data()
                    .get::<ScreencopySessions>()
                {
                    for session in &*sessions.0.borrow() {
                        session.cursor_enter(seat, InputType::Pointer)
                    }
                }
                PointerTarget::enter(
                    &p.windows.lock().unwrap()[active],
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
        })
    }

    pub(super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::Data> {
        self.0.loop_handle()
    }
}

impl Program for CosmicStackInternal {
    type Message = ();

    fn view(&self) -> Element<'_, Self::Message> {
        cosmic::iced::widget::text("TODO").into()
    }

    fn clip_mask(&self, size: Size<i32, smithay::utils::Buffer>, scale: f32) -> tiny_skia::Mask {
        let mut mask = self.mask.lock().unwrap();
        if mask.is_none() {
            let mut new_mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();

            let mut pb = tiny_skia::PathBuilder::new();
            let radius = 8. * scale;
            let (w, h) = (size.w as f32, size.h as f32);

            pb.move_to(0., h); // lower-left

            // upper-left rounded corner
            pb.line_to(0., radius);
            pb.quad_to(0., 0., radius, 0.);

            // upper-right rounded corner
            pb.line_to(w - radius, 0.);
            pb.quad_to(w, 0., w, radius);

            pb.line_to(w, h); // lower-right

            let path = pb.finish().unwrap();
            new_mask.fill_path(
                &path,
                tiny_skia::FillRule::EvenOdd,
                true,
                Default::default(),
            );

            *mask = Some(new_mask);
        }

        mask.clone().unwrap()
    }

    fn background_color(&self) -> Color {
        Color {
            r: 0.1176,
            g: 0.1176,
            b: 0.1176,
            a: 1.0,
        }
    }
}

impl IsAlive for CosmicStack {
    fn alive(&self) -> bool {
        self.0
            .with_program(|p| p.windows.lock().unwrap().iter().any(IsAlive::alive))
    }
}

impl SpaceElement for CosmicStack {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut bbox =
                SpaceElement::bbox(&p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]);
            bbox.size.h += TAB_HEIGHT;
            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        let mut point = *point;
        if point.y < TAB_HEIGHT as f64 {
            return true;
        }
        point.y -= TAB_HEIGHT as f64;
        self.0.with_program(|p| {
            SpaceElement::is_in_input_region(
                &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                &point,
            )
        })
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.0, activated);
        self.0.with_program(|p| {
            p.windows
                .lock()
                .unwrap()
                .iter()
                .for_each(|w| SpaceElement::set_activate(w, activated))
        })
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.0, output, overlap);
        self.0.with_program(|p| {
            p.windows
                .lock()
                .unwrap()
                .iter()
                .for_each(|w| SpaceElement::output_enter(w, output, overlap))
        })
    }
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
        self.0.with_program(|p| {
            p.windows
                .lock()
                .unwrap()
                .iter()
                .for_each(|w| SpaceElement::output_leave(w, output))
        })
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut geo =
                SpaceElement::geometry(&p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]);
            geo.size.h += TAB_HEIGHT;
            geo
        })
    }
    fn z_index(&self) -> u8 {
        self.0.with_program(|p| {
            SpaceElement::z_index(&p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)])
        })
    }
    fn refresh(&self) {
        self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();

            // don't let the stack become empty
            let active = windows[p.active.load(Ordering::SeqCst)].clone();
            windows.retain(IsAlive::alive);
            if windows.is_empty() {
                windows.push(active);
            }

            let len = windows.len();
            let _ = p
                .active
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |active| {
                    (active >= len).then_some(len - 1)
                });
            windows.iter().for_each(|w| SpaceElement::refresh(w))
        })
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
        self.0.with_program(|p| {
            let active = p.active.load(Ordering::SeqCst);
            p.previous_keyboard.store(active, Ordering::SeqCst);
            KeyboardTarget::enter(
                &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                seat,
                data,
                keys,
                serial,
            )
        })
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        let active = self.keyboard_leave_if_previous(seat, data, serial);
        self.0.with_program(|p| {
            KeyboardTarget::leave(&p.windows.lock().unwrap()[active], seat, data, serial)
        })
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
        self.0.with_program(|p| {
            KeyboardTarget::key(
                &p.windows.lock().unwrap()[active],
                seat,
                data,
                key,
                state,
                serial,
                time,
            )
        })
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        let active = self.keyboard_leave_if_previous(seat, data, serial);
        self.0.with_program(|p| {
            KeyboardTarget::modifiers(
                &p.windows.lock().unwrap()[active],
                seat,
                data,
                modifiers,
                serial,
            )
        })
    }
}

impl PointerTarget<State> for CosmicStack {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        if self.0.with_program(|p| {
            if let Some(sessions) = p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]
                .user_data()
                .get::<ScreencopySessions>()
            {
                for session in &*sessions.0.borrow() {
                    session.cursor_enter(seat, InputType::Pointer)
                }
            }

            if event.location.y < TAB_HEIGHT as f64 {
                let focus = p.swap_focus(Focus::Header);
                true
            } else {
                let focus = p.swap_focus(Focus::Window);

                *p.last_location.lock().unwrap() = Some((event.location, event.serial, event.time));
                let active = p.active.load(Ordering::SeqCst);
                p.previous_pointer.store(active, Ordering::SeqCst);

                PointerTarget::enter(
                    &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                    seat,
                    data,
                    event,
                );
                false
            }
        }) {
            PointerTarget::enter(&self.0, seat, data, event)
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let active =
            self.pointer_leave_if_previous(seat, data, event.serial, event.time, event.location);
        if let Some((previous, next)) = self.0.with_program(|p| {
            if let Some(sessions) = p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]
                .user_data()
                .get::<ScreencopySessions>()
            {
                for session in &*sessions.0.borrow() {
                    let buffer_loc = (event.location.x, event.location.y); // we always screencast windows at 1x1 scale
                    if let Some((geo, hotspot)) =
                        seat.cursor_geometry(buffer_loc, data.common.clock.now())
                    {
                        session.cursor_info(seat, InputType::Pointer, geo, hotspot);
                    }
                }
            }

            if event.location.y < TAB_HEIGHT as f64 {
                let previous = p.swap_focus(Focus::Header);
                if previous == Focus::Window {
                    PointerTarget::leave(
                        &p.windows.lock().unwrap()[active],
                        seat,
                        data,
                        event.serial,
                        event.time,
                    );
                }
                Some((previous, Focus::Header))
            } else {
                let mut event = event.clone();
                event.location.y -= TAB_HEIGHT as f64;

                let previous = p.swap_focus(Focus::Window);
                if previous != Focus::Window {
                    PointerTarget::enter(&p.windows.lock().unwrap()[active], seat, data, &event);
                } else {
                    PointerTarget::motion(&p.windows.lock().unwrap()[active], seat, data, &event);
                }

                Some((previous, Focus::Window))
            }
        }) {
            match (previous, next) {
                (Focus::Header, Focus::Header) => PointerTarget::motion(&self.0, seat, data, event),
                (_, Focus::Header) => PointerTarget::enter(&self.0, seat, data, event),
                (Focus::Header, _) => {
                    PointerTarget::leave(&self.0, seat, data, event.serial, event.time)
                }
                _ => {}
            }
        }
    }

    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        self.0.with_program(|p| {
            if p.current_focus() == Focus::Window {
                let window = &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)];
                window.relative_motion(seat, data, event)
            }
        })
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        if let Some((location, _serial, _time)) = self
            .0
            .with_program(|p| p.last_location.lock().unwrap().clone())
        {
            self.pointer_leave_if_previous(seat, data, event.serial, event.time, location);
        }

        match self.0.with_program(|p| p.current_focus()) {
            Focus::Header => PointerTarget::button(&self.0, seat, data, event),
            Focus::Window => self.0.with_program(|p| {
                PointerTarget::button(
                    &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                    seat,
                    data,
                    event,
                )
            }),
            _ => {}
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        if let Some((location, serial, time)) = self
            .0
            .with_program(|p| p.last_location.lock().unwrap().clone())
        {
            self.pointer_leave_if_previous(seat, data, serial, time, location);
        }

        match self.0.with_program(|p| p.current_focus()) {
            Focus::Header => PointerTarget::axis(&self.0, seat, data, frame),
            Focus::Window => self.0.with_program(|p| {
                PointerTarget::axis(
                    &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                    seat,
                    data,
                    frame,
                )
            }),
            _ => {}
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        if let Some((location, serial, time)) = self
            .0
            .with_program(|p| p.last_location.lock().unwrap().clone())
        {
            self.pointer_leave_if_previous(seat, data, serial, time, location);
        }

        let previous = self.0.with_program(|p| {
            if let Some(sessions) = p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]
                .user_data()
                .get::<ScreencopySessions>()
            {
                for session in &*sessions.0.borrow() {
                    session.cursor_leave(seat, InputType::Pointer)
                }
            }

            p.swap_focus(Focus::None)
        });
        assert!(previous != Focus::None);

        match previous {
            Focus::Header => PointerTarget::leave(&self.0, seat, data, serial, time),
            Focus::Window => self.0.with_program(|p| {
                PointerTarget::leave(
                    &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                    seat,
                    data,
                    serial,
                    time,
                )
            }),
            _ => {}
        }
    }
}

render_elements! {
    pub CosmicStackRenderElement<R> where R: ImportAll + ImportMem;
    Header=MemoryRenderBufferRenderElement<R>,
    Window=WaylandSurfaceRenderElement<R>,
}

impl<R> AsRenderElements<R> for CosmicStack
where
    R: Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = CosmicStackRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        mut location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        let mut elements = AsRenderElements::<R>::render_elements::<CosmicStackRenderElement<R>>(
            &self.0, renderer, location, scale, alpha,
        );
        location.y += TAB_HEIGHT;

        elements.extend(self.0.with_program(|p| {
            let elements = AsRenderElements::<R>::render_elements::<CosmicStackRenderElement<R>>(
                &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)],
                renderer,
                location,
                scale,
                alpha,
            );
            elements
        }));

        elements.into_iter().map(C::from).collect()
    }
}
