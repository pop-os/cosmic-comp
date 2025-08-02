use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    sync::{mpsc::Receiver, Arc, LazyLock, Mutex},
};

use cosmic::{
    iced::{
        advanced::{graphics::text::font_system, widget::Tree},
        event::Event,
        futures::{FutureExt, StreamExt},
        keyboard::{Event as KeyboardEvent, Modifiers as IcedModifiers},
        mouse::{Button as MouseButton, Cursor, Event as MouseEvent, ScrollDelta},
        touch::{Event as TouchEvent, Finger},
        window::Event as WindowEvent,
        Limits, Point as IcedPoint, Size as IcedSize, Task,
    },
    iced_core::{clipboard::Null as NullClipboard, id::Id, renderer::Style, Color, Length, Pixels},
    iced_runtime::{
        program::{Program as IcedProgram, State},
        task::into_stream,
        Action, Debug,
    },
    Theme,
};
use iced_tiny_skia::{
    graphics::{damage, Viewport},
    Layer,
};

use ordered_float::OrderedFloat;
use smithay::{
    backend::{
        allocator::Fourcc,
        input::{ButtonState, KeyState},
        renderer::{
            element::{
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
                AsRenderElements, Kind,
            },
            ImportMem, Renderer,
        },
    },
    desktop::space::{RenderZindex, SpaceElement},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent, TouchTarget,
            UpEvent,
        },
        Seat,
    },
    output::Output,
    reexports::calloop::RegistrationToken,
    reexports::calloop::{self, futures::Scheduler, LoopHandle},
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size,
        Transform,
    },
};

static ID: LazyLock<Id> = LazyLock::new(|| Id::new("Program"));

pub struct IcedElement<P: Program + Send + 'static>(pub(crate) Arc<Mutex<IcedElementInternal<P>>>);

impl<P: Program + Send + 'static> fmt::Debug for IcedElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

// SAFETY: It is not, we need to make sure we never move this into another thread and drop it there,
//  as on drop it could trigger RefCells in calloop.
unsafe impl<P: Program + Send + 'static> Send for IcedElementInternal<P> {}

impl<P: Program + Send + 'static> Clone for IcedElement<P> {
    fn clone(&self) -> Self {
        IcedElement(self.0.clone())
    }
}

impl<P: Program + Send + 'static> PartialEq for IcedElement<P> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<P: Program + Send + 'static> Eq for IcedElement<P> {}

impl<P: Program + Send + 'static> Hash for IcedElement<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.0) as usize).hash(state)
    }
}

pub trait Program {
    type Message: std::fmt::Debug + Send;
    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::State>,
        last_seat: Option<&(Seat<crate::state::State>, Serial)>,
    ) -> Task<Self::Message> {
        let _ = (message, loop_handle, last_seat);
        Task::none()
    }
    fn view(&self) -> cosmic::Element<'_, Self::Message>;

    fn background_color(&self, _theme: &cosmic::Theme) -> Color {
        Color::TRANSPARENT
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, BufferCoords>],
        scale: f32,
        theme: &cosmic::Theme,
    ) {
        let _ = (pixels, damage, scale, theme);
    }
}

struct ProgramWrapper<P: Program> {
    program: P,
    evlh: LoopHandle<'static, crate::state::State>,
    last_seat: Arc<Mutex<Option<(Seat<crate::state::State>, Serial)>>>,
}

impl<P: Program> IcedProgram for ProgramWrapper<P> {
    type Message = <P as Program>::Message;
    type Renderer = cosmic::Renderer;
    type Theme = cosmic::Theme;

    fn update(&mut self, message: Self::Message) -> Task<Self::Message> {
        let last_seat = self.last_seat.lock().unwrap();
        self.program.update(message, &self.evlh, last_seat.as_ref())
    }

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        self.program.view()
    }
}

pub(crate) struct IcedElementInternal<P: Program + Send + 'static> {
    // draw buffer
    additional_scale: f64,
    outputs: HashSet<Output>,
    buffers: HashMap<OrderedFloat<f64>, (MemoryRenderBuffer, Option<(Vec<Layer>, Color)>)>,
    pending_realloc: bool,

    // state
    size: Size<i32, Logical>,
    last_seat: Arc<Mutex<Option<(Seat<crate::state::State>, Serial)>>>,
    cursor_pos: Option<Point<f64, Logical>>,
    touch_map: HashMap<Finger, IcedPoint>,

    // iced
    theme: Theme,
    renderer: cosmic::Renderer,
    state: State<ProgramWrapper<P>>,
    debug: Debug,

    // futures
    handle: LoopHandle<'static, crate::state::State>,
    scheduler: Scheduler<Option<<P as Program>::Message>>,
    executor_token: Option<RegistrationToken>,
    rx: Receiver<Option<<P as Program>::Message>>,
}

impl<P: Program + Send + Clone + 'static> Clone for IcedElementInternal<P> {
    fn clone(&self) -> Self {
        let handle = self.handle.clone();
        let (executor, scheduler) = calloop::futures::executor().expect("Out of file descriptors");
        let (tx, rx) = std::sync::mpsc::channel();
        let executor_token = handle
            .insert_source(executor, move |message, _, _| {
                let _ = tx.send(message);
            })
            .ok();

        if !self.state.is_queue_empty() {
            tracing::warn!("Missing force_update call");
        }
        let mut renderer = cosmic::Renderer::new(cosmic::font::default(), Pixels(16.0));
        let mut debug = Debug::new();
        let state = State::new(
            ID.clone(),
            ProgramWrapper {
                program: self.state.program().program.clone(),
                evlh: handle.clone(),
                last_seat: self.last_seat.clone(),
            },
            IcedSize::new(self.size.w as f32, self.size.h as f32),
            &mut renderer,
            &mut debug,
        );

        IcedElementInternal {
            additional_scale: self.additional_scale,
            outputs: self.outputs.clone(),
            buffers: self.buffers.clone(),
            pending_realloc: self.pending_realloc.clone(),
            size: self.size.clone(),
            last_seat: self.last_seat.clone(),
            cursor_pos: self.cursor_pos.clone(),
            touch_map: self.touch_map.clone(),
            theme: self.theme.clone(),
            renderer,
            state,
            debug,
            handle,
            scheduler,
            executor_token,
            rx,
        }
    }
}

impl<P: Program + Send + 'static> fmt::Debug for IcedElementInternal<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IcedElementInternal")
            .field("additional_scale", &self.additional_scale)
            .field(
                "outputs",
                &self.outputs.iter().map(|o| o.name()).collect::<Vec<_>>(),
            )
            .field("buffers", &"...")
            .field("size", &self.size)
            .field("pending_realloc", &self.pending_realloc)
            .field("last_seat", &self.last_seat)
            .field("cursor_pos", &self.cursor_pos)
            .field("touch_map", &self.touch_map)
            .field("theme", &"...")
            .field("renderer", &"...")
            .field("state", &"...")
            .field("debug", &self.debug)
            .field("handle", &self.handle)
            .field("scheduler", &self.scheduler)
            .field("executor_token", &self.executor_token)
            .field("rx", &self.rx)
            .finish()
    }
}

impl<P: Program + Send + 'static> Drop for IcedElementInternal<P> {
    fn drop(&mut self) {
        self.handle.remove(self.executor_token.take().unwrap());
    }
}

impl<P: Program + Send + 'static> IcedElement<P> {
    pub fn new(
        program: P,
        size: impl Into<Size<i32, Logical>>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: cosmic::Theme,
    ) -> IcedElement<P> {
        let size = size.into();
        let last_seat = Arc::new(Mutex::new(None));
        let mut renderer = cosmic::Renderer::new(cosmic::font::default(), Pixels(16.0));
        let mut debug = Debug::new();

        let state = State::new(
            ID.clone(),
            ProgramWrapper {
                program,
                evlh: handle.clone(),
                last_seat: last_seat.clone(),
            },
            IcedSize::new(size.w as f32, size.h as f32),
            &mut renderer,
            &mut debug,
        );

        let (executor, scheduler) = calloop::futures::executor().expect("Out of file descriptors");
        let (tx, rx) = std::sync::mpsc::channel();
        let executor_token = handle
            .insert_source(executor, move |message, _, _| {
                let _ = tx.send(message);
            })
            .ok();

        let mut internal = IcedElementInternal {
            additional_scale: 1.0,
            outputs: HashSet::new(),
            buffers: HashMap::new(),
            pending_realloc: false,
            size,
            cursor_pos: None,
            last_seat,
            touch_map: HashMap::new(),
            theme,
            renderer,
            state,
            debug,
            handle,
            scheduler,
            executor_token,
            rx,
        };
        internal.update(true);

        IcedElement(Arc::new(Mutex::new(internal)))
    }

    pub fn with_program<R>(&self, func: impl FnOnce(&P) -> R) -> R {
        let internal = self.0.lock().unwrap();
        func(&internal.state.program().program)
    }

    pub fn minimum_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        let element = internal.state.program().program.view();
        let node = element
            .as_widget()
            .layout(
                // TODO Avoid creating a new tree here?
                &mut Tree::new(element.as_widget()),
                &internal.renderer,
                &Limits::new(IcedSize::ZERO, IcedSize::INFINITY)
                    .width(Length::Shrink)
                    .height(Length::Shrink),
            )
            .size();
        Size::from((node.width.ceil() as i32, node.height.ceil() as i32))
    }

    pub fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        self.0.lock().unwrap().handle.clone()
    }

    pub fn resize(&self, size: Size<i32, Logical>) {
        let mut internal = self.0.lock().unwrap();
        let internal_ref = &mut *internal;
        if internal_ref.size == size {
            return;
        }

        internal_ref.size = size;
        internal_ref.pending_realloc = true;
        internal_ref.update(true);
    }

    pub fn set_additional_scale(&self, scale: f64) {
        {
            let mut internal = self.0.lock().unwrap();
            let internal_ref = &mut *internal;
            if internal_ref.additional_scale == scale {
                return;
            }

            internal_ref.additional_scale = scale;
        }
        self.refresh();
    }

    pub fn force_update(&self) {
        self.0.lock().unwrap().update(true);
    }

    pub fn set_theme(&self, theme: cosmic::Theme) {
        let mut guard = self.0.lock().unwrap();
        guard.theme = theme.clone();
    }

    pub fn force_redraw(&self) {
        let mut internal = self.0.lock().unwrap();
        for (_buffer, old_primitives) in internal.buffers.values_mut() {
            *old_primitives = None;
        }
    }

    pub fn current_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        internal
            .size
            .to_f64()
            .upscale(internal.additional_scale)
            .to_i32_round()
    }

    pub fn queue_message(&self, msg: P::Message) {
        self.0.lock().unwrap().state.queue_message(msg);
    }
}

impl<P: Program + Send + 'static + Clone> IcedElement<P> {
    pub fn deep_clone(&self) -> Self {
        let internal = self.0.lock().unwrap();
        if !internal.state.is_queue_empty() {
            self.force_update();
        }
        IcedElement(Arc::new(Mutex::new(internal.clone())))
    }
}

impl<P: Program + Send + 'static> IcedElementInternal<P> {
    #[profiling::function]
    fn update(&mut self, force: bool) {
        while let Ok(Some(message)) = self.rx.try_recv() {
            self.state.queue_message(message);
        }

        if self.state.is_queue_empty() && !force {
            return;
        }

        let cursor = self
            .cursor_pos
            .map(|p| IcedPoint::new(p.x as f32, p.y as f32))
            .map(Cursor::Available)
            .unwrap_or(Cursor::Unavailable);

        let actions = self
            .state
            .update(
                ID.clone(),
                IcedSize::new(self.size.w as f32, self.size.h as f32),
                cursor,
                &mut self.renderer,
                &self.theme,
                &Style {
                    scale_factor: 1.0, // TODO: why is this
                    icon_color: self.theme.cosmic().on_bg_color().into(),
                    text_color: self.theme.cosmic().on_bg_color().into(),
                },
                &mut NullClipboard,
                &mut self.debug,
            )
            .1;

        if let Some(action) = actions {
            if let Some(t) = into_stream(action) {
                let _ = self.scheduler.schedule(t.into_future().map(|f| match f.0 {
                    Some(Action::Output(msg)) => Some(msg),
                    _ => None,
                }));
            }
        }
    }
}

impl<P: Program + Send + 'static> PointerTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorEntered));
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorMoved { position }));
        // TODO: Update iced widgets to handle touch using event position, not cursor_pos
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(false);
    }

    fn motion(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(false);
    }

    fn relative_motion(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &RelativeMotionEvent,
    ) {
    }

    fn button(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &ButtonEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let button = match event.button {
            0x110 => MouseButton::Left,
            0x111 => MouseButton::Right,
            0x112 => MouseButton::Middle,
            x => MouseButton::Other(x as u16),
        };
        internal.state.queue_event(Event::Mouse(match event.state {
            ButtonState::Pressed => MouseEvent::ButtonPressed(button),
            ButtonState::Released => MouseEvent::ButtonReleased(button),
        }));
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(false);
    }

    fn axis(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: AxisFrame,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::WheelScrolled {
                delta: if let Some(discrete) = frame.v120 {
                    ScrollDelta::Lines {
                        x: discrete.0 as f32 / 120.,
                        y: discrete.1 as f32 / 120.,
                    }
                } else {
                    ScrollDelta::Pixels {
                        x: frame.axis.0 as f32,
                        y: frame.axis.1 as f32,
                    }
                },
            }));
        internal.update(false);
    }

    fn frame(&self, _seat: &Seat<crate::state::State>, _data: &mut crate::state::State) {}

    fn leave(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _serial: Serial,
        _time: u32,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorLeft));
        internal.update(false);
    }

    fn gesture_swipe_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeBeginEvent,
    ) {
    }
    fn gesture_swipe_update(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeUpdateEvent,
    ) {
    }
    fn gesture_swipe_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureSwipeEndEvent,
    ) {
    }
    fn gesture_pinch_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchBeginEvent,
    ) {
    }
    fn gesture_pinch_update(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchUpdateEvent,
    ) {
    }
    fn gesture_pinch_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GesturePinchEndEvent,
    ) {
    }
    fn gesture_hold_begin(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureHoldBeginEvent,
    ) {
    }
    fn gesture_hold_end(
        &self,
        _: &Seat<crate::state::State>,
        _: &mut crate::state::State,
        _: &GestureHoldEndEvent,
    ) {
    }
}

impl<P: Program + Send + 'static> TouchTarget<crate::state::State> for IcedElement<P> {
    fn down(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &DownEvent,
        seq: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .state
            .queue_event(Event::Touch(TouchEvent::FingerPressed { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), seq));
        let _ = internal.update(false);
    }

    fn up(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &UpEvent,
        seq: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        if let Some(position) = internal.touch_map.remove(&id) {
            *internal.last_seat.lock().unwrap() = Some((seat.clone(), seq));
            internal
                .state
                .queue_event(Event::Touch(TouchEvent::FingerLifted { id, position }));
            let _ = internal.update(false);
        }
    }

    fn motion(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &TouchMotionEvent,
        seq: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), seq));
        internal
            .state
            .queue_event(Event::Touch(TouchEvent::FingerMoved { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        let _ = internal.update(false);
    }

    fn frame(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _seq: Serial,
    ) {
    }

    fn cancel(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _seq: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        for (id, position) in std::mem::take(&mut internal.touch_map) {
            internal
                .state
                .queue_event(Event::Touch(TouchEvent::FingerLost { id, position }));
        }
        let _ = internal.update(false);
    }

    fn shape(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &ShapeEvent,
        _seq: Serial,
    ) {
    }

    fn orientation(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &OrientationEvent,
        _seq: Serial,
    ) {
    }
}

impl<P: Program + Send + 'static> KeyboardTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _keys: Vec<KeysymHandle<'_>>,
        _serial: Serial,
    ) {
        // TODO convert keys
    }

    fn leave(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _serial: Serial,
    ) {
        // TODO remove all held keys
    }

    fn key(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _key: KeysymHandle<'_>,
        _state: KeyState,
        _serial: Serial,
        _time: u32,
    ) {
        // TODO convert keys
    }

    fn modifiers(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        modifiers: ModifiersState,
        _serial: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        let mut mods = IcedModifiers::empty();
        if modifiers.shift {
            mods.insert(IcedModifiers::SHIFT);
        }
        if modifiers.alt {
            mods.insert(IcedModifiers::ALT);
        }
        if modifiers.ctrl {
            mods.insert(IcedModifiers::CTRL);
        }
        if modifiers.logo {
            mods.insert(IcedModifiers::LOGO);
        }
        internal
            .state
            .queue_event(Event::Keyboard(KeyboardEvent::ModifiersChanged(mods)));
        let _ = internal.update(false);
    }
}

impl<P: Program + Send + 'static> IsAlive for IcedElement<P> {
    fn alive(&self) -> bool {
        true
    }
}

impl<P: Program + Send + 'static> SpaceElement for IcedElement<P> {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        let internal = self.0.lock().unwrap();
        Rectangle::from_size(
            internal
                .size
                .to_f64()
                .upscale(internal.additional_scale)
                .to_i32_round(),
        )
    }

    fn is_in_input_region(&self, _point: &Point<f64, Logical>) -> bool {
        true
    }

    fn set_activate(&self, activated: bool) {
        let mut internal = self.0.lock().unwrap();
        internal.state.queue_event(Event::Window(if activated {
            WindowEvent::Focused
        } else {
            WindowEvent::Unfocused
        }));
        let _ = internal.update(false);
    }

    fn output_enter(&self, output: &Output, _overlap: Rectangle<i32, Logical>) {
        let mut internal = self.0.lock().unwrap();
        let scale = output.current_scale().fractional_scale() * internal.additional_scale;

        let internal_size = internal.size;
        internal.buffers.entry(OrderedFloat(scale)).or_insert({
            let buffer_size = internal_size
                .to_f64()
                .to_buffer(scale, Transform::Normal)
                .to_i32_round();

            (
                MemoryRenderBuffer::new(Fourcc::Argb8888, buffer_size, 1, Transform::Normal, None),
                None,
            )
        });

        internal.outputs.insert(output.clone());
        std::mem::drop(internal);
        self.refresh();
    }

    fn output_leave(&self, output: &Output) {
        self.0.lock().unwrap().outputs.remove(output);
        self.refresh();
    }

    fn z_index(&self) -> u8 {
        // meh, user-provided?
        RenderZindex::Shell as u8
    }

    #[profiling::function]
    fn refresh(&self) {
        let mut internal = self.0.lock().unwrap();
        // makes partial borrows easier
        let internal_ref = &mut *internal;
        internal_ref.buffers.retain(|scale, _| {
            internal_ref.outputs.iter().any(|o| {
                o.current_scale().fractional_scale() * internal_ref.additional_scale == **scale
            })
        });
        for scale in internal_ref
            .outputs
            .iter()
            .map(|o| {
                OrderedFloat(o.current_scale().fractional_scale() * internal_ref.additional_scale)
            })
            .filter(|scale| !internal_ref.buffers.contains_key(scale))
            .collect::<Vec<_>>()
            .into_iter()
        {
            let buffer_size = internal_ref
                .size
                .to_f64()
                .to_buffer(*scale, Transform::Normal)
                .to_i32_round();
            internal_ref.buffers.insert(
                scale,
                (
                    MemoryRenderBuffer::new(
                        Fourcc::Argb8888,
                        buffer_size,
                        1,
                        Transform::Normal,
                        None,
                    ),
                    None,
                ),
            );
        }
        internal.update(false);
    }
}

impl<P, R> AsRenderElements<R> for IcedElement<P>
where
    P: Program + Send + 'static,
    R: Renderer + ImportMem,
    R::TextureId: Send + Clone + 'static,
{
    type RenderElement = MemoryRenderBufferRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        mut scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        let mut internal = self.0.lock().unwrap();
        // makes partial borrows easier
        let internal_ref = &mut *internal;
        if std::mem::replace(&mut internal_ref.pending_realloc, false) {
            for (scale, (buffer, old_primitives)) in internal_ref.buffers.iter_mut() {
                let buffer_size = internal_ref
                    .size
                    .to_f64()
                    .to_buffer(**scale, Transform::Normal)
                    .to_i32_round();
                buffer.render().resize(buffer_size);
                *old_primitives = None;
            }
        }

        scale = scale * internal_ref.additional_scale;
        if let Some((buffer, old_layers)) = internal_ref.buffers.get_mut(&OrderedFloat(scale.x)) {
            let size: Size<i32, BufferCoords> = internal_ref
                .size
                .to_f64()
                .to_buffer(scale.x, Transform::Normal)
                .to_i32_round();
            if size.w > 0 && size.h > 0 {
                let state_ref = &internal_ref.state;
                let mut clip_mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();
                let overlay = internal_ref.debug.overlay();
                let theme = &internal_ref.theme;

                _ = buffer.render().draw(|buf| {
                    let mut pixels =
                        tiny_skia::PixmapMut::from_bytes(buf, size.w as u32, size.h as u32)
                            .expect("Failed to create pixel map");

                    let background_color = state_ref.program().program.background_color(theme);
                    let bounds = IcedSize::new(size.w as u32, size.h as u32);
                    let viewport = Viewport::with_physical_size(bounds, scale.x);
                    let scale_x = scale.x as f32;
                    let current_layers = internal_ref.renderer.layers();
                    let mut damage: Vec<_> = old_layers
                        .as_ref()
                        .and_then(|(last_primitives, last_color)| {
                            (last_color == &background_color).then(|| {
                                damage::diff(
                                    &last_primitives,
                                    current_layers,
                                    |_| {
                                        vec![cosmic::iced::Rectangle::new(
                                            cosmic::iced::Point::default(),
                                            viewport.logical_size(),
                                        )]
                                    },
                                    Layer::damage,
                                )
                                .into_iter()
                                .filter(|d| {
                                    let width = d.width as u32;
                                    let height = d.height as u32;

                                    width > 1 && height > 1
                                })
                                .collect()
                            })
                        })
                        .unwrap_or_else(|| {
                            vec![cosmic::iced::Rectangle::with_size(viewport.logical_size())]
                        });
                    damage = damage::group(
                        damage,
                        cosmic::iced::Rectangle::with_size(viewport.logical_size()),
                    );

                    if !damage.is_empty() {
                        *old_layers = Some((current_layers.to_vec(), background_color));

                        internal_ref.renderer.draw(
                            &mut pixels,
                            &mut clip_mask,
                            &viewport,
                            &damage,
                            background_color,
                            &overlay,
                        );
                    }

                    let damage = damage
                        .into_iter()
                        .map(|d| d * scale_x)
                        .filter_map(|x| x.snap())
                        .map(|damage_rect| {
                            Rectangle::new(
                                (damage_rect.x as i32, damage_rect.y as i32).into(),
                                (damage_rect.width as i32, damage_rect.height as i32).into(),
                            )
                        })
                        .collect::<Vec<_>>();
                    state_ref.program().program.foreground(
                        &mut pixels,
                        &damage,
                        scale.x as f32,
                        theme,
                    );

                    Result::<_, ()>::Ok(damage)
                });

                // trim the shape cache
                {
                    let mut font_system = font_system().write().unwrap();
                    font_system.raw().shape_run_cache.trim(1024);
                }
            }

            match MemoryRenderBufferRenderElement::from_buffer(
                renderer,
                location.to_f64(),
                &buffer,
                Some(alpha),
                Some(Rectangle::from_size(
                    size.to_f64()
                        .to_logical(1., Transform::Normal)
                        .to_i32_round(),
                )),
                Some(
                    internal_ref
                        .size
                        .to_f64()
                        .upscale(internal_ref.additional_scale)
                        .to_i32_round(),
                ),
                Kind::Unspecified,
            ) {
                Ok(buffer) => {
                    return vec![C::from(buffer)];
                }
                Err(err) => tracing::warn!("What? {:?}", err),
            }
        }
        Vec::new()
    }
}
