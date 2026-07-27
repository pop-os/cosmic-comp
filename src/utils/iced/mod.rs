//! Iced 0.15 compositor UI element using `UserInterface` + `Cache`.
//!
//! Renders iced widget trees into smithay buffers via `iced_tiny_skia`,
//! without any libcosmic dependency. Uses `CompTheme` for theming.

use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    sync::{Arc, Mutex, mpsc::Receiver},
    time::Instant,
};

use super::iced_profiler::{ICED_PROFILER, UpdateRecord, UpdateSource, iced_perf_logging_enabled};

// iced 0.15 direct imports (no libcosmic re-exports)
use iced_core::{
    Color, Element, Font, Length, Pixels, Point as IcedPoint, Size as IcedSize,
    event::Event,
    keyboard::{Event as KeyboardEvent, Modifiers as IcedModifiers},
    layout::Limits,
    mouse::{
        Button as MouseButton, Cursor, Event as MouseEvent, Interaction as MouseInteraction,
        ScrollDelta,
    },
    renderer::Style as RendererStyle,
    time::Instant as IcedInstant,
    touch::{Event as TouchEvent, Finger},
    widget::Tree,
    window::{self, Event as WindowEvent},
};
use iced_graphics::damage;
use iced_graphics::{Viewport, text::font_system};
use iced_runtime::{
    Action, Task,
    task::into_stream,
    user_interface::{self, UserInterface},
};
use iced_tiny_skia::Layer;

use super::iced_keymap;
use futures_util::{FutureExt, StreamExt};
use ordered_float::OrderedFloat;
use smithay::{
    backend::{
        allocator::Fourcc,
        input::{ButtonState, KeyState},
        renderer::{
            ImportMem, Renderer,
            element::{
                AsRenderElements, Kind,
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
            },
        },
    },
    desktop::space::{RenderZindex, SpaceElement},
    input::{
        Seat,
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, FrameMarker, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent,
            TouchTarget, UpEvent,
        },
    },
    output::Output,
    reexports::calloop::{self, LoopHandle, RegistrationToken, futures::Scheduler},
    utils::{
        Buffer as BufferCoords, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size,
        Transform,
    },
};

// --- Theme ---
pub use crate::comp_theme::CompTheme;

/// Type alias for iced elements rendered in the compositor.
/// Uses `iced_core::Theme` so standard iced widgets and icetron components
/// can be used without custom Catalog impls.
pub type CompElement<'a, Message> =
    Element<'a, Message, iced_core::Theme, iced_tiny_skia::Renderer>;

// --- Public API (unchanged interface) ---

pub struct IcedElement<P: Program + Send + 'static>(pub(crate) Arc<Mutex<IcedElementInternal<P>>>);

impl<P: Program + Send + 'static> fmt::Debug for IcedElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

// SAFETY: We must ensure this is never moved to another thread and dropped there.
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

// --- Program trait (same interface, different Element type) ---

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

    /// Returns the view element.
    /// `theme` provides design tokens for pre-baking styles into closures.
    fn view<'a>(&'a self, theme: &'a CompTheme) -> CompElement<'a, Self::Message>;

    /// Program name for profiling logs. Defaults to the type name.
    fn program_name() -> &'static str {
        std::any::type_name::<Self>()
    }

    fn background_color(&self, _theme: &CompTheme) -> Color {
        Color::TRANSPARENT
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, BufferCoords>],
        scale: f32,
        theme: &CompTheme,
    ) {
        let _ = (pixels, damage, scale, theme);
    }
}

// --- Internal state ---

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
    last_touch_frame: Option<FrameMarker>,
    last_touch_serial: Option<Serial>,

    // iced 0.15: UserInterface cache + manual event queue (replaces State<ProgramWrapper>)
    theme: CompTheme,
    iced_theme: iced_core::Theme,
    renderer: iced_tiny_skia::Renderer,
    cache: user_interface::Cache,
    event_queue: Vec<Event>,
    mouse_interaction: MouseInteraction,
    needs_redraw: bool,

    // the actual program
    program: P,

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

        let renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));

        IcedElementInternal {
            additional_scale: self.additional_scale,
            outputs: self.outputs.clone(),
            buffers: self.buffers.clone(),
            pending_realloc: self.pending_realloc,
            size: self.size,
            last_seat: self.last_seat.clone(),
            cursor_pos: self.cursor_pos,
            touch_map: self.touch_map.clone(),
            last_touch_frame: None,
            last_touch_serial: None,
            theme: self.theme.clone(),
            iced_theme: self.theme.to_iced_theme(),
            renderer,
            cache: user_interface::Cache::default(),
            event_queue: Vec::new(),
            mouse_interaction: MouseInteraction::default(),
            needs_redraw: false,
            program: self.program.clone(),
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
            .field("last_touch_frame", &self.last_touch_frame)
            .field("last_touch_serial", &self.last_touch_serial)
            .field("theme", &"...")
            .field("renderer", &"...")
            .field("cache", &"...")
            .field("event_queue_len", &self.event_queue.len())
            .field("mouse_interaction", &self.mouse_interaction)
            .field("handle", &self.handle)
            .field("scheduler", &self.scheduler)
            .field("executor_token", &self.executor_token)
            .field("rx", &self.rx)
            .finish()
    }
}

/// Executor sources orphaned by an `IcedElementInternal` that was dropped off
/// the main loop thread — e.g. on a KMS surface render thread still holding an
/// `Arc` clone of a window's decoration in its cached frame. `LoopHandle::remove`
/// borrows a `RefCell` the main event loop also borrows while dispatching, so
/// calling it off-thread (or mid-dispatch) panics with "RefCell already
/// borrowed" and takes the whole compositor down. Instead we stash the token
/// here and drain it from the main loop's post-dispatch refresh, where removal
/// is safe. `RegistrationToken` is a plain key, so this is cheap and `Send`.
static PENDING_EXECUTOR_REMOVALS: Mutex<Vec<RegistrationToken>> = Mutex::new(Vec::new());

/// Remove executor sources left behind by `IcedElementInternal`s dropped off the
/// loop thread. Call from the main thread at a post-dispatch point (see
/// `crate::refresh`), where `LoopHandle::remove` is safe.
pub fn drain_pending_executor_removals(handle: &LoopHandle<'static, crate::state::State>) {
    let tokens = std::mem::take(&mut *PENDING_EXECUTOR_REMOVALS.lock().unwrap());
    for token in tokens {
        handle.remove(token);
    }
}

impl<P: Program + Send + 'static> Drop for IcedElementInternal<P> {
    fn drop(&mut self) {
        // May run on a non-main thread (a surface render thread that held the
        // last Arc to this element), so we must NOT touch the loop handle here.
        // Defer the source removal to the main-loop drain above.
        if let Some(token) = self.executor_token.take() {
            PENDING_EXECUTOR_REMOVALS.lock().unwrap().push(token);
        }
    }
}

// --- Public IcedElement methods ---

impl<P: Program + Send + 'static> IcedElement<P> {
    pub fn new(
        program: P,
        size: impl Into<Size<i32, Logical>>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: CompTheme,
    ) -> IcedElement<P> {
        let size = size.into();
        let last_seat = Arc::new(Mutex::new(None));
        let renderer = iced_tiny_skia::Renderer::new(Font::DEFAULT, Pixels(16.0));

        // Load icetron fonts into the global font system so themed text renders correctly.
        // Guarded by Once to avoid redundant write-lock + iteration on every IcedElement.
        static FONTS_LOADED: std::sync::Once = std::sync::Once::new();
        FONTS_LOADED.call_once(|| {
            let mut fs = font_system().write().expect("Write font system");
            for font_data in icetron_themes::fonts::ALL {
                fs.load_font(std::borrow::Cow::Borrowed(*font_data));
            }
        });

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
            iced_theme: theme.to_iced_theme(),
            last_touch_frame: None,
            last_touch_serial: None,
            theme,
            renderer,
            cache: user_interface::Cache::new(),
            event_queue: Vec::new(),
            mouse_interaction: MouseInteraction::default(),
            needs_redraw: false,
            program,
            handle,
            scheduler,
            executor_token,
            rx,
        };
        internal.update(UpdateSource::Forced);

        IcedElement(Arc::new(Mutex::new(internal)))
    }

    pub fn with_program<R>(&self, func: impl FnOnce(&P) -> R) -> R {
        let internal = self.0.lock().unwrap();
        func(&internal.program)
    }

    pub fn minimum_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        let mut element = internal.program.view(&internal.theme);
        let tree = &mut Tree::new(element.as_widget());
        let node = element
            .as_widget_mut()
            .layout(
                tree,
                &internal.renderer,
                &Limits::new(IcedSize::ZERO, IcedSize::INFINITE)
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
        internal_ref.update(UpdateSource::Forced);
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
        self.0.lock().unwrap().update(UpdateSource::Forced);
    }

    pub fn set_theme(&self, theme: CompTheme) {
        let mut guard = self.0.lock().unwrap();
        guard.iced_theme = theme.to_iced_theme();
        guard.theme = theme;
        guard.update(UpdateSource::Forced);
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
        // In iced 0.15, we process messages immediately by calling program.update
        // and scheduling any resulting tasks. We need to trigger a UI rebuild.
        let mut internal = self.0.lock().unwrap();
        let internal = &mut *internal; // Allow split-borrowing of fields
        let task = internal.program.update(
            msg,
            &internal.handle,
            internal.last_seat.lock().unwrap().as_ref(),
        );
        internal.schedule_task(task);
    }

    /// Returns the current mouse interaction state from the last UI update.
    pub fn mouse_interaction(&self) -> MouseInteraction {
        self.0.lock().unwrap().mouse_interaction
    }
}

impl<P: Program + Send + 'static + Clone> IcedElement<P> {
    pub fn deep_clone(&self) -> Self {
        let internal = self.0.lock().unwrap();
        IcedElement(Arc::new(Mutex::new(internal.clone())))
    }
}

// --- Core update cycle (rewritten for iced 0.15) ---

impl<P: Program + Send + 'static> IcedElementInternal<P> {
    /// Schedule a Task returned by program.update() onto the calloop executor.
    fn schedule_task(&self, task: Task<P::Message>) {
        if let Some(stream) = into_stream(task) {
            let _ = self
                .scheduler
                .schedule(stream.into_future().map(|f| match f.0 {
                    Some(Action::Output(msg)) => Some(msg),
                    _ => None,
                }));
        }
    }

    #[profiling::function]
    fn update(&mut self, source: UpdateSource) {
        let update_start = Instant::now();
        let force = matches!(source, UpdateSource::Forced);

        // Drain async task results and process them through the program
        while let Ok(Some(message)) = self.rx.try_recv() {
            let task = self.program.update(
                message,
                &self.handle,
                self.last_seat.lock().unwrap().as_ref(),
            );
            self.schedule_task(task);
        }

        if self.event_queue.is_empty() && !force {
            // Record skipped update
            if iced_perf_logging_enabled()
                && let Ok(mut profiler) = ICED_PROFILER.try_lock()
            {
                profiler.record_update(UpdateRecord {
                    program_name: P::program_name(),
                    source,
                    view_duration: std::time::Duration::ZERO,
                    build_duration: std::time::Duration::ZERO,
                    ui_update_duration: std::time::Duration::ZERO,
                    draw_duration: std::time::Duration::ZERO,
                    message_loop_duration: std::time::Duration::ZERO,
                    total_duration: update_start.elapsed(),
                    had_messages: false,
                    skipped: true,
                });
                profiler.maybe_report();
            }
            return;
        }

        // When force-updating with an empty event queue, inject a synthetic event
        // so that widget update() methods are called (e.g. animated_container
        // needs update() to detect target property changes from the new view).
        if force && self.event_queue.is_empty() {
            self.event_queue
                .push(Event::Window(WindowEvent::RedrawRequested(
                    IcedInstant::now(),
                )));
        }

        let cursor = self
            .cursor_pos
            .map(|p| IcedPoint::new(p.x as f32, p.y as f32))
            .map(Cursor::Available)
            .unwrap_or(Cursor::Unavailable);

        // Phase 1: view() — build the element tree
        let view_start = Instant::now();
        let element = self.program.view(&self.theme);
        let view_duration = view_start.elapsed();

        // Phase 2: build — create UserInterface from element tree
        let build_start = Instant::now();
        let bounds = IcedSize::new(self.size.w as f32, self.size.h as f32);
        let cache = std::mem::take(&mut self.cache);
        let mut interface = UserInterface::build(element, bounds, cache, &mut self.renderer);
        let build_duration = build_start.elapsed();

        // Phase 3: ui_update — process queued events, collecting messages
        let ui_update_start = Instant::now();
        let mut messages = Vec::new();
        let (state, _statuses) =
            interface.update(&self.event_queue, cursor, &mut self.renderer, &mut messages);
        self.event_queue.clear();
        let ui_update_duration = ui_update_start.elapsed();

        // Store the mouse interaction and check for animation redraw requests.
        match state {
            iced_runtime::user_interface::State::Updated {
                mouse_interaction,
                redraw_request,
                ..
            } => {
                self.mouse_interaction = mouse_interaction;
                // If widgets requested a redraw (e.g. animations in progress),
                // flag this element so the next compositor frame drives another update.
                let wants_redraw = redraw_request != window::RedrawRequest::Wait;
                self.needs_redraw = wants_redraw;
            }
            iced_runtime::user_interface::State::Outdated { .. } => {
                self.needs_redraw = true;
            }
        }

        // Phase 4: draw — populate renderer layers for rasterization
        let draw_start = Instant::now();
        let style = RendererStyle {
            text_color: self.theme.on_bg_color(),
        };
        interface.draw(&mut self.renderer, &self.iced_theme, &style, cursor);
        let draw_duration = draw_start.elapsed();

        // Preserve widget tree state for next frame
        self.cache = interface.into_cache();

        // Phase 5: message loop — process widget messages and rebuild if needed
        let msg_start = Instant::now();
        let had_messages = !messages.is_empty();
        if had_messages {
            for msg in messages {
                let task =
                    self.program
                        .update(msg, &self.handle, self.last_seat.lock().unwrap().as_ref());
                self.schedule_task(task);
            }
            // State changed from widget messages — rebuild the view and re-draw
            // so the UI reflects the new state immediately (e.g. trigger highlight
            // on dropdown open) without waiting for the next input event.
            let element = self.program.view(&self.theme);
            let cache = std::mem::take(&mut self.cache);
            let mut interface = UserInterface::build(element, bounds, cache, &mut self.renderer);
            interface.draw(&mut self.renderer, &self.iced_theme, &style, cursor);
            self.cache = interface.into_cache();
        }
        let message_loop_duration = msg_start.elapsed();

        let total_duration = update_start.elapsed();

        // Record to profiler
        if iced_perf_logging_enabled()
            && let Ok(mut profiler) = ICED_PROFILER.try_lock()
        {
            profiler.record_update(UpdateRecord {
                program_name: P::program_name(),
                source,
                view_duration,
                build_duration,
                ui_update_duration,
                draw_duration,
                message_loop_duration,
                total_duration,
                had_messages,
                skipped: false,
            });
            profiler.maybe_report();
        }
    }
}

// --- Input handling (unchanged interface, updated internals) ---

impl<P: Program + Send + 'static> PointerTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorEntered));
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
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
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event_location);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
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
        internal.event_queue.push(Event::Mouse(match event.state {
            ButtonState::Pressed => MouseEvent::ButtonPressed(button),
            ButtonState::Released => MouseEvent::ButtonReleased(button),
        }));
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn axis(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: AxisFrame,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::WheelScrolled {
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
        internal.update(UpdateSource::Input);
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
        internal.cursor_pos = None;
        internal
            .event_queue
            .push(Event::Mouse(MouseEvent::CursorLeft));
        internal.update(UpdateSource::Input);
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
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        internal
            .event_queue
            .push(Event::Touch(TouchEvent::FingerPressed { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        internal.last_touch_serial = Some(event.serial);
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
        internal.update(UpdateSource::Input);
    }

    fn up(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &UpEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        if let Some(position) = internal.touch_map.remove(&id) {
            *internal.last_seat.lock().unwrap() =
                Some((seat.clone(), internal.last_touch_serial.unwrap()));
            internal
                .event_queue
                .push(Event::Touch(TouchEvent::FingerLifted { id, position }));
            internal.update(UpdateSource::Input);
        }
    }

    fn motion(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &TouchMotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let id = Finger(i32::from(event.slot) as u64);
        let event_location = event.location.downscale(internal.additional_scale);
        let position = IcedPoint::new(event_location.x as f32, event_location.y as f32);
        *internal.last_seat.lock().unwrap() =
            Some((seat.clone(), internal.last_touch_serial.unwrap()));
        internal
            .event_queue
            .push(Event::Touch(TouchEvent::FingerMoved { id, position }));
        internal.touch_map.insert(id, position);
        internal.cursor_pos = Some(event_location);
        internal.update(UpdateSource::Input);
    }

    fn frame(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: FrameMarker,
    ) {
        self.0.lock().unwrap().last_touch_frame = Some(frame);
    }

    fn cancel(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        frame: FrameMarker,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal.last_touch_frame = Some(frame);
        for (id, position) in std::mem::take(&mut internal.touch_map) {
            internal
                .event_queue
                .push(Event::Touch(TouchEvent::FingerLost { id, position }));
        }
        internal.update(UpdateSource::Input);
    }

    fn shape(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &ShapeEvent,
    ) {
    }

    fn orientation(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _event: &OrientationEvent,
    ) {
    }

    fn last_frame(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
    ) -> Option<FrameMarker> {
        self.0.lock().unwrap().last_touch_frame
    }
}

impl<P: Program + Send + 'static> KeyboardTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        keys: Vec<KeysymHandle<'_>>,
        _serial: Serial,
    ) {
        let mut internal = self.0.lock().unwrap();
        for key in &keys {
            let keysym = key.modified_sym();
            let iced_key = iced_keymap::keysym_to_iced_key(keysym);
            let modified_key = iced_key.clone();
            let physical_key = iced_keymap::keycode_to_physical(key.raw_code());
            let location = iced_keymap::keysym_to_location(keysym);
            internal
                .event_queue
                .push(Event::Keyboard(KeyboardEvent::KeyPressed {
                    key: iced_key,
                    modified_key,
                    physical_key,
                    location,
                    modifiers: IcedModifiers::empty(),
                    text: keysym.key_char().filter(|c| !c.is_control()).map(|c| {
                        let mut buf = [0u8; 4];
                        iced_core::SmolStr::new(c.encode_utf8(&mut buf))
                    }),
                    repeat: false,
                }));
        }
        internal.update(UpdateSource::Input);
    }

    fn leave(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        _serial: Serial,
    ) {
        // Iced doesn't track held keys internally, so nothing to release.
    }

    fn key(
        &self,
        seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        _time: u32,
    ) {
        let mut internal = self.0.lock().unwrap();
        let keysym = key.modified_sym();
        let iced_key = iced_keymap::keysym_to_iced_key(keysym);
        let modified_key = iced_key.clone();
        let physical_key = iced_keymap::keycode_to_physical(key.raw_code());
        let location = iced_keymap::keysym_to_location(keysym);
        let modifiers = IcedModifiers::empty(); // modifiers() is called separately by smithay

        let event = match state {
            KeyState::Pressed => KeyboardEvent::KeyPressed {
                key: iced_key,
                modified_key,
                physical_key,
                location,
                modifiers,
                text: keysym.key_char().filter(|c| !c.is_control()).map(|c| {
                    let mut buf = [0u8; 4];
                    iced_core::SmolStr::new(c.encode_utf8(&mut buf))
                }),
                repeat: false,
            },
            KeyState::Released => KeyboardEvent::KeyReleased {
                key: iced_key,
                modified_key,
                physical_key,
                location,
                modifiers,
            },
        };
        internal.event_queue.push(Event::Keyboard(event));
        *internal.last_seat.lock().unwrap() = Some((seat.clone(), serial));
        internal.update(UpdateSource::Input);
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
            .event_queue
            .push(Event::Keyboard(KeyboardEvent::ModifiersChanged(mods)));
        internal.update(UpdateSource::Input);
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
        internal.event_queue.push(Event::Window(if activated {
            WindowEvent::Focused
        } else {
            WindowEvent::Unfocused
        }));
        internal.update(UpdateSource::Input);
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
        RenderZindex::Shell as u8
    }

    #[profiling::function]
    fn refresh(&self) {
        let mut internal = self.0.lock().unwrap();
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
        internal.update(UpdateSource::Refresh);
    }
}

// --- Render elements (rasterization via iced_tiny_skia) ---

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
        let internal_ref = &mut *internal;

        // Drive animation frames: if a previous update requested a redraw,
        // inject a RedrawRequested event so animation widgets can advance.
        let element_id = Arc::as_ptr(&self.0) as usize;
        if internal_ref.needs_redraw {
            internal_ref.needs_redraw = false;
            internal_ref
                .event_queue
                .push(Event::Window(WindowEvent::RedrawRequested(
                    IcedInstant::now(),
                )));
            internal_ref.update(UpdateSource::AnimRedraw);

            // Track animation burst
            if iced_perf_logging_enabled() {
                if let Ok(mut profiler) = ICED_PROFILER.try_lock() {
                    profiler.animation_burst_frame(element_id);
                }

                // Check if animation ended (no further redraw requested)
                if !internal_ref.needs_redraw
                    && let Ok(mut profiler) = ICED_PROFILER.try_lock()
                {
                    profiler.animation_burst_end(element_id);
                }
            }
        } else {
            // No animation active — end any tracked burst
            if iced_perf_logging_enabled()
                && let Ok(mut profiler) = ICED_PROFILER.try_lock()
            {
                profiler.animation_burst_end(element_id);
            }
        }

        // Track animation burst starts (needs_redraw was set by update above)
        if iced_perf_logging_enabled()
            && internal_ref.needs_redraw
            && let Ok(mut profiler) = ICED_PROFILER.try_lock()
        {
            profiler.animation_burst_start(element_id);
        }
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
                let mut clip_mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();
                let theme = &internal_ref.theme;

                _ = buffer.render().draw(|buf| {
                    let mut pixels =
                        tiny_skia::PixmapMut::from_bytes(buf, size.w as u32, size.h as u32)
                            .expect("Failed to create pixel map");

                    let background_color = internal_ref.program.background_color(theme);
                    let bounds = IcedSize::new(size.w as u32, size.h as u32);
                    let viewport = Viewport::with_physical_size(bounds, scale.x as f32);
                    let scale_x = scale.x as f32;

                    // Get current layers from the renderer (populated by last draw() call)
                    let current_layers = internal_ref.renderer.layers();

                    let mut damage_rects: Vec<_> = old_layers
                        .as_ref()
                        .and_then(|(last_primitives, last_color)| {
                            (last_color == &background_color).then(|| {
                                damage::diff(
                                    last_primitives,
                                    current_layers,
                                    |layer| vec![layer.bounds],
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
                            vec![iced_core::Rectangle::with_size(viewport.logical_size())]
                        });

                    damage_rects = damage::group(
                        damage_rects,
                        iced_core::Rectangle::with_size(viewport.logical_size()),
                    );

                    if !damage_rects.is_empty() {
                        *old_layers = Some((current_layers.to_vec(), background_color));

                        // iced 0.15: no overlay parameter
                        internal_ref.renderer.draw(
                            &mut pixels,
                            &mut clip_mask,
                            &viewport,
                            &damage_rects,
                            background_color,
                        );
                    }

                    let damage_output = damage_rects
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

                    internal_ref.program.foreground(
                        &mut pixels,
                        &damage_output,
                        scale.x as f32,
                        theme,
                    );

                    Result::<_, ()>::Ok(damage_output)
                });

                // Trim the cosmic-text shape cache
                {
                    let mut font_system = font_system().write().unwrap();
                    let _ = font_system.raw(); // cache trimming not available in this cosmic-text version
                }
            }

            match MemoryRenderBufferRenderElement::from_buffer(
                renderer,
                location.to_f64(),
                buffer,
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
