use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    sync::{mpsc::Receiver, Arc, Mutex},
    time::{Duration, Instant},
};

use cosmic::{
    iced::{
        advanced::widget::Tree,
        event::Event,
        keyboard::{Event as KeyboardEvent, Modifiers as IcedModifiers},
        mouse::{Button as MouseButton, Cursor, Event as MouseEvent, ScrollDelta},
        window::{Event as WindowEvent, Id},
        Command, Limits, Point as IcedPoint, Rectangle as IcedRectangle, Size as IcedSize,
    },
    iced_core::{clipboard::Null as NullClipboard, renderer::Style, Color, Font, Length, Pixels},
    iced_renderer::{graphics::Renderer as IcedGraphicsRenderer, Renderer as IcedRenderer},
    iced_runtime::{
        command::Action,
        program::{Program as IcedProgram, State},
        Debug,
    },
    Theme,
};
use iced_tiny_skia::{
    graphics::{damage, Viewport},
    Backend, Primitive,
};
pub type Element<'a, Message> = cosmic::iced::Element<'a, Message, IcedRenderer<Theme>>;

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

pub struct IcedElement<P: Program + Send + 'static>(Arc<Mutex<IcedElementInternal<P>>>);

impl<P: Program + Send + 'static> fmt::Debug for IcedElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

// SAFETY: We cannot really be sure about `iced_native::program::State` sadly,
// but the rest should be fine.
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
    ) -> Command<Self::Message> {
        let _ = (message, loop_handle);
        Command::none()
    }
    fn view(&self) -> Element<'_, Self::Message>;

    fn background_color(&self) -> Color {
        Color::TRANSPARENT
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, BufferCoords>],
        scale: f32,
    ) {
        let _ = (pixels, damage, scale);
    }
}

struct ProgramWrapper<P: Program>(P, LoopHandle<'static, crate::state::State>);
impl<P: Program> IcedProgram for ProgramWrapper<P> {
    type Message = <P as Program>::Message;
    type Renderer = IcedRenderer<Theme>;

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        self.0.update(message, &self.1)
    }

    fn view(&self) -> Element<'_, Self::Message> {
        self.0.view()
    }
}

struct IcedElementInternal<P: Program + Send + 'static> {
    // draw buffer
    outputs: HashSet<Output>,
    buffers: HashMap<OrderedFloat<f64>, (MemoryRenderBuffer, Option<(Vec<Primitive>, Color)>)>,
    pending_update: Option<Instant>,

    // state
    size: Size<i32, Logical>,
    cursor_pos: Option<Point<f64, Logical>>,

    // iced
    theme: Theme,
    renderer: IcedRenderer<Theme>,
    state: State<ProgramWrapper<P>>,
    debug: Debug,

    // futures
    handle: LoopHandle<'static, crate::state::State>,
    scheduler: Scheduler<<P as Program>::Message>,
    executor_token: Option<RegistrationToken>,
    rx: Receiver<<P as Program>::Message>,
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
        let mut renderer = IcedRenderer::TinySkia(IcedGraphicsRenderer::new(
            Backend::new(),
            Font::default(),
            Pixels(16.0),
        ));
        let mut debug = Debug::new();
        let state = State::new(
            Id::MAIN,
            ProgramWrapper(self.state.program().0.clone(), handle.clone()),
            IcedSize::new(self.size.w as f32, self.size.h as f32),
            &mut renderer,
            &mut debug,
        );

        IcedElementInternal {
            outputs: self.outputs.clone(),
            buffers: self.buffers.clone(),
            pending_update: self.pending_update.clone(),
            size: self.size.clone(),
            cursor_pos: self.cursor_pos.clone(),
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
            .field("buffers", &"...")
            .field("size", &self.size)
            .field("pending_update", &self.pending_update)
            .field("cursor_pos", &self.cursor_pos)
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
        let mut renderer = IcedRenderer::TinySkia(IcedGraphicsRenderer::new(
            Backend::new(),
            Font::default(),
            Pixels(16.0),
        ));
        let mut debug = Debug::new();

        let state = State::new(
            Id::MAIN,
            ProgramWrapper(program, handle.clone()),
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
            outputs: HashSet::new(),
            buffers: HashMap::new(),
            pending_update: None,
            size,
            cursor_pos: None,
            theme,
            renderer,
            state,
            debug,
            handle,
            scheduler,
            executor_token,
            rx,
        };
        let _ = internal.update(true);

        IcedElement(Arc::new(Mutex::new(internal)))
    }

    pub fn with_program<R>(&self, func: impl FnOnce(&P) -> R) -> R {
        let internal = self.0.lock().unwrap();
        func(&internal.state.program().0)
    }

    pub fn minimum_size(&self) -> Size<i32, Logical> {
        let internal = self.0.lock().unwrap();
        let element = internal.state.program().0.view();
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
        for (scale, (buffer, old_primitives)) in internal_ref.buffers.iter_mut() {
            let buffer_size = internal_ref
                .size
                .to_f64()
                .to_buffer(**scale, Transform::Normal)
                .to_i32_round();
            *buffer =
                MemoryRenderBuffer::new(Fourcc::Argb8888, buffer_size, 1, Transform::Normal, None);
            *old_primitives = None;
        }

        if internal_ref.pending_update.is_none() {
            internal_ref.pending_update = Some(Instant::now());
        }
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
        for (_buffer, ref mut old_primitives) in internal.buffers.values_mut() {
            *old_primitives = None;
        }
        internal.update(true);
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
    fn update(&mut self, mut force: bool) -> Vec<Action<<P as Program>::Message>> {
        while let Ok(message) = self.rx.try_recv() {
            self.state.queue_message(message);
            force = true;
        }

        if !force {
            return Vec::new();
        }

        let cursor = self
            .cursor_pos
            .map(|p| IcedPoint::new(p.x as f32, p.y as f32))
            .map(Cursor::Available)
            .unwrap_or(Cursor::Unavailable);

        let actions = self
            .state
            .update(
                Id::MAIN,
                IcedSize::new(self.size.w as f32, self.size.h as f32),
                cursor,
                &mut self.renderer,
                &self.theme,
                &Style {
                    scale_factor: 1.0, //TODO: why is this
                    icon_color: self.theme.cosmic().on_bg_color().into(),
                    text_color: self.theme.cosmic().on_bg_color().into(),
                },
                &mut NullClipboard,
                &mut self.debug,
            )
            .1;

        actions
            .into_iter()
            .filter_map(|action| {
                if let Action::Future(future) = action {
                    let _ = self.scheduler.schedule(future);
                    None
                } else {
                    Some(action)
                }
            })
            .collect::<Vec<_>>()
    }
}

impl<P: Program + Send + 'static> PointerTarget<crate::state::State> for IcedElement<P> {
    fn enter(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorEntered));
        let position = IcedPoint::new(event.location.x as f32, event.location.y as f32);
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event.location);
        let _ = internal.update(true);
    }

    fn motion(
        &self,
        _seat: &Seat<crate::state::State>,
        _data: &mut crate::state::State,
        event: &MotionEvent,
    ) {
        let mut internal = self.0.lock().unwrap();
        let position = IcedPoint::new(event.location.x as f32, event.location.y as f32);
        internal
            .state
            .queue_event(Event::Mouse(MouseEvent::CursorMoved { position }));
        internal.cursor_pos = Some(event.location);
        let _ = internal.update(true);
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
        _seat: &Seat<crate::state::State>,
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
        let _ = internal.update(true);
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
        let _ = internal.update(true);
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
        let _ = internal.update(true);
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
        let _ = internal.update(true);
    }
}

impl<P: Program + Send + 'static> IsAlive for IcedElement<P> {
    fn alive(&self) -> bool {
        true
    }
}

impl<P: Program + Send + 'static> SpaceElement for IcedElement<P> {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        Rectangle::from_loc_and_size((0, 0), self.0.lock().unwrap().size)
    }

    fn is_in_input_region(&self, _point: &Point<f64, Logical>) -> bool {
        true
    }

    fn set_activate(&self, activated: bool) {
        let mut internal = self.0.lock().unwrap();
        internal.state.queue_event(Event::Window(
            Id::MAIN,
            if activated {
                WindowEvent::Focused
            } else {
                WindowEvent::Unfocused
            },
        ));
        let _ = internal.update(true); // TODO
    }

    fn output_enter(&self, output: &Output, _overlap: Rectangle<i32, Logical>) {
        let mut internal = self.0.lock().unwrap();
        let scale = output.current_scale().fractional_scale();
        if !internal.buffers.contains_key(&OrderedFloat(scale)) {
            let buffer_size = internal
                .size
                .to_f64()
                .to_buffer(scale, Transform::Normal)
                .to_i32_round();
            internal.buffers.insert(
                OrderedFloat(scale),
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
        internal.outputs.insert(output.clone());
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
            internal_ref
                .outputs
                .iter()
                .any(|o| o.current_scale().fractional_scale() == **scale)
        });
        for scale in internal_ref
            .outputs
            .iter()
            .map(|o| OrderedFloat(o.current_scale().fractional_scale()))
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
        internal.update(true);
    }
}

impl<P, R> AsRenderElements<R> for IcedElement<P>
where
    P: Program + Send + 'static,
    R: Renderer + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = MemoryRenderBufferRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C> {
        let mut internal = self.0.lock().unwrap();
        // makes partial borrows easier
        let internal_ref = &mut *internal;
        let force = if matches!(
            internal_ref.pending_update,
            Some(instant) if Instant::now().duration_since(instant) > Duration::from_millis(25)
        ) {
            true
        } else {
            false
        };
        let _ = internal_ref.update(force);

        if let Some((buffer, ref mut old_primitives)) =
            internal_ref.buffers.get_mut(&OrderedFloat(scale.x))
        {
            let size: Size<i32, BufferCoords> = internal_ref
                .size
                .to_f64()
                .to_buffer(scale.x, Transform::Normal)
                .to_i32_round();

            if size.w > 0 && size.h > 0 {
                let IcedRenderer::TinySkia(renderer) = &mut internal_ref.renderer;
                let state_ref = &internal_ref.state;
                let mut clip_mask = tiny_skia::Mask::new(size.w as u32, size.h as u32).unwrap();
                let overlay = internal_ref.debug.overlay();

                buffer
                    .render()
                    .draw(move |buf| {
                        let mut pixels =
                            tiny_skia::PixmapMut::from_bytes(buf, size.w as u32, size.h as u32)
                                .expect("Failed to create pixel map");

                        renderer.with_primitives(|backend, primitives| {
                            let background_color = state_ref.program().0.background_color();
                            let bounds = IcedSize::new(size.w as u32, size.h as u32);
                            let viewport = Viewport::with_physical_size(bounds, scale.x);

                            let mut damage = old_primitives
                                .as_ref()
                                .and_then(|(last_primitives, last_color)| {
                                    (last_color == &background_color)
                                        .then(|| damage::list(last_primitives, primitives))
                                })
                                .unwrap_or_else(|| {
                                    vec![IcedRectangle::with_size(viewport.logical_size())]
                                });
                            damage = damage::group(damage, scale.x as f32, bounds);

                            if !damage.is_empty() {
                                backend.draw(
                                    &mut pixels,
                                    &mut clip_mask,
                                    primitives,
                                    &viewport,
                                    &damage,
                                    background_color,
                                    &overlay,
                                );

                                *old_primitives = Some((primitives.to_vec(), background_color));
                            }

                            let damage = damage
                                .into_iter()
                                .map(|x| x.snap())
                                .map(|damage_rect| {
                                    Rectangle::from_loc_and_size(
                                        (damage_rect.x as i32, damage_rect.y as i32),
                                        (damage_rect.width as i32, damage_rect.height as i32),
                                    )
                                })
                                .collect::<Vec<_>>();

                            state_ref
                                .program()
                                .0
                                .foreground(&mut pixels, &damage, scale.x as f32);

                            Result::<_, ()>::Ok(damage)
                        })
                    })
                    .unwrap();
            }

            if let Ok(buffer) = MemoryRenderBufferRenderElement::from_buffer(
                renderer,
                location.to_f64(),
                &buffer,
                Some(alpha),
                Some(Rectangle::from_loc_and_size(
                    (0., 0.),
                    size.to_f64().to_logical(1.0, Transform::Normal),
                )),
                Some(internal_ref.size),
                Kind::Unspecified,
            ) {
                return vec![C::from(buffer)];
            }
        }
        Vec::new()
    }
}
