use std::{
    fmt,
    sync::{mpsc::Receiver, Arc, Mutex},
};

use cosmic::iced_native::{
    command::Action,
    event::Event,
    keyboard::{Event as KeyboardEvent, Modifiers as IcedModifiers},
    mouse::{Button as MouseButton, Event as MouseEvent, ScrollDelta},
    program::{Program, State},
    renderer::Style,
    window::{Event as WindowEvent, Id},
    Debug, Point as IcedPoint, Size as IcedSize,
};
pub use cosmic::Renderer as IcedRenderer;
use cosmic::Theme;
use iced_swbuf::{native::*, Backend};

use smithay::{
    backend::{
        input::{ButtonState, KeyState},
        renderer::{
            element::{
                memory::{MemoryRenderBuffer, MemoryRenderBufferRenderElement},
                AsRenderElements,
            },
            ImportMem, Renderer,
        },
    },
    desktop::space::{RenderZindex, SpaceElement},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent, PointerTarget},
        Seat,
    },
    output::Output,
    reexports::calloop::RegistrationToken,
    reexports::calloop::{self, futures::Scheduler, LoopHandle},
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size, Transform},
};

#[derive(Debug)]
pub struct IcedElement<P: Program<Renderer = IcedRenderer> + Send + 'static>(
    Arc<Mutex<IcedElementInternal<P>>>,
);

// SAFETY: We cannot really be sure about `iced_native::program::State` sadly,
// but the rest should be fine.
unsafe impl<P: Program<Renderer = IcedRenderer> + Send + 'static> Send for IcedElementInternal<P> {}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> Clone for IcedElement<P> {
    fn clone(&self) -> Self {
        IcedElement(self.0.clone())
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> PartialEq for IcedElement<P> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

struct IcedElementInternal<P: Program<Renderer = IcedRenderer> + Send + 'static> {
    // draw buffer
    memory: MemoryRenderBuffer,
    needs_redraw: bool,

    // state
    size: Size<i32, Logical>,
    cursor_pos: Option<Point<f64, Logical>>,

    // iced
    theme: Theme,
    renderer: IcedRenderer,
    state: State<P>,
    debug: Debug,

    // futures
    handle: LoopHandle<'static, crate::state::Data>,
    scheduler: Scheduler<<P as Program>::Message>,
    executor_token: Option<RegistrationToken>,
    rx: Receiver<<P as Program>::Message>,
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> fmt::Debug for IcedElementInternal<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IcedElementInternal")
            .field("memory", &self.memory)
            .field("needs_redraw", &self.needs_redraw)
            .field("size", &self.size)
            .field("cursor_pos", &self.cursor_pos)
            .field("theme", &self.theme)
            .finish_non_exhaustive()
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> Drop for IcedElementInternal<P> {
    fn drop(&mut self) {
        self.handle.remove(self.executor_token.take().unwrap());
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> IcedElement<P> {
    pub fn new(
        program: P,
        size: impl Into<Size<i32, Logical>>,
        scale: i32,
        handle: LoopHandle<'static, crate::state::Data>,
    ) -> calloop::error::Result<IcedElement<P>> {
        let size = size.into();
        let buffer_size = size.to_buffer(scale, Transform::Normal);

        let mut renderer = IcedRenderer::new(Backend::new());
        let mut debug = Debug::new();

        let state = State::new(
            program,
            IcedSize::new(size.w as f32, size.h as f32),
            &mut renderer,
            &mut debug,
        );

        let (executor, scheduler) = calloop::futures::executor()?;
        let (tx, rx) = std::sync::mpsc::channel();
        let executor_token = handle.insert_source(executor, |message, _, _| {
            tx.send(message);
        })?;

        let mut internal = IcedElementInternal {
            memory: MemoryRenderBuffer::new(buffer_size, scale, Transform::Normal, None),
            needs_redraw: true,
            size,
            cursor_pos: None,
            theme: Theme::Dark, // TODO
            renderer,
            state,
            debug,
            handle,
            scheduler,
            executor_token: Some(executor_token),
            rx,
        };
        let _ = internal.update(true);

        Ok(IcedElement(Arc::new(Mutex::new(internal))))
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> IcedElementInternal<P> {
    fn update(&mut self, mut force: bool) -> Vec<Action<<P as Program>::Message>> {
        let cursor_pos = self.cursor_pos.unwrap_or(Point::from((-1.0, -1.0)));

        while let Ok(message) = self.rx.try_recv() {
            self.state.queue_message(message);
            force = true;
        }

        if !force {
            return Vec::new();
        }

        let actions = self
            .state
            .update(
                IcedSize::new(self.size.w as f32, self.size.h as f32),
                IcedPoint::new(cursor_pos.x as f32, cursor_pos.y as f32),
                &mut self.renderer,
                &self.theme,
                &Style {
                    text_color: self.theme.palette().text,
                },
                &mut cosmic::iced_native::clipboard::Null,
                &mut self.debug,
            )
            .1
            .map(|command| command.actions());

        if actions.is_some() {
            self.needs_redraw = true;
        }
        let actions = actions.unwrap_or_default();
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

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> PointerTarget<crate::state::State>
    for IcedElement<P>
{
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
        let _ = internal.update(true); // TODO
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
        let _ = internal.update(true); // TODO
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
            x => MouseButton::Other(x as u8),
        };
        internal.state.queue_event(Event::Mouse(match event.state {
            ButtonState::Pressed => MouseEvent::ButtonPressed(button),
            ButtonState::Released => MouseEvent::ButtonReleased(button),
        }));
        let _ = internal.update(true); // TODO
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
                delta: if let Some(discrete) = frame.discrete {
                    ScrollDelta::Lines {
                        x: discrete.0 as f32,
                        y: discrete.1 as f32,
                    }
                } else {
                    ScrollDelta::Pixels {
                        x: frame.axis.0 as f32,
                        y: frame.axis.1 as f32,
                    }
                },
            }));
        let _ = internal.update(true); // TODO
    }

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
        let _ = internal.update(true); // TODO
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> KeyboardTarget<crate::state::State>
    for IcedElement<P>
{
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
        let _ = internal.update(true); // TODO
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> IsAlive for IcedElement<P> {
    fn alive(&self) -> bool {
        true
    }
}

impl<P: Program<Renderer = IcedRenderer> + Send + 'static> SpaceElement for IcedElement<P> {
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

    fn output_enter(&self, _output: &Output, _overlap: Rectangle<i32, Logical>) {
        // TODO: Update scale, once supported to the highest one
    }

    fn output_leave(&self, _output: &Output) {
        // TODO: Update scale, once supported to the highest one
    }

    fn z_index(&self) -> u8 {
        // meh, user-provided?
        RenderZindex::Shell as u8
    }

    fn refresh(&self) {}
}

impl<P, R> AsRenderElements<R> for IcedElement<P>
where
    P: Program<Renderer = IcedRenderer> + Send + 'static,
    R: Renderer + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = MemoryRenderBufferRenderElement<R>;

    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        _scale: Scale<f64>,
    ) -> Vec<C> {
        let mut internal = self.0.lock().unwrap();

        let _ = internal.update(false); // TODO

        // makes partial borrows easier
        let internal_ref = &mut *internal;
        if internal_ref.needs_redraw {
            let renderer = &mut internal_ref.renderer;
            let size = internal_ref.size;
            internal_ref
                .memory
                .render()
                .draw(move |buf| {
                    let mut target = raqote::DrawTarget::from_backing(
                        size.w,
                        size.h,
                        bytemuck::cast_slice_mut::<_, u32>(buf),
                    );

                    target.clear(raqote::SolidSource::from_unpremultiplied_argb(0, 0, 0, 0));

                    let draw_options = raqote::DrawOptions {
                        // Default to antialiasing off for now
                        antialias: raqote::AntialiasMode::None,
                        ..Default::default()
                    };

                    // Having at least one clip fixes some font rendering issues
                    target.push_clip_rect(raqote::IntRect::new(
                        raqote::IntPoint::new(0, 0),
                        raqote::IntPoint::new(size.w, size.h),
                    ));

                    renderer.with_primitives(|backend, primitives| {
                        for primitive in primitives.iter() {
                            draw_primitive(&mut target, &draw_options, backend, primitive);
                        }
                    });

                    target.pop_clip();

                    Result::<_, ()>::Ok(vec![Rectangle::from_loc_and_size(
                        (0, 0),
                        size.to_buffer(1, Transform::Normal),
                    )])
                })
                .unwrap();
            internal_ref.needs_redraw = false;
        }

        let Ok(buffer) = MemoryRenderBufferRenderElement::from_buffer(
            renderer,
            location.to_f64(),
            &internal.memory,
            None,
            None,
            None,
            slog_scope::logger(),
        ) else {
            return Vec::new()
        };
        vec![C::from(buffer)]
    }
}
