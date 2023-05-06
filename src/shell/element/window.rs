use crate::{
    backend::render::{
        element::{AsGlowFrame, AsGlowRenderer},
        GlMultiFrame, GlMultiRenderer,
    },
    shell::Shell,
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::SeatExt,
    },
    wayland::handlers::screencopy::ScreencopySessions,
};
use calloop::LoopHandle;
use cosmic::iced_native::Command;
use cosmic_protocols::screencopy::v1::server::zcosmic_screencopy_session_v1::InputType;
use iced_softbuffer::native::raqote::{DrawOptions, DrawTarget, PathBuilder, SolidSource, Source};
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            element::{
                memory::MemoryRenderBufferRenderElement, surface::WaylandSurfaceRenderElement,
                AsRenderElements, Element, Id, RenderElement,
            },
            glow::GlowRenderer,
            multigpu::Error as MultiError,
            utils::CommitCounter,
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
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size, Transform},
    wayland::seat::WaylandFocus,
};
use std::{
    fmt,
    hash::Hash,
    sync::{
        atomic::{AtomicU8, Ordering},
        Arc, Mutex,
    },
};

use super::{surface::SSD_HEIGHT, CosmicSurface};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicWindow(IcedElement<CosmicWindowInternal>);

impl fmt::Debug for CosmicWindow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.with_program(|window| {
            f.debug_struct("CosmicWindow")
                .field("internal", window)
                .finish_non_exhaustive()
        })
    }
}

#[derive(Clone)]
pub struct CosmicWindowInternal {
    pub(super) window: CosmicSurface,
    /// TODO: This needs to be per seat
    pointer_entered: Arc<AtomicU8>,
    last_seat: Arc<Mutex<Option<(Seat<State>, Serial)>>>,
    last_title: Arc<Mutex<String>>,
}

impl fmt::Debug for CosmicWindowInternal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicWindowInternal")
            .field("window", &self.window)
            .field("pointer_entered", &self.pointer_entered)
            // skip seat to avoid loop
            .field("last_seat", &"...")
            .finish()
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Focus {
    None,
    Header,
    Window,
}

impl CosmicWindowInternal {
    pub fn swap_focus(&self, focus: Focus) -> Focus {
        unsafe {
            std::mem::transmute::<u8, Focus>(
                self.pointer_entered.swap(focus as u8, Ordering::SeqCst),
            )
        }
    }

    pub fn current_focus(&self) -> Focus {
        unsafe { std::mem::transmute::<u8, Focus>(self.pointer_entered.load(Ordering::SeqCst)) }
    }

    pub fn has_ssd(&self) -> bool {
        !self.window.is_decorated()
    }
}

impl CosmicWindow {
    pub fn new(
        window: impl Into<CosmicSurface>,
        handle: LoopHandle<'static, crate::state::Data>,
    ) -> CosmicWindow {
        let window = window.into();
        let width = window.geometry().size.w;
        let last_title = window.title();
        CosmicWindow(IcedElement::new(
            CosmicWindowInternal {
                window,
                pointer_entered: Arc::new(AtomicU8::new(Focus::None as u8)),
                last_seat: Arc::new(Mutex::new(None)),
                last_title: Arc::new(Mutex::new(last_title)),
            },
            (width, SSD_HEIGHT),
            handle,
        ))
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Logical>) {
        self.0.with_program(|p| {
            let loc = (
                geo.loc.x,
                geo.loc.y + if p.has_ssd() { SSD_HEIGHT } else { 0 },
            );
            let size = (
                geo.size.w,
                std::cmp::max(geo.size.h - if p.has_ssd() { SSD_HEIGHT } else { 0 }, 0),
            );
            p.window
                .set_geometry(Rectangle::from_loc_and_size(loc, size));
        });
        self.0.resize(Size::from((geo.size.w, SSD_HEIGHT)));
    }

    pub fn surface(&self) -> CosmicSurface {
        self.0.with_program(|p| p.window.clone())
    }

    pub fn contains_surface(&self, window: &CosmicSurface) -> bool {
        self.0.with_program(|p| &p.window == window)
    }

    pub fn offset(&self) -> Point<i32, Logical> {
        let has_ssd = self.0.with_program(|p| p.has_ssd());
        if has_ssd {
            Point::from((0, SSD_HEIGHT))
        } else {
            Point::from((0, 0))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    DragStart,
    Maximize,
    Close,
}

impl Program for CosmicWindowInternal {
    type Message = Message;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::Data>,
    ) -> Command<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    if let Some(surface) = self.window.wl_surface() {
                        loop_handle.insert_idle(move |data| {
                            Shell::move_request(&mut data.state, &surface, &seat, serial);
                        });
                    }
                }
            }
            Message::Maximize => {
                if let Some((seat, _serial)) = self.last_seat.lock().unwrap().clone() {
                    if let Some(surface) = self.window.wl_surface() {
                        loop_handle.insert_idle(move |data| {
                            if let Some(mapped) = data
                                .state
                                .common
                                .shell
                                .element_for_wl_surface(&surface)
                                .cloned()
                            {
                                if let Some(workspace) =
                                    data.state.common.shell.space_for_mut(&mapped)
                                {
                                    let output = seat.active_output();
                                    let (window, _) = mapped
                                        .windows()
                                        .find(|(w, _)| w.wl_surface().as_ref() == Some(&surface))
                                        .unwrap();
                                    workspace.maximize_request(&window, &output)
                                }
                            }
                        });
                    }
                }
            }
            Message::Close => self.window.close(),
        }
        Command::none()
    }

    fn background(&self, target: &mut DrawTarget<&mut [u32]>) {
        let radius = 8.;
        let (w, h) = (target.width() as f32, target.height() as f32);

        if !(self.window.is_maximized() || self.window.is_fullscreen()) {
            let mut pb = PathBuilder::new();
            pb.move_to(0., h); // lower-left

            // upper-left rounded corner
            pb.line_to(0., radius);
            pb.quad_to(0., 0., radius, 0.);

            // upper-right rounded corner
            pb.line_to(w - radius, 0.);
            pb.quad_to(w, 0., w, radius);

            pb.line_to(w, h); // lower-right

            let path = pb.finish();
            target.push_clip(&path);
        }

        if self.window.is_activated() {
            target.clear(SolidSource::from_unpremultiplied_argb(u8::MAX, 30, 30, 30));
        } else {
            target.clear(SolidSource::from_unpremultiplied_argb(u8::MAX, 39, 39, 39));
        }

        target.pop_clip();
    }

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        cosmic::widget::header_bar()
            .title(self.last_title.lock().unwrap().clone())
            .on_drag(Message::DragStart)
            .on_maximize(Message::Maximize)
            .on_close(Message::Close)
            .into_element()
    }

    fn foreground(&self, target: &mut DrawTarget<&mut [u32]>) {
        if !self.window.is_activated() {
            let radius = 8.;
            let (w, h) = (target.width() as f32, target.height() as f32);

            if !(self.window.is_maximized() || self.window.is_fullscreen()) {
                let mut pb = PathBuilder::new();
                pb.move_to(0., h); // lower-left

                // upper-left rounded corner
                pb.line_to(0., radius);
                pb.quad_to(0., 0., radius, 0.);

                // upper-right rounded corner
                pb.line_to(w - radius, 0.);
                pb.quad_to(w, 0., w, radius);

                pb.line_to(w, h); // lower-right

                let path = pb.finish();
                target.push_clip(&path);
            }

            let mut options = DrawOptions::new();
            options.alpha = 0.4;
            target.fill_rect(
                0.,
                0.,
                w,
                h,
                &Source::Solid(SolidSource::from_unpremultiplied_argb(u8::MAX, 0, 0, 0)),
                &options,
            );
        }
    }
}

impl IsAlive for CosmicWindow {
    fn alive(&self) -> bool {
        self.0.with_program(|p| p.window.alive())
    }
}

impl SpaceElement for CosmicWindow {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut bbox = SpaceElement::bbox(&p.window);
            if p.has_ssd() {
                bbox.size.h += SSD_HEIGHT;
            }
            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        let mut point = *point;
        if !self.bbox().contains(point.to_i32_round()) {
            return false;
        }
        self.0.with_program(|p| {
            if p.has_ssd() {
                if point.y < SSD_HEIGHT as f64 {
                    return true;
                } else {
                    point.y -= SSD_HEIGHT as f64;
                }
            }
            SpaceElement::is_in_input_region(&p.window, &point)
        })
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.0, activated);
        self.0
            .with_program(|p| SpaceElement::set_activate(&p.window, activated));
    }
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.0, output, overlap);
        self.0
            .with_program(|p| SpaceElement::output_enter(&p.window, output, overlap));
    }
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
        self.0
            .with_program(|p| SpaceElement::output_leave(&p.window, output));
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut geo = SpaceElement::geometry(&p.window);
            if p.has_ssd() {
                geo.size.h += SSD_HEIGHT;
            }
            geo
        })
    }
    fn z_index(&self) -> u8 {
        self.0.with_program(|p| SpaceElement::z_index(&p.window))
    }
    fn refresh(&self) {
        SpaceElement::refresh(&self.0);
        if self.0.with_program(|p| {
            SpaceElement::refresh(&p.window);
            let title = p.window.title();
            let mut last_title = p.last_title.lock().unwrap();
            if *last_title != title {
                *last_title = title;
                true
            } else {
                false
            }
        }) {
            self.0.force_update();
        }
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
        self.0
            .with_program(|p| KeyboardTarget::enter(&p.window, seat, data, keys, serial))
    }
    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial) {
        self.0
            .with_program(|p| KeyboardTarget::leave(&p.window, seat, data, serial))
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
        self.0
            .with_program(|p| KeyboardTarget::key(&p.window, seat, data, key, state, serial, time))
    }
    fn modifiers(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        self.0
            .with_program(|p| KeyboardTarget::modifiers(&p.window, seat, data, modifiers, serial))
    }
}

impl PointerTarget<State> for CosmicWindow {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        if self.0.with_program(|p| {
            if let Some(sessions) = p.window.user_data().get::<ScreencopySessions>() {
                for session in &*sessions.0.borrow() {
                    session.cursor_enter(seat, InputType::Pointer)
                }
            }

            if p.has_ssd() {
                if event.location.y < SSD_HEIGHT as f64 {
                    let focus = p.swap_focus(Focus::Header);
                    assert_eq!(focus, Focus::None);
                    return true;
                } else {
                    let focus = p.swap_focus(Focus::Window);
                    assert_eq!(focus, Focus::None);

                    let mut event = event.clone();
                    event.location.y -= SSD_HEIGHT as f64;
                    PointerTarget::enter(&p.window, seat, data, &event)
                }
            } else {
                p.swap_focus(Focus::Window);
                PointerTarget::enter(&p.window, seat, data, event)
            }
            false
        }) {
            PointerTarget::enter(&self.0, seat, data, event)
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        if let Some((previous, next)) = self.0.with_program(|p| {
            if let Some(sessions) = p.window.user_data().get::<ScreencopySessions>() {
                for session in &*sessions.0.borrow() {
                    let buffer_loc = (event.location.x, event.location.y); // we always screencast windows at 1x1 scale
                    if let Some((geo, hotspot)) =
                        seat.cursor_geometry(buffer_loc, data.common.clock.now())
                    {
                        session.cursor_info(seat, InputType::Pointer, geo, hotspot);
                    }
                }
            }

            if p.has_ssd() {
                if event.location.y < SSD_HEIGHT as f64 {
                    let previous = p.swap_focus(Focus::Header);
                    if previous == Focus::Window {
                        PointerTarget::leave(&p.window, seat, data, event.serial, event.time);
                    }
                    Some((previous, Focus::Header))
                } else {
                    let mut event = event.clone();
                    event.location.y -= SSD_HEIGHT as f64;

                    let previous = p.swap_focus(Focus::Window);
                    if previous != Focus::Window {
                        PointerTarget::enter(&p.window, seat, data, &event);
                    } else {
                        PointerTarget::motion(&p.window, seat, data, &event);
                    }

                    Some((previous, Focus::Window))
                }
            } else {
                p.swap_focus(Focus::Window);
                PointerTarget::motion(&p.window, seat, data, event);
                None
            }
        }) {
            match (previous, next) {
                (Focus::Header, Focus::Header) => PointerTarget::motion(&self.0, seat, data, event),
                (_, Focus::Header) => PointerTarget::enter(&self.0, seat, data, event),
                (Focus::Header, _) => {
                    PointerTarget::leave(&self.0, seat, data, event.serial, event.time)
                }
                _ => {}
            };
        }
    }

    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        self.0.with_program(|p| {
            if !p.has_ssd() || p.current_focus() == Focus::Window {
                PointerTarget::relative_motion(&p.window, seat, data, event)
            }
        })
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self.0.with_program(|p| p.current_focus()) {
            Focus::Header => {
                self.0.with_program(|p| {
                    *p.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
                });
                PointerTarget::button(&self.0, seat, data, event)
            }
            Focus::Window => self
                .0
                .with_program(|p| PointerTarget::button(&p.window, seat, data, event)),
            _ => {}
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self.0.with_program(|p| p.current_focus()) {
            Focus::Header => PointerTarget::axis(&self.0, seat, data, frame),
            Focus::Window => self
                .0
                .with_program(|p| PointerTarget::axis(&p.window, seat, data, frame)),
            _ => {}
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        let previous = self.0.with_program(|p| {
            if let Some(sessions) = p.window.user_data().get::<ScreencopySessions>() {
                for session in &*sessions.0.borrow() {
                    session.cursor_leave(seat, InputType::Pointer)
                }
            }

            p.swap_focus(Focus::None)
        });
        match previous {
            Focus::Header => PointerTarget::leave(&self.0, seat, data, serial, time),
            Focus::Window => self
                .0
                .with_program(|p| PointerTarget::leave(&p.window, seat, data, serial, time)),
            _ => {}
        }
    }
}

pub enum CosmicWindowRenderElement<R>
where
    R: ImportAll + ImportMem + AsGlowRenderer + Renderer,
    <R as Renderer>::TextureId: 'static,
{
    Header(MemoryRenderBufferRenderElement<GlowRenderer>),
    Window(WaylandSurfaceRenderElement<R>),
}

impl<R> From<WaylandSurfaceRenderElement<R>> for CosmicWindowRenderElement<R>
where
    R: ImportAll + ImportMem + AsGlowRenderer + Renderer,
    <R as Renderer>::TextureId: 'static,
{
    fn from(elem: WaylandSurfaceRenderElement<R>) -> Self {
        CosmicWindowRenderElement::Window(elem)
    }
}

impl<R> From<MemoryRenderBufferRenderElement<GlowRenderer>> for CosmicWindowRenderElement<R>
where
    R: ImportAll + ImportMem + AsGlowRenderer + Renderer,
    <R as Renderer>::TextureId: 'static,
{
    fn from(elem: MemoryRenderBufferRenderElement<GlowRenderer>) -> Self {
        CosmicWindowRenderElement::Header(elem)
    }
}

impl<R> Element for CosmicWindowRenderElement<R>
where
    R: AsGlowRenderer + Renderer + ImportAll + ImportMem,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> &Id {
        match self {
            CosmicWindowRenderElement::Header(h) => h.id(),
            CosmicWindowRenderElement::Window(w) => w.id(),
        }
    }

    fn current_commit(&self) -> CommitCounter {
        match self {
            CosmicWindowRenderElement::Header(h) => h.current_commit(),
            CosmicWindowRenderElement::Window(w) => w.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, smithay::utils::Buffer> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.src(),
            CosmicWindowRenderElement::Window(w) => w.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.geometry(scale),
            CosmicWindowRenderElement::Window(w) => w.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.location(scale),
            CosmicWindowRenderElement::Window(w) => w.location(scale),
        }
    }

    fn transform(&self) -> Transform {
        match self {
            CosmicWindowRenderElement::Header(h) => h.transform(),
            CosmicWindowRenderElement::Window(w) => w.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.damage_since(scale, commit),
            CosmicWindowRenderElement::Window(w) => w.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.opaque_regions(scale),
            CosmicWindowRenderElement::Window(w) => w.opaque_regions(scale),
        }
    }
}

impl RenderElement<GlowRenderer> for CosmicWindowRenderElement<GlowRenderer> {
    fn draw<'a>(
        &self,
        frame: &mut <GlowRenderer as Renderer>::Frame<'a>,
        src: Rectangle<f64, smithay::utils::Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlowRenderer as Renderer>::Error> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.draw(frame, src, dst, damage),
            CosmicWindowRenderElement::Window(w) => w.draw(frame, src, dst, damage),
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut GlowRenderer,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage> {
        match self {
            CosmicWindowRenderElement::Header(h) => h.underlying_storage(renderer),
            CosmicWindowRenderElement::Window(w) => w.underlying_storage(renderer),
        }
    }
}

impl<'a, 'b> RenderElement<GlMultiRenderer<'a, 'b>>
    for CosmicWindowRenderElement<GlMultiRenderer<'a, 'b>>
{
    fn draw<'c>(
        &self,
        frame: &mut GlMultiFrame<'a, 'b, 'c>,
        src: Rectangle<f64, smithay::utils::Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlMultiRenderer<'a, 'b> as Renderer>::Error> {
        match self {
            CosmicWindowRenderElement::Header(h) => h
                .draw(frame.glow_frame_mut(), src, dst, damage)
                .map_err(|err| MultiError::Render(err)),
            CosmicWindowRenderElement::Window(w) => w.draw(frame, src, dst, damage),
        }
    }

    fn underlying_storage(
        &self,
        renderer: &mut GlMultiRenderer<'a, 'b>,
    ) -> Option<smithay::backend::renderer::element::UnderlyingStorage> {
        match self {
            CosmicWindowRenderElement::Header(h) => {
                h.underlying_storage(renderer.glow_renderer_mut())
            }
            CosmicWindowRenderElement::Window(w) => w.underlying_storage(renderer),
        }
    }
}

impl<R> AsRenderElements<R> for CosmicWindow
where
    R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
    <R as Renderer>::TextureId: 'static,
    CosmicWindowRenderElement<R>: RenderElement<R>,
{
    type RenderElement = CosmicWindowRenderElement<R>;
    fn render_elements<C: From<Self::RenderElement>>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        let has_ssd = self.0.with_program(|p| p.has_ssd());

        let window_loc = if has_ssd {
            location + Point::from((0, (SSD_HEIGHT as f64 * scale.y) as i32))
        } else {
            location
        };

        let mut elements = self.0.with_program(|p| {
            AsRenderElements::<R>::render_elements::<CosmicWindowRenderElement<R>>(
                &p.window, renderer, window_loc, scale,
            )
        });
        if has_ssd {
            elements.extend(AsRenderElements::<GlowRenderer>::render_elements::<
                CosmicWindowRenderElement<R>,
            >(
                &self.0, renderer.glow_renderer_mut(), location, scale
            ))
        }

        elements.into_iter().map(C::from).collect()
    }
}
