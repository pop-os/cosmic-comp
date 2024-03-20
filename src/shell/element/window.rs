use crate::{
    backend::render::cursor::{CursorShape, CursorState},
    shell::{
        grabs::{ReleaseMode, ResizeEdge},
        Shell,
    },
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::*,
    },
    wayland::handlers::screencopy::ScreencopySessions,
};
use calloop::LoopHandle;
use cosmic::{iced::Command, widget::mouse_area, Apply};
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
        pointer::{
            AxisFrame, ButtonEvent, CursorImageStatus, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, MotionEvent,
            PointerTarget, RelativeMotionEvent,
        },
        Seat,
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    render_elements,
    utils::{Buffer as BufferCoords, IsAlive, Logical, Point, Rectangle, Serial, Size},
    wayland::seat::WaylandFocus,
};
use std::{
    cell::RefCell,
    fmt,
    hash::Hash,
    sync::{
        atomic::{AtomicBool, AtomicU8, Ordering},
        Arc, Mutex,
    },
};
use wayland_backend::server::ObjectId;

use super::{
    surface::{RESIZE_BORDER, SSD_HEIGHT},
    CosmicSurface,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicWindow(IcedElement<CosmicWindowInternal>);

impl fmt::Debug for CosmicWindow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicWindow")
            .field("internal", &self.0)
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub struct CosmicWindowInternal {
    pub(super) window: CosmicSurface,
    mask: Arc<Mutex<Option<tiny_skia::Mask>>>,
    activated: Arc<AtomicBool>,
    /// TODO: This needs to be per seat
    pointer_entered: Arc<AtomicU8>,
    last_seat: Arc<Mutex<Option<(Seat<State>, Serial)>>>,
    last_title: Arc<Mutex<String>>,
}

impl fmt::Debug for CosmicWindowInternal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicWindowInternal")
            .field("window", &self.window)
            .field("activated", &self.activated.load(Ordering::SeqCst))
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
    ResizeTop,
    ResizeLeft,
    ResizeRight,
    ResizeBottom,
    ResizeTopRight,
    ResizeTopLeft,
    ResizeBottomRight,
    ResizeBottomLeft,
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

    pub fn has_ssd(&self, pending: bool) -> bool {
        !self.window.is_decorated(pending)
    }
}

impl CosmicWindow {
    pub fn new(
        window: impl Into<CosmicSurface>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: cosmic::Theme,
    ) -> CosmicWindow {
        let window = window.into();
        let width = window.geometry().size.w;
        let last_title = window.title();
        CosmicWindow(IcedElement::new(
            CosmicWindowInternal {
                window,
                mask: Arc::new(Mutex::new(None)),
                activated: Arc::new(AtomicBool::new(false)),
                pointer_entered: Arc::new(AtomicU8::new(Focus::None as u8)),
                last_seat: Arc::new(Mutex::new(None)),
                last_title: Arc::new(Mutex::new(last_title)),
            },
            (width, SSD_HEIGHT),
            handle,
            theme,
        ))
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        self.0.with_program(|p| {
            let mut size = p.window.pending_size()?;
            if p.has_ssd(true) {
                size.h += SSD_HEIGHT;
            }
            Some(size)
        })
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>) {
        self.0.with_program(|p| {
            let loc = (
                geo.loc.x,
                geo.loc.y + if p.has_ssd(true) { SSD_HEIGHT } else { 0 },
            );
            let size = (
                geo.size.w,
                std::cmp::max(geo.size.h - if p.has_ssd(true) { SSD_HEIGHT } else { 0 }, 0),
            );
            p.window
                .set_geometry(Rectangle::from_loc_and_size(loc, size));
            p.mask.lock().unwrap().take();
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
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));
        if has_ssd {
            Point::from((0, SSD_HEIGHT))
        } else {
            Point::from((0, 0))
        }
    }

    pub(super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        self.0.loop_handle()
    }

    pub fn split_render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: smithay::utils::Point<i32, smithay::utils::Physical>,
        scale: smithay::utils::Scale<f64>,
        alpha: f32,
    ) -> (Vec<C>, Vec<C>)
    where
        R: Renderer + ImportAll + ImportMem,
        <R as Renderer>::TextureId: 'static,
        C: From<CosmicWindowRenderElement<R>>,
    {
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));

        let window_loc = if has_ssd {
            location + Point::from((0, (SSD_HEIGHT as f64 * scale.y) as i32))
        } else {
            location
        };

        let (mut window_elements, popup_elements) = self.0.with_program(|p| {
            p.window
                .split_render_elements::<R, CosmicWindowRenderElement<R>>(
                    renderer, window_loc, scale, alpha,
                )
        });

        if has_ssd {
            let ssd_loc = location
                + self
                    .0
                    .with_program(|p| p.window.geometry().loc)
                    .to_physical_precise_round(scale);
            window_elements.extend(AsRenderElements::<R>::render_elements::<
                CosmicWindowRenderElement<R>,
            >(&self.0, renderer, ssd_loc, scale, alpha))
        }

        (
            window_elements.into_iter().map(C::from).collect(),
            popup_elements.into_iter().map(C::from).collect(),
        )
    }

    pub(crate) fn set_theme(&self, theme: cosmic::Theme) {
        self.0.set_theme(theme);
    }

    pub(crate) fn force_redraw(&self) {
        self.0.force_redraw();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    DragStart,
    Minimize,
    Maximize,
    Close,
    Menu,
}

impl Program for CosmicWindowInternal {
    type Message = Message;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::State>,
    ) -> Command<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    if let Some(surface) = self.window.wl_surface() {
                        loop_handle.insert_idle(move |state| {
                            Shell::move_request(
                                state,
                                &surface,
                                &seat,
                                serial,
                                ReleaseMode::NoMouseButtons,
                                false,
                            );
                        });
                    }
                }
            }
            Message::Minimize => {
                if let Some(surface) = self.window.wl_surface() {
                    loop_handle.insert_idle(move |state| {
                        if let Some(mapped) =
                            state.common.shell.element_for_wl_surface(&surface).cloned()
                        {
                            state.common.shell.minimize_request(&mapped)
                        }
                    });
                }
            }
            Message::Maximize => {
                if let Some(surface) = self.window.wl_surface() {
                    loop_handle.insert_idle(move |state| {
                        if let Some(mapped) =
                            state.common.shell.element_for_wl_surface(&surface).cloned()
                        {
                            let seat = state.common.last_active_seat().clone();
                            state.common.shell.maximize_toggle(&mapped, &seat)
                        }
                    });
                }
            }
            Message::Close => self.window.close(),
            Message::Menu => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    if let Some(surface) = self.window.wl_surface() {
                        loop_handle.insert_idle(move |state| {
                            if let Some(mapped) =
                                state.common.shell.element_for_wl_surface(&surface).cloned()
                            {
                                let position = if let Some((output, set)) =
                                    state.common.shell.workspaces.sets.iter().find(|(_, set)| {
                                        set.sticky_layer.mapped().any(|m| m == &mapped)
                                    }) {
                                    set.sticky_layer
                                        .element_geometry(&mapped)
                                        .unwrap()
                                        .loc
                                        .to_global(output)
                                } else if let Some(workspace) =
                                    state.common.shell.space_for_mut(&mapped)
                                {
                                    let Some(elem_geo) = workspace.element_geometry(&mapped) else {
                                        return;
                                    };
                                    elem_geo.loc.to_global(&workspace.output)
                                } else {
                                    return;
                                };

                                let mut cursor = seat
                                    .get_pointer()
                                    .unwrap()
                                    .current_location()
                                    .to_i32_round();
                                cursor.y -= SSD_HEIGHT;
                                Shell::menu_request(
                                    state,
                                    &surface,
                                    &seat,
                                    serial,
                                    cursor - position.as_logical(),
                                    false,
                                );
                            }
                        });
                    }
                }
            }
        }
        Command::none()
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, BufferCoords>],
        scale: f32,
    ) {
        if !self.window.is_activated(false) {
            let mut mask = self.mask.lock().unwrap();
            if self.window.is_maximized(false) {
                mask.take();
            } else if mask.is_none() {
                let (w, h) = (pixels.width(), pixels.height());
                let mut new_mask = tiny_skia::Mask::new(w, h).unwrap();

                let mut pb = tiny_skia::PathBuilder::new();
                let radius = 8. * scale;
                let (w, h) = (w as f32, h as f32);

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

            let mut paint = tiny_skia::Paint::default();
            paint.set_color_rgba8(0, 0, 0, 102);

            for rect in damage {
                pixels.fill_rect(
                    tiny_skia::Rect::from_xywh(
                        rect.loc.x as f32,
                        rect.loc.y as f32,
                        rect.size.w as f32,
                        rect.size.h as f32,
                    )
                    .unwrap(),
                    &paint,
                    Default::default(),
                    mask.as_ref(),
                );
            }
        }
    }

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        cosmic::widget::header_bar()
            .title(self.last_title.lock().unwrap().clone())
            .on_drag(Message::DragStart)
            .on_minimize(Message::Minimize)
            .on_maximize(Message::Maximize)
            .on_close(Message::Close)
            .apply(mouse_area)
            .on_right_press(Message::Menu)
            .into()
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
            if p.has_ssd(false) {
                bbox.loc -= Point::from((RESIZE_BORDER, RESIZE_BORDER));
                bbox.size += Size::from((RESIZE_BORDER * 2, RESIZE_BORDER * 2));
                bbox.size.h += SSD_HEIGHT;
            }
            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        let mut point = *point;
        self.0.with_program(|p| {
            if p.has_ssd(false) {
                let geo = p.window.geometry();

                let point_i32 = point.to_i32_round::<i32>();
                if (point_i32.x - geo.loc.x >= -RESIZE_BORDER && point_i32.x - geo.loc.x < 0)
                    || (point_i32.y - geo.loc.y >= -RESIZE_BORDER && point_i32.y - geo.loc.y < 0)
                    || (point_i32.x - geo.loc.x >= geo.size.w
                        && point_i32.x - geo.loc.x < geo.size.w + RESIZE_BORDER)
                    || (point_i32.y - geo.loc.y >= geo.size.h
                        && point_i32.y - geo.loc.y < geo.size.h + SSD_HEIGHT + RESIZE_BORDER)
                {
                    return true;
                }

                if point_i32.y - geo.loc.y < SSD_HEIGHT {
                    return true;
                }

                point.y -= SSD_HEIGHT as f64;
            }
            SpaceElement::is_in_input_region(&p.window, &point)
        })
    }
    fn set_activate(&self, activated: bool) {
        if self
            .0
            .with_program(|p| p.activated.load(Ordering::SeqCst) != activated)
        {
            SpaceElement::set_activate(&self.0, activated);
            self.0.force_redraw();
            self.0.with_program(|p| {
                p.activated.store(activated, Ordering::SeqCst);
                SpaceElement::set_activate(&p.window, activated);
            });
        }
    }
    #[profiling::function]
    fn output_enter(&self, output: &Output, overlap: Rectangle<i32, Logical>) {
        SpaceElement::output_enter(&self.0, output, overlap);
        self.0
            .with_program(|p| SpaceElement::output_enter(&p.window, output, overlap));
    }
    #[profiling::function]
    fn output_leave(&self, output: &Output) {
        SpaceElement::output_leave(&self.0, output);
        self.0
            .with_program(|p| SpaceElement::output_leave(&p.window, output));
    }
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut geo = SpaceElement::geometry(&p.window);
            if p.has_ssd(false) {
                geo.size.h += SSD_HEIGHT;
            }
            geo
        })
    }
    fn z_index(&self) -> u8 {
        self.0.with_program(|p| SpaceElement::z_index(&p.window))
    }
    #[profiling::function]
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
        let mut event = event.clone();
        if self.0.with_program(|p| {
            if let Some(sessions) = p.window.user_data().get::<ScreencopySessions>() {
                for session in &*sessions.0.borrow() {
                    session.cursor_enter(seat, InputType::Pointer)
                }
            }

            if p.has_ssd(false) {
                let geo = p.window.geometry();
                let loc = event.location.to_i32_round::<i32>();
                let (old_focus, shape) = if loc.y - geo.loc.y < 0 && loc.x - geo.loc.x < 0 {
                    (
                        p.swap_focus(Focus::ResizeTopLeft),
                        CursorShape::NorthWestResize,
                    )
                } else if loc.y - geo.loc.y < 0 && loc.x - geo.loc.x >= geo.size.w {
                    (
                        p.swap_focus(Focus::ResizeTopRight),
                        CursorShape::NorthEastResize,
                    )
                } else if loc.y - geo.loc.y < 0 {
                    (p.swap_focus(Focus::ResizeTop), CursorShape::NorthResize)
                } else if loc.y - geo.loc.y >= SSD_HEIGHT + geo.size.h && loc.x - geo.loc.x < 0 {
                    (
                        p.swap_focus(Focus::ResizeBottomLeft),
                        CursorShape::SouthWestResize,
                    )
                } else if loc.y - geo.loc.y >= SSD_HEIGHT + geo.size.h
                    && loc.x - geo.loc.x >= geo.size.w
                {
                    (
                        p.swap_focus(Focus::ResizeBottomRight),
                        CursorShape::SouthEastResize,
                    )
                } else if loc.y - geo.loc.y >= SSD_HEIGHT + geo.size.h {
                    (p.swap_focus(Focus::ResizeBottom), CursorShape::SouthResize)
                } else if loc.x - geo.loc.x < 0 {
                    (p.swap_focus(Focus::ResizeLeft), CursorShape::WestResize)
                } else if loc.x - geo.loc.x >= geo.size.w {
                    (p.swap_focus(Focus::ResizeRight), CursorShape::EastResize)
                } else if loc.y - geo.loc.y < SSD_HEIGHT {
                    (p.swap_focus(Focus::Header), CursorShape::Default)
                } else {
                    let focus = p.swap_focus(Focus::Window);
                    assert_eq!(focus, Focus::None);

                    let mut event = event.clone();
                    event.location.y -= SSD_HEIGHT as f64;
                    PointerTarget::enter(&p.window, seat, data, &event);
                    return false;
                };

                assert_eq!(old_focus, Focus::None);

                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.set_shape(shape);
                let cursor_status = seat
                    .user_data()
                    .get::<RefCell<CursorImageStatus>>()
                    .unwrap();
                *cursor_status.borrow_mut() = CursorImageStatus::default_named();
                shape == CursorShape::Default
            } else {
                p.swap_focus(Focus::Window);
                PointerTarget::enter(&p.window, seat, data, &event);
                false
            }
        }) {
            event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
            PointerTarget::enter(&self.0, seat, data, &event)
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
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

            if p.has_ssd(false) {
                let geo = p.window.geometry();
                let loc = event.location.to_i32_round::<i32>();
                let (next, shape) = if loc.y < 0 && loc.x < 0 {
                    (Focus::ResizeTopLeft, CursorShape::NorthWestResize)
                } else if loc.y < 0 && loc.x >= geo.size.w {
                    (Focus::ResizeTopRight, CursorShape::NorthEastResize)
                } else if loc.y < 0 {
                    (Focus::ResizeTop, CursorShape::NorthResize)
                } else if loc.y >= SSD_HEIGHT + geo.size.h && loc.x < 0 {
                    (Focus::ResizeBottomLeft, CursorShape::SouthWestResize)
                } else if loc.y >= SSD_HEIGHT + geo.size.h && loc.x >= geo.size.w {
                    (Focus::ResizeBottomRight, CursorShape::SouthEastResize)
                } else if loc.y >= SSD_HEIGHT + geo.size.h {
                    (Focus::ResizeBottom, CursorShape::SouthResize)
                } else if loc.x < 0 {
                    (Focus::ResizeLeft, CursorShape::WestResize)
                } else if loc.x >= geo.size.w {
                    (Focus::ResizeRight, CursorShape::EastResize)
                } else if loc.y < SSD_HEIGHT {
                    (Focus::Header, CursorShape::Default)
                } else {
                    event.location.y -= SSD_HEIGHT as f64;

                    let previous = p.swap_focus(Focus::Window);
                    if previous != Focus::Window {
                        PointerTarget::enter(&p.window, seat, data, &event);
                    } else {
                        PointerTarget::motion(&p.window, seat, data, &event);
                    }

                    return Some((previous, Focus::Window));
                };

                let previous = p.swap_focus(next);
                if previous == Focus::Window {
                    PointerTarget::leave(&p.window, seat, data, event.serial, event.time);
                }

                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.set_shape(shape);
                let cursor_status = seat
                    .user_data()
                    .get::<RefCell<CursorImageStatus>>()
                    .unwrap();
                *cursor_status.borrow_mut() = CursorImageStatus::default_named();

                Some((previous, next))
            } else {
                p.swap_focus(Focus::Window);
                PointerTarget::motion(&p.window, seat, data, &event);
                None
            }
        }) {
            event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
            match (previous, next) {
                (Focus::Header, Focus::Header) => {
                    PointerTarget::motion(&self.0, seat, data, &event)
                }
                (_, Focus::Header) => PointerTarget::enter(&self.0, seat, data, &event),
                (Focus::Header, _) => {
                    PointerTarget::leave(&self.0, seat, data, event.serial, event.time)
                }
                _ => {}
            };
        }
    }

    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
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
            Focus::None => {}
            x => {
                let serial = event.serial;
                let seat = seat.clone();
                let Some(surface) = self.wl_surface() else {
                    return;
                };
                self.0.loop_handle().insert_idle(move |state| {
                    Shell::resize_request(
                        state,
                        &surface,
                        &seat,
                        serial,
                        match x {
                            Focus::ResizeTop => ResizeEdge::TOP,
                            Focus::ResizeTopLeft => ResizeEdge::TOP_LEFT,
                            Focus::ResizeTopRight => ResizeEdge::TOP_RIGHT,
                            Focus::ResizeBottom => ResizeEdge::BOTTOM,
                            Focus::ResizeBottomLeft => ResizeEdge::BOTTOM_LEFT,
                            Focus::ResizeBottomRight => ResizeEdge::BOTTOM_RIGHT,
                            Focus::ResizeLeft => ResizeEdge::LEFT,
                            Focus::ResizeRight => ResizeEdge::RIGHT,
                            Focus::Header | Focus::Window | Focus::None => unreachable!(),
                        },
                    )
                });
            }
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

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self.0.with_program(|p| p.current_focus()) {
            Focus::Header => PointerTarget::frame(&self.0, seat, data),
            Focus::Window => self
                .0
                .with_program(|p| PointerTarget::frame(&p.window, seat, data)),
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

            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(CursorShape::Default);
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

    fn gesture_swipe_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeBeginEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_swipe_begin(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_swipe_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeUpdateEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_swipe_update(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_swipe_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeEndEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_swipe_end(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_pinch_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchBeginEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_pinch_begin(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_pinch_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchUpdateEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_pinch_update(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_pinch_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchEndEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_pinch_end(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_hold_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureHoldBeginEvent,
    ) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_hold_begin(&p.window, seat, data, event)
            }
        })
    }

    fn gesture_hold_end(&self, seat: &Seat<State>, data: &mut State, event: &GestureHoldEndEvent) {
        self.0.with_program(|p| {
            if !p.has_ssd(false) || p.current_focus() == Focus::Window {
                PointerTarget::gesture_hold_end(&p.window, seat, data, event)
            }
        })
    }
}

impl WaylandFocus for CosmicWindow {
    fn wl_surface(&self) -> Option<WlSurface> {
        self.0.with_program(|p| p.window.wl_surface())
    }

    fn same_client_as(&self, object_id: &ObjectId) -> bool {
        self.0.with_program(|p| p.window.same_client_as(object_id))
    }
}

render_elements! {
    pub CosmicWindowRenderElement<R> where R: ImportAll + ImportMem;
    Header = MemoryRenderBufferRenderElement<R>,
    Window = WaylandSurfaceRenderElement<R>,
}
