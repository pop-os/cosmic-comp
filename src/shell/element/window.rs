use crate::{
    backend::render::cursor::CursorState,
    hooks::{Decorations, HOOKS},
    shell::{
        focus::target::PointerFocusTarget,
        grabs::{ReleaseMode, ResizeEdge},
    },
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::*,
    },
};
use calloop::LoopHandle;
use cosmic::iced::{Color, Task};
use cosmic_comp_config::AppearanceConfig;
use smithay::{
    backend::{
        input::KeyState,
        renderer::{
            ImportAll, ImportMem, Renderer,
            element::{
                AsRenderElements, memory::MemoryRenderBufferRenderElement,
                surface::WaylandSurfaceRenderElement,
            },
        },
    },
    desktop::{WindowSurfaceType, space::SpaceElement},
    input::{
        Seat,
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, CursorImageStatus, GestureHoldBeginEvent,
            GestureHoldEndEvent, GesturePinchBeginEvent, GesturePinchEndEvent,
            GesturePinchUpdateEvent, GestureSwipeBeginEvent, GestureSwipeEndEvent,
            GestureSwipeUpdateEvent, MotionEvent, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent, TouchTarget,
            UpEvent,
        },
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    render_elements,
    utils::{IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
    wayland::seat::WaylandFocus,
};
use std::{
    borrow::Cow,
    fmt,
    hash::Hash,
    sync::{
        Mutex,
        atomic::{AtomicBool, AtomicU8, Ordering},
    },
};
use wayland_backend::server::ObjectId;

use super::CosmicSurface;

pub const SSD_HEIGHT: i32 = 36;
pub const RESIZE_BORDER: i32 = 10;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicWindow(pub(super) IcedElement<CosmicWindowInternal>);

impl fmt::Debug for CosmicWindow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicWindow")
            .field("internal", &self.0)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct CosmicWindowInternal {
    pub(super) window: CosmicSurface,
    activated: AtomicBool,
    /// TODO: This needs to be per seat
    pointer_entered: AtomicU8,
    last_title: Mutex<String>,
    appearance_conf: Mutex<AppearanceConfig>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Focus {
    Header = 1,
    ResizeTop,
    ResizeLeft,
    ResizeRight,
    ResizeBottom,
    ResizeTopRight,
    ResizeTopLeft,
    ResizeBottomRight,
    ResizeBottomLeft,
}

impl Focus {
    pub fn under(
        surface: &CosmicSurface,
        header_height: i32,
        location: Point<f64, Logical>,
    ) -> Option<Focus> {
        let geo = surface.geometry();
        let loc = location.to_i32_round::<i32>() - geo.loc;
        if loc.y < 0 && loc.x < 0 {
            Some(Focus::ResizeTopLeft)
        } else if loc.y < 0 && loc.x >= geo.size.w {
            Some(Focus::ResizeTopRight)
        } else if loc.y < 0 {
            Some(Focus::ResizeTop)
        } else if loc.y >= header_height + geo.size.h && loc.x < 0 {
            Some(Focus::ResizeBottomLeft)
        } else if loc.y >= header_height + geo.size.h && loc.x >= geo.size.w {
            Some(Focus::ResizeBottomRight)
        } else if loc.y >= header_height + geo.size.h {
            Some(Focus::ResizeBottom)
        } else if loc.x < 0 {
            Some(Focus::ResizeLeft)
        } else if loc.x >= geo.size.w {
            Some(Focus::ResizeRight)
        } else if loc.y < header_height {
            Some(Focus::Header)
        } else {
            None
        }
    }

    pub fn cursor_shape(&self) -> CursorIcon {
        match self {
            Focus::ResizeTopLeft => CursorIcon::NwResize,
            Focus::ResizeTopRight => CursorIcon::NeResize,
            Focus::ResizeTop => CursorIcon::NResize,
            Focus::ResizeBottomLeft => CursorIcon::SwResize,
            Focus::ResizeBottomRight => CursorIcon::SeResize,
            Focus::ResizeBottom => CursorIcon::SResize,
            Focus::ResizeLeft => CursorIcon::WResize,
            Focus::ResizeRight => CursorIcon::EResize,
            Focus::Header => CursorIcon::Default,
        }
    }

    pub unsafe fn from_u8(value: u8) -> Option<Focus> {
        match value {
            0 => None,
            focus => unsafe { Some(std::mem::transmute::<u8, Focus>(focus)) },
        }
    }
}

impl CosmicWindowInternal {
    pub fn swap_focus(&self, focus: Option<Focus>) -> Option<Focus> {
        let value = focus.map_or(0, |x| x as u8);
        unsafe { Focus::from_u8(self.pointer_entered.swap(value, Ordering::SeqCst)) }
    }

    pub fn current_focus(&self) -> Option<Focus> {
        unsafe { Focus::from_u8(self.pointer_entered.load(Ordering::SeqCst)) }
    }

    /// returns if the window has any current or pending server-side decorations
    pub fn has_ssd(&self, pending: bool) -> bool {
        !self.window.is_decorated(pending)
    }

    /// returns if the window is currently or pending tiled
    pub fn is_tiled(&self, pending: bool) -> bool {
        self.window.is_tiled(pending).unwrap_or(false)
    }
}

impl CosmicWindow {
    pub fn new(
        window: impl Into<CosmicSurface>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: cosmic::Theme,
        appearance: AppearanceConfig,
    ) -> CosmicWindow {
        let window = window.into();
        let width = window.geometry().size.w;
        let last_title = window.title();
        CosmicWindow(IcedElement::new(
            CosmicWindowInternal {
                window,
                activated: AtomicBool::new(false),
                pointer_entered: AtomicU8::new(0),
                last_title: Mutex::new(last_title),
                appearance_conf: Mutex::new(appearance),
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
            let ssd_height = if p.has_ssd(true) { SSD_HEIGHT } else { 0 };
            let loc = (geo.loc.x, geo.loc.y + ssd_height);
            let size = (geo.size.w, std::cmp::max(geo.size.h - ssd_height, 0));
            p.window
                .set_geometry(Rectangle::new(loc.into(), size.into()), ssd_height as u32);
        });
    }

    pub fn on_commit(&self, surface: &WlSurface) {
        let mut geo = None;
        self.0.with_program(|p| {
            if &p.window == surface {
                p.window.0.on_commit();
                geo = Some(p.window.geometry());
            }
        });
        if let Some(geo) = geo {
            self.0.resize(Size::from((geo.size.w, SSD_HEIGHT)));
        }
    }

    pub fn surface(&self) -> CosmicSurface {
        self.0.with_program(|p| p.window.clone())
    }

    pub fn focus_under(
        &self,
        mut relative_pos: Point<f64, Logical>,
        surface_type: WindowSurfaceType,
    ) -> Option<(PointerFocusTarget, Point<f64, Logical>)> {
        self.0.with_program(|p| {
            let mut offset = Point::from((0., 0.));
            let mut window_ui = None;
            let has_ssd = p.has_ssd(false);
            if (has_ssd || p.is_tiled(false)) && surface_type.contains(WindowSurfaceType::TOPLEVEL)
            {
                let geo = p.window.geometry();

                let point_i32 = relative_pos.to_i32_round::<i32>();
                let ssd_height = if has_ssd { SSD_HEIGHT } else { 0 };

                if (point_i32.x - geo.loc.x >= -RESIZE_BORDER && point_i32.x - geo.loc.x < 0)
                    || (point_i32.y - geo.loc.y >= -RESIZE_BORDER && point_i32.y - geo.loc.y < 0)
                    || (point_i32.x - geo.loc.x >= geo.size.w
                        && point_i32.x - geo.loc.x < geo.size.w + RESIZE_BORDER)
                    || (point_i32.y - geo.loc.y >= geo.size.h + ssd_height
                        && point_i32.y - geo.loc.y < geo.size.h + ssd_height + RESIZE_BORDER)
                {
                    window_ui = Some((
                        PointerFocusTarget::WindowUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }

                if has_ssd && (point_i32.y - geo.loc.y < SSD_HEIGHT) {
                    window_ui = Some((
                        PointerFocusTarget::WindowUI(self.clone()),
                        Point::from((0., 0.)),
                    ));
                }
            }

            if has_ssd {
                relative_pos.y -= SSD_HEIGHT as f64;
                offset.y += SSD_HEIGHT as f64;
            }

            window_ui.or_else(|| {
                p.window
                    .focus_under(relative_pos, surface_type)
                    .map(|(target, surface_offset)| (target, offset + surface_offset))
            })
        })
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

    pub fn popup_render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> Vec<C>
    where
        R: Renderer + ImportAll + ImportMem,
        R::TextureId: Send + Clone + 'static,
        C: From<CosmicWindowRenderElement<R>>,
    {
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));

        let window_loc = if has_ssd {
            location + Point::from((0, (SSD_HEIGHT as f64 * scale.y) as i32))
        } else {
            location
        };

        self.0.with_program(|p| {
            p.window
                .popup_render_elements::<R, CosmicWindowRenderElement<R>>(
                    renderer, window_loc, scale, alpha,
                )
                .into_iter()
                .map(C::from)
                .collect()
        })
    }

    pub fn render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
        scanout_override: Option<bool>,
    ) -> Vec<C>
    where
        R: Renderer + ImportAll + ImportMem,
        R::TextureId: Send + Clone + 'static,
        C: From<CosmicWindowRenderElement<R>>,
    {
        let has_ssd = self.0.with_program(|p| p.has_ssd(false));

        let window_loc = if has_ssd {
            location + Point::from((0, (SSD_HEIGHT as f64 * scale.y) as i32))
        } else {
            location
        };

        let mut elements = Vec::new();

        elements.extend(self.0.with_program(|p| {
            p.window.render_elements::<R, CosmicWindowRenderElement<R>>(
                renderer,
                window_loc,
                scale,
                alpha,
                scanout_override,
            )
        }));

        if has_ssd {
            let ssd_loc = location
                + self
                    .0
                    .with_program(|p| p.window.geometry().loc)
                    .to_physical_precise_round(scale);
            elements.extend(AsRenderElements::<R>::render_elements::<
                CosmicWindowRenderElement<R>,
            >(&self.0, renderer, ssd_loc, scale, alpha))
        }

        elements.into_iter().map(C::from).collect()
    }

    pub(crate) fn set_theme(&self, theme: cosmic::Theme) {
        self.0.set_theme(theme);
    }

    pub fn update_appearance_conf(&self, appearance: &AppearanceConfig) {
        self.0.with_program(|p| {
            let mut conf = p.appearance_conf.lock().unwrap();
            if &*conf != appearance {
                *conf = *appearance;
                if appearance.clip_floating_windows {
                    p.window.set_tiled(true);
                } else {
                    if !p.tiled.load(Ordering::Acquire) {
                        p.window.set_tiled(false);
                    }
                }
            }
        })
    }

    pub(crate) fn force_redraw(&self) {
        self.0.force_redraw();
    }

    pub fn min_size(&self) -> Option<Size<i32, Logical>> {
        self.0
            .with_program(|p| p.window.min_size_without_ssd())
            .map(|size| {
                if self.0.with_program(|p| !p.window.is_decorated(false)) {
                    size + (0, SSD_HEIGHT).into()
                } else {
                    size
                }
            })
    }
    pub fn max_size(&self) -> Option<Size<i32, Logical>> {
        self.0
            .with_program(|p| p.window.max_size_without_ssd())
            .map(|size| {
                if self.0.with_program(|p| !p.window.is_decorated(false)) {
                    size + (0, SSD_HEIGHT).into()
                } else {
                    size
                }
            })
    }

    pub fn corner_radius(&self, geometry_size: Size<i32, Logical>) -> Option<[u8; 4]> {
        self.0
            .with_program(|p| p.window.corner_radius(geometry_size))
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
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> Task<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = last_seat.cloned() {
                    if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                        loop_handle.insert_idle(move |state| {
                            let res = state.common.shell.write().move_request(
                                &surface,
                                &seat,
                                serial,
                                ReleaseMode::NoMouseButtons,
                                false,
                                &state.common.config,
                                &state.common.event_loop_handle,
                                false,
                            );
                            if let Some((grab, focus)) = res {
                                if grab.is_touch_grab() {
                                    seat.get_touch().unwrap().set_grab(state, grab, serial);
                                } else {
                                    seat.get_pointer()
                                        .unwrap()
                                        .set_grab(state, grab, serial, focus);
                                }
                            }
                        });
                    }
                }
            }
            Message::Minimize => {
                if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                    loop_handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        shell.minimize_request(&surface)
                    });
                }
            }
            Message::Maximize => {
                if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                    loop_handle.insert_idle(move |state| {
                        let mut shell = state.common.shell.write();
                        if let Some(mapped) = shell.element_for_surface(&surface).cloned() {
                            let seat = shell.seats.last_active().clone();
                            shell.maximize_toggle(&mapped, &seat, &state.common.event_loop_handle)
                        }
                    });
                }
            }
            Message::Close => self.window.close(),
            Message::Menu => {
                if let Some((seat, serial)) = last_seat.cloned() {
                    if let Some(surface) = self.window.wl_surface().map(Cow::into_owned) {
                        loop_handle.insert_idle(move |state| {
                            let shell = state.common.shell.read();
                            if let Some(mapped) = shell.element_for_surface(&surface).cloned() {
                                let position = if let Some((output, set)) =
                                    shell.workspaces.sets.iter().find(|(_, set)| {
                                        set.sticky_layer.mapped().any(|m| m == &mapped)
                                    }) {
                                    set.sticky_layer
                                        .element_geometry(&mapped)
                                        .unwrap()
                                        .loc
                                        .to_global(output)
                                } else if let Some(workspace) = shell.space_for(&mapped) {
                                    let Some(elem_geo) = workspace.element_geometry(&mapped) else {
                                        return;
                                    };
                                    elem_geo.loc.to_global(&workspace.output)
                                } else {
                                    return;
                                };

                                let pointer = seat.get_pointer().unwrap();
                                let mut cursor = pointer.current_location().to_i32_round();
                                cursor.y -= SSD_HEIGHT;

                                let res = shell.menu_request(
                                    &surface,
                                    &seat,
                                    serial,
                                    cursor - position.as_logical(),
                                    false,
                                    &state.common.config,
                                    &state.common.event_loop_handle,
                                );

                                std::mem::drop(shell);
                                if let Some((grab, focus)) = res {
                                    pointer.set_grab(state, grab, serial, focus);
                                }
                            }
                        });
                    }
                }
            }
        }
        Task::none()
    }

    fn background_color(&self, theme: &cosmic::Theme) -> Color {
        if self.window.is_maximized(false) {
            theme.cosmic().background.base.into()
        } else {
            Color::TRANSPARENT
        }
    }

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        HOOKS.get().unwrap().window_decorations.view(self)
    }
}

#[derive(Debug)]
pub struct DefaultDecorations;

impl Decorations<CosmicWindowInternal, Message> for DefaultDecorations {
    fn view(&self, win: &CosmicWindowInternal) -> cosmic::Element<'_, Message> {
        let mut header = cosmic::widget::header_bar()
            .title(win.last_title.lock().unwrap().clone())
            .on_drag(Message::DragStart)
            .on_close(Message::Close)
            .focused(win.window.is_activated(false))
            .on_double_click(Message::Maximize)
            .on_right_click(Message::Menu)
            .is_ssd(true);

        if cosmic::config::show_minimize() {
            header = header.on_minimize(Message::Minimize)
        }
        if cosmic::config::show_maximize() {
            header = header.on_maximize(Message::Maximize)
        }

        header.into()
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
            let has_ssd = p.has_ssd(false);

            if has_ssd || p.is_tiled(false) {
                bbox.loc -= Point::from((RESIZE_BORDER, RESIZE_BORDER));
                bbox.size += Size::from((RESIZE_BORDER * 2, RESIZE_BORDER * 2));
            }
            if has_ssd {
                bbox.size.h += SSD_HEIGHT;
            }

            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        self.focus_under(*point, WindowSurfaceType::ALL).is_some()
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
        if self.0.with_program(|p| {
            SpaceElement::refresh(&p.window);
            if !p.has_ssd(true) {
                return false;
            }

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
        } else {
            SpaceElement::refresh(&self.0);
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
        self.0.with_program(|p| {
            let has_ssd = p.has_ssd(false);
            if has_ssd || p.is_tiled(false) {
                let Some(next) = Focus::under(
                    &p.window,
                    if has_ssd { SSD_HEIGHT } else { 0 },
                    event.location,
                ) else {
                    return;
                };

                let old_focus = p.swap_focus(Some(next));
                assert_eq!(old_focus, None);

                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.lock().unwrap().set_shape(next.cursor_shape());
                seat.set_cursor_image_status(CursorImageStatus::default_named());
            }
        });

        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        PointerTarget::enter(&self.0, seat, data, &event)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let has_ssd = p.has_ssd(false);
            if has_ssd || p.is_tiled(false) {
                let Some(next) = Focus::under(
                    &p.window,
                    if has_ssd { SSD_HEIGHT } else { 0 },
                    event.location,
                ) else {
                    return;
                };
                let _previous = p.swap_focus(Some(next));

                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.lock().unwrap().set_shape(next.cursor_shape());
                seat.set_cursor_image_status(CursorImageStatus::default_named());
            }
        });

        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        PointerTarget::motion(&self.0, seat, data, &event)
    }

    fn relative_motion(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &RelativeMotionEvent,
    ) {
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => PointerTarget::button(&self.0, seat, data, event),
            Some(x) => {
                let serial = event.serial;
                let seat = seat.clone();
                let Some(surface) = self.wl_surface().map(Cow::into_owned) else {
                    return;
                };
                self.0.loop_handle().insert_idle(move |state| {
                    let res = state.common.shell.write().resize_request(
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
                            Focus::Header => unreachable!(),
                        },
                        state.common.config.cosmic_conf.edge_snap_threshold,
                        false,
                    );

                    if let Some((grab, focus)) = res {
                        if grab.is_touch_grab() {
                            seat.get_touch().unwrap().set_grab(state, grab, serial);
                        } else {
                            seat.get_pointer()
                                .unwrap()
                                .set_grab(state, grab, serial, focus);
                        }
                    }
                });
            }
            None => {}
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::axis(&self.0, seat, data, frame)
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        if let Some(Focus::Header) = self.0.with_program(|p| p.current_focus()) {
            PointerTarget::frame(&self.0, seat, data)
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        self.0.with_program(|p| {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().unset_shape();
            let _previous = p.swap_focus(None);
        });
        PointerTarget::leave(&self.0, seat, data, serial, time)
    }

    fn gesture_swipe_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeBeginEvent,
    ) {
    }

    fn gesture_swipe_update(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeUpdateEvent,
    ) {
    }

    fn gesture_swipe_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureSwipeEndEvent,
    ) {
    }

    fn gesture_pinch_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchBeginEvent,
    ) {
    }

    fn gesture_pinch_update(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchUpdateEvent,
    ) {
    }

    fn gesture_pinch_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GesturePinchEndEvent,
    ) {
    }

    fn gesture_hold_begin(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureHoldBeginEvent,
    ) {
    }

    fn gesture_hold_end(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &GestureHoldEndEvent,
    ) {
    }
}

impl TouchTarget<State> for CosmicWindow {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, seq: Serial) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            event.location -= p.window.geometry().loc.to_f64();
        });
        TouchTarget::down(&self.0, seat, data, &event, seq)
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent, seq: Serial) {
        TouchTarget::up(&self.0, seat, data, event, seq)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent, seq: Serial) {
        let mut event = event.clone();
        event.location -= self.0.with_program(|p| p.window.geometry().loc.to_f64());
        TouchTarget::motion(&self.0, seat, data, &event, seq)
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        TouchTarget::frame(&self.0, seat, data, seq)
    }

    fn cancel(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        TouchTarget::cancel(&self.0, seat, data, seq)
    }

    fn shape(&self, seat: &Seat<State>, data: &mut State, event: &ShapeEvent, seq: Serial) {
        TouchTarget::shape(&self.0, seat, data, event, seq)
    }

    fn orientation(
        &self,
        _seat: &Seat<State>,
        _data: &mut State,
        _event: &OrientationEvent,
        _seq: Serial,
    ) {
    }
}

impl WaylandFocus for CosmicWindow {
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        self.0.with_program(|p| {
            p.window
                .wl_surface()
                .map(|s| Cow::Owned(Cow::into_owned(s)))
        })
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
