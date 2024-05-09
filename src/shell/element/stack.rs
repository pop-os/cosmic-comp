use super::{surface::RESIZE_BORDER, window::Focus, CosmicSurface};
use crate::{
    backend::render::cursor::{CursorShape, CursorState},
    shell::{
        focus::{target::PointerFocusTarget, FocusDirection},
        grabs::{ReleaseMode, ResizeEdge},
        layout::tiling::NodeDesc,
        Direction,
    },
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::*,
    },
};
use calloop::LoopHandle;
use cosmic::{
    iced::{id::Id, widget as iced_widget},
    iced_core::{border::Radius, Background, Border, Color, Length},
    iced_runtime::Command,
    iced_widget::scrollable::AbsoluteOffset,
    theme, widget as cosmic_widget, Apply, Element as CosmicElement,
};
use once_cell::sync::Lazy;
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
    desktop::{space::SpaceElement, WindowSurfaceType},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        pointer::{
            AxisFrame, ButtonEvent, CursorImageStatus, GestureHoldBeginEvent, GestureHoldEndEvent,
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
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    render_elements,
    utils::{Buffer, IsAlive, Logical, Physical, Point, Rectangle, Scale, Serial, Size},
    wayland::seat::WaylandFocus,
};
use std::{
    cell::RefCell,
    fmt,
    hash::Hash,
    sync::{
        atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

mod tab;
mod tab_text;
mod tabs;

use self::{
    tab::{Tab, TabMessage},
    tabs::Tabs,
};

static SCROLLABLE_ID: Lazy<Id> = Lazy::new(|| Id::new("scrollable"));

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CosmicStack(IcedElement<CosmicStackInternal>);

impl fmt::Debug for CosmicStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CosmicStack")
            .field("internal", &self.0)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone)]
pub struct CosmicStackInternal {
    windows: Arc<Mutex<Vec<CosmicSurface>>>,
    active: Arc<AtomicUsize>,
    activated: Arc<AtomicBool>,
    group_focused: Arc<AtomicBool>,
    scroll_to_focus: Arc<AtomicBool>,
    previous_keyboard: Arc<AtomicUsize>,
    pointer_entered: Arc<AtomicU8>,
    reenter: Arc<AtomicBool>,
    potential_drag: Arc<Mutex<Option<usize>>>,
    override_alive: Arc<AtomicBool>,
    last_seat: Arc<Mutex<Option<(Seat<State>, Serial)>>>,
    geometry: Arc<Mutex<Option<Rectangle<i32, Global>>>>,
    mask: Arc<Mutex<Option<tiny_skia::Mask>>>,
}

impl CosmicStackInternal {
    pub fn swap_focus(&self, focus: Option<Focus>) -> Option<Focus> {
        let value = focus.map_or(0, |x| x as u8);
        unsafe { Focus::from_u8(self.pointer_entered.swap(value, Ordering::SeqCst)) }
    }

    pub fn current_focus(&self) -> Option<Focus> {
        unsafe { Focus::from_u8(self.pointer_entered.load(Ordering::SeqCst)) }
    }
}

pub const TAB_HEIGHT: i32 = 24;

#[derive(Debug, Clone)]
pub enum MoveResult {
    Handled,
    MoveOut(CosmicSurface, LoopHandle<'static, crate::state::State>),
    Default,
}

impl CosmicStack {
    pub fn new<I: Into<CosmicSurface>>(
        windows: impl Iterator<Item = I>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: cosmic::Theme,
    ) -> CosmicStack {
        let windows = windows.map(Into::into).collect::<Vec<_>>();
        assert!(!windows.is_empty());

        for window in &windows {
            window.try_force_undecorated(true);
            window.set_tiled(true);
            window.send_configure();
        }

        let width = windows[0].geometry().size.w;
        CosmicStack(IcedElement::new(
            CosmicStackInternal {
                windows: Arc::new(Mutex::new(windows)),
                active: Arc::new(AtomicUsize::new(0)),
                activated: Arc::new(AtomicBool::new(false)),
                group_focused: Arc::new(AtomicBool::new(false)),
                scroll_to_focus: Arc::new(AtomicBool::new(false)),
                previous_keyboard: Arc::new(AtomicUsize::new(0)),
                pointer_entered: Arc::new(AtomicU8::new(0)),
                reenter: Arc::new(AtomicBool::new(false)),
                potential_drag: Arc::new(Mutex::new(None)),
                override_alive: Arc::new(AtomicBool::new(true)),
                last_seat: Arc::new(Mutex::new(None)),
                geometry: Arc::new(Mutex::new(None)),
                mask: Arc::new(Mutex::new(None)),
            },
            (width, TAB_HEIGHT),
            handle,
            theme,
        ))
    }

    pub fn add_window(&self, window: impl Into<CosmicSurface>, idx: Option<usize>) {
        let window = window.into();
        window.try_force_undecorated(true);
        window.set_tiled(true);
        self.0.with_program(|p| {
            if let Some(mut geo) = p.geometry.lock().unwrap().clone() {
                geo.loc.y += TAB_HEIGHT;
                geo.size.h -= TAB_HEIGHT;
                window.set_geometry(geo);
            }
            window.send_configure();
            if let Some(idx) = idx {
                p.windows.lock().unwrap().insert(idx, window);
                let old_idx = p.active.swap(idx, Ordering::SeqCst);
                if old_idx == idx {
                    p.reenter.store(true, Ordering::SeqCst);
                    p.previous_keyboard.store(old_idx, Ordering::SeqCst);
                }
            } else {
                let mut windows = p.windows.lock().unwrap();
                windows.push(window);
                p.active.store(windows.len() - 1, Ordering::SeqCst);
            }
            p.scroll_to_focus.store(true, Ordering::SeqCst);
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
    }

    pub fn remove_window(&self, window: &CosmicSurface) {
        self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();
            if windows.len() == 1 {
                p.override_alive.store(false, Ordering::SeqCst);
                let window = windows.get(0).unwrap();
                window.try_force_undecorated(false);
                window.set_tiled(false);
                return;
            }

            let Some(idx) = windows.iter().position(|w| w == window) else {
                return;
            };
            if idx == p.active.load(Ordering::SeqCst) {
                p.reenter.store(true, Ordering::SeqCst);
            }
            let window = windows.remove(idx);
            window.try_force_undecorated(false);
            window.set_tiled(false);

            p.active.fetch_min(windows.len() - 1, Ordering::SeqCst);
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
    }

    pub fn remove_idx(&self, idx: usize) -> Option<CosmicSurface> {
        let window = self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();
            if windows.len() == 1 {
                p.override_alive.store(false, Ordering::SeqCst);
                let window = windows.get(0).unwrap();
                window.try_force_undecorated(false);
                window.set_tiled(false);
                return Some(window.clone());
            }
            if windows.len() <= idx {
                return None;
            }
            if idx == p.active.load(Ordering::SeqCst) {
                p.reenter.store(true, Ordering::SeqCst);
            }
            let window = windows.remove(idx);
            window.try_force_undecorated(false);
            window.set_tiled(false);

            p.active.fetch_min(windows.len() - 1, Ordering::SeqCst);

            Some(window)
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw();
        window
    }

    pub fn len(&self) -> usize {
        self.0.with_program(|p| p.windows.lock().unwrap().len())
    }

    pub fn handle_focus(&self, direction: FocusDirection, swap: Option<NodeDesc>) -> bool {
        let result = self.0.with_program(|p| match direction {
            FocusDirection::Left => {
                if !p.group_focused.load(Ordering::SeqCst) {
                    if let Ok(old) =
                        p.active
                            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |val| {
                                val.checked_sub(1)
                            })
                    {
                        p.previous_keyboard.store(old, Ordering::SeqCst);
                        p.scroll_to_focus.store(true, Ordering::SeqCst);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            FocusDirection::Right => {
                if !p.group_focused.load(Ordering::SeqCst) {
                    let max = p.windows.lock().unwrap().len();
                    if let Ok(old) =
                        p.active
                            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |val| {
                                if val < max - 1 {
                                    Some(val + 1)
                                } else {
                                    None
                                }
                            })
                    {
                        p.previous_keyboard.store(old, Ordering::SeqCst);
                        p.scroll_to_focus.store(true, Ordering::SeqCst);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            FocusDirection::Out if swap.is_none() => {
                if !p.group_focused.swap(true, Ordering::SeqCst) {
                    p.windows.lock().unwrap().iter().for_each(|w| {
                        w.set_activated(false);
                        w.send_configure();
                    });
                    true
                } else {
                    false
                }
            }
            FocusDirection::In if swap.is_none() => {
                if !p.group_focused.swap(false, Ordering::SeqCst) {
                    p.windows.lock().unwrap().iter().for_each(|w| {
                        w.set_activated(true);
                        w.send_configure();
                    });
                    true
                } else {
                    false
                }
            }
            _ => false,
        });

        if result {
            self.0
                .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
            self.0.force_update();
        }

        result
    }

    pub fn handle_move(&self, direction: Direction) -> MoveResult {
        let loop_handle = self.0.loop_handle();
        let result = self.0.with_program(|p| {
            if p.group_focused.load(Ordering::SeqCst) {
                return MoveResult::Default;
            }

            let active = p.active.load(Ordering::SeqCst);
            let mut windows = p.windows.lock().unwrap();

            let next = match direction {
                Direction::Left => active.checked_sub(1),
                Direction::Right => (active + 1 < windows.len()).then_some(active + 1),
                Direction::Down | Direction::Up => None,
            };

            if let Some(val) = next {
                let old = p.active.swap(val, Ordering::SeqCst);
                windows.swap(old, val);
                p.previous_keyboard.store(old, Ordering::SeqCst);
                p.scroll_to_focus.store(true, Ordering::SeqCst);
                MoveResult::Handled
            } else {
                if windows.len() == 1 {
                    return MoveResult::Default;
                }
                let window = windows.remove(active);
                if active == windows.len() {
                    p.active.store(active - 1, Ordering::SeqCst);
                    p.scroll_to_focus.store(true, Ordering::SeqCst);
                }
                window.try_force_undecorated(false);
                window.set_tiled(false);

                MoveResult::MoveOut(window, loop_handle)
            }
        });

        if !matches!(result, MoveResult::Default) {
            self.0
                .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
            self.0.force_update();
        }

        result
    }

    pub fn active(&self) -> CosmicSurface {
        self.0
            .with_program(|p| p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].clone())
    }

    pub fn has_active(&self, window: &CosmicSurface) -> bool {
        self.0
            .with_program(|p| &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)] == window)
    }

    pub fn whole_stack_focused(&self) -> bool {
        self.0
            .with_program(|p| p.group_focused.load(Ordering::SeqCst))
    }

    pub fn set_active(&self, window: &CosmicSurface) {
        self.0.with_program(|p| {
            if let Some(val) = p.windows.lock().unwrap().iter().position(|w| w == window) {
                let old = p.active.swap(val, Ordering::SeqCst);
                p.previous_keyboard.store(old, Ordering::SeqCst);
            }
        });
        self.0
            .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
        self.0.force_redraw()
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

    pub fn focus_under(
        &self,
        mut relative_pos: Point<f64, Logical>,
    ) -> Option<(PointerFocusTarget, Point<i32, Logical>)> {
        self.0.with_program(|p| {
            let mut stack_ui = None;
            let geo = p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].geometry();

            let point_i32 = relative_pos.to_i32_round::<i32>();
            if (point_i32.x - geo.loc.x >= -RESIZE_BORDER && point_i32.x - geo.loc.x < 0)
                || (point_i32.y - geo.loc.y >= -RESIZE_BORDER && point_i32.y - geo.loc.y < 0)
                || (point_i32.x - geo.loc.x >= geo.size.w
                    && point_i32.x - geo.loc.x < geo.size.w + RESIZE_BORDER)
                || (point_i32.y - geo.loc.y >= geo.size.h
                    && point_i32.y - geo.loc.y < geo.size.h + TAB_HEIGHT + RESIZE_BORDER)
            {
                stack_ui = Some((
                    PointerFocusTarget::StackUI(self.clone()),
                    Point::from((0, 0)),
                ));
            }

            if point_i32.y - geo.loc.y < TAB_HEIGHT {
                stack_ui = Some((
                    PointerFocusTarget::StackUI(self.clone()),
                    Point::from((0, 0)),
                ));
            }

            relative_pos.y -= TAB_HEIGHT as f64;

            let active_window = &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)];
            active_window
                .0
                .surface_under(relative_pos, WindowSurfaceType::ALL)
                .map(|(surface, surface_offset)| {
                    (
                        PointerFocusTarget::WlSurface {
                            surface,
                            toplevel: Some(active_window.clone().into()),
                        },
                        surface_offset + Point::from((0, TAB_HEIGHT)),
                    )
                })
                .or(stack_ui)
        })
    }

    pub fn offset(&self) -> Point<i32, Logical> {
        Point::from((0, TAB_HEIGHT))
    }

    pub fn pending_size(&self) -> Option<Size<i32, Logical>> {
        self.0.with_program(|p| {
            p.geometry
                .lock()
                .unwrap()
                .clone()
                .map(|geo| geo.size.as_logical())
        })
    }

    pub fn set_geometry(&self, geo: Rectangle<i32, Global>) {
        self.0.with_program(|p| {
            let loc = (geo.loc.x, geo.loc.y + TAB_HEIGHT);
            let size = (geo.size.w, geo.size.h - TAB_HEIGHT);

            let win_geo = Rectangle::from_loc_and_size(loc, size);
            for window in p.windows.lock().unwrap().iter() {
                window.set_geometry(win_geo);
            }

            *p.geometry.lock().unwrap() = Some(geo);
            p.mask.lock().unwrap().take();
        });
    }

    pub fn on_commit(&self, surface: &WlSurface) {
        if let Some(surface) = self.surfaces().find(|w| w == surface) {
            surface.0.on_commit();
            if self.active() == surface {
                self.0
                    .resize(Size::from((surface.geometry().size.w, TAB_HEIGHT)));
            }
        }
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
            if previous != active || p.reenter.swap(false, Ordering::SeqCst) {
                let windows = p.windows.lock().unwrap();
                if let Some(previous_surface) = windows.get(previous) {
                    if previous != active {
                        KeyboardTarget::leave(previous_surface, seat, data, serial);
                    }
                }
                KeyboardTarget::enter(
                    &windows[active],
                    seat,
                    data,
                    Vec::new(), /* TODO */
                    serial,
                )
            }
            active
        })
    }

    pub(in super::super) fn focus_stack(&self) {
        self.0
            .with_program(|p| p.group_focused.store(true, Ordering::SeqCst));
    }

    pub(in super::super) fn loop_handle(&self) -> LoopHandle<'static, crate::state::State> {
        self.0.loop_handle()
    }

    pub fn split_render_elements<R, C>(
        &self,
        renderer: &mut R,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
        alpha: f32,
    ) -> (Vec<C>, Vec<C>)
    where
        R: Renderer + ImportAll + ImportMem,
        <R as Renderer>::TextureId: Clone + 'static,
        C: From<CosmicStackRenderElement<R>>,
    {
        let offset = self
            .0
            .with_program(|p| {
                p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]
                    .geometry()
                    .loc
            })
            .to_physical_precise_round(scale);
        let stack_loc = location + offset;
        let window_loc = location + Point::from((0, (TAB_HEIGHT as f64 * scale.y) as i32));

        let elements = AsRenderElements::<R>::render_elements::<CosmicStackRenderElement<R>>(
            &self.0, renderer, stack_loc, scale, alpha,
        );

        let (window_elements, popup_elements) = self.0.with_program(|p| {
            let windows = p.windows.lock().unwrap();
            let active = p.active.load(Ordering::SeqCst);

            let (window_elements, popup_elements) = windows[active]
                .split_render_elements::<R, CosmicStackRenderElement<R>>(
                    renderer, window_loc, scale, alpha,
                );

            (window_elements, popup_elements)
        });

        (
            elements
                .into_iter()
                .map(C::from)
                .chain(window_elements.into_iter().map(C::from))
                .collect(),
            popup_elements.into_iter().map(C::from).collect(),
        )
    }

    pub(crate) fn set_theme(&self, theme: cosmic::Theme) {
        self.0.set_theme(theme);
    }

    pub(crate) fn force_redraw(&self) {
        self.0.force_redraw();
    }

    fn start_drag(&self, data: &mut State, seat: &Seat<State>, serial: Serial) {
        if let Some(dragged_out) = self
            .0
            .with_program(|p| p.potential_drag.lock().unwrap().take())
        {
            if let Some(surface) = self
                .0
                .with_program(|p| p.windows.lock().unwrap().get(dragged_out).cloned())
            {
                let seat = seat.clone();
                surface.try_force_undecorated(false);
                surface.send_configure();
                if let Some(surface) = surface.wl_surface() {
                    let _ = data.common.event_loop_handle.insert_idle(move |state| {
                        let res = state.common.shell.write().unwrap().move_request(
                            &surface,
                            &seat,
                            serial,
                            ReleaseMode::NoMouseButtons,
                            true,
                            &state.common.config,
                            &state.common.event_loop_handle,
                            &state.common.xdg_activation_state,
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
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    DragStart,
    Menu,
    TabMenu(usize),
    PotentialTabDragStart(usize),
    Activate(usize),
    Close(usize),
    ScrollForward,
    ScrollBack,
    Scrolled,
}

impl TabMessage for Message {
    fn activate(idx: usize) -> Self {
        Message::Activate(idx)
    }

    fn is_activate(&self) -> Option<usize> {
        match self {
            Message::Activate(idx) => Some(*idx),
            _ => None,
        }
    }

    fn scroll_back() -> Self {
        Message::ScrollBack
    }

    fn scroll_further() -> Self {
        Message::ScrollForward
    }

    fn populate_scroll(&mut self, mut current_offset: AbsoluteOffset) -> Option<AbsoluteOffset> {
        match self {
            Message::ScrollBack => Some({
                current_offset.x -= 10.;
                current_offset
            }),
            Message::ScrollForward => Some({
                current_offset.x += 10.;
                current_offset
            }),
            _ => None,
        }
    }

    fn scrolled() -> Self {
        Message::Scrolled
    }
}

impl Program for CosmicStackInternal {
    type Message = Message;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::State>,
    ) -> Command<Self::Message> {
        match message {
            Message::DragStart => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    let active = self.active.load(Ordering::SeqCst);
                    if let Some(surface) = self.windows.lock().unwrap()[active].wl_surface() {
                        loop_handle.insert_idle(move |state| {
                            let res = state.common.shell.write().unwrap().move_request(
                                &surface,
                                &seat,
                                serial,
                                ReleaseMode::NoMouseButtons,
                                false,
                                &state.common.config,
                                &state.common.event_loop_handle,
                                &state.common.xdg_activation_state,
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
            Message::PotentialTabDragStart(idx) => {
                *self.potential_drag.lock().unwrap() = Some(idx);
            }
            Message::Activate(idx) => {
                *self.potential_drag.lock().unwrap() = None;
                if let Some(surface) = self.windows.lock().unwrap().get(idx).cloned() {
                    loop_handle.insert_idle(move |state| {
                        if let Some(mapped) = state
                            .common
                            .shell
                            .read()
                            .unwrap()
                            .element_for_surface(&surface)
                        {
                            mapped.stack_ref().unwrap().set_active(&surface);
                        }
                    });
                    self.scroll_to_focus.store(true, Ordering::SeqCst);
                }
            }
            Message::Close(idx) => {
                if let Some(val) = self.windows.lock().unwrap().get(idx) {
                    val.close()
                }
            }
            Message::Scrolled => {
                self.scroll_to_focus.store(false, Ordering::SeqCst);
            }
            Message::Menu => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    let active = self.active.load(Ordering::SeqCst);
                    if let Some(surface) = self.windows.lock().unwrap()[active].wl_surface() {
                        loop_handle.insert_idle(move |state| {
                            let shell = state.common.shell.read().unwrap();
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

                                let mut cursor = seat
                                    .get_pointer()
                                    .unwrap()
                                    .current_location()
                                    .to_i32_round();
                                cursor.y -= TAB_HEIGHT;
                                let res = shell.menu_request(
                                    &surface,
                                    &seat,
                                    serial,
                                    cursor - position.as_logical(),
                                    true,
                                    &state.common.config,
                                    &state.common.event_loop_handle,
                                );

                                std::mem::drop(shell);
                                if let Some((grab, focus)) = res {
                                    seat.get_pointer()
                                        .unwrap()
                                        .set_grab(state, grab, serial, focus);
                                }
                            }
                        });
                    }
                }
            }
            Message::TabMenu(idx) => {
                if let Some((seat, serial)) = self.last_seat.lock().unwrap().clone() {
                    if let Some(surface) = self.windows.lock().unwrap()[idx].wl_surface() {
                        loop_handle.insert_idle(move |state| {
                            let shell = state.common.shell.read().unwrap();
                            if let Some(mapped) = shell.element_for_surface(&surface).cloned() {
                                if let Some(workspace) = shell.space_for(&mapped) {
                                    let Some(elem_geo) = workspace.element_geometry(&mapped) else {
                                        return;
                                    };
                                    let position = elem_geo.loc.to_global(&workspace.output);

                                    let mut cursor = seat
                                        .get_pointer()
                                        .unwrap()
                                        .current_location()
                                        .to_i32_round();
                                    cursor.y -= TAB_HEIGHT;
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
                                        seat.get_pointer()
                                            .unwrap()
                                            .set_grab(state, grab, serial, focus);
                                    }
                                }
                            }
                        });
                    }
                }
            }
            _ => unreachable!(),
        }
        Command::none()
    }

    fn view(&self) -> CosmicElement<'_, Self::Message> {
        let windows = self.windows.lock().unwrap();
        if self.geometry.lock().unwrap().is_none() {
            return iced_widget::row(Vec::new()).into();
        };
        let active = self.active.load(Ordering::SeqCst);
        let group_focused = self.group_focused.load(Ordering::SeqCst);

        let elements = vec![
            cosmic_widget::icon::from_name("window-stack-symbolic")
                .size(16)
                .prefer_svg(true)
                .icon()
                .style(if group_focused {
                    theme::Svg::custom(|theme| iced_widget::svg::Appearance {
                        color: Some(if theme.cosmic().is_dark {
                            Color::BLACK
                        } else {
                            Color::WHITE
                        }),
                    })
                } else {
                    theme::Svg::Default
                })
                .apply(iced_widget::container)
                .padding([4, 24])
                .center_y()
                .apply(iced_widget::mouse_area)
                .on_press(Message::DragStart)
                .on_right_press(Message::Menu)
                .into(),
            CosmicElement::new(
                Tabs::new(
                    windows.iter().enumerate().map(|(i, w)| {
                        let user_data = w.user_data();
                        user_data.insert_if_missing(Id::unique);
                        Tab::new(
                            w.title(),
                            w.app_id(),
                            user_data.get::<Id>().unwrap().clone(),
                        )
                        .on_press(Message::PotentialTabDragStart(i))
                        .on_right_click(Message::TabMenu(i))
                        .on_close(Message::Close(i))
                    }),
                    active,
                    windows[active].is_activated(false),
                    group_focused,
                )
                .id(SCROLLABLE_ID.clone())
                .force_visible(
                    self.scroll_to_focus
                        .load(Ordering::SeqCst)
                        .then_some(active),
                )
                .height(Length::Fill)
                .width(Length::Fill),
            ),
            iced_widget::horizontal_space(0)
                .apply(iced_widget::container)
                .padding([64, 24])
                .apply(iced_widget::mouse_area)
                .on_press(Message::DragStart)
                .on_right_press(Message::Menu)
                .into(),
        ];

        iced_widget::row(elements)
            .height(TAB_HEIGHT as u16)
            .width(Length::Fill) //width as u16)
            .apply(iced_widget::container)
            .center_y()
            .style(if self.group_focused.load(Ordering::SeqCst) {
                theme::Container::custom(|theme| iced_widget::container::Appearance {
                    icon_color: Some(Color::from(theme.cosmic().background.on)),
                    text_color: Some(Color::from(theme.cosmic().background.on)),
                    background: Some(Background::Color(theme.cosmic().accent_color().into())),
                    border: Border {
                        radius: Radius::from([8.0, 8.0, 0.0, 0.0]),
                        width: 0.0,
                        color: Color::TRANSPARENT,
                    },
                    shadow: Default::default(),
                })
            } else {
                theme::Container::custom(|theme| iced_widget::container::Appearance {
                    icon_color: Some(Color::from(theme.cosmic().background.on)),
                    text_color: Some(Color::from(theme.cosmic().background.on)),
                    background: Some(Background::Color(tab::primary_container_color(
                        theme.cosmic(),
                    ))),
                    border: Border {
                        radius: Radius::from([8.0, 8.0, 0.0, 0.0]),
                        width: 0.0,
                        color: Color::TRANSPARENT,
                    },
                    shadow: Default::default(),
                })
            })
            .into()
    }

    fn foreground(
        &self,
        pixels: &mut tiny_skia::PixmapMut<'_>,
        damage: &[Rectangle<i32, Buffer>],
        scale: f32,
    ) {
        if self.group_focused.load(Ordering::SeqCst) {
            let border = Rectangle::from_loc_and_size(
                (0, ((TAB_HEIGHT as f32 * scale) - scale).floor() as i32),
                (pixels.width() as i32, scale.ceil() as i32),
            );

            let mut paint = tiny_skia::Paint::default();
            let (b, g, r, a) = theme::COSMIC_DARK.accent_color().into_components();
            paint.set_color(tiny_skia::Color::from_rgba(r, g, b, a).unwrap());

            for rect in damage {
                if let Some(overlap) = rect.intersection(border) {
                    pixels.fill_rect(
                        tiny_skia::Rect::from_xywh(
                            overlap.loc.x as f32,
                            overlap.loc.y as f32,
                            overlap.size.w as f32,
                            overlap.size.h as f32,
                        )
                        .unwrap(),
                        &paint,
                        Default::default(),
                        None,
                    )
                }
            }
        }
    }
}

impl IsAlive for CosmicStack {
    fn alive(&self) -> bool {
        self.0.with_program(|p| {
            p.override_alive.load(Ordering::SeqCst)
                && p.windows.lock().unwrap().iter().any(IsAlive::alive)
        })
    }
}

impl SpaceElement for CosmicStack {
    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.0.with_program(|p| {
            let mut bbox =
                SpaceElement::bbox(&p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]);
            bbox.loc -= Point::from((RESIZE_BORDER, RESIZE_BORDER));
            bbox.size += Size::from((RESIZE_BORDER * 2, RESIZE_BORDER * 2));
            bbox.size.h += TAB_HEIGHT;
            bbox
        })
    }
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool {
        self.focus_under(*point).is_some()
    }
    fn set_activate(&self, activated: bool) {
        SpaceElement::set_activate(&self.0, activated);
        let changed = self.0.with_program(|p| {
            if !p.group_focused.load(Ordering::SeqCst) {
                p.windows
                    .lock()
                    .unwrap()
                    .iter()
                    .for_each(|w| SpaceElement::set_activate(w, activated))
            }
            p.activated.swap(activated, Ordering::SeqCst) != activated
        });

        if changed {
            self.0.force_redraw();
        }
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
        SpaceElement::refresh(&self.0);
        self.0.with_program(|p| {
            let mut windows = p.windows.lock().unwrap();

            // don't let the stack become empty
            let old_active = p.active.load(Ordering::SeqCst);
            let active = windows[old_active].clone();
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
            let active = p.active.load(Ordering::SeqCst);

            if old_active != active {
                self.0
                    .resize(Size::from((self.active().geometry().size.w, TAB_HEIGHT)));
                self.0.force_redraw();
            }

            windows.iter().enumerate().for_each(|(i, w)| {
                if i == active {
                    w.set_suspended(false);
                } else {
                    w.set_suspended(true);
                }
                w.send_configure();

                SpaceElement::refresh(w)
            });
        });
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
        self.0.force_redraw();
        self.0.with_program(|p| {
            p.group_focused.store(false, Ordering::SeqCst);
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
            if !p.group_focused.load(Ordering::SeqCst) {
                KeyboardTarget::key(
                    &p.windows.lock().unwrap()[active],
                    seat,
                    data,
                    key,
                    state,
                    serial,
                    time,
                )
            }
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
            if !p.group_focused.load(Ordering::SeqCst) {
                KeyboardTarget::modifiers(
                    &p.windows.lock().unwrap()[active],
                    seat,
                    data,
                    modifiers,
                    serial,
                )
            }
        })
    }
}

impl PointerTarget<State> for CosmicStack {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let active_window = &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)];
            let Some(next) = Focus::under(active_window, TAB_HEIGHT, event.location) else {
                return;
            };
            let _old_focus = p.swap_focus(Some(next));

            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(next.cursor_shape());
            let cursor_status = seat
                .user_data()
                .get::<RefCell<CursorImageStatus>>()
                .unwrap();
            *cursor_status.borrow_mut() = CursorImageStatus::default_named();
        });

        event.location -= self.0.with_program(|p| {
            p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)]
                .geometry()
                .loc
                .to_f64()
        });
        PointerTarget::enter(&self.0, seat, data, &event)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &MotionEvent) {
        let mut event = event.clone();
        self.0.with_program(|p| {
            let active = p.active.load(Ordering::SeqCst);
            let active_window = &p.windows.lock().unwrap()[active];
            let Some(next) = Focus::under(active_window, TAB_HEIGHT, event.location) else {
                return;
            };
            let _previous = p.swap_focus(Some(next));

            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(next.cursor_shape());
            let cursor_status = seat
                .user_data()
                .get::<RefCell<CursorImageStatus>>()
                .unwrap();
            *cursor_status.borrow_mut() = CursorImageStatus::default_named();
        });

        let active_window_geo = self.0.with_program(|p| {
            p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].geometry()
        });
        event.location -= active_window_geo.loc.to_f64();

        PointerTarget::motion(&self.0, seat, data, &event);
        if event.location.y < 0.0
            || event.location.y > TAB_HEIGHT as f64
            || event.location.x < 64.0
            || event.location.x > (active_window_geo.size.w as f64 - 64.0)
        {
            self.start_drag(data, seat, event.serial);
        }
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
            Some(Focus::Header) => {
                self.0.with_program(|p| {
                    *p.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
                });
                PointerTarget::button(&self.0, seat, data, event)
            }
            Some(x) => {
                let serial = event.serial;
                let seat = seat.clone();
                let Some(surface) = self.0.with_program(|p| {
                    let window = &p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)];
                    window.wl_surface()
                }) else {
                    return;
                };
                self.0.loop_handle().insert_idle(move |state| {
                    let res = state.common.shell.write().unwrap().resize_request(
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
        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => PointerTarget::axis(&self.0, seat, data, frame),
            _ => {}
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self.0.with_program(|p| p.current_focus()) {
            Some(Focus::Header) => PointerTarget::frame(&self.0, seat, data),
            _ => {}
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        self.0.with_program(|p| {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(CursorShape::Default);
            let _previous = p.swap_focus(None);
        });

        PointerTarget::leave(&self.0, seat, data, serial, time);

        if let Some(dragged_out) = self
            .0
            .with_program(|p| p.potential_drag.lock().unwrap().take())
        {
            if let Some(surface) = self
                .0
                .with_program(|p| p.windows.lock().unwrap().get(dragged_out).cloned())
            {
                let seat = seat.clone();
                surface.try_force_undecorated(false);
                surface.send_configure();
                if let Some(surface) = surface.wl_surface() {
                    let _ = data.common.event_loop_handle.insert_idle(move |state| {
                        let res = state.common.shell.write().unwrap().move_request(
                            &surface,
                            &seat,
                            serial,
                            ReleaseMode::NoMouseButtons,
                            true,
                            &state.common.config,
                            &state.common.event_loop_handle,
                            &state.common.xdg_activation_state,
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

impl TouchTarget<State> for CosmicStack {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, seq: Serial) {
        let mut event = event.clone();
        let active_window_geo = self.0.with_program(|p| {
            *p.last_seat.lock().unwrap() = Some((seat.clone(), event.serial));
            p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].geometry()
        });
        event.location -= active_window_geo.loc.to_f64();
        TouchTarget::down(&self.0, seat, data, &event, seq)
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent, seq: Serial) {
        TouchTarget::up(&self.0, seat, data, &event, seq)
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent, seq: Serial) {
        let mut event = event.clone();
        let active_window_geo = self.0.with_program(|p| {
            p.windows.lock().unwrap()[p.active.load(Ordering::SeqCst)].geometry()
        });
        event.location -= active_window_geo.loc.to_f64();
        TouchTarget::motion(&self.0, seat, data, &event, seq);

        if event.location.y < 0.0
            || event.location.y > TAB_HEIGHT as f64
            || event.location.x < 64.0
            || event.location.x > (active_window_geo.size.w as f64 - 64.0)
        {
            self.start_drag(data, seat, seq);
        }
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

render_elements! {
    pub CosmicStackRenderElement<R> where R: ImportAll + ImportMem;
    Header = MemoryRenderBufferRenderElement<R>,
    Window = WaylandSurfaceRenderElement<R>,
}
