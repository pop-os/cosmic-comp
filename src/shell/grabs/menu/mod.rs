use std::{
    fmt,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};

use calloop::LoopHandle;
use cosmic::{
    iced::{Alignment, Background},
    iced_core::{alignment::Horizontal, Border, Length, Rectangle as IcedRectangle},
    iced_widget::{self, text::Style as TextStyle, Column, Row},
    theme,
    widget::{button, divider, horizontal_space, icon::from_name, text},
    Apply as _, Task,
};
use smithay::{
    backend::{
        input::{ButtonState, TouchSlot},
        renderer::{
            element::{memory::MemoryRenderBufferRenderElement, AsRenderElements},
            ImportMem, Renderer,
        },
    },
    desktop::space::SpaceElement,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent as PointerMotionEvent, PointerGrab,
            PointerInnerHandle, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, GrabStartData as TouchGrabStartData, MotionEvent as TouchMotionEvent,
            TouchGrab, TouchInnerHandle, TouchTarget, UpEvent,
        },
        Seat,
    },
    output::Output,
    utils::{Logical, Point, Rectangle, Serial, Size},
};

use crate::{
    shell::{focus::target::PointerFocusTarget, SeatExt},
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::*,
    },
};

use super::{GrabStartData, ResizeEdge};

mod default;
mod item;
pub use self::default::*;

pub struct MenuGrabState {
    elements: Arc<Mutex<Vec<Element>>>,
    screen_space_relative: Option<Output>,
    scale: Arc<Mutex<f64>>,
}
pub type SeatMenuGrabState = Mutex<Option<MenuGrabState>>;

impl MenuGrabState {
    pub fn render<I, R>(&self, renderer: &mut R, output: &Output) -> Vec<I>
    where
        R: Renderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
        I: From<MemoryRenderBufferRenderElement<R>>,
    {
        let scale = output.current_scale().fractional_scale();
        self.elements
            .lock()
            .unwrap()
            .iter()
            .flat_map(|elem| {
                elem.iced.render_elements(
                    renderer,
                    elem.position
                        .to_local(output)
                        .as_logical()
                        .to_physical_precise_round(scale),
                    scale.into(),
                    1.0,
                )
            })
            .collect()
    }

    pub fn is_in_screen_space(&self) -> bool {
        self.screen_space_relative.is_some()
    }

    pub fn set_theme(&self, theme: cosmic::Theme) {
        for element in &*self.elements.lock().unwrap() {
            element.iced.set_theme(theme.clone())
        }
    }
}

#[derive(Clone)]
pub enum Item {
    Separator,
    Submenu {
        title: String,
        items: Vec<Item>,
    },
    Entry {
        title: String,
        shortcut: Option<String>,
        on_press: Arc<Box<dyn Fn(&LoopHandle<'_, State>) + Send + Sync>>,
        toggled: bool,
        submenu: bool,
        disabled: bool,
    },
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Separator => write!(f, "Separator"),
            Self::Submenu { title, items } => f
                .debug_struct("Submenu")
                .field("title", title)
                .field("items", items)
                .finish(),
            Self::Entry {
                title,
                shortcut,
                on_press: _,
                toggled,
                submenu,
                disabled,
            } => f
                .debug_struct("Entry")
                .field("title", title)
                .field("shortcut", shortcut)
                .field("on_press", &"...")
                .field("toggled", toggled)
                .field("submenu", submenu)
                .field("disabled", disabled)
                .finish(),
        }
    }
}

impl Item {
    pub fn new<S: Into<String>, F: Fn(&LoopHandle<'_, State>) + Send + Sync + 'static>(
        title: S,
        on_press: F,
    ) -> Item {
        Item::Entry {
            title: title.into(),
            shortcut: None,
            on_press: Arc::new(Box::new(on_press)),
            toggled: false,
            submenu: false,
            disabled: false,
        }
    }

    pub fn new_submenu<S: Into<String>>(title: S, items: Vec<Item>) -> Item {
        Item::Submenu {
            title: title.into(),
            items,
        }
    }

    pub fn shortcut(mut self, shortcut: impl Into<Option<String>>) -> Self {
        if let Item::Entry {
            shortcut: ref mut s,
            ..
        } = self
        {
            *s = shortcut.into();
        }
        self
    }

    pub fn toggled(mut self, toggled: bool) -> Self {
        if let Item::Entry {
            toggled: ref mut t, ..
        } = self
        {
            *t = toggled;
        }
        self
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        if let Item::Entry {
            disabled: ref mut d,
            ..
        } = self
        {
            *d = disabled;
        }
        self
    }
}

/// Menu that comes up when right-clicking an application header bar
#[derive(Debug)]
pub struct ContextMenu {
    items: Vec<Item>,
    selected: AtomicBool,
    row_width: Mutex<Option<f32>>,
}

impl ContextMenu {
    pub fn new(items: Vec<Item>) -> ContextMenu {
        ContextMenu {
            items,
            selected: AtomicBool::new(false),
            row_width: Mutex::new(None),
        }
    }

    pub fn set_row_width(&self, width: f32) {
        *self.row_width.lock().unwrap() = Some(width);
    }
}

#[derive(Debug, Clone)]
pub enum Message {
    ItemEntered(usize, IcedRectangle<f32>),
    ItemPressed(usize),
    ItemLeft(usize, IcedRectangle<f32>),
}

impl item::CursorEvents for Message {
    fn cursor_entered(idx: usize, bounds: IcedRectangle<f32>) -> Self {
        Message::ItemEntered(idx, bounds)
    }

    fn cursor_left(idx: usize, bounds: IcedRectangle<f32>) -> Self {
        Message::ItemLeft(idx, bounds)
    }
}

impl Program for ContextMenu {
    type Message = Message;

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, crate::state::State>,
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> Task<Self::Message> {
        match message {
            Message::ItemPressed(idx) => {
                if let Some(Item::Entry { on_press, .. }) = self.items.get_mut(idx) {
                    (on_press)(loop_handle);
                    self.selected.store(true, Ordering::SeqCst);
                }
                // TODO: If Submenu, then also expand on "Pressed" for touch events.
                // But right now we don't have any touch responsive menus with submenus
            }
            Message::ItemEntered(idx, bounds) => {
                if let Some(Item::Submenu { items, .. }) = self.items.get_mut(idx) {
                    if let Some((seat, _)) = last_seat.cloned() {
                        let items = items.clone();
                        let _ = loop_handle.insert_idle(move |state| {
                            let grab_state = seat
                                .user_data()
                                .get::<SeatMenuGrabState>()
                                .unwrap()
                                .lock()
                                .unwrap();

                            if let Some(grab_state) = &*grab_state {
                                let mut elements = grab_state.elements.lock().unwrap();

                                let position = elements.last().unwrap().position;
                                let element = IcedElement::new(
                                    ContextMenu::new(items),
                                    Size::default(),
                                    state.common.event_loop_handle.clone(),
                                    state.common.theme.clone(),
                                );

                                let min_size = element.minimum_size();
                                element.with_program(|p| {
                                    *p.row_width.lock().unwrap() = Some(min_size.w as f32);
                                });
                                element.resize(min_size);

                                let output = seat.active_output();
                                let position = [
                                    // to the right -> down
                                    Rectangle::new(
                                        position
                                            + Point::from((
                                                bounds.width.ceil() as i32,
                                                bounds.y.ceil() as i32,
                                            )),
                                        min_size.as_global(),
                                    ),
                                    // to the right -> up
                                    Rectangle::new(
                                        position
                                            + Point::from((
                                                bounds.width.ceil() as i32,
                                                bounds.y.ceil() as i32
                                                    + bounds.height.ceil() as i32
                                                    - min_size.h,
                                            )),
                                        min_size.as_global(),
                                    ),
                                    // to the left -> down
                                    Rectangle::new(
                                        position
                                            + Point::from((-min_size.w, bounds.y.ceil() as i32)),
                                        min_size.as_global(),
                                    ),
                                    // to the left -> up
                                    Rectangle::new(
                                        position
                                            + Point::from((
                                                -min_size.w,
                                                bounds.y.ceil() as i32
                                                    + bounds.height.ceil() as i32
                                                    - min_size.h,
                                            )),
                                        min_size.as_global(),
                                    ),
                                ]
                                .iter()
                                .rev() // preference of max_by_key is backwards
                                .max_by_key(|rect| {
                                    output
                                        .geometry()
                                        .intersection(**rect)
                                        .map(|rect| rect.size.w * rect.size.h)
                                })
                                .unwrap()
                                .loc;
                                element.output_enter(&output, element.bbox());
                                element.set_additional_scale(*grab_state.scale.lock().unwrap());

                                elements.push(Element {
                                    iced: element,
                                    position,
                                    pointer_entered: false,
                                    touch_entered: None,
                                })
                            }
                        });
                    }
                }
            }
            Message::ItemLeft(idx, _) => {
                if let Some(Item::Submenu { .. }) = self.items.get_mut(idx) {
                    if let Some((seat, _)) = last_seat.cloned() {
                        let _ = loop_handle.insert_idle(move |_| {
                            let grab_state = seat
                                .user_data()
                                .get::<SeatMenuGrabState>()
                                .unwrap()
                                .lock()
                                .unwrap();

                            if let Some(grab_state) = &*grab_state {
                                let mut elements = grab_state.elements.lock().unwrap();
                                elements.pop();
                            }
                        });
                    }
                }
            }
        };

        Task::none()
    }

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        let width = self
            .row_width
            .lock()
            .unwrap()
            .map(|size| Length::Fixed(size))
            .unwrap_or(Length::Shrink);
        let mode = match width {
            Length::Shrink => Length::Shrink,
            _ => Length::Fill,
        };

        Column::with_children(self.items.iter().enumerate().map(|(idx, item)| {
            match item {
                Item::Separator => divider::horizontal::light().into(),
                Item::Submenu { title, .. } => Row::with_children(vec![
                    horizontal_space().width(16).into(),
                    text::body(title).width(mode).into(),
                    from_name("go-next-symbolic")
                        .size(16)
                        .prefer_svg(true)
                        .icon()
                        .into(),
                ])
                .spacing(8)
                .width(width)
                .padding([8, 16])
                .align_y(Alignment::Center)
                .apply(|row| item::SubmenuItem::new(row, idx))
                .style(theme::Button::MenuItem)
                .into(),
                Item::Entry {
                    title,
                    shortcut,
                    toggled,
                    disabled,
                    ..
                } => {
                    let mut components = vec![
                        if *toggled {
                            from_name("object-select-symbolic")
                                .size(16)
                                .prefer_svg(true)
                                .icon()
                                .class(theme::Svg::custom(|theme| iced_widget::svg::Style {
                                    color: Some(theme.cosmic().accent.base.into()),
                                }))
                                .into()
                        } else {
                            horizontal_space().width(16).into()
                        },
                        text::body(title)
                            .width(mode)
                            .class(if *disabled {
                                theme::Text::Custom(|theme| {
                                    let mut color = theme.cosmic().background.component.on;
                                    color.alpha *= 0.5;
                                    TextStyle {
                                        color: Some(color.into()),
                                    }
                                })
                            } else {
                                theme::Text::Default
                            })
                            .into(),
                        horizontal_space().width(16).into(),
                    ];
                    if let Some(shortcut) = shortcut.as_ref() {
                        components.push(
                            text::body(shortcut)
                                .align_x(Horizontal::Right)
                                .width(Length::Shrink)
                                .class(theme::Text::Custom(|theme| {
                                    let mut color = theme.cosmic().background.component.on;
                                    color.alpha *= 0.75;
                                    TextStyle {
                                        color: Some(color.into()),
                                    }
                                }))
                                .into(),
                        );
                    }

                    Row::with_children(components)
                        .spacing(8)
                        .width(mode)
                        .align_y(Alignment::Center)
                        .apply(button::custom)
                        .width(width)
                        .padding([8, 16])
                        .on_press_maybe((!disabled).then_some(Message::ItemPressed(idx)))
                        .class(theme::Button::MenuItem)
                        .into()
                }
            }
        }))
        .width(Length::Shrink)
        .apply(iced_widget::container)
        .padding(1)
        .class(theme::Container::custom(|theme| {
            let cosmic = theme.cosmic();
            let component = &cosmic.background.component;
            iced_widget::container::Style {
                icon_color: Some(cosmic.accent.base.into()),
                text_color: Some(component.on.into()),
                background: Some(Background::Color(component.base.into())),
                border: Border {
                    radius: cosmic.radius_s().into(),
                    width: 1.0,
                    color: component.divider.into(),
                },
                shadow: Default::default(),
            }
        }))
        .width(Length::Shrink)
        .into()
    }
}

pub struct Element {
    iced: IcedElement<ContextMenu>,
    position: Point<i32, Global>,
    pointer_entered: bool,
    touch_entered: Option<TouchSlot>,
}

pub struct MenuGrab {
    elements: Arc<Mutex<Vec<Element>>>,
    start_data: GrabStartData,
    seat: Seat<State>,
    screen_space_relative: Option<Output>,
    scale: Arc<Mutex<f64>>,
}

impl PointerGrab<State> for MenuGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &PointerMotionEvent,
    ) {
        {
            let mut guard = self.elements.lock().unwrap();
            let elements = &mut *guard;
            let event_location = if let Some(output) = self.screen_space_relative.as_ref() {
                if state.common.shell.read().zoom_state().is_some() {
                    event
                        .location
                        .as_global()
                        .to_zoomed(output)
                        .to_global(output)
                        .as_logical()
                } else {
                    event.location
                }
            } else {
                event.location
            };

            if let Some(i) = elements.iter().position(|elem| {
                let mut bbox = elem.iced.bbox();
                bbox.loc = elem.position.as_logical();

                bbox.contains(event_location.to_i32_round())
            }) {
                let element = &mut elements[i];

                let new_event = PointerMotionEvent {
                    location: event_location - element.position.as_logical().to_f64(),
                    serial: event.serial,
                    time: event.time,
                };
                if !element.pointer_entered {
                    PointerTarget::enter(&element.iced, &self.seat, state, &new_event);
                    element.pointer_entered = true;
                } else {
                    PointerTarget::motion(&element.iced, &self.seat, state, &new_event);
                }
            } else {
                elements.iter_mut().for_each(|element| {
                    if element.pointer_entered {
                        PointerTarget::leave(
                            &element.iced,
                            &self.seat,
                            state,
                            event.serial,
                            event.time,
                        );
                        element.pointer_entered = false;
                    }
                })
            }
        }
        handle.motion(state, None, event);
    }

    fn relative_motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &RelativeMotionEvent,
    ) {
        // While the grab is active, no client has pointer focus
        handle.relative_motion(state, None, event);
    }

    fn button(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &ButtonEvent,
    ) {
        let any_entered = self
            .elements
            .lock()
            .unwrap()
            .iter()
            .any(|elem| elem.pointer_entered);
        if !any_entered {
            if event.state == ButtonState::Pressed {
                handle.unset_grab(self, state, event.serial, event.time, true);
            }
        } else {
            let selected = {
                let elements = self.elements.lock().unwrap();
                let mut selected = false;
                for element in elements.iter().filter(|elem| elem.pointer_entered) {
                    PointerTarget::button(&element.iced, &self.seat, state, event);
                    selected = true;
                }
                selected
            };
            if selected && event.state == ButtonState::Released {
                handle.unset_grab(self, state, event.serial, event.time, true);
            } else {
                handle.button(state, event);
            }
        }
    }

    fn axis(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        details: AxisFrame,
    ) {
        handle.axis(state, details);
    }

    fn frame(&mut self, data: &mut State, handle: &mut PointerInnerHandle<'_, State>) {
        handle.frame(data)
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event)
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event)
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event)
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event)
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event)
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event)
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event)
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        event: &GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event)
    }

    fn start_data(&self) -> &PointerGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Pointer(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl TouchGrab<State> for MenuGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &DownEvent,
        seq: Serial,
    ) {
        {
            let mut guard = self.elements.lock().unwrap();
            let elements = &mut *guard;
            let event_location = if let Some(output) = self.screen_space_relative.as_ref() {
                if data.common.shell.read().zoom_state().is_some() {
                    event
                        .location
                        .as_global()
                        .to_zoomed(output)
                        .to_global(output)
                        .as_logical()
                } else {
                    event.location
                }
            } else {
                event.location
            };

            if let Some(i) = elements.iter().position(|elem| {
                let mut bbox = elem.iced.bbox();
                bbox.loc = elem.position.as_logical();

                bbox.contains(event_location.to_i32_round())
            }) {
                let element = &mut elements[i];

                let new_event = DownEvent {
                    slot: event.slot,
                    location: event_location - element.position.as_logical().to_f64(),
                    serial: event.serial,
                    time: event.time,
                };
                if element.touch_entered.is_none() {
                    TouchTarget::down(&element.iced, &self.seat, data, &new_event, seq);
                    element.touch_entered = Some(event.slot);
                }
            }
        }
        handle.down(data, None, event, seq);
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &UpEvent,
        seq: Serial,
    ) {
        {
            let elements = self.elements.lock().unwrap();
            for element in elements.iter().filter(|elem| {
                elem.touch_entered
                    .as_ref()
                    .is_some_and(|slot| *slot == event.slot)
            }) {
                TouchTarget::up(&element.iced, &self.seat, data, event, seq);
            }
        }
        handle.unset_grab(self, data);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &TouchMotionEvent,
        seq: Serial,
    ) {
        {
            let elements = self.elements.lock().unwrap();
            for element in elements.iter().filter(|elem| {
                elem.touch_entered
                    .as_ref()
                    .is_some_and(|slot| *slot == event.slot)
            }) {
                TouchTarget::motion(&element.iced, &self.seat, data, event, seq);
            }
        }
        handle.motion(data, None, event, seq);
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        handle.frame(data, seq);
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        {
            let mut elements = self.elements.lock().unwrap();
            for element in elements.iter_mut() {
                let _ = element.touch_entered.take();
            }
        }
        handle.cancel(data, seq);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &smithay::input::touch::ShapeEvent,
        seq: Serial,
    ) {
        handle.shape(data, event, seq);
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &smithay::input::touch::OrientationEvent,
        seq: Serial,
    ) {
        handle.orientation(data, event, seq);
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MenuAlignment {
    pub x: AxisAlignment,
    pub y: AxisAlignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AxisAlignment {
    Corner(u32),
    Centered,
    PreferCentered,
}

impl MenuAlignment {
    pub const CORNER: Self = MenuAlignment {
        x: AxisAlignment::Corner(0),
        y: AxisAlignment::Corner(0),
    };
    pub const PREFER_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::PreferCentered,
        y: AxisAlignment::PreferCentered,
    };
    pub const CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Centered,
        y: AxisAlignment::Centered,
    };
    pub const HORIZONTALLY_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Centered,
        y: AxisAlignment::Corner(0),
    };
    pub const VERTICALLY_CENTERED: Self = MenuAlignment {
        x: AxisAlignment::Corner(0),
        y: AxisAlignment::Centered,
    };

    pub fn horizontally_centered(offset: u32, fixed: bool) -> MenuAlignment {
        MenuAlignment {
            x: if fixed {
                AxisAlignment::Centered
            } else {
                AxisAlignment::PreferCentered
            },
            y: AxisAlignment::Corner(offset),
        }
    }

    pub fn vertically_centered(offset: u32, fixed: bool) -> MenuAlignment {
        MenuAlignment {
            x: AxisAlignment::Corner(offset),
            y: if fixed {
                AxisAlignment::Centered
            } else {
                AxisAlignment::PreferCentered
            },
        }
    }

    fn rectangles(
        &self,
        position: Point<i32, Global>,
        size: Size<i32, Global>,
    ) -> Vec<Rectangle<i32, Global>> {
        fn for_alignment(
            position: Point<i32, Global>,
            size: Size<i32, Global>,
            x: AxisAlignment,
            y: AxisAlignment,
        ) -> Vec<Rectangle<i32, Global>> {
            match (x, y) {
                (AxisAlignment::Corner(x_offset), AxisAlignment::Corner(y_offset)) => {
                    let offset = Point::from((x_offset as i32, y_offset as i32));
                    vec![
                        Rectangle::new(position + offset, size), // normal
                        Rectangle::new(
                            position - Point::from((size.w, 0))
                                + Point::from((-(x_offset as i32), y_offset as i32)),
                            size,
                        ), // flipped left
                        Rectangle::new(
                            position
                                - Point::from((0, size.h))
                                - Point::from((x_offset as i32, -(y_offset as i32))),
                            size,
                        ), // flipped up
                        Rectangle::new(position - size.to_point() - offset, size), // flipped left & up
                    ]
                }
                (AxisAlignment::Centered, AxisAlignment::Corner(offset)) => {
                    let x = position.x - ((size.w as f64 / 2.).round() as i32);
                    vec![
                        Rectangle::new(Point::from((x, position.y + offset as i32)), size), // below
                        Rectangle::new(Point::from((x, position.y - size.h - offset as i32)), size), // above
                    ]
                }
                (AxisAlignment::Corner(offset), AxisAlignment::Centered) => {
                    let y = position.y - ((size.h as f64 / 2.).round() as i32);
                    vec![
                        Rectangle::new(Point::from((position.x + offset as i32, y)), size), // left
                        Rectangle::new(Point::from((position.x - size.w - offset as i32, y)), size), // right
                    ]
                }
                (AxisAlignment::Centered, AxisAlignment::Centered) => {
                    vec![Rectangle::new(
                        position - size.to_f64().downscale(2.).to_i32_round().to_point(),
                        size,
                    )]
                }
                (AxisAlignment::PreferCentered, AxisAlignment::PreferCentered) => for_alignment(
                    position,
                    size,
                    AxisAlignment::Centered,
                    AxisAlignment::Centered,
                )
                .into_iter()
                .chain(
                    for_alignment(
                        position,
                        size,
                        AxisAlignment::Centered,
                        AxisAlignment::Corner(0),
                    )
                    .into_iter(),
                )
                .chain(
                    for_alignment(
                        position,
                        size,
                        AxisAlignment::Corner(0),
                        AxisAlignment::Centered,
                    )
                    .into_iter(),
                )
                .chain(
                    for_alignment(
                        position,
                        size,
                        AxisAlignment::Corner(0),
                        AxisAlignment::Corner(0),
                    )
                    .into_iter(),
                )
                .collect(),
                (AxisAlignment::PreferCentered, y) => {
                    for_alignment(position, size, AxisAlignment::Centered, y)
                        .into_iter()
                        .chain(
                            for_alignment(position, size, AxisAlignment::Corner(0), y).into_iter(),
                        )
                        .collect()
                }
                (x, AxisAlignment::PreferCentered) => {
                    for_alignment(position, size, x, AxisAlignment::Centered)
                        .into_iter()
                        .chain(
                            for_alignment(position, size, x, AxisAlignment::Corner(0)).into_iter(),
                        )
                        .collect()
                }
            }
        }

        for_alignment(position, size, self.x, self.y)
    }
}

impl MenuGrab {
    pub fn new(
        start_data: GrabStartData,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        position: Point<i32, Global>,
        alignment: MenuAlignment,
        screen_space_relative: Option<f64>,
        handle: LoopHandle<'static, crate::state::State>,
        theme: cosmic::Theme,
    ) -> MenuGrab {
        let items = items.collect::<Vec<_>>();
        let element = IcedElement::new(ContextMenu::new(items), Size::default(), handle, theme);
        let min_size = element.minimum_size();
        element.with_program(|p| {
            *p.row_width.lock().unwrap() = Some(min_size.w as f32);
        });
        element.resize(min_size);

        let output = seat.active_output();
        // TODO: This feels a lot like cheap xdg-positioner. Refactor and unify
        let position = alignment
            .rectangles(
                position,
                min_size
                    .to_f64()
                    .upscale(screen_space_relative.unwrap_or(1.))
                    .to_i32_round()
                    .as_global(),
            )
            .iter()
            .rev() // preference of max_by_key is backwards
            .max_by_key(|rect| {
                output
                    .geometry()
                    .intersection(**rect)
                    .map(|rect| rect.size.w * rect.size.h)
            })
            .unwrap()
            .loc;

        element.output_enter(&output, element.bbox());
        if let Some(scale) = screen_space_relative {
            element.set_additional_scale(scale);
        }

        let elements = Arc::new(Mutex::new(vec![Element {
            iced: element,
            position,
            pointer_entered: false,
            touch_entered: None,
        }]));

        let scale = Arc::new(Mutex::new(screen_space_relative.unwrap_or(1.)));
        let screen_space_relative = screen_space_relative.is_some().then_some(output);

        let grab_state = MenuGrabState {
            elements: elements.clone(),
            screen_space_relative: screen_space_relative.clone(),
            scale: scale.clone(),
        };

        *seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap() = Some(grab_state);

        MenuGrab {
            elements,
            start_data,
            seat: seat.clone(),
            screen_space_relative,
            scale,
        }
    }

    pub fn set_additional_scale(&self, scale: f64) {
        *self.scale.lock().unwrap() = scale;
        for element in &*self.elements.lock().unwrap() {
            element.iced.set_additional_scale(scale);
        }
    }

    pub fn is_touch_grab(&self) -> bool {
        match self.start_data {
            GrabStartData::Touch(_) => true,
            GrabStartData::Pointer(_) => false,
        }
    }
}

impl Drop for MenuGrab {
    fn drop(&mut self) {
        self.seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .lock()
            .unwrap()
            .take();
    }
}
