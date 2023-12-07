use std::{
    cell::RefCell,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};

use anyhow::Context;
use calloop::LoopHandle;
use cosmic::{
    iced::Background,
    iced_core::{alignment::Horizontal, Length, Rectangle as IcedRectangle},
    iced_widget::{self, horizontal_rule, text::Appearance as TextAppearance, Column, Row},
    theme,
    widget::{button, horizontal_space, icon::from_name, text},
    Apply as _, Command,
};
use smithay::{
    backend::{
        allocator::Fourcc,
        input::ButtonState,
        renderer::{
            damage::OutputDamageTracker,
            element::{
                memory::MemoryRenderBufferRenderElement, surface::WaylandSurfaceRenderElement,
                AsRenderElements,
            },
            gles::GlesRenderbuffer,
            ExportMem, ImportAll, ImportMem, Offscreen, Renderer,
        },
    },
    desktop::{space::SpaceElement, utils::bbox_from_surface_tree},
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            PointerTarget, RelativeMotionEvent,
        },
        Seat,
    },
    output::Output,
    utils::{Logical, Point, Scale, Size, Transform},
    wayland::seat::WaylandFocus,
};
use tracing::warn;

use crate::{
    backend::kms::source_node_for_surface,
    config::{Action, StaticConfig},
    fl,
    shell::{
        element::{CosmicMapped, CosmicSurface},
        focus::target::PointerFocusTarget,
        grabs::ReleaseMode,
        Shell,
    },
    state::{BackendData, Common, State},
    utils::{
        iced::{IcedElement, Program},
        prelude::{Global, PointGlobalExt, PointLocalExt, SeatExt},
    },
};

use super::ResizeEdge;

mod item;

pub struct MenuGrabState {
    elements: Arc<Mutex<Vec<Element>>>,
}
pub type SeatMenuGrabState = RefCell<Option<MenuGrabState>>;

impl MenuGrabState {
    pub fn render<I, R>(&self, renderer: &mut R, output: &Output) -> Vec<I>
    where
        R: Renderer + ImportMem,
        <R as Renderer>::TextureId: 'static,
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
    ) -> Command<Self::Message> {
        match message {
            Message::ItemPressed(idx) => {
                if let Some(Item::Entry { on_press, .. }) = self.items.get_mut(idx) {
                    (on_press)(loop_handle);
                    self.selected.store(true, Ordering::SeqCst);
                }
            }
            Message::ItemEntered(idx, bounds) => {
                if let Some(Item::Submenu { items, .. }) = self.items.get_mut(idx) {
                    let items = items.clone();
                    let _ = loop_handle.insert_idle(move |state| {
                        let seat = state.common.last_active_seat();
                        let grab_state = seat
                            .user_data()
                            .get::<SeatMenuGrabState>()
                            .unwrap()
                            .borrow_mut();

                        if let Some(grab_state) = &*grab_state {
                            let mut elements = grab_state.elements.lock().unwrap();
                            let mut position = elements.last().unwrap().position;
                            position.x += bounds.width.ceil() as i32;
                            position.y += bounds.y.ceil() as i32;

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
                            element.output_enter(&seat.active_output(), element.bbox());

                            elements.push(Element {
                                iced: element,
                                position,
                                pointer_entered: false,
                            })
                        }
                    });
                }
            }
            Message::ItemLeft(idx, _) => {
                if let Some(Item::Submenu { .. }) = self.items.get_mut(idx) {
                    let _ = loop_handle.insert_idle(|state| {
                        let seat = state.common.last_active_seat();
                        let grab_state = seat
                            .user_data()
                            .get::<SeatMenuGrabState>()
                            .unwrap()
                            .borrow_mut();

                        if let Some(grab_state) = &*grab_state {
                            let mut elements = grab_state.elements.lock().unwrap();
                            elements.pop();
                        }
                    });
                }
            }
        };

        Command::none()
    }

    fn view(&self) -> crate::utils::iced::Element<'_, Self::Message> {
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

        Column::with_children(
            self.items
                .iter()
                .enumerate()
                .map(|(idx, item)| match item {
                    Item::Separator => horizontal_rule(1)
                        .style(theme::Rule::LightDivider)
                        .width(Length::Shrink)
                        .into(),
                    Item::Submenu { title, .. } => Row::with_children(vec![
                        horizontal_space(16).into(),
                        text(title).width(mode).into(),
                        from_name("go-next-symbolic")
                            .size(16)
                            .prefer_svg(true)
                            .icon()
                            .into(),
                    ])
                    .spacing(8)
                    .width(width)
                    .padding([8, 24])
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
                                    .style(theme::Svg::custom(|theme| {
                                        iced_widget::svg::Appearance {
                                            color: Some(theme.cosmic().accent.base.into()),
                                        }
                                    }))
                                    .into()
                            } else {
                                horizontal_space(16).into()
                            },
                            text(title).width(mode).into(),
                        ];
                        if let Some(shortcut) = shortcut.as_ref() {
                            components.push(
                                text(shortcut)
                                    .line_height(20.)
                                    .size(14)
                                    .horizontal_alignment(Horizontal::Right)
                                    .width(Length::Shrink)
                                    .style(theme::Text::Custom(|theme| {
                                        let mut color = theme.cosmic().background.component.on;
                                        color.alpha *= 0.75;
                                        TextAppearance {
                                            color: Some(color.into()),
                                        }
                                    }))
                                    .into(),
                            );
                        }
                        components.push(horizontal_space(16).into());

                        Row::with_children(components)
                            .spacing(8)
                            .width(mode)
                            .apply(button)
                            .width(width)
                            .padding([8, 24])
                            .on_press_maybe((!disabled).then_some(Message::ItemPressed(idx)))
                            .style(theme::Button::MenuItem)
                            .into()
                    }
                })
                .collect(),
        )
        .width(Length::Shrink)
        .apply(iced_widget::container)
        .padding(1)
        .style(theme::Container::custom(|theme| {
            let cosmic = theme.cosmic();
            let component = &cosmic.background.component;
            iced_widget::container::Appearance {
                icon_color: Some(cosmic.accent.base.into()),
                text_color: Some(component.on.into()),
                background: Some(Background::Color(component.base.into())),
                border_radius: 8.0.into(),
                border_width: 1.0,
                border_color: component.divider.into(),
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
}

pub struct MenuGrab {
    elements: Arc<Mutex<Vec<Element>>>,
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
}

impl PointerGrab<State> for MenuGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        {
            let mut guard = self.elements.lock().unwrap();
            let elements = &mut *guard;
            if let Some(i) = elements.iter().position(|elem| {
                let mut bbox = elem.iced.bbox();
                bbox.loc = elem.position.as_logical();

                bbox.contains(event.location.to_i32_round())
            }) {
                let element = &mut elements[i];

                let new_event = MotionEvent {
                    location: event.location - element.position.as_logical().to_f64(),
                    serial: event.serial,
                    time: event.time,
                };
                if !element.pointer_entered {
                    PointerTarget::enter(&element.iced, &self.seat, state, &new_event);
                    element.pointer_entered = true;
                } else {
                    element.iced.motion(&self.seat, state, &new_event);
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
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
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
                handle.unset_grab(state, event.serial, event.time, true);
            }
        } else {
            let selected = {
                let elements = self.elements.lock().unwrap();
                let mut selected = false;
                for element in elements.iter().filter(|elem| elem.pointer_entered) {
                    element.iced.button(&self.seat, state, event);
                    selected = true;
                }
                selected
            };
            if selected && event.state == ButtonState::Released {
                handle.unset_grab(state, event.serial, event.time, true);
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
        &self.start_data
    }
}

impl MenuGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        seat: &Seat<State>,
        items: impl Iterator<Item = Item>,
        position: Point<i32, Global>,
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

        element.output_enter(&seat.active_output(), element.bbox());

        let elements = Arc::new(Mutex::new(vec![Element {
            iced: element,
            position,
            pointer_entered: false,
        }]));

        let grab_state = MenuGrabState {
            elements: elements.clone(),
        };

        *seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .borrow_mut() = Some(grab_state);

        MenuGrab {
            elements,
            start_data,
            seat: seat.clone(),
        }
    }
}

impl Drop for MenuGrab {
    fn drop(&mut self) {
        self.seat
            .user_data()
            .get::<SeatMenuGrabState>()
            .unwrap()
            .borrow_mut()
            .take();
    }
}

pub fn window_items(
    window: &CosmicMapped,
    is_tiled: bool,
    is_stacked: bool,
    tiling_enabled: bool,
    possible_resizes: ResizeEdge,
    config: &StaticConfig,
) -> impl Iterator<Item = Item> {
    //let is_always_on_top = false; // TODO check window (potentially shell?)
    //let is_always_on_visible_ws = false; // TODO check window (potentially shell?)

    let maximize_clone = window.clone();
    let tile_clone = window.clone();
    let move_prev_clone = window.clone();
    let move_next_clone = window.clone();
    let move_clone = window.clone();
    let resize_top_clone = window.clone();
    let resize_left_clone = window.clone();
    let resize_right_clone = window.clone();
    let resize_bottom_clone = window.clone();
    let unstack_clone = window.clone();
    let screenshot_clone = window.clone();
    let stack_clone = window.clone();
    let close_clone = window.clone();

    vec![
        is_stacked.then_some(Item::new(fl!("window-menu-unstack"), move |handle| {
            let unstack_clone = unstack_clone.clone();
            let _ = handle.insert_idle(move |state| {
                let seat = state.common.last_active_seat().clone();
                let Some(ws) = state.common.shell.space_for_mut(&unstack_clone) else { return };
                if let Some(new_focus) = ws.toggle_stacking(&unstack_clone) {
                    Common::set_focus(state, Some(&new_focus), &seat, None);
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::ToggleStacking))),
        is_stacked.then_some(Item::Separator),
        //Some(Item::new(fl!("window-menu-minimize"), |handle| {})),
        Some(Item::new(fl!("window-menu-maximize"), move |handle| {
            let maximize_clone = maximize_clone.clone();
            let _ = handle.insert_idle(move |state| {
                if let Some(space) = state.common.shell.space_for_mut(&maximize_clone) {
                    if maximize_clone.is_maximized(false) {
                        space.unmaximize_request(&maximize_clone.active_window());
                    } else {
                        space.maximize_request(&maximize_clone.active_window());
                    }
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::Maximize))
        .toggled(window.is_maximized(false))),
        tiling_enabled.then_some(Item::new(fl!("window-menu-tiled"), move |handle| {
            let tile_clone = tile_clone.clone();
            let _ = handle.insert_idle(move |state| {
                let seat = state.common.last_active_seat().clone();
                if let Some(ws) = state.common.shell.space_for_mut(&tile_clone) {
                    ws.toggle_floating_window(&seat, &tile_clone);
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::ToggleWindowFloating))
        .toggled(!is_tiled)),
        Some(Item::Separator),
        // TODO: Where to save?
        Some(Item::new(fl!("window-menu-screenshot"), move |handle| {
            let screenshot_clone = screenshot_clone.clone();
            let _ = handle.insert_idle(move |state| {
                fn render_window<R>(
                    renderer: &mut R,
                    window: &CosmicSurface,
                    offset: &time::UtcOffset,
                ) -> anyhow::Result<()>
                where
                    R: Renderer
                        + ImportAll
                        + Offscreen<GlesRenderbuffer>
                        + ExportMem,
                    <R as Renderer>::TextureId: 'static,
                    <R as Renderer>::Error: Send + Sync + 'static,
                {
                    let bbox = bbox_from_surface_tree(&window.wl_surface().unwrap(), (0, 0));
                    let elements = AsRenderElements::<R>::render_elements::<WaylandSurfaceRenderElement<R>>(
                        window,
                        renderer,
                        (-bbox.loc.x, -bbox.loc.y).into(),
                        Scale::from(1.0),
                        1.0,
                    );

                    // TODO: 10-bit
                    let format = Fourcc::Abgr8888;
                    let render_buffer =
                        Offscreen::<GlesRenderbuffer>::create_buffer(renderer, format, bbox.size.to_buffer(1, Transform::Normal))?;
                    renderer.bind(render_buffer)?;
                    let mut output_damage_tracker = OutputDamageTracker::new(bbox.size.to_physical(1), 1.0, Transform::Normal);
                    output_damage_tracker.render_output(renderer, 0, &elements, [0.0, 0.0, 0.0, 0.0]).map_err(|err| match err {
                        smithay::backend::renderer::damage::Error::Rendering(err) => err,
                        smithay::backend::renderer::damage::Error::OutputNoMode(_) => unreachable!(),
                    })?;
                    let mapping = renderer.copy_framebuffer(bbox.to_buffer(1, Transform::Normal, &bbox.size), format)?;
                    let gl_data = renderer.map_texture(&mapping)?;

                    if let Ok(Some(path)) = xdg_user::pictures() {
                        let local_timestamp = time::OffsetDateTime::now_utc().to_offset(*offset);
                        let mut title = window.title();
                        title.truncate(227); // 255 - time - png
                        let name = sanitize_filename::sanitize(format!("{}_{}.png",
                            title,
                            local_timestamp.format(time::macros::format_description!("[year]-[month]-[day]_[hour]:[minute]:[second]_[subsecond digits:4]")).unwrap(),
                        ));
                        let file = std::fs::File::create(path.join(name))?;

                        let ref mut writer = std::io::BufWriter::new(file);
                        let mut encoder = png::Encoder::new(writer, bbox.size.w as u32, bbox.size.h as u32);
                        encoder.set_color(png::ColorType::Rgba);
                        encoder.set_depth(png::BitDepth::Eight);
                        encoder.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2));     // 1.0 / 2.2, unscaled, but rounded
                        let source_chromaticities = png::SourceChromaticities::new(     // Using unscaled instantiation here
                            (0.31270, 0.32900),
                            (0.64000, 0.33000),
                            (0.30000, 0.60000),
                            (0.15000, 0.06000)
                        );
                        encoder.set_source_chromaticities(source_chromaticities);
                        let mut writer = encoder.write_header()?;
                        writer.write_image_data(&gl_data)?;
                    }

                    Ok(())
                }

                if let Some(surface) = screenshot_clone.active_window().wl_surface() {
                    let res = match &mut state.backend {
                        BackendData::Kms(kms) => {
                            let node = source_node_for_surface(&surface, &state.common.display_handle)
                                .unwrap_or(kms.primary);
                            kms
                                .api
                                .single_renderer(&node)
                                .with_context(|| "Failed to get renderer for screenshot")
                                .and_then(|mut multirenderer| render_window(&mut multirenderer, &screenshot_clone.active_window(), &state.common.local_offset))
                        },
                        BackendData::Winit(winit) => {
                            render_window(winit.backend.renderer(), &screenshot_clone.active_window(), &state.common.local_offset)
                        },
                        BackendData::X11(x11) => {
                            render_window(&mut x11.renderer, &screenshot_clone.active_window(), &state.common.local_offset)
                        },
                        BackendData::Unset => unreachable!(),
                    };
                    if let Err(err) =  res {
                        warn!(?err, "Failed to take screenshot")
                    }
                }
            });
        })),
        Some(Item::Separator),
        Some(Item::new(fl!("window-menu-move"), move |handle| {
            let move_clone = move_clone.clone();
            let _ = handle.insert_idle(move |state| {
                if let Some(surface) = move_clone.wl_surface() {
                    let seat = state.common.last_active_seat().clone();
                    Shell::move_request(state, &surface, &seat, None, ReleaseMode::Click);
                }
            });
        })),
        Some(Item::new_submenu(fl!("window-menu-resize"), vec![
            Item::new(fl!("window-menu-resize-edge-top"), move |handle| {
                let resize_clone = resize_top_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let seat = state.common.last_active_seat().clone();
                    Shell::menu_resize_request(state, &resize_clone, &seat, ResizeEdge::TOP);
                });
            }).disabled(!possible_resizes.contains(ResizeEdge::TOP)),
            Item::new(fl!("window-menu-resize-edge-left"), move |handle| {
                let resize_clone = resize_left_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let seat = state.common.last_active_seat().clone();
                    Shell::menu_resize_request(state, &resize_clone, &seat, ResizeEdge::LEFT);
                });
            }).disabled(!possible_resizes.contains(ResizeEdge::LEFT)),
            Item::new(fl!("window-menu-resize-edge-right"), move |handle| {
                let resize_clone = resize_right_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let seat = state.common.last_active_seat().clone();
                    Shell::menu_resize_request(state, &resize_clone, &seat, ResizeEdge::RIGHT);
                });
            }).disabled(!possible_resizes.contains(ResizeEdge::RIGHT)),
            Item::new(fl!("window-menu-resize-edge-bottom"), move |handle| {
                let resize_clone = resize_bottom_clone.clone();
                let _ = handle.insert_idle(move |state| {
                    let seat = state.common.last_active_seat().clone();
                    Shell::menu_resize_request(state, &resize_clone, &seat, ResizeEdge::BOTTOM);
                });
            }).disabled(!possible_resizes.contains(ResizeEdge::BOTTOM)),
        ])),
        Some(Item::new(fl!("window-menu-move-prev-workspace"), move |handle| {
            let move_prev_clone = move_prev_clone.clone();
            let _ = handle.insert_idle(move |state| {
                let seat = state.common.last_active_seat().clone();
                let (current_handle, output) = {
                    let Some(ws) = state.common.shell.space_for(&move_prev_clone) else { return };
                    (ws.handle, ws.output.clone())
                };
                let maybe_handle = state
                    .common
                    .shell
                    .workspaces
                    .spaces_for_output(&output)
                    .enumerate()
                    .find_map(|(i, space)| (space.handle == current_handle).then_some(i))
                    .and_then(|i| i.checked_sub(1))
                    .and_then(|i| {
                        state
                            .common
                            .shell
                            .workspaces
                            .get(i, &output)
                            .map(|s| s.handle)
                    });
                if let Some(prev_handle) = maybe_handle
                {
                    Shell::move_window(
                        state,
                        &seat,
                        &move_prev_clone,
                        &current_handle,
                        &prev_handle,
                        true,
                        None,
                    );
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::MoveToPreviousWorkspace))),
        Some(Item::new(fl!("window-menu-move-next-workspace"), move |handle| {
            let move_next_clone = move_next_clone.clone();
            let _ = handle.insert_idle(move |state| {
                let seat = state.common.last_active_seat().clone();
                let (current_handle, output) = {
                    let Some(ws) = state.common.shell.space_for(&move_next_clone) else { return };
                    (ws.handle, ws.output.clone())
                };
                let maybe_handle = state
                    .common
                    .shell
                    .workspaces
                    .spaces_for_output(&output)
                    .skip_while(|space| space.handle != current_handle)
                    .skip(1)
                    .next()
                    .map(|space| space.handle);
                if let Some(next_handle) = maybe_handle
                {
                    Shell::move_window(
                        state,
                        &seat,
                        &move_next_clone,
                        &current_handle,
                        &next_handle,
                        true,
                        None,
                    );
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::MoveToNextWorkspace))),
        (!is_stacked).then_some(Item::new(fl!("window-menu-stack"), move |handle| {
            let stack_clone = stack_clone.clone();
            let _ = handle.insert_idle(move |state| {
                let seat = state.common.last_active_seat().clone();
                let Some(ws) = state.common.shell.space_for_mut(&stack_clone) else { return };
                if let Some(new_focus) = ws.toggle_stacking(&stack_clone) {
                    Common::set_focus(state, Some(&new_focus), &seat, None);
                }
            });
        })
        .shortcut(config.get_shortcut_for_action(&Action::ToggleStacking))),
        Some(Item::Separator),
        //Some(Item::new(fl!("window-menu-always-on-top"), |handle| {}).toggled(is_always_on_top)),
        //Some(Item::new(fl!("window-menu-always-on-visible-ws"), |handle| {})
        //    .toggled(is_always_on_visible_ws)),
        //Some(Item::Separator),
        if is_stacked {
            Some(Item::new(fl!("window-menu-close-all"), move |_handle| {
                for (window, _) in close_clone.windows() {
                    window.close();
                }
            }))
        } else {
            Some(Item::new(fl!("window-menu-close"), move |_handle| {
                close_clone.send_close();
            })
            .shortcut(config.get_shortcut_for_action(&Action::Close)))
        },
    ].into_iter().flatten()
}
