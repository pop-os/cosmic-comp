use std::{sync::Mutex, time::Instant};

use calloop::LoopHandle;
use cosmic::{
    Apply,
    iced::{Alignment, Background, Border, Length, alignment::Vertical},
    iced_widget, theme,
    widget::{self, icon::Named},
};
use cosmic_comp_config::ZoomMovement;
use cosmic_config::ConfigSet;
use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::renderer::{ImportMem, Renderer, element::AsRenderElements},
    desktop::space::SpaceElement,
    input::{
        Seat,
        pointer::{
            AxisFrame, ButtonEvent, Focus, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            MotionEvent as PointerMotionEvent, PointerTarget, RelativeMotionEvent,
        },
        touch::{
            DownEvent, MotionEvent as TouchMotionEvent, OrientationEvent, ShapeEvent, TouchTarget,
            UpEvent,
        },
    },
    output::Output,
    utils::{IsAlive, Point, Rectangle, Serial, Size},
};
use tracing::error;

use crate::{
    state::State,
    utils::{
        iced::{IcedElement, Program},
        prelude::*,
        tween::EasePoint,
    },
};

use super::{
    ANIMATION_DURATION, check_grab_preconditions,
    focus::target::PointerFocusTarget,
    grabs::{ContextMenu, Item, MenuAlignment, MenuGrab},
};

#[derive(Debug, Clone)]
pub struct ZoomState {
    pub(super) seat: Seat<State>,
    pub(super) show_overlay: bool,
    pub(super) increment: u32,
    pub(super) movement: ZoomMovement,
}

#[derive(Debug)]
pub struct OutputZoomState {
    pub(super) level: f64,
    pub(super) previous_level: Option<(f64, Instant)>,
    focal_point: Point<f64, Local>,
    previous_point: Option<(Point<f64, Local>, Instant)>,
    element: ZoomElement,
}

impl OutputZoomState {
    pub fn new(
        seat: &Seat<State>,
        output: &Output,
        level: f64,
        increment: u32,
        movement: ZoomMovement,
        loop_handle: LoopHandle<'static, State>,
        theme: cosmic::Theme,
    ) -> OutputZoomState {
        let cursor_position = seat.get_pointer().unwrap().current_location().as_global();
        let output_geometry = output.geometry().to_f64();
        let focal_point = if output_geometry.contains(cursor_position) {
            match movement {
                ZoomMovement::Continuously | ZoomMovement::OnEdge => {
                    cursor_position.to_local(output)
                }
                ZoomMovement::Centered => {
                    let mut zoomed_output_geometry = output.geometry().to_f64().downscale(level);
                    zoomed_output_geometry.loc =
                        cursor_position - zoomed_output_geometry.size.downscale(2.).to_point();

                    let mut focal_point = zoomed_output_geometry
                        .loc
                        .to_local(output)
                        .upscale(level)
                        .to_global(output);
                    focal_point.x = focal_point.x.clamp(
                        output_geometry.loc.x,
                        (output_geometry.loc.x + output_geometry.size.w).next_down(),
                    );
                    focal_point.y = focal_point.y.clamp(
                        output_geometry.loc.y,
                        (output_geometry.loc.y + output_geometry.size.h).next_down(),
                    );
                    focal_point.to_local(output)
                }
            }
        } else {
            (output_geometry.size.w / 2., output_geometry.size.h / 2.).into()
        };

        let program = ZoomProgram::new(level, movement, increment);
        let element = IcedElement::new(program, Size::default(), loop_handle, theme);
        let mut size = element.minimum_size();
        size.w = (size.w + 32/*TODO: figure out why iced is calculating too little*/)
            .min(output_geometry.size.w.round() as i32);
        element.set_activate(true);
        element.resize(size);
        element.output_enter(output, Rectangle::new(Point::from((0, 0)), size));
        element.set_additional_scale(level.min(4.));

        OutputZoomState {
            level,
            previous_level: None,
            focal_point,
            previous_point: None,
            element,
        }
    }

    pub fn animating_focal_point(&mut self) -> Point<f64, Local> {
        if let Some((old_point, start)) = self.previous_point.as_ref() {
            let duration_since = Instant::now().duration_since(*start);
            if duration_since > ANIMATION_DURATION {
                self.previous_point.take();
                return self.focal_point;
            }

            let percentage =
                duration_since.as_millis() as f32 / ANIMATION_DURATION.as_millis() as f32;
            ease(
                EaseInOutCubic,
                EasePoint(*old_point),
                EasePoint(self.focal_point),
                percentage,
            )
            .0
        } else {
            self.focal_point
        }
    }

    pub fn current_focal_point(&mut self) -> Point<f64, Local> {
        self.focal_point
    }

    pub fn current_level(&self) -> f64 {
        self.level
    }

    pub fn animating_level(&self) -> f64 {
        if let Some((old_level, start)) = self.previous_level.as_ref() {
            let percentage = Instant::now().duration_since(*start).as_millis() as f32
                / ANIMATION_DURATION.as_millis() as f32;

            ease(EaseInOutCubic, *old_level, self.level, percentage)
        } else {
            self.level
        }
    }

    pub fn is_animating(&self) -> bool {
        self.previous_point.is_some() || self.previous_level.is_some()
    }

    pub fn refresh(&mut self) -> bool {
        if self
            .previous_level
            .as_ref()
            .is_some_and(|(_, start)| Instant::now().duration_since(*start) > ANIMATION_DURATION)
        {
            self.previous_level.take();
        }
        self.element.refresh();
        self.level == 1. && self.previous_level.is_none()
    }

    pub fn update(&mut self, level: f64, animate: bool, movement: ZoomMovement, increment: u32) {
        self.previous_level = animate.then_some((self.animating_level(), Instant::now()));
        self.level = level;
        self.element.set_additional_scale(level.min(4.));
        self.element.queue_message(ZoomMessage::Update {
            level,
            movement,
            increment,
        });
    }

    fn render<R, C>(&mut self, renderer: &mut R, output: &Output) -> Vec<C>
    where
        C: From<<IcedElement<ZoomProgram> as AsRenderElements<R>>::RenderElement>,
        R: Renderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let size = self.element.current_size().to_f64();
        let output_geo = output.geometry().to_f64();
        let scale = output.current_scale();
        let location = Point::from((
            output_geo.size.w / 2. - size.w / 2.,
            output_geo.size.h / 4. * 3. - size.h / 2.,
        ))
        .to_physical(scale.fractional_scale())
        .to_i32_round();

        self.element
            .render_elements(renderer, location, scale.fractional_scale().into(), 1.0)
    }
}

impl ZoomState {
    pub fn current_seat(&self) -> Seat<State> {
        self.seat.clone()
    }

    pub fn current_level(&self, output: &Output) -> f64 {
        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        output_state.lock().unwrap().current_level()
    }

    pub fn animating_level(&self, output: &Output) -> f64 {
        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        output_state.lock().unwrap().animating_level()
    }

    pub fn animating_focal_point(&self, output: Option<&Output>) -> Point<f64, Global> {
        let active_output = self.seat.active_output();
        let output = output.unwrap_or(&active_output);
        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        let res = output_state
            .lock()
            .unwrap()
            .animating_focal_point()
            .to_global(output);
        res
    }

    pub fn current_focal_point(&self, output: Option<&Output>) -> Point<f64, Global> {
        let active_output = self.seat.active_output();
        let output = output.unwrap_or(&active_output);
        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        let res = output_state
            .lock()
            .unwrap()
            .current_focal_point()
            .to_global(output);
        res
    }

    pub fn update_focal_point(
        &mut self,
        output: &Output,
        cursor_position: Point<f64, Global>,
        original_position: Point<f64, Global>,
        movement: ZoomMovement,
    ) {
        let cursor_position = cursor_position.to_i32_round();
        let original_position = original_position.to_i32_round();
        let output_geometry = output.geometry();
        let mut zoomed_output_geometry = output.zoomed_geometry().unwrap();

        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        let mut output_state_ref = output_state.lock().unwrap();

        // animate movement type changes
        if self.movement != movement {
            output_state_ref.previous_point = Some((output_state_ref.focal_point, Instant::now()));
            self.movement = movement;
        }

        let cursor_position = cursor_position.to_local(output);
        match movement {
            ZoomMovement::Continuously => output_state_ref.focal_point = cursor_position.to_f64(),
            ZoomMovement::OnEdge => {
                if !zoomed_output_geometry
                    .overlaps_or_touches(Rectangle::new(original_position, Size::from((16, 16))))
                {
                    zoomed_output_geometry.loc = cursor_position.to_global(output)
                        - zoomed_output_geometry.size.downscale(2).to_point();
                    let mut focal_point = zoomed_output_geometry
                        .loc
                        .to_local(output)
                        .upscale(
                            output_geometry.size.w
                                / (output_geometry.size.w - zoomed_output_geometry.size.w),
                        )
                        .to_global(output);
                    focal_point.x = focal_point.x.clamp(
                        output_geometry.loc.x,
                        output_geometry.loc.x + output_geometry.size.w - 1,
                    );
                    focal_point.y = focal_point.y.clamp(
                        output_geometry.loc.y,
                        output_geometry.loc.y + output_geometry.size.h - 1,
                    );
                    output_state_ref.previous_point =
                        Some((output_state_ref.focal_point, Instant::now()));
                    output_state_ref.focal_point = focal_point.to_local(output).to_f64();
                } else if !zoomed_output_geometry.contains(cursor_position.to_global(output)) {
                    let mut diff = output_state_ref.focal_point.to_global(output)
                        + (cursor_position.to_global(output) - original_position)
                            .to_f64()
                            .upscale(output_state_ref.level);
                    diff.x = diff.x.clamp(
                        output_geometry.loc.x as f64,
                        ((output_geometry.loc.x + output_geometry.size.w) as f64).next_down(),
                    );
                    diff.y = diff.y.clamp(
                        output_geometry.loc.y as f64,
                        ((output_geometry.loc.y + output_geometry.size.h) as f64).next_down(),
                    );
                    diff -= output_state_ref.focal_point.to_global(output);

                    output_state_ref.focal_point += diff.as_logical().as_local();
                }
            }
            ZoomMovement::Centered => {
                zoomed_output_geometry.loc = cursor_position.to_global(output)
                    - zoomed_output_geometry.size.downscale(2).to_point();

                let mut focal_point = zoomed_output_geometry
                    .loc
                    .to_local(output)
                    .upscale(
                        output_geometry
                            .size
                            .w
                            .checked_div(output_geometry.size.w - zoomed_output_geometry.size.w)
                            .unwrap_or(1),
                    )
                    .to_global(output);
                focal_point.x = focal_point.x.clamp(
                    output_geometry.loc.x,
                    output_geometry.loc.x + output_geometry.size.w - 1,
                );
                focal_point.y = focal_point.y.clamp(
                    output_geometry.loc.y,
                    output_geometry.loc.y + output_geometry.size.h - 1,
                );
                output_state_ref.focal_point = focal_point.to_local(output).to_f64();
            }
        }
    }

    pub fn surface_under(
        &self,
        output: &Output,
        pos: Point<f64, Global>,
    ) -> Option<(PointerFocusTarget, Point<f64, Global>)> {
        let output_geometry = output.geometry();
        let zoomed_output_geometry = output.zoomed_geometry().unwrap().to_f64();
        let local_pos = global_pos_to_screen_space(pos, output);

        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        let output_state_ref = output_state.lock().unwrap();

        let size = output_state_ref.element.current_size().to_f64().as_local();
        let location = Point::<f64, Local>::from((
            output_geometry.size.w as f64 / 2. - size.w / 2.,
            output_geometry.size.h as f64 / 4. * 3. - size.h / 2.,
        ));
        let area = Rectangle::<_, Local>::new(location, size);

        if area.contains(local_pos) {
            return Some((
                PointerFocusTarget::ZoomUI(output_state_ref.element.clone().into()),
                {
                    // and vise-versa from screen-space to zoom-space...
                    let scaled_loc = location.downscale(output_state_ref.level);
                    let global_loc = Point::<f64, Global>::from((scaled_loc.x, scaled_loc.y))
                        + zoomed_output_geometry.loc;

                    // HACK: We do have the right position now `global_loc`, but smithay calculates
                    // the relative position for us... Which will be wrong given the cursor movement will
                    // be scaled, while this element isn't, as it exists in screen-space and not workspace-space.
                    // So we shift the location relatively to make up for the scaled movement...
                    let diff = (pos - global_loc).upscale(output_state_ref.level - 1.);

                    global_loc - diff
                },
            ));
        }

        None
    }

    pub fn render<R, C>(renderer: &mut R, output: &Output) -> Vec<C>
    where
        C: From<<IcedElement<ZoomProgram> as AsRenderElements<R>>::RenderElement>,
        R: Renderer + ImportMem,
        R::TextureId: Send + Clone + 'static,
    {
        let output_state = output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
        output_state.lock().unwrap().render(renderer, output)
    }
}

fn global_pos_to_screen_space(
    pos: impl Into<Point<f64, Global>>,
    output: &Output,
) -> Point<f64, Local> {
    let pos = pos.into();
    let zoomed_output_geometry = output.zoomed_geometry().unwrap().to_f64();
    let level = output
        .user_data()
        .get::<Mutex<OutputZoomState>>()
        .unwrap()
        .lock()
        .unwrap()
        .current_level();

    // lets try to get the global cursor position into screen space
    let relative_to_zoom_geo = Point::<f64, Local>::from((
        pos.x - zoomed_output_geometry.loc.x,
        pos.y - zoomed_output_geometry.loc.y,
    ));
    relative_to_zoom_geo.upscale(level)
}

pub type ZoomElement = IcedElement<ZoomProgram>;

pub struct ZoomProgram {
    level: f64,
    increments: Vec<u32>,
    increment_idx: usize,
    movement: ZoomMovement,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ZoomMessage {
    Decrease,
    Increase,
    Increment,
    More,
    Close,
    Update {
        level: f64,
        increment: u32,
        movement: ZoomMovement,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MenuMessage {
    ViewContinuously,
    ViewOnEdge,
    ViewCentered,
    OpenSettings,
}

impl ZoomProgram {
    pub fn new(level: f64, movement: ZoomMovement, increment: u32) -> Self {
        let mut increments = vec![25, 50, 100, 150, 200];
        if !increments.contains(&increment) {
            increments.push(increment);
        }
        increments.sort();
        let increment_idx = increments.iter().position(|val| *val == increment).unwrap();

        ZoomProgram {
            level,
            increments,
            increment_idx,
            movement,
        }
    }
}

impl Program for ZoomProgram {
    type Message = ZoomMessage;

    fn view(&self) -> cosmic::Element<'_, Self::Message> {
        widget::row::with_children(vec![
            widget::button::icon(Named::new("list-remove-symbolic").size(16).prefer_svg(true))
                .on_press(ZoomMessage::Decrease)
                .into(),
            widget::text(format!("{}%", (self.level * 100.).round()))
                .align_y(Vertical::Center)
                .width(Length::Shrink)
                .into(),
            widget::button::icon(Named::new("list-add-symbolic").size(16).prefer_svg(true))
                .on_press(ZoomMessage::Increase)
                .into(),
            widget::divider::vertical::default().into(),
            widget::button::text(format!("{}%", self.increments[self.increment_idx]))
                .trailing_icon(Named::new("pan-down-symbolic").size(16).prefer_svg(true))
                .on_press(ZoomMessage::Increment)
                .class(theme::Button::MenuFolder)
                .into(),
            widget::button::icon(Named::new("view-more-symbolic").size(16).prefer_svg(true))
                .on_press(ZoomMessage::More)
                .into(),
            widget::divider::vertical::default().into(),
            widget::button::icon(
                Named::new("window-close-symbolic")
                    .size(16)
                    .prefer_svg(true),
            )
            .on_press(ZoomMessage::Close)
            .into(),
        ])
        .spacing(8.)
        .height(Length::Fixed(32.))
        .width(Length::Shrink)
        .align_y(Alignment::Center)
        .apply(widget::container)
        .padding(8)
        .class(theme::Container::custom(|theme| {
            let cosmic = theme.cosmic();
            let component = &cosmic.background.component;
            iced_widget::container::Style {
                icon_color: Some(component.on.into()),
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
        .into()
    }

    fn update(
        &mut self,
        message: Self::Message,
        loop_handle: &LoopHandle<'static, State>,
        last_seat: Option<&(Seat<State>, Serial)>,
    ) -> cosmic::Task<Self::Message> {
        match message {
            ZoomMessage::Decrease => {
                let _ = loop_handle.insert_idle(|state| {
                    let seat = state.common.shell.read().seats.last_active().clone();
                    let increment =
                        state.common.config.cosmic_conf.accessibility_zoom.increment as f64 / 100.0;

                    state.update_zoom(&seat, -increment, true);
                });
            }
            ZoomMessage::Increase => {
                let _ = loop_handle.insert_idle(|state| {
                    let seat = state.common.shell.read().seats.last_active().clone();
                    let increment =
                        state.common.config.cosmic_conf.accessibility_zoom.increment as f64 / 100.0;

                    state.update_zoom(&seat, increment, true);
                });
            }
            ZoomMessage::More => {
                let movement = self.movement;
                if let Some((seat, serial)) = last_seat.cloned() {
                    let _ = loop_handle.insert_idle(move |state| {
                        if let Some(start_data) =
                            check_grab_preconditions(&seat, Some(serial), None)
                        {
                            let shell = state.common.shell.read();
                            let output = seat.active_output();

                            if shell.zoom_state().is_some() {
                                let location = global_pos_to_screen_space(
                                    start_data.location().as_global(),
                                    &output,
                                );

                                let output_geometry = output.geometry();
                                let output_state =
                                    output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
                                let output_state_ref = output_state.lock().unwrap();

                                let elem_size =
                                    output_state_ref.element.current_size().to_f64().as_local();
                                let elem_location = Point::<f64, Local>::from((
                                    output_geometry.size.w as f64 / 2. - elem_size.w / 2.,
                                    output_geometry.size.h as f64 / 4. * 3. - elem_size.h / 2.,
                                ));
                                let position = Point::<_, Local>::from((
                                    location.x,
                                    elem_location.y + elem_size.h / 2.,
                                ));
                                let level = output_state_ref.level;
                                std::mem::drop(output_state_ref);

                                let grab = MenuGrab::new(
                                    start_data,
                                    &seat,
                                    vec![
                                        Item::new(
                                            crate::fl!("a11y-zoom-move-continuously"),
                                            move |handle| {
                                                let _ = handle.insert_idle(move |state| {
                                                    state
                                                        .common
                                                        .config
                                                        .cosmic_conf
                                                        .accessibility_zoom
                                                        .view_moves = ZoomMovement::Continuously;
                                                    if let Err(err) =
                                                        state.common.config.cosmic_helper.set(
                                                            "accessibility_zoom",
                                                            state
                                                                .common
                                                                .config
                                                                .cosmic_conf
                                                                .accessibility_zoom,
                                                        )
                                                    {
                                                        error!(
                                                            ?err,
                                                            "Failed to update zoom config"
                                                        );
                                                    }
                                                    state.common.update_config();
                                                });
                                            },
                                        )
                                        .toggled(movement == ZoomMovement::Continuously),
                                        Item::new(
                                            crate::fl!("a11y-zoom-move-onedge"),
                                            move |handle| {
                                                let _ = handle.insert_idle(move |state| {
                                                    state
                                                        .common
                                                        .config
                                                        .cosmic_conf
                                                        .accessibility_zoom
                                                        .view_moves = ZoomMovement::OnEdge;
                                                    if let Err(err) =
                                                        state.common.config.cosmic_helper.set(
                                                            "accessibility_zoom",
                                                            state
                                                                .common
                                                                .config
                                                                .cosmic_conf
                                                                .accessibility_zoom,
                                                        )
                                                    {
                                                        error!(
                                                            ?err,
                                                            "Failed to update zoom config"
                                                        );
                                                    }
                                                    state.common.update_config();
                                                });
                                            },
                                        )
                                        .toggled(movement == ZoomMovement::OnEdge),
                                        Item::new(
                                            crate::fl!("a11y-zoom-move-centered"),
                                            move |handle| {
                                                let _ = handle.insert_idle(move |state| {
                                                    state
                                                        .common
                                                        .config
                                                        .cosmic_conf
                                                        .accessibility_zoom
                                                        .view_moves = ZoomMovement::Centered;
                                                    if let Err(err) =
                                                        state.common.config.cosmic_helper.set(
                                                            "accessibility_zoom",
                                                            state
                                                                .common
                                                                .config
                                                                .cosmic_conf
                                                                .accessibility_zoom,
                                                        )
                                                    {
                                                        error!(
                                                            ?err,
                                                            "Failed to update zoom config"
                                                        );
                                                    }
                                                    state.common.update_config();
                                                });
                                            },
                                        )
                                        .toggled(movement == ZoomMovement::Centered),
                                        Item::Separator,
                                        Item::new(crate::fl!("a11y-zoom-settings"), |handle| {
                                            let _ = handle.insert_idle(move |state| {
                                                state.spawn_command(
                                                    "cosmic-settings accessibility-magnifier"
                                                        .into(),
                                                );
                                            });
                                        }),
                                    ]
                                    .into_iter(),
                                    position.to_global(&output).to_i32_round(),
                                    MenuAlignment::horizontally_centered(
                                        (elem_size.h / 2.).round() as u32,
                                        false,
                                    ),
                                    Some(level.min(4.)),
                                    state.common.event_loop_handle.clone(),
                                    state.common.theme.clone(),
                                );

                                std::mem::drop(shell);
                                if grab.is_touch_grab() {
                                    seat.get_touch().unwrap().set_grab(state, grab, serial);
                                } else {
                                    seat.get_pointer().unwrap().set_grab(
                                        state,
                                        grab,
                                        serial,
                                        Focus::Clear,
                                    );
                                }
                            }
                        }
                    });
                }
            }
            ZoomMessage::Increment => {
                if let Some((seat, serial)) = last_seat.cloned() {
                    let increments = self.increments.clone();
                    let _ = loop_handle.insert_idle(move |state| {
                        if let Some(start_data) =
                            check_grab_preconditions(&seat, Some(serial), None)
                        {
                            let shell = state.common.shell.read();
                            let output = seat.active_output();

                            if shell.zoom_state().is_some() {
                                let location = global_pos_to_screen_space(
                                    start_data.location().as_global(),
                                    &output,
                                );

                                let output_geometry = output.geometry();
                                let output_state =
                                    output.user_data().get::<Mutex<OutputZoomState>>().unwrap();
                                let output_state_ref = output_state.lock().unwrap();

                                let elem_size =
                                    output_state_ref.element.current_size().to_f64().as_local();
                                let elem_location = Point::<f64, Local>::from((
                                    output_geometry.size.w as f64 / 2. - elem_size.w / 2.,
                                    output_geometry.size.h as f64 / 4. * 3. - elem_size.h / 2.,
                                ));
                                let position = Point::<_, Local>::from((
                                    location.x,
                                    elem_location.y + (elem_size.h / 2.),
                                ));
                                let level = output_state_ref.level;
                                std::mem::drop(output_state_ref);

                                let grab = MenuGrab::new(
                                    start_data,
                                    &seat,
                                    increments.into_iter().map(|val| {
                                        Item::new(format!("{}%", val), move |handle| {
                                            let _ = handle.insert_idle(move |state| {
                                                state
                                                    .common
                                                    .config
                                                    .cosmic_conf
                                                    .accessibility_zoom
                                                    .increment = val;
                                                state.common.update_config();
                                                if let Err(err) =
                                                    state.common.config.cosmic_helper.set(
                                                        "accessibility_zoom",
                                                        state
                                                            .common
                                                            .config
                                                            .cosmic_conf
                                                            .accessibility_zoom,
                                                    )
                                                {
                                                    error!(?err, "Failed to update zoom config");
                                                }
                                            });
                                        })
                                    }),
                                    position.to_global(&output).to_i32_round(),
                                    MenuAlignment::PREFER_CENTERED,
                                    Some(level.min(4.)),
                                    state.common.event_loop_handle.clone(),
                                    state.common.theme.clone(),
                                );

                                std::mem::drop(shell);
                                if grab.is_touch_grab() {
                                    seat.get_touch().unwrap().set_grab(state, grab, serial);
                                } else {
                                    seat.get_pointer().unwrap().set_grab(
                                        state,
                                        grab,
                                        serial,
                                        Focus::Clear,
                                    );
                                }
                            }
                        }
                    });
                }
            }
            ZoomMessage::Close => {
                let _ = loop_handle.insert_idle(|state| {
                    state
                        .common
                        .config
                        .cosmic_conf
                        .accessibility_zoom
                        .show_overlay = false;
                    if let Err(err) = state.common.config.cosmic_helper.set(
                        "accessibility_zoom",
                        state.common.config.cosmic_conf.accessibility_zoom,
                    ) {
                        error!(?err, "Failed to update zoom config");
                    }
                    state.common.update_config();
                });
            }
            ZoomMessage::Update {
                level,
                increment,
                movement,
            } => {
                self.level = level;
                self.movement = movement;

                if let Some(pos) = self.increments.iter().position(|val| *val == increment) {
                    self.increment_idx = pos;
                } else {
                    let mut increments = vec![25, 50, 100, 150, 200];
                    if !increments.contains(&increment) {
                        increments.push(increment);
                    }
                    increments.sort();
                    self.increment_idx =
                        increments.iter().position(|val| *val == increment).unwrap();
                    self.increments = increments;
                }
            }
        }
        cosmic::Task::none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ZoomFocusTarget {
    Main(ZoomElement),
    Menu(IcedElement<ContextMenu>),
}

impl From<ZoomElement> for ZoomFocusTarget {
    fn from(value: ZoomElement) -> Self {
        ZoomFocusTarget::Main(value)
    }
}

impl From<IcedElement<ContextMenu>> for ZoomFocusTarget {
    fn from(value: IcedElement<ContextMenu>) -> Self {
        ZoomFocusTarget::Menu(value)
    }
}

impl PointerTarget<State> for ZoomFocusTarget {
    fn enter(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::enter(elem, seat, data, event),
            ZoomFocusTarget::Menu(elem) => PointerTarget::enter(elem, seat, data, event),
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &PointerMotionEvent) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::motion(elem, seat, data, event),
            ZoomFocusTarget::Menu(elem) => PointerTarget::motion(elem, seat, data, event),
        }
    }

    fn relative_motion(&self, seat: &Seat<State>, data: &mut State, event: &RelativeMotionEvent) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::relative_motion(elem, seat, data, event),
            ZoomFocusTarget::Menu(elem) => PointerTarget::relative_motion(elem, seat, data, event),
        }
    }

    fn button(&self, seat: &Seat<State>, data: &mut State, event: &ButtonEvent) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::button(elem, seat, data, event),
            ZoomFocusTarget::Menu(elem) => PointerTarget::button(elem, seat, data, event),
        }
    }

    fn axis(&self, seat: &Seat<State>, data: &mut State, frame: AxisFrame) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::axis(elem, seat, data, frame),
            ZoomFocusTarget::Menu(elem) => PointerTarget::axis(elem, seat, data, frame),
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::frame(elem, seat, data),
            ZoomFocusTarget::Menu(elem) => PointerTarget::frame(elem, seat, data),
        }
    }

    fn gesture_swipe_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeBeginEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_swipe_begin(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_swipe_begin(elem, seat, data, event)
            }
        }
    }

    fn gesture_swipe_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeUpdateEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_swipe_update(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_swipe_update(elem, seat, data, event)
            }
        }
    }

    fn gesture_swipe_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureSwipeEndEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_swipe_end(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_swipe_end(elem, seat, data, event)
            }
        }
    }

    fn gesture_pinch_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchBeginEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_pinch_begin(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_pinch_begin(elem, seat, data, event)
            }
        }
    }

    fn gesture_pinch_update(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchUpdateEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_pinch_update(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_pinch_update(elem, seat, data, event)
            }
        }
    }

    fn gesture_pinch_end(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GesturePinchEndEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_pinch_end(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_pinch_end(elem, seat, data, event)
            }
        }
    }

    fn gesture_hold_begin(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &GestureHoldBeginEvent,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => {
                PointerTarget::gesture_hold_begin(elem, seat, data, event)
            }
            ZoomFocusTarget::Menu(elem) => {
                PointerTarget::gesture_hold_begin(elem, seat, data, event)
            }
        }
    }

    fn gesture_hold_end(&self, seat: &Seat<State>, data: &mut State, event: &GestureHoldEndEvent) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::gesture_hold_end(elem, seat, data, event),
            ZoomFocusTarget::Menu(elem) => PointerTarget::gesture_hold_end(elem, seat, data, event),
        }
    }

    fn leave(&self, seat: &Seat<State>, data: &mut State, serial: Serial, time: u32) {
        match self {
            ZoomFocusTarget::Main(elem) => PointerTarget::leave(elem, seat, data, serial, time),
            ZoomFocusTarget::Menu(elem) => PointerTarget::leave(elem, seat, data, serial, time),
        }
    }
}

impl TouchTarget<State> for ZoomFocusTarget {
    fn down(&self, seat: &Seat<State>, data: &mut State, event: &DownEvent, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::down(elem, seat, data, event, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::down(elem, seat, data, event, seq),
        }
    }

    fn up(&self, seat: &Seat<State>, data: &mut State, event: &UpEvent, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::up(elem, seat, data, event, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::up(elem, seat, data, event, seq),
        }
    }

    fn motion(&self, seat: &Seat<State>, data: &mut State, event: &TouchMotionEvent, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::motion(elem, seat, data, event, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::motion(elem, seat, data, event, seq),
        }
    }

    fn frame(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::frame(elem, seat, data, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::frame(elem, seat, data, seq),
        }
    }

    fn cancel(&self, seat: &Seat<State>, data: &mut State, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::cancel(elem, seat, data, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::cancel(elem, seat, data, seq),
        }
    }

    fn shape(&self, seat: &Seat<State>, data: &mut State, event: &ShapeEvent, seq: Serial) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::shape(elem, seat, data, event, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::shape(elem, seat, data, event, seq),
        }
    }

    fn orientation(
        &self,
        seat: &Seat<State>,
        data: &mut State,
        event: &OrientationEvent,
        seq: Serial,
    ) {
        match self {
            ZoomFocusTarget::Main(elem) => TouchTarget::orientation(elem, seat, data, event, seq),
            ZoomFocusTarget::Menu(elem) => TouchTarget::orientation(elem, seat, data, event, seq),
        }
    }
}

impl IsAlive for ZoomFocusTarget {
    fn alive(&self) -> bool {
        match self {
            ZoomFocusTarget::Main(elem) => elem.alive(),
            ZoomFocusTarget::Menu(elem) => elem.alive(),
        }
    }
}
