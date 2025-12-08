// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        BackdropShader, IndicatorShader, Key, Usage, cursor::CursorState, element::AsGlowRenderer,
    },
    shell::{
        CosmicMapped, CosmicSurface, Direction, ManagedLayer,
        element::{
            CosmicMappedRenderElement,
            stack_hover::{StackHover, stack_hover},
        },
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
        layout::floating::TiledCorners,
    },
    utils::prelude::*,
    wayland::protocols::toplevel_info::{toplevel_enter_output, toplevel_enter_workspace},
};

use calloop::LoopHandle;
use cosmic::theme::CosmicTheme;
use smithay::{
    backend::{
        input::ButtonState,
        renderer::{
            ImportAll, ImportMem, Renderer,
            element::{AsRenderElements, RenderElement, utils::RescaleRenderElement},
        },
    },
    desktop::{WindowSurfaceType, layer_map_for_output, space::SpaceElement},
    input::{
        Seat,
        pointer::{
            AxisFrame, ButtonEvent, CursorIcon, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        touch::{self, GrabStartData as TouchGrabStartData, TouchGrab, TouchInnerHandle},
    },
    output::Output,
    utils::{IsAlive, Logical, Point, Rectangle, SERIAL_COUNTER, Scale, Serial},
};
use std::{
    collections::HashSet,
    sync::{Mutex, atomic::Ordering},
    time::Instant,
};

use super::{GrabStartData, ReleaseMode};

pub type SeatMoveGrabState = Mutex<Option<MoveGrabState>>;

const RESCALE_ANIMATION_DURATION: f64 = 150.0;

pub struct MoveGrabState {
    window: CosmicMapped,
    window_offset: Point<i32, Logical>,
    indicator_thickness: u8,
    start: Instant,
    previous: ManagedLayer,
    snapping_zone: Option<SnappingZone>,
    stacking_indicator: Option<(StackHover, Point<i32, Logical>)>,
    location: Point<f64, Logical>,
    cursor_output: Output,
}

impl MoveGrabState {
    #[profiling::function]
    pub fn render<I, R>(&self, renderer: &mut R, output: &Output, theme: &CosmicTheme) -> Vec<I>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        R::TextureId: Send + Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        I: From<CosmicMappedRenderElement<R>>,
    {
        let scale = if self.previous == ManagedLayer::Tiling {
            0.6 + ((1.0
                - (Instant::now().duration_since(self.start).as_millis() as f64
                    / RESCALE_ANIMATION_DURATION)
                    .min(1.0))
                * 0.4)
        } else {
            1.0
        };
        let alpha = if &self.cursor_output == output {
            1.0
        } else {
            0.4
        };

        let mut window_geo = self.window.geometry();
        window_geo.loc += self.location.to_i32_round() + self.window_offset;
        if output
            .geometry()
            .as_logical()
            .intersection(window_geo)
            .is_none()
        {
            return Vec::new();
        }

        let output_scale: Scale<f64> = output.current_scale().fractional_scale().into();
        let scaling_offset =
            self.window_offset - self.window_offset.to_f64().upscale(scale).to_i32_round();
        let render_location = self.location.to_i32_round() - output.geometry().loc.as_logical()
            + self.window_offset
            - scaling_offset;

        let active_window_hint = crate::theme::active_window_hint(theme);
        let radius = self
            .element()
            .corner_radius(window_geo.size, self.indicator_thickness);

        let focus_element = if self.indicator_thickness > 0 {
            Some(CosmicMappedRenderElement::from(
                IndicatorShader::focus_element(
                    renderer,
                    Key::Window(Usage::MoveGrabIndicator, self.window.key()),
                    Rectangle::new(
                        render_location,
                        self.window
                            .geometry()
                            .size
                            .to_f64()
                            .upscale(scale)
                            .to_i32_round(),
                    )
                    .as_local(),
                    self.indicator_thickness,
                    radius,
                    alpha,
                    [
                        active_window_hint.red,
                        active_window_hint.green,
                        active_window_hint.blue,
                    ],
                ),
            ))
        } else {
            None
        };

        let non_exclusive_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };

        let gaps = (theme.gaps.0 as i32, theme.gaps.1 as i32);
        let thickness = self.indicator_thickness.max(1);

        let snapping_indicator = match &self.snapping_zone {
            Some(t) if &self.cursor_output == output => {
                let base_color = theme.palette.neutral_9;
                let overlay_geometry = t.overlay_geometry(non_exclusive_geometry, gaps);
                vec![
                    CosmicMappedRenderElement::from(IndicatorShader::element(
                        renderer,
                        Key::Window(Usage::SnappingIndicator, self.window.key()),
                        overlay_geometry,
                        thickness,
                        [
                            theme.radius_s()[0] as u8,
                            theme.radius_s()[1] as u8,
                            theme.radius_s()[2] as u8,
                            theme.radius_s()[3] as u8,
                        ],
                        1.0,
                        [
                            active_window_hint.red,
                            active_window_hint.green,
                            active_window_hint.blue,
                        ],
                    )),
                    CosmicMappedRenderElement::from(BackdropShader::element(
                        renderer,
                        Key::Window(Usage::SnappingIndicator, self.window.key()),
                        t.overlay_geometry(non_exclusive_geometry, gaps),
                        theme.radius_s()[0], // TODO: Fix once shaders support 4 corner radii customization
                        0.4,
                        [base_color.red, base_color.green, base_color.blue],
                    )),
                ]
            }
            _ => vec![],
        };

        let w_elements = self
            .window
            .render_elements::<R, CosmicMappedRenderElement<R>>(
                renderer,
                (render_location - self.window.geometry().loc)
                    .to_physical_precise_round(output_scale),
                output_scale,
                alpha,
                Some(false),
            );
        let p_elements = self
            .window
            .popup_render_elements::<R, CosmicMappedRenderElement<R>>(
                renderer,
                (render_location - self.window.geometry().loc)
                    .to_physical_precise_round(output_scale),
                output_scale,
                alpha,
            );
        let shadow_element = self.window.shadow_render_element(
            renderer,
            (render_location - self.window.geometry().loc).to_physical_precise_round(output_scale),
            output_scale,
            alpha,
        );

        self.stacking_indicator
            .iter()
            .flat_map(|(indicator, location)| {
                indicator.render_elements(
                    renderer,
                    location.to_physical_precise_round(output_scale),
                    output_scale,
                    1.0,
                )
            })
            .chain(p_elements)
            .chain(focus_element)
            .chain(
                w_elements
                    .into_iter()
                    .chain(shadow_element)
                    .map(|elem| match elem {
                        CosmicMappedRenderElement::Stack(stack) => {
                            CosmicMappedRenderElement::GrabbedStack(
                                RescaleRenderElement::from_element(
                                    stack,
                                    render_location.to_physical_precise_round(
                                        output.current_scale().fractional_scale(),
                                    ),
                                    scale,
                                ),
                            )
                        }
                        CosmicMappedRenderElement::Window(window) => {
                            CosmicMappedRenderElement::GrabbedWindow(
                                RescaleRenderElement::from_element(
                                    window,
                                    render_location.to_physical_precise_round(
                                        output.current_scale().fractional_scale(),
                                    ),
                                    scale,
                                ),
                            )
                        }
                        x => x,
                    }),
            )
            .chain(snapping_indicator)
            .map(I::from)
            .collect()
    }

    pub fn element(&self) -> CosmicMapped {
        self.window.clone()
    }

    pub fn window(&self) -> CosmicSurface {
        self.window.active_window()
    }
}

struct NotSend<T>(pub T);
unsafe impl<T> Send for NotSend<T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SnappingZone {
    Maximize,
    Top,
    TopLeft,
    Left,
    BottomLeft,
    Bottom,
    BottomRight,
    Right,
    TopRight,
}

const SNAP_RANGE: i32 = 32;
const SNAP_RANGE_MAXIMIZE: i32 = 22;
const SNAP_RANGE_TOP: i32 = 16;

impl SnappingZone {
    pub fn contains(
        &self,
        point: Point<i32, Local>,
        output_geometry: Rectangle<i32, Local>,
    ) -> bool {
        if !output_geometry.contains(point) {
            return false;
        }
        let top_zone_32 = point.y < output_geometry.loc.y + SNAP_RANGE_MAXIMIZE;
        let top_zone_56 = point.y < output_geometry.loc.y + SNAP_RANGE_MAXIMIZE + SNAP_RANGE_TOP;
        let left_zone = point.x < output_geometry.loc.x + SNAP_RANGE;
        let right_zone = point.x > output_geometry.loc.x + output_geometry.size.w - SNAP_RANGE;
        let bottom_zone = point.y > output_geometry.loc.y + output_geometry.size.h - SNAP_RANGE;
        let left_6th = point.x < output_geometry.loc.x + (output_geometry.size.w / 6);
        let right_6th = point.x > output_geometry.loc.x + (output_geometry.size.w * 5 / 6);
        let top_4th = point.y < output_geometry.loc.y + (output_geometry.size.h / 4);
        let bottom_4th = point.y > output_geometry.loc.y + (output_geometry.size.h * 3 / 4);
        match self {
            SnappingZone::Maximize => top_zone_32 && !left_6th && !right_6th,
            SnappingZone::Top => top_zone_56 && !top_zone_32 && !left_6th && !right_6th,
            SnappingZone::TopLeft => (top_zone_56 && left_6th) || (left_zone && top_4th),
            SnappingZone::Left => left_zone && !top_4th && !bottom_4th,
            SnappingZone::BottomLeft => (bottom_zone && left_6th) || (left_zone && bottom_4th),
            SnappingZone::Bottom => bottom_zone && !left_6th && !right_6th,
            SnappingZone::BottomRight => (bottom_zone && right_6th) || (right_zone && bottom_4th),
            SnappingZone::Right => right_zone && !top_4th && !bottom_4th,
            SnappingZone::TopRight => (top_zone_56 && right_6th) || (right_zone && top_4th),
        }
    }
    pub fn overlay_geometry(
        &self,
        non_exclusive_geometry: Rectangle<i32, Logical>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        match self {
            SnappingZone::Maximize => non_exclusive_geometry.as_local(),
            SnappingZone::Top => TiledCorners::Top.relative_geometry(non_exclusive_geometry, gaps),
            SnappingZone::TopLeft => {
                TiledCorners::TopLeft.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Left => {
                TiledCorners::Left.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::BottomLeft => {
                TiledCorners::BottomLeft.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Bottom => {
                TiledCorners::Bottom.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::BottomRight => {
                TiledCorners::BottomRight.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::Right => {
                TiledCorners::Right.relative_geometry(non_exclusive_geometry, gaps)
            }
            SnappingZone::TopRight => {
                TiledCorners::TopRight.relative_geometry(non_exclusive_geometry, gaps)
            }
        }
    }
}

pub struct MoveGrab {
    window: CosmicMapped,
    start_data: GrabStartData,
    seat: Seat<State>,
    cursor_output: Output,
    window_outputs: HashSet<Output>,
    previous: ManagedLayer,
    release: ReleaseMode,
    edge_snap_threshold: f64,
    // SAFETY: This is only used on drop which will always be on the main thread
    evlh: NotSend<LoopHandle<'static, State>>,
}

impl MoveGrab {
    fn update_location(&mut self, state: &mut State, location: Point<f64, Logical>) {
        let mut shell = state.common.shell.write();

        let Some(current_output) = shell
            .outputs()
            .find(|output| {
                output
                    .geometry()
                    .as_logical()
                    .overlaps_or_touches(Rectangle::new(location.to_i32_floor(), (0, 0).into()))
            })
            .cloned()
        else {
            return;
        };
        if self.cursor_output != current_output {
            shell
                .workspaces
                .active_mut(&self.cursor_output)
                .unwrap()
                .tiling_layer
                .cleanup_drag();
            self.cursor_output = current_output.clone();
        }

        let mut borrow = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .map(|s| s.lock().unwrap());
        if let Some(grab_state) = borrow.as_mut().and_then(|s| s.as_mut()) {
            grab_state.location = location;
            grab_state.cursor_output = self.cursor_output.clone();

            let mut window_geo = self.window.geometry();
            window_geo.loc += location.to_i32_round() + grab_state.window_offset;

            if matches!(self.previous, ManagedLayer::Floating | ManagedLayer::Sticky) {
                let loc = grab_state.window_offset.to_f64() + grab_state.location;
                let size = window_geo.size.to_f64();
                let output_geom = self.cursor_output.geometry().to_f64().as_logical();
                let output_loc = output_geom.loc;
                let output_size = output_geom.size;

                grab_state.location.x = if (loc.x - output_loc.x).abs() < self.edge_snap_threshold {
                    output_loc.x - grab_state.window_offset.x as f64
                } else if ((loc.x + size.w) - (output_loc.x + output_size.w)).abs()
                    < self.edge_snap_threshold
                {
                    output_loc.x + output_size.w - grab_state.window_offset.x as f64 - size.w
                } else {
                    grab_state.location.x
                };
                grab_state.location.y = if (loc.y - output_loc.y).abs() < self.edge_snap_threshold {
                    output_loc.y - grab_state.window_offset.y as f64
                } else if ((loc.y + size.h) - (output_loc.y + output_size.h)).abs()
                    < self.edge_snap_threshold
                {
                    output_loc.y + output_size.h - grab_state.window_offset.y as f64 - size.h
                } else {
                    grab_state.location.y
                };
            }

            for output in shell.outputs() {
                if let Some(overlap) = output.geometry().as_logical().intersection(window_geo) {
                    if self.window_outputs.insert(output.clone()) {
                        self.window.output_enter(output, overlap);
                        if let Some(indicator) =
                            grab_state.stacking_indicator.as_ref().map(|x| &x.0)
                        {
                            indicator.output_enter(output, overlap);
                        }
                    }
                } else if self.window_outputs.remove(output) {
                    self.window.output_leave(output);
                    if let Some(indicator) = grab_state.stacking_indicator.as_ref().map(|x| &x.0) {
                        indicator.output_leave(output);
                    }
                }
            }

            let indicator_location = shell.stacking_indicator(&current_output, self.previous);
            if indicator_location.is_some() != grab_state.stacking_indicator.is_some() {
                grab_state.stacking_indicator = indicator_location.map(|geo| {
                    let element = stack_hover(
                        state.common.event_loop_handle.clone(),
                        geo.size.as_logical(),
                        state.common.theme.clone(),
                    );
                    for output in &self.window_outputs {
                        element.output_enter(
                            output,
                            Rectangle::from_size(output.geometry().size.as_logical()),
                        );
                    }
                    (element, geo.loc.as_logical())
                });
            }

            // Check for overlapping with zones
            if grab_state.previous == ManagedLayer::Floating {
                let output_geometry = current_output.geometry().to_local(&current_output);
                grab_state.snapping_zone = [
                    SnappingZone::Maximize,
                    SnappingZone::Top,
                    SnappingZone::TopLeft,
                    SnappingZone::Left,
                    SnappingZone::BottomLeft,
                    SnappingZone::Bottom,
                    SnappingZone::BottomRight,
                    SnappingZone::Right,
                    SnappingZone::TopRight,
                ]
                .iter()
                .find(|&x| {
                    x.contains(
                        location
                            .as_global()
                            .to_local(&current_output)
                            .to_i32_floor(),
                        output_geometry,
                    )
                })
                .cloned();
            }
        }
        drop(borrow);
    }
}

impl PointerGrab<State> for MoveGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &MotionEvent,
    ) {
        self.update_location(state, event.location);

        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            handle.unset_grab(self, state, event.serial, event.time, true);
        }
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
        handle.button(state, event);
        match self.release {
            ReleaseMode::NoMouseButtons => {
                if handle.current_pressed().is_empty() {
                    handle.unset_grab(self, state, event.serial, event.time, true);
                }
            }
            ReleaseMode::Click => {
                if event.state == ButtonState::Pressed {
                    handle.unset_grab(self, state, event.serial, event.time, true);
                }
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

impl TouchGrab<State> for MoveGrab {
    fn down(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &touch::DownEvent,
        seq: Serial,
    ) {
        handle.down(data, None, event, seq)
    }

    fn up(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::UpEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            handle.unset_grab(self, data);
        }

        handle.up(data, event, seq);
    }

    fn motion(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<f64, Logical>)>,
        event: &touch::MotionEvent,
        seq: Serial,
    ) {
        if event.slot == <Self as TouchGrab<State>>::start_data(self).slot {
            self.update_location(data, event.location);
        }

        handle.motion(data, None, event, seq);
    }

    fn frame(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, seq: Serial) {
        handle.frame(data, seq)
    }

    fn cancel(&mut self, data: &mut State, handle: &mut TouchInnerHandle<'_, State>, _seq: Serial) {
        handle.unset_grab(self, data);
    }

    fn shape(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::ShapeEvent,
        seq: Serial,
    ) {
        handle.shape(data, event, seq)
    }

    fn orientation(
        &mut self,
        data: &mut State,
        handle: &mut TouchInnerHandle<'_, State>,
        event: &touch::OrientationEvent,
        seq: Serial,
    ) {
        handle.orientation(data, event, seq)
    }

    fn start_data(&self) -> &TouchGrabStartData<State> {
        match &self.start_data {
            GrabStartData::Touch(start_data) => start_data,
            _ => unreachable!(),
        }
    }

    fn unset(&mut self, _data: &mut State) {}
}

impl MoveGrab {
    pub fn new(
        start_data: GrabStartData,
        window: CosmicMapped,
        seat: &Seat<State>,
        initial_window_location: Point<i32, Global>,
        cursor_output: Output,
        indicator_thickness: u8,
        edge_snap_threshold: f64,
        previous_layer: ManagedLayer,
        release: ReleaseMode,
        evlh: LoopHandle<'static, State>,
    ) -> MoveGrab {
        let mut outputs = HashSet::new();
        outputs.insert(cursor_output.clone());
        window.output_enter(&cursor_output, window.geometry()); // not accurate but...
        window.moved_since_mapped.store(true, Ordering::SeqCst);

        let grab_state = MoveGrabState {
            window: window.clone(),
            window_offset: (initial_window_location
                - start_data.location().as_global().to_i32_round())
            .as_logical(),
            indicator_thickness,
            start: Instant::now(),
            stacking_indicator: None,
            snapping_zone: None,
            previous: previous_layer,
            location: start_data.location(),
            cursor_output: cursor_output.clone(),
        };

        *seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .lock()
            .unwrap() = Some(grab_state);

        {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.lock().unwrap().set_shape(CursorIcon::Grabbing);
        }

        MoveGrab {
            window,
            start_data,
            seat: seat.clone(),
            cursor_output,
            window_outputs: outputs,
            previous: previous_layer,
            release,
            edge_snap_threshold,
            evlh: NotSend(evlh),
        }
    }

    pub fn is_tiling_grab(&self) -> bool {
        self.previous == ManagedLayer::Tiling
    }

    pub fn is_touch_grab(&self) -> bool {
        match self.start_data {
            GrabStartData::Touch(_) => true,
            GrabStartData::Pointer(_) => false,
        }
    }
}

impl Drop for MoveGrab {
    fn drop(&mut self) {
        // No more buttons are pressed, release the grab.
        let output = self.cursor_output.clone();
        let seat = self.seat.clone();
        let window_outputs = self.window_outputs.drain().collect::<HashSet<_>>();
        let previous = self.previous;
        let window = self.window.clone();
        let is_touch_grab = matches!(self.start_data, GrabStartData::Touch(_));
        let cursor_output = self.cursor_output.clone();

        let _ = self.evlh.0.insert_idle(move |state| {
            let position: Option<(CosmicMapped, Point<i32, Global>)> = if let Some(grab_state) =
                seat.user_data()
                    .get::<SeatMoveGrabState>()
                    .and_then(|s| s.lock().unwrap().take())
            {
                if grab_state.window.alive() {
                    let window_location =
                        (grab_state.location.to_i32_round() + grab_state.window_offset).as_global();
                    let mut shell = state.common.shell.write();

                    let workspace_handle = shell.active_space(&output).unwrap().handle;
                    for old_output in window_outputs.iter().filter(|o| *o != &output) {
                        grab_state.window.output_leave(old_output);
                    }

                    for (window, _) in grab_state.window.windows() {
                        toplevel_enter_output(&window, &output);
                        if previous != ManagedLayer::Sticky {
                            toplevel_enter_workspace(&window, &workspace_handle);
                        }
                    }

                    match previous {
                        ManagedLayer::Sticky => {
                            grab_state.window.set_geometry(Rectangle::new(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let set = shell.workspaces.sets.get_mut(&output).unwrap();
                            let (window, location) = set
                                .sticky_layer
                                .drop_window(grab_state.window, window_location.to_local(&output));

                            Some((window, location.to_global(&output)))
                        }
                        ManagedLayer::Tiling
                            if shell.active_space(&output).unwrap().tiling_enabled =>
                        {
                            let (window, location) = shell
                                .active_space_mut(&output)
                                .unwrap()
                                .tiling_layer
                                .drop_window(grab_state.window);
                            Some((window, location.to_global(&output)))
                        }
                        _ => {
                            grab_state.window.set_geometry(Rectangle::new(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let theme = shell.theme.clone();
                            let workspace = shell.active_space_mut(&output).unwrap();
                            let (window, location) = workspace.floating_layer.drop_window(
                                grab_state.window,
                                window_location.to_local(&workspace.output),
                            );

                            if matches!(previous, ManagedLayer::Floating) {
                                if let Some(sz) = grab_state.snapping_zone {
                                    if sz == SnappingZone::Maximize {
                                        shell.maximize_toggle(
                                            &window,
                                            &seat,
                                            &state.common.event_loop_handle,
                                        );
                                    } else {
                                        let directions = match sz {
                                            SnappingZone::Maximize => vec![],
                                            SnappingZone::Top => vec![Direction::Up],
                                            SnappingZone::TopLeft => {
                                                vec![Direction::Up, Direction::Left]
                                            }
                                            SnappingZone::Left => vec![Direction::Left],
                                            SnappingZone::BottomLeft => {
                                                vec![Direction::Down, Direction::Left]
                                            }
                                            SnappingZone::Bottom => vec![Direction::Down],
                                            SnappingZone::BottomRight => {
                                                vec![Direction::Down, Direction::Right]
                                            }
                                            SnappingZone::Right => vec![Direction::Right],
                                            SnappingZone::TopRight => {
                                                vec![Direction::Up, Direction::Right]
                                            }
                                        };
                                        for direction in directions {
                                            workspace.floating_layer.move_element(
                                                direction,
                                                &seat,
                                                ManagedLayer::Floating,
                                                &theme,
                                                &window,
                                            );
                                        }
                                    }
                                }
                            }
                            Some((window, location.to_global(&output)))
                        }
                    }
                } else {
                    let mut shell = state.common.shell.write();
                    shell
                        .workspaces
                        .active_mut(&cursor_output)
                        .unwrap()
                        .tiling_layer
                        .cleanup_drag();
                    shell.set_overview_mode(None, state.common.event_loop_handle.clone());
                    None
                }
            } else {
                None
            };

            {
                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.lock().unwrap().unset_shape();
            }

            if let Some((mapped, position)) = position {
                let serial = SERIAL_COUNTER.next_serial();
                if !is_touch_grab {
                    let pointer = seat.get_pointer().unwrap();
                    let current_location = pointer.current_location();

                    if let Some((target, offset)) = mapped.focus_under(
                        current_location - position.as_logical().to_f64(),
                        WindowSurfaceType::ALL,
                    ) {
                        pointer.motion(
                            state,
                            Some((
                                target,
                                position.as_logical().to_f64() - window.geometry().loc.to_f64()
                                    + offset,
                            )),
                            &MotionEvent {
                                location: pointer.current_location(),
                                serial,
                                time: state.common.clock.now().as_millis(),
                            },
                        );
                    }
                }
                Shell::set_focus(
                    state,
                    Some(&KeyboardFocusTarget::from(mapped)),
                    &seat,
                    Some(serial),
                    false,
                )
            }
        });
    }
}
