// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        cursor::{CursorShape, CursorState},
        element::AsGlowRenderer,
        IndicatorShader, Key, Usage,
    },
    shell::{
        element::{
            stack_hover::{stack_hover, StackHover},
            CosmicMappedRenderElement,
        },
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
        CosmicMapped, CosmicSurface, ManagedLayer,
    },
    utils::prelude::*,
};

use calloop::LoopHandle;
use cosmic::theme::CosmicTheme;
use smithay::{
    backend::{
        input::ButtonState,
        renderer::{
            element::{utils::RescaleRenderElement, AsRenderElements, RenderElement},
            ImportAll, ImportMem, Renderer,
        },
    },
    desktop::space::SpaceElement,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GestureHoldBeginEvent, GestureHoldEndEvent,
            GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent,
            GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent,
            GrabStartData as PointerGrabStartData, MotionEvent, PointerGrab, PointerInnerHandle,
            RelativeMotionEvent,
        },
        Seat,
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, SERIAL_COUNTER},
    wayland::compositor::SurfaceData,
};
use std::{
    cell::RefCell,
    collections::HashSet,
    sync::atomic::Ordering,
    time::{Duration, Instant},
};

use super::ReleaseMode;

pub type SeatMoveGrabState = RefCell<Option<MoveGrabState>>;

const RESCALE_ANIMATION_DURATION: f64 = 150.0;

pub struct MoveGrabState {
    window: CosmicMapped,
    window_offset: Point<i32, Logical>,
    indicator_thickness: u8,
    start: Instant,
    previous: ManagedLayer,
    stacking_indicator: Option<(StackHover, Point<i32, Logical>)>,
}

impl MoveGrabState {
    pub fn render<I, R>(
        &self,
        renderer: &mut R,
        seat: &Seat<State>,
        output: &Output,
        theme: &CosmicTheme,
    ) -> Vec<I>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        I: From<CosmicMappedRenderElement<R>>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let scale = if self.previous == ManagedLayer::Tiling {
            0.6 + ((1.0
                - (Instant::now().duration_since(self.start).as_millis() as f64
                    / RESCALE_ANIMATION_DURATION)
                    .min(1.0))
                * 0.4)
        } else {
            1.0
        };
        let alpha = if &seat.active_output() == output {
            1.0
        } else {
            0.4
        };

        let cursor_at = seat.get_pointer().unwrap().current_location();

        let mut window_geo = self.window.geometry();
        window_geo.loc += cursor_at.to_i32_round() + self.window_offset;
        if !output
            .geometry()
            .as_logical()
            .intersection(window_geo)
            .is_some()
        {
            return Vec::new();
        }

        let output_scale: Scale<f64> = output.current_scale().fractional_scale().into();
        let scaling_offset =
            self.window_offset - self.window_offset.to_f64().upscale(scale).to_i32_round();
        let render_location = cursor_at.to_i32_round() - output.geometry().loc.as_logical()
            + self.window_offset
            - scaling_offset;

        let active_window_hint = crate::theme::active_window_hint(theme);
        let focus_element = if self.indicator_thickness > 0 {
            Some(
                CosmicMappedRenderElement::from(IndicatorShader::focus_element(
                    renderer,
                    Key::Window(Usage::MoveGrabIndicator, self.window.clone()),
                    Rectangle::from_loc_and_size(
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
                    output_scale.x,
                    alpha,
                    [
                        active_window_hint.red,
                        active_window_hint.green,
                        active_window_hint.blue,
                    ],
                ))
                .into(),
            )
        } else {
            None
        };

        let (window_elements, popup_elements) = self
            .window
            .split_render_elements::<R, CosmicMappedRenderElement<R>>(
                renderer,
                (render_location - self.window.geometry().loc)
                    .to_physical_precise_round(output_scale),
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
            .chain(popup_elements)
            .chain(focus_element)
            .chain(window_elements.into_iter().map(|elem| match elem {
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
            }))
            .map(I::from)
            .collect()
    }

    pub fn send_frames(
        &self,
        output: &Output,
        time: impl Into<Duration>,
        throttle: Option<Duration>,
        primary_scan_out_output: impl FnMut(&WlSurface, &SurfaceData) -> Option<Output> + Copy,
    ) {
        self.window
            .active_window()
            .send_frame(output, time, throttle, primary_scan_out_output)
    }

    pub fn window(&self) -> CosmicSurface {
        self.window.active_window()
    }
}

struct NotSend<T>(pub T);
unsafe impl<T> Send for NotSend<T> {}

pub struct MoveGrab {
    window: CosmicMapped,
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
    cursor_output: Output,
    window_outputs: HashSet<Output>,
    previous: ManagedLayer,
    release: ReleaseMode,
    // SAFETY: This is only used on drop which will always be on the main thread
    evlh: NotSend<LoopHandle<'static, State>>,
}

impl PointerGrab<State> for MoveGrab {
    fn motion(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        _focus: Option<(PointerFocusTarget, Point<i32, Logical>)>,
        event: &MotionEvent,
    ) {
        let Some(current_output) = state
            .common
            .shell
            .outputs()
            .find(|output| {
                output
                    .geometry()
                    .as_logical()
                    .overlaps_or_touches(Rectangle::from_loc_and_size(
                        handle.current_location().to_i32_floor(),
                        (0,0),
                    ))
            })
            .cloned()
        else {
            return;
        };
        if self.cursor_output != current_output {
            state
                .common
                .shell
                .workspaces
                .active_mut(&self.cursor_output)
                .tiling_layer
                .cleanup_drag();
            self.cursor_output = current_output.clone();
        }

        let mut borrow = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .map(|s| s.borrow_mut());
        if let Some(grab_state) = borrow.as_mut().and_then(|s| s.as_mut()) {
            let mut window_geo = self.window.geometry();
            window_geo.loc += event.location.to_i32_round() + grab_state.window_offset;
            for output in state.common.shell.outputs() {
                if let Some(overlap) = output.geometry().as_logical().intersection(window_geo) {
                    if self.window_outputs.insert(output.clone()) {
                        self.window.output_enter(output, overlap);
                        if let Some(indicator) =
                            grab_state.stacking_indicator.as_ref().map(|x| &x.0)
                        {
                            indicator.output_enter(output, overlap);
                        }
                    }
                } else if self.window_outputs.remove(&output) {
                    self.window.output_leave(output);
                    if let Some(indicator) = grab_state.stacking_indicator.as_ref().map(|x| &x.0) {
                        indicator.output_leave(output);
                    }
                }
            }

            if self.previous == ManagedLayer::Tiling {
                let indicator_location = state
                    .common
                    .shell
                    .active_space(&current_output)
                    .tiling_layer
                    .stacking_indicator();

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
                                Rectangle::from_loc_and_size(
                                    (0, 0),
                                    output.geometry().size.as_logical(),
                                ),
                            );
                        }
                        (element, geo.loc.as_logical())
                    });
                }
            }
        }
        drop(borrow);

        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            handle.unset_grab(state, event.serial, event.time, true);
        }
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
        handle.button(state, event);
        match self.release {
            ReleaseMode::NoMouseButtons => {
                if handle.current_pressed().is_empty() {
                    handle.unset_grab(state, event.serial, event.time, true);
                }
            }
            ReleaseMode::Click => {
                if event.state == ButtonState::Pressed {
                    handle.unset_grab(state, event.serial, event.time, true);
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
        &self.start_data
    }
}

impl MoveGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        window: CosmicMapped,
        seat: &Seat<State>,
        initial_cursor_location: Point<f64, Global>,
        initial_window_location: Point<i32, Global>,
        indicator_thickness: u8,
        previous_layer: ManagedLayer,
        release: ReleaseMode,
        evlh: LoopHandle<'static, State>,
    ) -> MoveGrab {
        let output = seat.active_output();
        let mut outputs = HashSet::new();
        outputs.insert(output.clone());
        window.output_enter(&output, window.geometry()); // not accurate but...
        window.moved_since_mapped.store(true, Ordering::SeqCst);

        let grab_state = MoveGrabState {
            window: window.clone(),
            window_offset: (initial_window_location - initial_cursor_location.to_i32_round())
                .as_logical(),
            indicator_thickness,
            start: Instant::now(),
            stacking_indicator: None,
            previous: previous_layer,
        };

        *seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .unwrap()
            .borrow_mut() = Some(grab_state);

        {
            let cursor_state = seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(CursorShape::Grab);
        }

        MoveGrab {
            window,
            start_data,
            seat: seat.clone(),
            window_outputs: outputs,
            cursor_output: output,
            previous: previous_layer,
            release,
            evlh: NotSend(evlh),
        }
    }

    pub fn is_tiling_grab(&self) -> bool {
        self.previous == ManagedLayer::Tiling
    }
}

impl Drop for MoveGrab {
    fn drop(&mut self) {
        // No more buttons are pressed, release the grab.
        let output = self.seat.active_output();
        let seat = self.seat.clone();
        let window_outputs = self.window_outputs.drain().collect::<HashSet<_>>();
        let previous = self.previous;
        let window = self.window.clone();

        let _ = self.evlh.0.insert_idle(move |state| {
            let pointer = seat.get_pointer().unwrap();

            let position: Option<(CosmicMapped, Point<i32, Global>)> = if let Some(grab_state) =
                seat.user_data()
                    .get::<SeatMoveGrabState>()
                    .and_then(|s| s.borrow_mut().take())
            {
                if grab_state.window.alive() {
                    let window_location = (pointer.current_location().to_i32_round()
                        + grab_state.window_offset)
                        .as_global();

                    let workspace_handle = state.common.shell.active_space(&output).handle;
                    for old_output in window_outputs.iter().filter(|o| *o != &output) {
                        grab_state.window.output_leave(old_output);
                    }
                    for (window, _) in grab_state.window.windows() {
                        state
                            .common
                            .shell
                            .toplevel_info_state
                            .toplevel_enter_output(&window, &output);
                        if previous != ManagedLayer::Sticky {
                            state
                                .common
                                .shell
                                .toplevel_info_state
                                .toplevel_enter_workspace(&window, &workspace_handle);
                        }
                    }

                    match previous {
                        ManagedLayer::Tiling => {
                            let (window, location) = state
                                .common
                                .shell
                                .active_space_mut(&output)
                                .tiling_layer
                                .drop_window(grab_state.window);
                            Some((window, location.to_global(&output)))
                        }
                        ManagedLayer::Floating => {
                            grab_state.window.set_geometry(Rectangle::from_loc_and_size(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let workspace = state.common.shell.active_space_mut(&output);
                            workspace.floating_layer.map_internal(
                                grab_state.window,
                                Some(window_location.to_local(&workspace.output)),
                                None,
                            );

                            Some((window.clone(), window_location))
                        }
                        ManagedLayer::Sticky => {
                            grab_state.window.set_geometry(Rectangle::from_loc_and_size(
                                window_location,
                                grab_state.window.geometry().size.as_global(),
                            ));
                            let set = state.common.shell.workspaces.sets.get_mut(&output).unwrap();
                            set.sticky_layer.map_internal(
                                grab_state.window,
                                Some(window_location.to_local(&output)),
                                None,
                            );

                            Some((window.clone(), window_location))
                        }
                    }
                } else {
                    None
                }
            } else {
                None
            };

            {
                let cursor_state = seat.user_data().get::<CursorState>().unwrap();
                cursor_state.set_shape(CursorShape::Default);
            }

            if let Some((mapped, position)) = position {
                let serial = SERIAL_COUNTER.next_serial();
                pointer.motion(
                    state,
                    Some((
                        PointerFocusTarget::from(mapped.clone()),
                        position.as_logical() - window.geometry().loc,
                    )),
                    &MotionEvent {
                        location: pointer.current_location(),
                        serial,
                        time: 0,
                    },
                );
                Common::set_focus(
                    state,
                    Some(&KeyboardFocusTarget::from(mapped)),
                    &seat,
                    Some(serial),
                )
            }
        });
    }
}
