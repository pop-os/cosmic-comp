// SPDX-License-Identifier: GPL-3.0-only

use crate::{
    backend::render::{
        cursor::{CursorShape, CursorState},
        element::AsGlowRenderer,
        IndicatorShader,
    },
    shell::{
        element::{
            stack_hover::{stack_hover, StackHover},
            CosmicMappedRenderElement,
        },
        focus::target::{KeyboardFocusTarget, PointerFocusTarget},
        CosmicMapped, CosmicSurface,
    },
    utils::prelude::*,
};

use smithay::{
    backend::renderer::{
        element::{utils::RescaleRenderElement, AsRenderElements, RenderElement},
        ImportAll, ImportMem, Renderer,
    },
    desktop::space::SpaceElement,
    input::{
        pointer::{
            AxisFrame, ButtonEvent, GrabStartData as PointerGrabStartData, MotionEvent,
            PointerGrab, PointerInnerHandle, RelativeMotionEvent,
        },
        Seat,
    },
    output::Output,
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, Serial},
    wayland::compositor::SurfaceData,
};
use std::{
    cell::RefCell,
    collections::HashSet,
    time::{Duration, Instant},
};

pub type SeatMoveGrabState = RefCell<Option<MoveGrabState>>;

const RESCALE_ANIMATION_DURATION: f64 = 150.0;

pub struct MoveGrabState {
    window: CosmicMapped,
    window_offset: Point<i32, Logical>,
    indicator_thickness: u8,
    start: Instant,
    tiling: bool,
    stacking_indicator: Option<(StackHover, Point<i32, Logical>)>,
}

impl MoveGrabState {
    pub fn render<I, R>(&self, renderer: &mut R, seat: &Seat<State>, output: &Output) -> Vec<I>
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        I: From<CosmicMappedRenderElement<R>>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let scale = if self.tiling {
            0.6 + ((1.0
                - (Instant::now().duration_since(self.start).as_millis() as f64
                    / RESCALE_ANIMATION_DURATION)
                    .min(1.0))
                * 0.4)
        } else {
            1.0
        };

        let cursor_at = seat.get_pointer().unwrap().current_location();

        let mut window_geo = self.window.geometry();
        window_geo.loc += cursor_at.to_i32_round() + self.window_offset;
        if !output.geometry().intersection(window_geo).is_some() {
            return Vec::new();
        }

        let output_scale: Scale<f64> = output.current_scale().fractional_scale().into();
        let scaling_offset =
            self.window_offset - self.window_offset.to_f64().upscale(scale).to_i32_round();
        let render_location =
            cursor_at.to_i32_round() - output.geometry().loc + self.window_offset - scaling_offset;

        let focus_element = if self.indicator_thickness > 0 {
            Some(
                CosmicMappedRenderElement::from(IndicatorShader::focus_element(
                    renderer,
                    self.window.clone(),
                    Rectangle::from_loc_and_size(
                        render_location,
                        self.window
                            .geometry()
                            .size
                            .to_f64()
                            .upscale(scale)
                            .to_i32_round(),
                    ),
                    self.indicator_thickness,
                    output_scale.x,
                    1.0,
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
                1.0,
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

pub struct MoveGrab {
    window: CosmicMapped,
    start_data: PointerGrabStartData<State>,
    seat: Seat<State>,
    cursor_output: Output,
    window_outputs: HashSet<Output>,
    tiling: bool,
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
                    .contains(handle.current_location().to_i32_round())
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
                .cleanup_drag(&self.cursor_output);
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
            for output in &state.common.shell.outputs {
                if let Some(overlap) = output.geometry().intersection(window_geo) {
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

            if self.tiling {
                let indicator_location = state
                    .common
                    .shell
                    .active_space(&current_output)
                    .tiling_layer
                    .stacking_indicator();

                if indicator_location.is_some() != grab_state.stacking_indicator.is_some() {
                    grab_state.stacking_indicator = indicator_location.map(|geo| {
                        let element = stack_hover(state.common.event_loop_handle.clone(), geo.size);
                        for output in &self.window_outputs {
                            element.output_enter(output, output.geometry());
                        }
                        (element, geo.loc)
                    });
                }
            }
        }
        drop(borrow);

        // While the grab is active, no client has pointer focus
        handle.motion(state, None, event);
        if !self.window.alive() {
            self.ungrab(state, handle, event.serial, event.time);
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
        if handle.current_pressed().is_empty() {
            self.ungrab(state, handle, event.serial, event.time);
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

    fn start_data(&self) -> &PointerGrabStartData<State> {
        &self.start_data
    }
}

impl MoveGrab {
    pub fn new(
        start_data: PointerGrabStartData<State>,
        window: CosmicMapped,
        seat: &Seat<State>,
        initial_cursor_location: Point<f64, Logical>,
        initial_window_location: Point<i32, Logical>,
        indicator_thickness: u8,
        was_tiled: bool,
    ) -> MoveGrab {
        let output = seat.active_output();
        let mut outputs = HashSet::new();
        outputs.insert(output.clone());
        window.output_enter(&output, window.geometry()); // not accurate but...

        let grab_state = MoveGrabState {
            window: window.clone(),
            window_offset: initial_window_location - initial_cursor_location.to_i32_round(),
            indicator_thickness,
            start: Instant::now(),
            stacking_indicator: None,
            tiling: was_tiled,
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
            tiling: was_tiled,
        }
    }

    pub fn is_tiling_grab(&self) -> bool {
        self.tiling
    }

    fn ungrab(
        &mut self,
        state: &mut State,
        handle: &mut PointerInnerHandle<'_, State>,
        serial: Serial,
        time: u32,
    ) {
        // No more buttons are pressed, release the grab.
        let output = self.seat.active_output();

        let position = if let Some(grab_state) = self
            .seat
            .user_data()
            .get::<SeatMoveGrabState>()
            .and_then(|s| s.borrow_mut().take())
        {
            if grab_state.window.alive() {
                let window_location = handle.current_location().to_i32_round()
                    - output.geometry().loc
                    + grab_state.window_offset;

                let workspace_handle = state.common.shell.active_space(&output).handle;
                for old_output in self.window_outputs.iter().filter(|o| *o != &output) {
                    grab_state.window.output_leave(old_output);
                }
                for (window, _) in grab_state.window.windows() {
                    state
                        .common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_workspace(&window, &workspace_handle);
                    state
                        .common
                        .shell
                        .toplevel_info_state
                        .toplevel_enter_output(&window, &output);
                }

                if self.tiling {
                    Some(
                        state
                            .common
                            .shell
                            .active_space_mut(&output)
                            .tiling_layer
                            .drop_window(grab_state.window, &output, handle.current_location()),
                    )
                } else {
                    let offset = state
                        .common
                        .shell
                        .active_space(&output)
                        .floating_layer
                        .space
                        .output_geometry(&output)
                        .unwrap()
                        .loc;
                    grab_state.window.set_geometry(Rectangle::from_loc_and_size(
                        window_location + offset,
                        grab_state.window.geometry().size,
                    ));
                    state
                        .common
                        .shell
                        .active_space_mut(&output)
                        .floating_layer
                        .map_internal(grab_state.window, &output, Some(window_location + offset));

                    let pointer_pos = handle.current_location();
                    let relative_pos = state.common.shell.map_global_to_space(pointer_pos, &output);
                    Some((
                        self.window.clone(),
                        window_location + offset + (pointer_pos - relative_pos).to_i32_round(),
                    ))
                }
            } else {
                None
            }
        } else {
            None
        };

        handle.unset_grab(state, serial, time);

        {
            let cursor_state = self.seat.user_data().get::<CursorState>().unwrap();
            cursor_state.set_shape(CursorShape::Default);
        }

        if let Some((mapped, position)) = position {
            handle.motion(
                state,
                Some((
                    PointerFocusTarget::from(mapped.clone()),
                    position - self.window.geometry().loc,
                )),
                &MotionEvent {
                    location: handle.current_location(),
                    serial,
                    time,
                },
            );
            Common::set_focus(
                state,
                Some(&KeyboardFocusTarget::from(mapped)),
                &self.seat,
                Some(serial),
            )
        }
    }
}
