// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::{HashMap, VecDeque},
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use keyframe::{ease, functions::EaseInOutCubic};
use smithay::{
    backend::renderer::{
        element::{
            utils::{Relocate, RelocateRenderElement, RescaleRenderElement},
            AsRenderElements, RenderElement,
        },
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind, Space, WindowSurfaceType},
    input::Seat,
    output::Output,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, Size},
    wayland::seat::WaylandFocus,
};

use crate::{
    backend::render::{element::AsGlowRenderer, IndicatorShader, Key, Usage},
    shell::{
        element::{
            resize_indicator::ResizeIndicator,
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult, TAB_HEIGHT},
            window::CosmicWindowRenderElement,
            CosmicMapped, CosmicMappedRenderElement, CosmicWindow, MaximizedState,
        },
        focus::{
            target::{KeyboardFocusTarget, PointerFocusTarget},
            FocusStackMut,
        },
        grabs::{GrabStartData, ReleaseMode, ResizeEdge},
        CosmicSurface, Direction, ManagedLayer, MoveResult, ResizeDirection, ResizeMode,
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
};

mod grabs;
pub use self::grabs::*;

pub const ANIMATION_DURATION: Duration = Duration::from_millis(200);
pub const MINIMIZE_ANIMATION_DURATION: Duration = Duration::from_millis(320);

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pub(crate) space: Space<CosmicMapped>,
    spawn_order: Vec<CosmicMapped>,
    animations: HashMap<CosmicMapped, Animation>,
    hovered_stack: Option<(CosmicMapped, Rectangle<i32, Local>)>,
    dirty: AtomicBool,
    pub theme: cosmic::Theme,
}

#[derive(Debug)]
enum Animation {
    Tiled {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
    },
    Minimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
    Unminimize {
        start: Instant,
        previous_geometry: Rectangle<i32, Local>,
        target_geometry: Rectangle<i32, Local>,
    },
}

impl Animation {
    fn start(&self) -> &Instant {
        match self {
            Animation::Tiled { start, .. } => start,
            Animation::Minimize { start, .. } => start,
            Animation::Unminimize { start, .. } => start,
        }
    }

    fn alpha(&self) -> f32 {
        match self {
            Animation::Tiled { .. } => 1.0,
            Animation::Minimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(MINIMIZE_ANIMATION_DURATION)
                    .as_secs_f32()
                    / MINIMIZE_ANIMATION_DURATION.as_secs_f32();
                1.0 - ((percentage - 0.5).max(0.0) * 2.0)
            }
            Animation::Unminimize { start, .. } => {
                let percentage = Instant::now()
                    .duration_since(*start)
                    .min(MINIMIZE_ANIMATION_DURATION)
                    .as_secs_f32()
                    / MINIMIZE_ANIMATION_DURATION.as_secs_f32();
                (percentage * 2.0).min(1.0)
            }
        }
    }

    fn previous_geometry(&self) -> &Rectangle<i32, Local> {
        match self {
            Animation::Tiled {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Minimize {
                previous_geometry, ..
            } => previous_geometry,
            Animation::Unminimize {
                previous_geometry, ..
            } => previous_geometry,
        }
    }

    fn geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        current_geometry: Rectangle<i32, Local>,
        tiled_state: Option<&TiledCorners>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        let (duration, target_rect) = match self {
            Animation::Minimize {
                target_geometry, ..
            }
            | Animation::Unminimize {
                target_geometry, ..
            } => (MINIMIZE_ANIMATION_DURATION, target_geometry.clone()),
            Animation::Tiled { .. } => {
                let target_geometry = if let Some(target_rect) =
                    tiled_state.map(|state| state.relative_geometry(output_geometry, gaps))
                {
                    target_rect
                } else {
                    current_geometry
                };
                (ANIMATION_DURATION, target_geometry)
            }
        };
        let previous_rect = self.previous_geometry().clone();
        let start = *self.start();
        let now = Instant::now();
        let progress =
            now.duration_since(start).min(duration).as_secs_f64() / duration.as_secs_f64();

        ease(
            EaseInOutCubic,
            EaseRectangle(previous_rect),
            EaseRectangle(target_rect),
            progress,
        )
        .unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TiledCorners {
    Top,
    TopRight,
    Right,
    BottomRight,
    Bottom,
    BottomLeft,
    Left,
    TopLeft,
}

impl TiledCorners {
    pub fn relative_geometry(
        &self,
        output_geometry: Rectangle<i32, Logical>,
        gaps: (i32, i32),
    ) -> Rectangle<i32, Local> {
        let (_, inner) = gaps;
        let (loc, size) = match self {
            TiledCorners::Bottom => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomLeft => (
                Point::from((
                    output_geometry.loc.x + inner,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::BottomRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + (output_geometry.size.h / 2) + inner / 2,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Left => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
            TiledCorners::Top => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w - inner * 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopLeft => (
                Point::from((output_geometry.loc.x + inner, output_geometry.loc.y + inner)),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::TopRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h / 2 - inner * 3 / 2,
                )),
            ),
            TiledCorners::Right => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2) + inner / 2,
                    output_geometry.loc.y + inner,
                )),
                Size::from((
                    output_geometry.size.w / 2 - inner * 3 / 2,
                    output_geometry.size.h - inner * 2,
                )),
            ),
        };

        Rectangle::from_loc_and_size(loc, size).as_local()
    }
}

impl FloatingLayout {
    pub fn new(theme: cosmic::Theme, output: &Output) -> FloatingLayout {
        let mut layout = Self {
            theme,
            ..Default::default()
        };
        layout.space.map_output(output, (0, 0));
        layout
    }

    pub fn set_output(&mut self, output: &Output) {
        let old_output = self.space.outputs().next().unwrap().clone();
        self.space.unmap_output(&old_output);
        self.space.map_output(output, (0, 0));

        let old_output_geometry = {
            let layers = layer_map_for_output(&old_output);
            layers.non_exclusive_zone()
        };
        let output_geometry = {
            let layers = layer_map_for_output(&output);
            layers.non_exclusive_zone()
        };

        for mapped in self
            .space
            .elements()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            let tiled_state = mapped.floating_tiled.lock().unwrap().clone();
            if let Some(tiled_state) = tiled_state {
                let geometry = tiled_state.relative_geometry(output_geometry, self.gaps());
                self.map_internal(
                    mapped,
                    Some(geometry.loc),
                    Some(geometry.size.as_logical()),
                    None,
                );
            } else {
                let geometry = self.space.element_geometry(&mapped).unwrap();
                let new_loc = (
                    geometry.loc.x.saturating_sub(old_output_geometry.loc.x)
                        / old_output_geometry.size.w
                        * output_geometry.size.w
                        + output_geometry.loc.x,
                    geometry.loc.y.saturating_sub(old_output_geometry.loc.y)
                        / old_output_geometry.size.h
                        * output_geometry.size.h
                        + output_geometry.loc.y,
                );
                self.map_internal(mapped, Some(Point::from(new_loc)), None, None);
            }
        }

        self.refresh();
    }

    pub fn map(
        &mut self,
        mapped: impl Into<CosmicMapped>,
        position: impl Into<Option<Point<i32, Local>>>,
    ) {
        let mapped = mapped.into();
        let position = position.into();

        self.map_internal(mapped, position, None, None)
    }

    pub fn map_maximized(
        &mut self,
        mapped: CosmicMapped,
        previous_geometry: Rectangle<i32, Local>,
        animate: bool,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone().as_local();

        mapped.set_bounds(geometry.size.as_logical());
        mapped.set_tiled(true);
        mapped.set_maximized(true);
        mapped.set_geometry(geometry.to_global(&output));
        mapped.configure();

        if let Some(pos) = self.spawn_order.iter().position(|m| m == &mapped) {
            self.spawn_order.truncate(pos);
        }

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);

        if animate {
            self.animations.insert(
                mapped.clone(),
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                },
            );
        }
        if mapped.floating_tiled.lock().unwrap().take().is_some() {
            if let Some(state) = mapped.maximized_state.lock().unwrap().as_mut() {
                if let Some(real_old_geo) = mapped.last_geometry.lock().unwrap().clone() {
                    state.original_geometry = real_old_geo;
                }
            };
        }
        self.space
            .map_element(mapped, geometry.loc.as_logical(), true);
    }

    pub(in crate::shell) fn map_internal(
        &mut self,
        mapped: CosmicMapped,
        position: Option<Point<i32, Local>>,
        size: Option<Size<i32, Logical>>,
        prev: Option<Rectangle<i32, Local>>,
    ) {
        let already_mapped = self.space.element_geometry(&mapped).map(RectExt::as_local);
        let mut win_geo = mapped.geometry().as_local();

        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let output_geometry = layers.non_exclusive_zone();
        mapped.set_bounds(output_geometry.size);
        let last_geometry = mapped.last_geometry.lock().unwrap().clone();
        let min_size = mapped.min_size().unwrap_or((320, 240).into());

        if let Some(size) = size
            .map(SizeExt::as_local)
            .or(last_geometry.map(|g| g.size))
        {
            win_geo.size = size;
        } else {
            let max_size = mapped.max_size().unwrap_or(
                (
                    min_size.w.max(output_geometry.size.w / 3 * 2),
                    min_size.h.max(output_geometry.size.h / 3 * 2),
                )
                    .into(),
            );

            // if the current geometry is too large
            if win_geo.size.w > max_size.w {
                // try a more reasonable size
                let mut width = output_geometry.size.w / 3 * 2;
                if max_size.w != 0 {
                    // don't go larger then the max_size ...
                    width = std::cmp::min(max_size.w, width);
                }
                if min_size.w != 0 {
                    // ... but also don't go smaller than the min_size
                    width = std::cmp::max(min_size.w, width);
                }
                win_geo.size.w = width;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.w = std::cmp::min(win_geo.size.w, output_geometry.size.w);

            if win_geo.size.h > max_size.h {
                // try a more reasonable size
                let mut height = output_geometry.size.h / 3 * 2;
                if max_size.h != 0 {
                    // don't go larger then the max_size ...
                    height = std::cmp::min(max_size.h, height);
                }
                if min_size.h != 0 {
                    // ... but also don't go smaller than the min_size
                    height = std::cmp::max(min_size.h, height);
                }
                win_geo.size.h = height;
            }
            // but no matter the supported sizes, don't be larger than our non-exclusive-zone
            win_geo.size.h = std::cmp::min(win_geo.size.h, output_geometry.size.h);
        }

        let position = position
            .or_else(|| last_geometry.map(|g| g.loc))
            .unwrap_or_else(|| {
                // cleanup moved windows
                if let Some(pos) = self
                    .spawn_order
                    .iter()
                    .position(|w| !w.alive() || w.moved_since_mapped.load(Ordering::SeqCst))
                {
                    self.spawn_order.truncate(pos);
                }

                let three_fours_width = (output_geometry.size.w / 4 * 3).max(360);

                // figure out new position
                let pos = self
                    .spawn_order
                    .last()
                    .and_then(|window| self.space.element_geometry(window))
                    .filter(|geo| {
                        geo.size.w < three_fours_width
                            && win_geo.size.w < three_fours_width
                            && output_geometry.contains_rect(*geo)
                    })
                    .map(|geometry| {
                        let mut geometry: Rectangle<u32, Logical> = Rectangle::from_loc_and_size(
                            (geometry.loc.x as u32, geometry.loc.y as u32),
                            (geometry.size.w as u32, geometry.size.h as u32),
                        );

                        // move down
                        geometry.loc.y += 48;

                        // do we need to address the height?
                        let new_column = if geometry.loc.y + min_size.h as u32
                            <= (output_geometry.loc.y + output_geometry.size.h - 16) as u32
                        {
                            // alternate to the sides
                            let offset = if self
                                .spawn_order
                                .iter()
                                .flat_map(|w| self.space.element_geometry(w))
                                .filter(|geo| geo.size.w < three_fours_width)
                                .count()
                                % 2
                                == 0
                            {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub(96 + (win_geo.size.w as u32))
                            } else {
                                (geometry.loc.x + geometry.size.w)
                                    .checked_sub((win_geo.size.w as u32).saturating_sub(48))
                            };

                            if let Some(offset) = offset {
                                geometry.loc.x = offset;
                                // do we need to resize?
                                if geometry.loc.y as i32 + win_geo.size.h
                                    > output_geometry.loc.y + output_geometry.size.h - 16
                                {
                                    win_geo.size.h =
                                        (output_geometry.loc.y + output_geometry.size.h - 16)
                                            - geometry.loc.y as i32;
                                }

                                false
                            } else {
                                true
                            }
                        } else {
                            true
                        };

                        if new_column {
                            let min_y = self
                                .spawn_order
                                .iter()
                                .flat_map(|w| {
                                    self.space
                                        .element_geometry(w)
                                        .filter(|geo| geo.size.w < three_fours_width)
                                        .map(|geo| geo.loc.y)
                                })
                                .min()
                                .unwrap() as u32;
                            geometry.loc.y = min_y.saturating_sub(16);

                            match geometry.loc.x.checked_sub(144) {
                                Some(new_x) => geometry.loc.x = new_x,
                                None => {
                                    // if we go out to the left, cycle around to the right
                                    geometry.loc.x =
                                        ((output_geometry.loc.x + output_geometry.size.w) as u32)
                                            .saturating_sub(geometry.size.w + 16)
                                }
                            };
                        }

                        // check padding again
                        if geometry.loc.x < (output_geometry.loc.x + 16) as u32 {
                            geometry.loc.x = (output_geometry.loc.x + 16) as u32;
                        }
                        if geometry.loc.y < (output_geometry.loc.y + 16) as u32 {
                            geometry.loc.y = (output_geometry.loc.y + 16) as u32;
                        }
                        // if the width would be too high, we wouldn't be here
                        if geometry.loc.y as i32 + win_geo.size.h
                            > (output_geometry.loc.y + output_geometry.size.h - 16)
                        {
                            win_geo.size.h = output_geometry.loc.y + output_geometry.size.h
                                - 16
                                - geometry.loc.y as i32;
                        }

                        Point::<i32, Logical>::from((geometry.loc.x as i32, geometry.loc.y as i32))
                    })
                    .unwrap_or_else(|| {
                        (
                            output_geometry.loc.x + output_geometry.size.w / 2 - win_geo.size.w / 2,
                            output_geometry.loc.y
                                + (output_geometry.size.h / 2 - win_geo.size.h / 2)
                                    .min(output_geometry.size.h / 8),
                        )
                            .into()
                    })
                    .as_local();

                mapped.moved_since_mapped.store(false, Ordering::SeqCst);
                self.spawn_order.push(mapped.clone());

                pos
            });

        mapped.set_tiled(false);
        mapped
            .set_geometry(Rectangle::from_loc_and_size(position, win_geo.size).to_global(&output));
        mapped.configure();

        if let Some(previous_geometry) = prev.or(already_mapped) {
            self.animations.insert(
                mapped.clone(),
                Animation::Tiled {
                    start: Instant::now(),
                    previous_geometry,
                },
            );
        }
        self.space.map_element(mapped, position.as_logical(), false);
    }

    pub fn remap_minimized(
        &mut self,
        mapped: CosmicMapped,
        from: Rectangle<i32, Local>,
        position: Point<i32, Local>,
    ) {
        let output = self.space.outputs().next().unwrap().clone();
        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone().as_local();
        mapped.set_bounds(geometry.size.as_logical());
        let window_size = mapped.geometry().size;

        if mapped.is_maximized(false) {
            mapped.set_geometry(geometry.to_global(&output));
            mapped.configure();
        } else {
            mapped.set_geometry(Rectangle::from_loc_and_size(
                position.to_global(&output),
                window_size.as_global(),
            ));
        }

        mapped.set_minimized(false);
        self.space
            .map_element(mapped.clone(), position.as_logical(), true);
        let target_geometry = self.space.element_geometry(&mapped).unwrap().as_local();

        self.animations.insert(
            mapped,
            Animation::Unminimize {
                start: Instant::now(),
                previous_geometry: from,
                target_geometry,
            },
        );
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> bool {
        if let Some(_) = window.floating_tiled.lock().unwrap().take() {
            if let Some(last_size) = window.last_geometry.lock().unwrap().map(|geo| geo.size) {
                if let Some(location) = self.space.element_location(window) {
                    window.set_tiled(false);
                    window.set_geometry(
                        Rectangle::from_loc_and_size(location, last_size.as_logical())
                            .as_local()
                            .to_global(self.space.outputs().next().unwrap()),
                    );
                    window.configure();
                }
            }
        } else if !window.is_maximized(true) && !window.is_fullscreen(true) {
            if let Some(location) = self.space.element_location(window) {
                *window.last_geometry.lock().unwrap() = Some(
                    Rectangle::from_loc_and_size(
                        location,
                        window
                            .pending_size()
                            .unwrap_or_else(|| window.geometry().size),
                    )
                    .as_local(),
                )
            }
        }

        let _ = self.animations.remove(window);

        let was_unmaped = self.space.elements().any(|e| e == window);
        self.space.unmap_elem(&window);
        if was_unmaped {
            if let Some(pos) = self.spawn_order.iter().position(|w| w == window) {
                self.spawn_order.truncate(pos);
            }
            window.moved_since_mapped.store(true, Ordering::SeqCst);
        }
        was_unmaped
    }

    pub fn unmap_minimize(
        &mut self,
        window: &CosmicMapped,
        to: Rectangle<i32, Local>,
    ) -> Option<(CosmicMapped, Point<i32, Local>)> {
        let previous_geometry = self.space.element_geometry(window);
        self.space.unmap_elem(&window);
        if let Some(previous_geometry) = previous_geometry {
            if let Some(pos) = self.spawn_order.iter().position(|w| w == window) {
                self.spawn_order.truncate(pos);
            }
            window.moved_since_mapped.store(true, Ordering::SeqCst);
            self.animations.insert(
                window.clone(),
                Animation::Minimize {
                    start: Instant::now(),
                    previous_geometry: previous_geometry.as_local(),
                    target_geometry: to,
                },
            );

            window.set_minimized(true);
            Some((window.clone(), previous_geometry.loc.as_local()))
        } else {
            None
        }
    }

    pub fn drop_window(
        &mut self,
        window: CosmicMapped,
        position: Point<i32, Local>,
    ) -> (CosmicMapped, Point<i32, Local>) {
        if self
            .hovered_stack
            .as_ref()
            .is_some_and(|(stack, _)| stack == &window || !stack.alive())
        {
            let _ = self.hovered_stack.take();
        }

        if let Some((mapped, geo)) = self.hovered_stack.take() {
            let stack = mapped.stack_ref().unwrap();
            for surface in window.windows().map(|s| s.0) {
                stack.add_window(surface, None);
            }
            (mapped, geo.loc)
        } else {
            self.map_internal(window.clone(), Some(position), None, None);
            (window, position)
        }
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        self.space.element_geometry(elem).map(RectExt::as_local)
    }

    pub fn element_under(&mut self, location: Point<f64, Local>) -> Option<KeyboardFocusTarget> {
        self.space
            .element_under(location.as_logical())
            .map(|(mapped, _)| mapped.clone().into())
    }

    pub fn surface_under(
        &mut self,
        location: Point<f64, Local>,
    ) -> Option<(PointerFocusTarget, Point<i32, Local>)> {
        let res = self
            .space
            .element_under(location.as_logical())
            .map(|(mapped, p)| (mapped.clone(), p.as_local()));

        if let Some((mapped, _)) = res.as_ref() {
            let geometry = self.space.element_geometry(mapped).unwrap();
            let offset = location.y.round() as i32 - geometry.loc.y;
            if mapped.is_stack() && offset.is_positive() && offset <= TAB_HEIGHT {
                self.hovered_stack = Some((mapped.clone(), geometry.as_local()));
            } else {
                self.hovered_stack.take();
            }
        } else {
            self.hovered_stack.take();
        }

        res.and_then(|(element, space_offset)| {
            let point = location - space_offset.to_f64();
            element
                .focus_under(point.as_logical())
                .map(|(surface, surface_offset)| {
                    (surface, space_offset + surface_offset.as_local())
                })
        })
    }

    pub fn stacking_indicator(&self) -> Option<Rectangle<i32, Local>> {
        self.hovered_stack.as_ref().map(|(_, geo)| geo.clone())
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        start_data: GrabStartData,
        edges: ResizeEdge,
        release: ReleaseMode,
    ) -> Option<ResizeSurfaceGrab> {
        if seat.get_pointer().is_some() {
            let location = self.space.element_location(&mapped)?.as_local();
            let size = mapped.geometry().size;
            mapped.moved_since_mapped.store(true, Ordering::SeqCst);

            Some(grabs::ResizeSurfaceGrab::new(
                start_data,
                mapped.clone(),
                edges,
                location,
                size,
                seat,
                release,
            ))
        } else {
            None
        }
    }

    pub fn resize(
        &mut self,
        focused: &KeyboardFocusTarget,
        direction: ResizeDirection,
        edge: ResizeEdge,
        amount: i32,
    ) -> bool {
        let Some(toplevel) = focused.toplevel() else {
            return false;
        };
        let Some(mapped) = self
            .space
            .elements()
            .find(|m| m.has_surface(&toplevel, WindowSurfaceType::TOPLEVEL))
        else {
            return false;
        };
        if mapped.is_maximized(true) {
            return false;
        }

        let Some(original_geo) = self.space.element_geometry(mapped) else {
            return false; // we don't have that window
        };
        let mut geo = original_geo.clone();

        if edge.contains(ResizeEdge::RIGHT) || edge.contains(ResizeEdge::LEFT) {
            if direction == ResizeDirection::Inwards {
                geo.size.w = (geo.size.w as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.w += amount;
            }
            if edge.contains(ResizeEdge::LEFT) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.x += amount;
                } else {
                    geo.loc.x = (geo.loc.x as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }
        if edge.contains(ResizeEdge::BOTTOM) || edge.contains(ResizeEdge::TOP) {
            if direction == ResizeDirection::Inwards {
                geo.size.h = (geo.size.h as u32).saturating_sub(amount as u32) as i32;
            } else {
                geo.size.h += amount;
            }
            if edge.contains(ResizeEdge::TOP) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.y += amount;
                } else {
                    geo.loc.y = (geo.loc.y as u32).saturating_sub(amount as u32) as i32;
                }
            }
        }

        let bounding_box = self
            .space
            .output_geometry(self.space.outputs().next().unwrap())
            .unwrap();
        let (min_size, max_size) = (mapped.min_size(), mapped.max_size());
        let min_width = min_size.map(|s| s.w).unwrap_or(360);
        let min_height = min_size.map(|s| s.h).unwrap_or(240);
        let max_width = max_size.map(|s| s.w).unwrap_or(i32::max_value());
        let max_height = max_size.map(|s| s.h).unwrap_or(i32::max_value());

        geo.size.w = min_width.max(geo.size.w).min(max_width);
        geo.size.h = min_height.max(geo.size.h).min(max_height);
        geo = geo.intersection(bounding_box).unwrap();

        *mapped.resize_state.lock().unwrap() = Some(ResizeState::Resizing(ResizeData {
            edges: edge,
            initial_window_location: original_geo.loc.as_local(),
            initial_window_size: original_geo.size,
        }));

        mapped.moved_since_mapped.store(true, Ordering::SeqCst);
        mapped.set_resizing(true);
        mapped.set_geometry(
            geo.as_local()
                .to_global(self.space.outputs().next().unwrap()),
        );
        mapped.configure();

        true
    }

    pub fn toggle_stacking(&mut self, mapped: &CosmicMapped) -> Option<KeyboardFocusTarget> {
        if !self.space.elements().any(|m| m == mapped) {
            return None;
        }

        let output = self.space.outputs().next().unwrap().clone();
        let mut mapped = mapped.clone();
        let geo = self.space.element_geometry(&mapped).unwrap();
        let location = geo.loc;

        if mapped.is_window() {
            // if it is just a window
            self.space.unmap_elem(&mapped);
            mapped.convert_to_stack((&output, mapped.bbox()), self.theme.clone());
            self.map_internal(
                mapped.clone(),
                Some(location.as_local()),
                Some(geo.size),
                None,
            );
            Some(KeyboardFocusTarget::Element(mapped))
        } else {
            // if we have a stack
            let mut surfaces = mapped.windows().map(|(s, _)| s).collect::<VecDeque<_>>();
            let first = surfaces.pop_front().expect("Stack without a window?");

            self.space.unmap_elem(&mapped);
            let handle = mapped.loop_handle();
            mapped.convert_to_surface(first, (&output, mapped.bbox()), self.theme.clone());

            // map the rest
            for other in surfaces {
                other.try_force_undecorated(false);
                other.set_tiled(false);
                let window = CosmicMapped::from(CosmicWindow::new(
                    other,
                    handle.clone(),
                    self.theme.clone(),
                ));
                window.output_enter(&output, window.bbox());

                {
                    let layer_map = layer_map_for_output(&output);
                    window.set_bounds(layer_map.non_exclusive_zone().size);
                }

                self.map(window, None);
            }
            self.space.map_element(mapped.clone(), location, false);

            Some(KeyboardFocusTarget::Element(mapped))
        }
    }

    pub fn toggle_stacking_focused<'a>(
        &mut self,
        seat: &Seat<State>,
        mut focus_stack: FocusStackMut,
    ) -> Option<KeyboardFocusTarget> {
        let Some(KeyboardFocusTarget::Element(elem)) = seat.get_keyboard().unwrap().current_focus()
        else {
            return None;
        };

        let res = self.toggle_stacking(&elem);
        focus_stack.append(&elem);
        res
    }

    pub fn move_element<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: &cosmic::Theme,
        element: &CosmicMapped,
    ) -> MoveResult {
        match element.handle_move(direction) {
            StackMoveResult::Handled => MoveResult::Done,
            StackMoveResult::MoveOut(surface, loop_handle) => {
                let mapped: CosmicMapped =
                    CosmicWindow::new(surface, loop_handle, theme.clone()).into();
                let output = seat.active_output();
                let pos = self.space.element_geometry(element).unwrap().loc
                    + match direction {
                        Direction::Up => Point::from((5, -10)),
                        Direction::Down => Point::from((5, 10)),
                        Direction::Left => Point::from((-10, 5)),
                        Direction::Right => Point::from((10, 5)),
                    };
                let position = self
                    .space
                    .output_geometry(&output)
                    .unwrap()
                    .overlaps({
                        let mut geo = mapped.geometry();
                        geo.loc += pos;
                        geo
                    })
                    .then_some(pos);

                self.map_internal(mapped.clone(), position.map(PointExt::as_local), None, None);
                MoveResult::ShiftFocus(KeyboardFocusTarget::Element(mapped))
            }
            StackMoveResult::Default => {
                let mut tiled_state = element.floating_tiled.lock().unwrap();

                let output = self.space.outputs().next().unwrap().clone();
                let layers = layer_map_for_output(&output);
                let output_geometry = layers.non_exclusive_zone();
                std::mem::drop(layers);

                let current_geometry = self
                    .space
                    .element_geometry(element)
                    .map(RectExt::as_local)
                    .unwrap();
                let start_rectangle = if let Some(anim) = self.animations.remove(element) {
                    anim.geometry(
                        output_geometry,
                        current_geometry,
                        tiled_state.as_ref(),
                        self.gaps(),
                    )
                } else {
                    current_geometry
                };

                let new_state = match (direction, &*tiled_state) {
                    // figure out if we are moving between workspaces/outputs
                    (
                        Direction::Up,
                        Some(TiledCorners::Top)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::TopRight),
                    )
                    | (
                        Direction::Down,
                        Some(TiledCorners::Bottom)
                        | Some(TiledCorners::BottomLeft)
                        | Some(TiledCorners::BottomRight),
                    )
                    | (
                        Direction::Left,
                        Some(TiledCorners::Left)
                        | Some(TiledCorners::TopLeft)
                        | Some(TiledCorners::BottomLeft),
                    )
                    | (
                        Direction::Right,
                        Some(TiledCorners::Right)
                        | Some(TiledCorners::TopRight)
                        | Some(TiledCorners::BottomRight),
                    ) => {
                        return MoveResult::MoveFurther(KeyboardFocusTarget::Element(
                            element.clone(),
                        ));
                    }

                    // to we go maximized?
                    (Direction::Up, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Top))
                    | (Direction::Left, Some(TiledCorners::Right))
                    | (Direction::Right, Some(TiledCorners::Left)) => {
                        std::mem::drop(tiled_state);

                        let mut maximized_state = element.maximized_state.lock().unwrap();
                        *maximized_state = Some(MaximizedState {
                            original_geometry: start_rectangle,
                            original_layer: layer,
                        });
                        std::mem::drop(maximized_state);

                        self.map_maximized(element.clone(), start_rectangle, true);
                        return MoveResult::Done;
                    }

                    // figure out if we need to quater tile
                    (Direction::Up, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Top)) => TiledCorners::TopLeft,
                    (Direction::Right, Some(TiledCorners::Top))
                    | (Direction::Up, Some(TiledCorners::Right)) => TiledCorners::TopRight,
                    (Direction::Down, Some(TiledCorners::Left))
                    | (Direction::Left, Some(TiledCorners::Bottom)) => TiledCorners::BottomLeft,
                    (Direction::Right, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Right)) => TiledCorners::BottomRight,
                    // figure out if we need to extend a quater tile
                    (Direction::Up, Some(TiledCorners::BottomLeft))
                    | (Direction::Down, Some(TiledCorners::TopLeft)) => TiledCorners::Left,
                    (Direction::Up, Some(TiledCorners::BottomRight))
                    | (Direction::Down, Some(TiledCorners::TopRight)) => TiledCorners::Right,
                    (Direction::Left, Some(TiledCorners::TopRight))
                    | (Direction::Right, Some(TiledCorners::TopLeft)) => TiledCorners::Top,
                    (Direction::Left, Some(TiledCorners::BottomRight))
                    | (Direction::Right, Some(TiledCorners::BottomLeft)) => TiledCorners::Bottom,
                    // else we have a simple case
                    (Direction::Up, _) => TiledCorners::Top,
                    (Direction::Right, _) => TiledCorners::Right,
                    (Direction::Down, _) => TiledCorners::Bottom,
                    (Direction::Left, _) => TiledCorners::Left,
                };

                let new_geo = new_state.relative_geometry(output_geometry, self.gaps());
                let (new_pos, new_size) = (new_geo.loc, new_geo.size);
                element.set_tiled(true); // TODO: More fine grained?
                element.set_maximized(false);

                if tiled_state.is_none() {
                    let last_geometry = element
                        .maximized_state
                        .lock()
                        .unwrap()
                        .take()
                        .map(|state| state.original_geometry)
                        .or_else(|| self.space.element_geometry(element).map(RectExt::as_local));

                    *element.last_geometry.lock().unwrap() = last_geometry;
                }

                *tiled_state = Some(new_state);
                std::mem::drop(tiled_state);

                element.moved_since_mapped.store(true, Ordering::SeqCst);
                let element = element.clone();
                self.map_internal(
                    element,
                    Some(new_pos),
                    Some(new_size.as_logical()),
                    Some(start_rectangle),
                );

                MoveResult::Done
            }
        }
    }

    pub fn move_current_element<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        layer: ManagedLayer,
        theme: cosmic::Theme,
    ) -> MoveResult {
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::None;
        };

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return MoveResult::None;
                };
                self.space
                    .elements()
                    .find(|elem| elem.wl_surface().as_ref() == Some(&toplevel_surface))
            }
            KeyboardFocusTarget::Element(elem) => self.space.elements().find(|x| *x == &elem),
            _ => None,
        }) else {
            return MoveResult::None;
        };

        self.move_element(direction, seat, layer, &theme, &focused.clone())
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.space.elements().rev()
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        self.mapped().flat_map(|e| e.windows().map(|(w, _)| w))
    }

    #[profiling::function]
    pub fn refresh(&mut self) {
        self.space.refresh();

        if let Some(pos) = self.spawn_order.iter().position(|w| !w.alive()) {
            self.spawn_order.truncate(pos);
        }

        for element in self
            .space
            .elements()
            .filter(|e| self.space.outputs_for_element(e).is_empty())
            .cloned()
            .collect::<Vec<_>>()
            .into_iter()
        {
            // TODO what about windows leaving to the top with no headerbar to drag? can that happen? (Probably if the user is moving outputs down)
            *element.last_geometry.lock().unwrap() = None;
            self.map_internal(element, None, None, None);
        }
    }

    pub fn animations_going(&self) -> bool {
        self.dirty.swap(false, Ordering::SeqCst) || !self.animations.is_empty()
    }

    pub fn update_animation_state(&mut self) {
        let was_empty = self.animations.is_empty();
        self.animations.retain(|_, anim| {
            let duration = match anim {
                Animation::Tiled { .. } => ANIMATION_DURATION,
                _ => MINIMIZE_ANIMATION_DURATION,
            };
            Instant::now().duration_since(*anim.start()) < duration
        });
        if self.animations.is_empty() != was_empty {
            self.dirty.store(true, Ordering::SeqCst);
        }
    }

    pub fn merge(&mut self, other: FloatingLayout) {
        for element in other.space.elements() {
            let elem_loc = other
                .space
                .element_geometry(element)
                .unwrap()
                .loc
                .as_local();
            self.map_internal(element.clone(), Some(elem_loc), None, None);
        }
        self.refresh(); //fixup any out of bounds elements
    }

    #[profiling::function]
    pub fn render<R>(
        &self,
        renderer: &mut R,
        focused: Option<&CosmicMapped>,
        mut resize_indicator: Option<(ResizeMode, ResizeIndicator)>,
        indicator_thickness: u8,
        alpha: f32,
        theme: &cosmic::theme::CosmicTheme,
    ) -> (
        Vec<CosmicMappedRenderElement<R>>,
        Vec<CosmicMappedRenderElement<R>>,
    )
    where
        R: Renderer + ImportAll + ImportMem + AsGlowRenderer,
        <R as Renderer>::TextureId: Clone + 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        let output = self.space.outputs().next().unwrap();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };
        let output_scale = output.current_scale().fractional_scale();

        let mut window_elements = Vec::new();
        let mut popup_elements = Vec::new();

        for elem in self
            .animations
            .iter()
            .filter(|(_, anim)| matches!(anim, Animation::Minimize { .. }))
            .map(|(elem, _)| elem)
            .chain(self.space.elements().rev())
        {
            let (mut geometry, alpha) = self
                .animations
                .get(elem)
                .map(|anim| (*anim.previous_geometry(), alpha * anim.alpha()))
                .unwrap_or_else(|| (self.space.element_geometry(elem).unwrap().as_local(), alpha));

            let render_location = geometry.loc - elem.geometry().loc.as_local();
            let (mut w_elements, p_elements) = elem.split_render_elements(
                renderer,
                render_location
                    .as_logical()
                    .to_physical_precise_round(output_scale),
                output_scale.into(),
                alpha,
            );

            if let Some(anim) = self.animations.get(elem) {
                let original_geo = anim.previous_geometry();
                geometry = anim.geometry(
                    output_geometry,
                    self.space
                        .element_geometry(elem)
                        .map(RectExt::as_local)
                        .unwrap_or(geometry),
                    elem.floating_tiled.lock().unwrap().as_ref(),
                    self.gaps(),
                );

                let buffer_size = elem.geometry().size;
                let scale = Scale {
                    x: geometry.size.w as f64 / buffer_size.w as f64,
                    y: geometry.size.h as f64 / buffer_size.h as f64,
                };

                w_elements = w_elements
                    .into_iter()
                    .map(|element| match element {
                        CosmicMappedRenderElement::Stack(elem) => {
                            CosmicMappedRenderElement::MovingStack({
                                let rescaled = RescaleRenderElement::from_element(
                                    elem,
                                    original_geo
                                        .loc
                                        .as_logical()
                                        .to_physical_precise_round(output_scale),
                                    scale,
                                );
                                let relocated = RelocateRenderElement::from_element(
                                    rescaled,
                                    (geometry.loc - original_geo.loc)
                                        .as_logical()
                                        .to_physical_precise_round(output_scale),
                                    Relocate::Relative,
                                );
                                relocated
                            })
                        }
                        CosmicMappedRenderElement::Window(elem) => {
                            CosmicMappedRenderElement::MovingWindow({
                                let rescaled = RescaleRenderElement::from_element(
                                    elem,
                                    original_geo
                                        .loc
                                        .as_logical()
                                        .to_physical_precise_round(output_scale),
                                    scale,
                                );
                                let relocated = RelocateRenderElement::from_element(
                                    rescaled,
                                    (geometry.loc - original_geo.loc)
                                        .as_logical()
                                        .to_physical_precise_round(output_scale),
                                    Relocate::Relative,
                                );
                                relocated
                            })
                        }
                        x => x,
                    })
                    .collect();
            }

            if focused == Some(elem) && !elem.is_maximized(false) {
                if let Some((mode, resize)) = resize_indicator.as_mut() {
                    let mut resize_geometry = geometry.clone();
                    resize_geometry.loc -= (18, 18).into();
                    resize_geometry.size += (36, 36).into();

                    resize.resize(resize_geometry.size.as_logical());
                    resize.output_enter(output, Rectangle::default() /* unused */);
                    window_elements.extend(
                        resize
                            .render_elements::<CosmicWindowRenderElement<R>>(
                                renderer,
                                resize_geometry
                                    .loc
                                    .as_logical()
                                    .to_physical_precise_round(output_scale),
                                output_scale.into(),
                                alpha * mode.alpha().unwrap_or(1.0),
                            )
                            .into_iter()
                            .map(CosmicMappedRenderElement::Window),
                    );
                }

                let active_window_hint = crate::theme::active_window_hint(theme);

                if indicator_thickness > 0 {
                    let element = IndicatorShader::focus_element(
                        renderer,
                        Key::Window(Usage::FocusIndicator, elem.clone()),
                        geometry,
                        indicator_thickness,
                        output_scale,
                        alpha,
                        [
                            active_window_hint.red,
                            active_window_hint.green,
                            active_window_hint.blue,
                        ],
                    );
                    window_elements.push(element.into());
                }
            }

            window_elements.extend(w_elements);
            popup_elements.extend(p_elements);
        }

        (window_elements, popup_elements)
    }

    fn gaps(&self) -> (i32, i32) {
        let g = self.theme.cosmic().gaps;
        (g.0 as i32, g.1 as i32)
    }
}
