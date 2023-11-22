// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::HashMap,
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
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, Size},
    wayland::seat::WaylandFocus,
};

use crate::{
    backend::render::{element::AsGlowRenderer, IndicatorShader, Key, Usage},
    shell::{
        element::{
            resize_indicator::ResizeIndicator,
            stack::{CosmicStackRenderElement, MoveResult as StackMoveResult},
            window::CosmicWindowRenderElement,
            CosmicMapped, CosmicMappedRenderElement, CosmicWindow,
        },
        focus::{target::KeyboardFocusTarget, FocusDirection},
        grabs::ResizeEdge,
        CosmicSurface, Direction, FocusResult, MoveResult, ResizeDirection, ResizeMode,
    },
    state::State,
    utils::{prelude::*, tween::EaseRectangle},
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
};

mod grabs;
pub use self::grabs::*;

pub const ANIMATION_DURATION: Duration = Duration::from_millis(200);

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pub(crate) space: Space<CosmicMapped>,
    spawn_order: Vec<CosmicMapped>,
    tiling_animations: HashMap<CosmicMapped, (Instant, Rectangle<i32, Local>)>,
    dirty: AtomicBool,
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
    ) -> Rectangle<i32, Local> {
        let (loc, size) = match self {
            TiledCorners::Bottom => (
                Point::from((
                    output_geometry.loc.x,
                    output_geometry.loc.y + (output_geometry.size.h / 2),
                )),
                Size::from((output_geometry.size.w, output_geometry.size.h / 2)),
            ),
            TiledCorners::BottomLeft => (
                Point::from((
                    output_geometry.loc.x,
                    output_geometry.loc.y + (output_geometry.size.h / 2),
                )),
                Size::from((output_geometry.size.w / 2, output_geometry.size.h / 2)),
            ),
            TiledCorners::BottomRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2),
                    output_geometry.loc.y + (output_geometry.size.h / 2),
                )),
                Size::from((output_geometry.size.w / 2, output_geometry.size.h / 2)),
            ),
            TiledCorners::Left => (
                output_geometry.loc,
                Size::from((output_geometry.size.w / 2, output_geometry.size.h)),
            ),
            TiledCorners::Top => (
                output_geometry.loc,
                Size::from((output_geometry.size.w, output_geometry.size.h / 2)),
            ),
            TiledCorners::TopLeft => (
                output_geometry.loc,
                Size::from((output_geometry.size.w / 2, output_geometry.size.h / 2)),
            ),
            TiledCorners::TopRight => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2),
                    output_geometry.loc.y,
                )),
                Size::from((output_geometry.size.w / 2, output_geometry.size.h / 2)),
            ),
            TiledCorners::Right => (
                Point::from((
                    output_geometry.loc.x + (output_geometry.size.w / 2),
                    output_geometry.loc.y,
                )),
                Size::from((output_geometry.size.w / 2, output_geometry.size.h)),
            ),
        };

        Rectangle::from_loc_and_size(loc, size).as_local()
    }
}

impl FloatingLayout {
    pub fn new(output: &Output) -> FloatingLayout {
        let mut layout = Self::default();
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
                let geometry = tiled_state.relative_geometry(output_geometry);
                self.map_internal(mapped, Some(geometry.loc), Some(geometry.size.as_logical()));
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
                self.map_internal(mapped, Some(Point::from(new_loc)), None);
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

        self.map_internal(mapped, position, None)
    }

    pub fn map_maximized(&mut self, mapped: CosmicMapped) {
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

        self.space
            .map_element(mapped, geometry.loc.as_logical(), true);
    }

    pub(in crate::shell) fn map_internal(
        &mut self,
        mapped: CosmicMapped,
        position: Option<Point<i32, Local>>,
        size: Option<Size<i32, Logical>>,
    ) {
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

            // if the last_geometry is too large
            if win_geo.size.w > output_geometry.size.w {
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
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.w = std::cmp::min(width, output_geometry.size.w);
            }
            if win_geo.size.h > output_geometry.size.h {
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
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.h = std::cmp::min(height, output_geometry.size.h);
            }
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
                                    <= output_geometry.loc.y + output_geometry.size.h - 16
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
        self.space.map_element(mapped, position.as_logical(), false);
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
        } else if !window.is_maximized(true) || !window.is_fullscreen(true) {
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

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Local>> {
        self.space.element_geometry(elem).map(RectExt::as_local)
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeSurfaceGrab> {
        if seat.get_pointer().is_some() {
            let location = self.space.element_location(&mapped).unwrap().as_local();
            let size = mapped.geometry().size;
            mapped.moved_since_mapped.store(true, Ordering::SeqCst);

            Some(grabs::ResizeSurfaceGrab::new(
                start_data,
                mapped.clone(),
                edges,
                location,
                size,
                seat,
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

    pub fn next_focus<'a>(
        &mut self,
        direction: FocusDirection,
        seat: &Seat<State>,
        _focus_stack: impl Iterator<Item = &'a CosmicMapped> + 'a,
    ) -> FocusResult {
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return FocusResult::None
        };

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return FocusResult::None
                };
                self.space.elements().find(|elem| elem.wl_surface().as_ref() == Some(&toplevel_surface))
            },
            KeyboardFocusTarget::Element(elem) => self.space.elements().find(|e| *e == &elem),
            _ => None,
        }) else {
            return FocusResult::None
        };

        if focused.handle_focus(direction, None) {
            return FocusResult::Handled;
        }

        let geometry = self.space.element_geometry(focused).unwrap();

        let next = match direction {
            FocusDirection::Up => self.space.elements().min_by_key(|other| {
                let res = geometry.loc.y - self.space.element_geometry(other).unwrap().loc.y;
                if res.is_positive() {
                    res
                } else {
                    i32::MAX
                }
            }),
            FocusDirection::Down => self.space.elements().max_by_key(|other| {
                let res = geometry.loc.y - self.space.element_geometry(other).unwrap().loc.y;
                if res.is_negative() {
                    res
                } else {
                    i32::MIN
                }
            }),
            FocusDirection::Left => self.space.elements().min_by_key(|other| {
                let res = geometry.loc.x - self.space.element_geometry(other).unwrap().loc.x;
                if res.is_positive() {
                    res
                } else {
                    i32::MAX
                }
            }),
            FocusDirection::Right => self.space.elements().max_by_key(|other| {
                let res = geometry.loc.x - self.space.element_geometry(other).unwrap().loc.x;
                if res.is_negative() {
                    res
                } else {
                    i32::MIN
                }
            }),
            _ => return FocusResult::None,
        };

        next.map(|elem| FocusResult::Some(KeyboardFocusTarget::Element(elem.clone())))
            .unwrap_or(FocusResult::None)
    }

    pub fn move_current_element<'a>(
        &mut self,
        direction: Direction,
        seat: &Seat<State>,
        theme: cosmic::Theme,
    ) -> MoveResult {
        let Some(target) = seat.get_keyboard().unwrap().current_focus() else {
            return MoveResult::None
        };

        let Some(focused) = (match target {
            KeyboardFocusTarget::Popup(popup) => {
                let Some(toplevel_surface) = (match popup {
                    PopupKind::Xdg(xdg) => get_popup_toplevel(&xdg),
                    PopupKind::InputMethod(_) => unreachable!(),
                }) else {
                    return MoveResult::None
                };
                self.space.elements().find(|elem| elem.wl_surface().as_ref() == Some(&toplevel_surface))
            },
            KeyboardFocusTarget::Element(elem) => self.space.elements().find(|e| *e == &elem),
            _ => None,
        }) else {
            return MoveResult::None
        };

        match focused.handle_move(direction) {
            StackMoveResult::Handled => MoveResult::Done,
            StackMoveResult::MoveOut(surface, loop_handle) => {
                let mapped: CosmicMapped = CosmicWindow::new(surface, loop_handle, theme).into();
                let output = seat.active_output();
                let pos = self.space.element_geometry(focused).unwrap().loc
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

                self.map_internal(mapped.clone(), position.map(PointExt::as_local), None);
                MoveResult::ShiftFocus(KeyboardFocusTarget::Element(mapped))
            }
            StackMoveResult::Default => {
                let mut tiled_state = focused.floating_tiled.lock().unwrap();

                let output = self.space.outputs().next().unwrap().clone();
                let layers = layer_map_for_output(&output);
                let output_geometry = layers.non_exclusive_zone();
                std::mem::drop(layers);

                let start_rectangle = if let Some((previous_start, previous_rect)) =
                    self.tiling_animations.remove(focused)
                {
                    if let Some(target_rect) = tiled_state
                        .as_ref()
                        .map(|state| state.relative_geometry(output_geometry))
                    {
                        ease(
                            EaseInOutCubic,
                            EaseRectangle(previous_rect),
                            EaseRectangle(target_rect),
                            Instant::now()
                                .duration_since(previous_start)
                                .max(ANIMATION_DURATION)
                                .as_secs_f64()
                                / ANIMATION_DURATION.as_secs_f64(),
                        )
                        .unwrap()
                    } else {
                        self.space
                            .element_geometry(focused)
                            .map(RectExt::as_local)
                            .unwrap()
                    }
                } else {
                    self.space
                        .element_geometry(focused)
                        .map(RectExt::as_local)
                        .unwrap()
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
                            focused.clone(),
                        ));
                    }

                    // to we go maximized?
                    (Direction::Up, Some(TiledCorners::Bottom))
                    | (Direction::Down, Some(TiledCorners::Top))
                    | (Direction::Left, Some(TiledCorners::Right))
                    | (Direction::Right, Some(TiledCorners::Left)) => {
                        self.tiling_animations
                            .insert(focused.clone(), (Instant::now(), start_rectangle));
                        *tiled_state = None;
                        std::mem::drop(tiled_state);

                        self.map_maximized(focused.clone());
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

                let new_geo = new_state.relative_geometry(output_geometry);
                let (new_pos, new_size) = (new_geo.loc, new_geo.size);
                focused.set_tiled(true); // TODO: More fine grained?
                focused.set_maximized(false);

                if tiled_state.is_none() && focused.is_maximized(false) {
                    *focused.last_geometry.lock().unwrap() =
                        self.space.element_geometry(focused).map(RectExt::as_local);
                }

                self.tiling_animations
                    .insert(focused.clone(), (Instant::now(), start_rectangle));
                *tiled_state = Some(new_state);
                std::mem::drop(tiled_state);

                focused.moved_since_mapped.store(true, Ordering::SeqCst);
                let focused = focused.clone();
                self.map_internal(focused, Some(new_pos), Some(new_size.as_logical()));

                MoveResult::Done
            }
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.space.elements().rev()
    }

    pub fn windows(&self) -> impl Iterator<Item = CosmicSurface> + '_ {
        self.mapped().flat_map(|e| e.windows().map(|(w, _)| w))
    }

    pub fn refresh(&mut self) {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

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
            self.map_internal(element, None, None);
        }
    }

    pub fn animations_going(&self) -> bool {
        self.dirty.swap(false, Ordering::SeqCst) || !self.tiling_animations.is_empty()
    }

    pub fn update_animation_state(&mut self) {
        self.tiling_animations
            .retain(|_, (start, _)| Instant::now().duration_since(*start) < ANIMATION_DURATION);
        if self.tiling_animations.is_empty() {
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
            self.map_internal(element.clone(), Some(elem_loc), None);
        }
        self.refresh(); //fixup any out of bounds elements
    }

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
        <R as Renderer>::TextureId: 'static,
        CosmicMappedRenderElement<R>: RenderElement<R>,
        CosmicWindowRenderElement<R>: RenderElement<R>,
        CosmicStackRenderElement<R>: RenderElement<R>,
    {
        #[cfg(feature = "debug")]
        puffin::profile_function!();

        let output = self.space.outputs().next().unwrap();
        let output_geometry = {
            let layers = layer_map_for_output(output);
            layers.non_exclusive_zone()
        };
        let output_scale = output.current_scale().fractional_scale();

        let mut window_elements = Vec::new();
        let mut popup_elements = Vec::new();

        self.space.elements().rev().for_each(|elem| {
            let mut geometry = self
                .tiling_animations
                .get(elem)
                .map(|(_, rect)| *rect)
                .unwrap_or_else(|| self.space.element_geometry(elem).unwrap().as_local());

            let render_location = geometry.loc - elem.geometry().loc.as_local();
            let (mut w_elements, p_elements) = elem.split_render_elements(
                renderer,
                render_location
                    .as_logical()
                    .to_physical_precise_round(output_scale),
                output_scale.into(),
                alpha,
            );

            if let Some((start, original_geo)) = self.tiling_animations.get(elem) {
                if let Some(target_rect) = elem
                    .floating_tiled
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map(|state| state.relative_geometry(output_geometry))
                    .or_else(|| {
                        elem.is_maximized(true)
                            .then_some(output_geometry.as_local())
                    })
                {
                    geometry = ease(
                        EaseInOutCubic,
                        EaseRectangle(original_geo.clone()),
                        EaseRectangle(target_rect),
                        Instant::now()
                            .duration_since(*start)
                            .min(ANIMATION_DURATION)
                            .as_millis() as f32
                            / ANIMATION_DURATION.as_millis() as f32,
                    )
                    .unwrap();

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
        });

        (window_elements, popup_elements)
    }
}
