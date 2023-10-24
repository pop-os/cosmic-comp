// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    backend::renderer::{
        element::{AsRenderElements, RenderElement},
        ImportAll, ImportMem, Renderer,
    },
    desktop::{layer_map_for_output, space::SpaceElement, PopupKind, Space, WindowSurfaceType},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    utils::{Logical, Point, Rectangle, Size},
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
    utils::prelude::*,
    wayland::handlers::xdg_shell::popup::get_popup_toplevel,
};

mod grabs;
pub use self::grabs::*;

#[derive(Debug, Default)]
pub struct FloatingLayout {
    pub(in crate::shell) space: Space<CosmicMapped>,
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

        /*
        TODO: rescale all positions? (evem rescale windows?)
         */

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
        let geometry = layers.non_exclusive_zone();
        mapped.set_bounds(geometry.size);
        let last_geometry = mapped.last_geometry.lock().unwrap().clone();

        if let Some(size) = size
            .map(SizeExt::as_local)
            .or(last_geometry.map(|g| g.size))
        {
            win_geo.size = size;
        } else {
            let (min_size, max_size) = (
                mapped.min_size().unwrap_or((0, 0).into()),
                mapped.max_size().unwrap_or((0, 0).into()),
            );
            if win_geo.size.w > geometry.size.w / 3 * 2 {
                // try a more reasonable size
                let mut width = geometry.size.w / 3 * 2;
                if max_size.w != 0 {
                    // don't go larger then the max_size ...
                    width = std::cmp::min(max_size.w, width);
                }
                if min_size.w != 0 {
                    // ... but also don't go smaller than the min_size
                    width = std::cmp::max(min_size.w, width);
                }
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.w = std::cmp::min(width, geometry.size.w);
            }
            if win_geo.size.h > geometry.size.h / 3 * 2 {
                // try a more reasonable size
                let mut height = geometry.size.h / 3 * 2;
                if max_size.h != 0 {
                    // don't go larger then the max_size ...
                    height = std::cmp::min(max_size.h, height);
                }
                if min_size.h != 0 {
                    // ... but also don't go smaller than the min_size
                    height = std::cmp::max(min_size.h, height);
                }
                // but no matter the supported sizes, don't be larger than our non-exclusive-zone
                win_geo.size.h = std::cmp::min(height, geometry.size.h);
            }
        }

        let position = position
            .or_else(|| last_geometry.map(|g| g.loc))
            .unwrap_or_else(|| {
                (
                    geometry.loc.x + (geometry.size.w / 2) - (win_geo.size.w / 2) + win_geo.loc.x,
                    geometry.loc.y + (geometry.size.h / 2) - (win_geo.size.h / 2) + win_geo.loc.y,
                )
                    .into()
            });

        mapped.set_tiled(false);
        mapped
            .set_geometry(Rectangle::from_loc_and_size(position, win_geo.size).to_global(&output));
        mapped.configure();
        self.space.map_element(mapped, position.as_logical(), false);
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> bool {
        if !window.is_maximized(true) || !window.is_fullscreen(true) {
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
            let location = self.space.element_location(&mapped).unwrap();
            let size = mapped.geometry().size;

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
                geo.size.w -= amount;
            } else {
                geo.size.w += amount;
            }
            if edge.contains(ResizeEdge::LEFT) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.x += amount;
                } else {
                    geo.loc.x -= amount;
                }
            }
        }
        if edge.contains(ResizeEdge::BOTTOM) || edge.contains(ResizeEdge::TOP) {
            if direction == ResizeDirection::Inwards {
                geo.size.h -= amount;
            } else {
                geo.size.h += amount;
            }
            if edge.contains(ResizeEdge::TOP) {
                if direction == ResizeDirection::Inwards {
                    geo.loc.y += amount;
                } else {
                    geo.loc.y -= amount;
                }
            }
        }

        let Some(bounding_box) = self
            .space
            .outputs()
            .map(|o| self.space.output_geometry(o).unwrap())
            .filter(|output_geo| output_geo.overlaps(geo))
            .fold(None, |res, output_geo| match res {
                None => Some(output_geo),
                Some(other) => Some(other.merge(output_geo)),
            })
        else {
            return true;
        };

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
            initial_window_location: original_geo.loc,
            initial_window_size: original_geo.size,
        }));

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
            StackMoveResult::Handled => return MoveResult::Done,
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
                return MoveResult::ShiftFocus(KeyboardFocusTarget::Element(mapped));
            }
            StackMoveResult::Default => {}
        };

        MoveResult::MoveFurther(KeyboardFocusTarget::Element(focused.clone()))
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
        let output_scale = output.current_scale().fractional_scale();

        let mut window_elements = Vec::new();
        let mut popup_elements = Vec::new();

        self.space.elements().rev().for_each(|elem| {
            let render_location = self.space.element_location(elem).unwrap() - elem.geometry().loc;
            let (w_elements, p_elements) = elem.split_render_elements(
                renderer,
                render_location.to_physical_precise_round(output_scale),
                output_scale.into(),
                alpha,
            );

            if focused == Some(elem) && !elem.is_maximized(false) {
                let mut indicator_geometry = Rectangle::from_loc_and_size(
                    self.space.element_location(elem).unwrap(),
                    elem.geometry().size,
                )
                .as_local();

                if let Some((mode, resize)) = resize_indicator.as_mut() {
                    indicator_geometry.loc -= (18, 18).into();
                    indicator_geometry.size += (36, 36).into();
                    resize.resize(indicator_geometry.size.as_logical());
                    resize.output_enter(output, Rectangle::default() /* unused */);
                    window_elements.extend(
                        resize
                            .render_elements::<CosmicWindowRenderElement<R>>(
                                renderer,
                                indicator_geometry
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
                        indicator_geometry,
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
