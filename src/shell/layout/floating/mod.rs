// SPDX-License-Identifier: GPL-3.0-only

use smithay::{
    backend::renderer::{ImportAll, Renderer},
    desktop::{layer_map_for_output, space::SpaceElement, Space, Window},
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::Output,
    render_elements,
    utils::{Logical, Point, Rectangle, Serial},
};
use std::collections::HashMap;

use crate::{
    shell::{
        element::{CosmicMapped, CosmicMappedRenderElement},
        grabs::ResizeEdge,
        OutputNotMapped,
    },
    state::State,
    utils::prelude::*,
};

mod grabs;
pub use self::grabs::*;

#[derive(Debug)]
pub struct FloatingLayout {
    pub(in crate::shell) space: Space<CosmicMapped>,
}

impl Default for FloatingLayout {
    fn default() -> Self {
        FloatingLayout {
            space: Space::new(None),
        }
    }
}

impl FloatingLayout {
    pub fn new() -> FloatingLayout {
        Default::default()
    }

    pub fn map_output(&mut self, output: &Output, location: Point<i32, Logical>) {
        self.space.map_output(output, location)
    }

    pub fn unmap_output(&mut self, output: &Output) {
        self.space.unmap_output(output);
        self.refresh();
    }

    pub fn map(
        &mut self,
        mapped: impl Into<CosmicMapped>,
        seat: &Seat<State>,
        position: impl Into<Option<Point<i32, Logical>>>,
    ) {
        let mapped = mapped.into();
        let output = seat.active_output();
        let position = position.into();

        self.map_internal(mapped, &output, position)
    }

    pub(in crate::shell) fn map_internal(
        &mut self,
        mapped: CosmicMapped,
        output: &Output,
        position: Option<Point<i32, Logical>>,
    ) {
        let mut win_geo = mapped.geometry();

        let layers = layer_map_for_output(&output);
        let geometry = layers.non_exclusive_zone();
        let last_geometry = mapped.last_geometry.lock().unwrap().clone();

        let mut geo_updated = false;
        if let Some(size) = last_geometry.map(|g| g.size) {
            geo_updated = win_geo.size != size;
            win_geo.size = size;
        }
        {
            let (min_size, max_size) = (mapped.min_size(), mapped.max_size());
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
                geo_updated = true;
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
                geo_updated = true;
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
        if geo_updated {
            mapped.set_size(win_geo.size);
        }
        mapped.configure();

        self.space.map_element(mapped, position, false);
    }

    pub fn unmap(&mut self, window: &CosmicMapped) -> bool {
        #[allow(irrefutable_let_patterns)]
        let is_maximized = window.is_maximized();

        if !is_maximized {
            if let Some(location) = self.space.element_location(window) {
                *window.last_geometry.lock().unwrap() = Some(Rectangle::from_loc_and_size(
                    location,
                    window.geometry().size,
                ));
            }
        }

        let was_unmaped = self.space.elements().any(|e| e == window);
        self.space.unmap_elem(&window);
        was_unmaped
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Logical>> {
        self.space.element_geometry(elem)
    }

    pub fn maximize_request(&mut self, window: &Window) {
        if let Some(mapped) = self
            .space
            .elements()
            .find(|m| m.windows().any(|(w, _)| &w == window))
        {
            if let Some(location) = self.space.element_location(mapped) {
                *mapped.last_geometry.lock().unwrap() = Some(Rectangle::from_loc_and_size(
                    location,
                    mapped.geometry().size,
                ));
            }
        }
    }

    pub fn unmaximize_request(&mut self, window: &Window) {
        let maybe_mapped = self
            .space
            .elements()
            .find(|m| m.windows().any(|(w, _)| &w == window))
            .cloned();

        if let Some(mapped) = maybe_mapped {
            let last_geometry = mapped.last_geometry.lock().unwrap().clone();
            mapped.set_size(last_geometry.map(|g| g.size).expect("No previous size?"));
            let last_location = last_geometry.map(|g| g.loc).expect("No previous location?");
            self.space.map_element(mapped, last_location, true);
        }
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        serial: Serial,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeSurfaceGrab> {
        if let Some(pointer) = seat.get_pointer() {
            let location = self.space.element_location(&mapped).unwrap();
            let size = mapped.geometry().size;

            Some(grabs::ResizeSurfaceGrab::new(
                start_data,
                mapped.clone(),
                edges,
                location,
                size,
            ))

            //pointer.set_grab(state, grab, serial, Focus::Clear);
        } else {
            None
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.space.elements()
    }

    pub fn windows(&self) -> impl Iterator<Item = Window> + '_ {
        self.mapped().flat_map(|e| e.windows().map(|(w, _)| w))
    }

    pub fn refresh(&mut self) {
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
            let output = self.space.outputs().next().unwrap().clone();
            self.map_internal(element, &output, None);
        }
    }

    pub fn most_overlapped_output_for_element(&self, elem: &CosmicMapped) -> Option<Output> {
        let elem_geo = self.space.element_geometry(elem)?;

        if self.space.outputs().nth(1).is_none() {
            return self.space.outputs().next().cloned();
        }

        Some(
            self.space
                .outputs_for_element(elem)
                .into_iter()
                .max_by_key(|o| {
                    let output_geo = self.space.output_geometry(o).unwrap();
                    if let Some(intersection) = output_geo.intersection(elem_geo) {
                        intersection.size.w * intersection.size.h
                    } else {
                        0
                    }
                })
                .unwrap_or(self.space.outputs().next().unwrap().clone()),
        )
    }

    pub fn merge(&mut self, other: FloatingLayout) {
        let mut output_pos_map = HashMap::new();
        for output in self.space.outputs() {
            output_pos_map.insert(
                output.clone(),
                self.space.output_geometry(output).unwrap().loc
                    - other
                        .space
                        .output_geometry(output)
                        .map(|geo| geo.loc)
                        .unwrap_or_else(|| (0, 0).into()),
            );
        }
        for element in other.space.elements() {
            let mut elem_geo = other.space.element_geometry(element).unwrap();
            let output = other
                .space
                .outputs_for_element(element)
                .into_iter()
                .filter(|o| self.space.outputs().any(|o2| o == o2))
                .max_by_key(|o| {
                    let output_geo = other.space.output_geometry(o).unwrap();
                    let intersection = output_geo.intersection(elem_geo).unwrap();
                    intersection.size.w * intersection.size.h
                })
                .unwrap_or(self.space.outputs().next().unwrap().clone());
            elem_geo.loc += output_pos_map
                .get(&output)
                .copied()
                .unwrap_or_else(|| (0, 0).into());
            self.space.map_element(element.clone(), elem_geo.loc, false);
        }
        self.refresh(); //fixup any out of bounds elements
    }

    pub fn render_output<R>(
        &self,
        output: &Output,
    ) -> Result<Vec<FloatingRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll,
        <R as Renderer>::TextureId: 'static,
    {
        let output_scale = output.current_scale().fractional_scale();
        let output_geo = self.space.output_geometry(output).ok_or(OutputNotMapped)?;
        Ok(self
            .space
            .render_elements_for_region::<R, _>(&output_geo, output_scale)
            .into_iter()
            .map(FloatingRenderElement::from)
            .collect())
    }
}

render_elements! {
    pub FloatingRenderElement<R> where R: ImportAll;
    Window=CosmicMappedRenderElement<R>,
}
