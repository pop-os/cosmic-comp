use crate::{
    shell::{
        element::CosmicWindow,
        layout::{floating::FloatingLayout, tiling::TilingLayout},
    },
    state::State,
    utils::prelude::*,
    wayland::protocols::workspace::WorkspaceHandle,
};

use indexmap::IndexSet;
use smithay::{
    backend::renderer::{
        element::{surface::WaylandSurfaceRenderElement, AsRenderElements},
        ImportAll, Renderer,
    },
    desktop::{
        layer_map_for_output, space::SpaceElement, Kind, LayerSurface, Space, Window,
        WindowSurfaceType,
    },
    input::{pointer::GrabStartData as PointerGrabStartData, Seat},
    output::{Output, WeakOutput},
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel::{self, ResizeEdge},
        wayland_server::{protocol::wl_surface::WlSurface, DisplayHandle},
    },
    render_elements,
    utils::{IsAlive, Logical, Point, Rectangle, Scale, Serial},
    wayland::shell::wlr_layer::Layer,
};
use std::{collections::HashMap, time::Duration};

use super::{
    element::CosmicMapped,
    focus::{FocusStack, FocusStackMut},
    layout::{
        floating::{FloatingRenderElement, ResizeSurfaceGrab},
        tiling::TilingRenderElement,
    },
};

#[derive(Debug)]
pub struct Workspace {
    pub tiling_layer: TilingLayout,
    pub floating_layer: FloatingLayout,
    pub tiling_enabled: bool,
    pub fullscreen: HashMap<Output, Window>,
    pub handle: WorkspaceHandle,
    pub focus_stack: FocusStacks,
}

#[derive(Debug, Default)]
pub struct FocusStacks(HashMap<Seat<State>, IndexSet<CosmicMapped>>);

impl Workspace {
    pub fn new(handle: WorkspaceHandle) -> Workspace {
        Workspace {
            tiling_layer: TilingLayout::new(),
            floating_layer: FloatingLayout::new(),
            tiling_enabled: true,
            fullscreen: HashMap::new(),
            handle,
            focus_stack: FocusStacks::default(),
        }
    }

    pub fn refresh(&mut self) {
        self.fullscreen.retain(|_, w| w.alive());
        self.floating_layer.refresh();
        self.tiling_layer.refresh();
    }

    pub fn commit(&mut self, surface: &WlSurface) {
        if let Some(mapped) = self.element_for_surface(surface) {
            mapped
                .windows()
                .find(|(w, _)| w.toplevel().wl_surface() == surface)
                .unwrap()
                .0
                .on_commit();
        }
    }

    pub fn map_output(&mut self, output: &Output, position: Point<i32, Logical>) {
        self.tiling_layer.map_output(output, position);
        self.floating_layer.map_output(output, position);
    }

    pub fn unmap_output(&mut self, output: &Output) {
        if let Some(dead_output_window) = self.fullscreen.remove(output) {
            self.unfullscreen_request(&dead_output_window);
        }
        self.tiling_layer.unmap_output(output);
        self.floating_layer.unmap_output(output);
        self.refresh();
    }

    pub fn element_for_surface(&self, surface: &WlSurface) -> Option<&CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(_, w, _)| w))
            .find(|e| {
                e.windows()
                    .any(|(w, _)| w.toplevel().wl_surface() == surface)
            })
    }

    pub fn outputs_for_element(&self, elem: &CosmicMapped) -> impl Iterator<Item = Output> {
        self.floating_layer
            .space
            .outputs_for_element(elem)
            .into_iter()
            .chain(self.tiling_layer.output_for_element(elem).cloned())
    }

    pub fn output_under(&self, point: Point<i32, Logical>) -> Option<&Output> {
        let space = &self.floating_layer.space;
        space.outputs().find(|o| {
            let internal_output_geo = space.output_geometry(o).unwrap();
            let external_output_geo = o.geometry();
            internal_output_geo.contains(point - external_output_geo.loc + internal_output_geo.loc)
        })
    }

    pub fn element_under(
        &self,
        location: Point<f64, Logical>,
    ) -> Option<(&CosmicMapped, Point<i32, Logical>)> {
        self.floating_layer
            .space
            .element_under(location)
            .or_else(|| {
                self.tiling_layer.mapped().find_map(|(_, mapped, loc)| {
                    let test_point = location - loc.to_f64() + mapped.geometry().loc.to_f64();
                    mapped
                        .is_in_input_region(&test_point)
                        .then_some((mapped, loc - mapped.geometry().loc))
                })
            })
    }

    pub fn element_geometry(&self, elem: &CosmicMapped) -> Option<Rectangle<i32, Logical>> {
        let space = &self.floating_layer.space;
        let outputs = space.outputs().collect::<Vec<_>>();
        let offset = if outputs.len() == 1
            && space.output_geometry(&outputs[0]).unwrap().loc == Point::from((0, 0))
        {
            outputs[0].geometry().loc
        } else {
            (0, 0).into()
        };

        self.floating_layer
            .space
            .element_geometry(elem)
            .or_else(|| self.tiling_layer.element_geometry(elem))
            .map(|mut geo| {
                geo.loc += offset;
                geo
            })
    }

    pub fn maximize_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(output) {
            return;
        }

        self.floating_layer.maximize_request(window);

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Maximized);
                state.states.unset(xdg_toplevel::State::Fullscreen);
            });
        }

        self.set_fullscreen(window, output)
    }
    pub fn unmaximize_request(&mut self, window: &Window) {
        if self.fullscreen.values().any(|w| w == window) {
            self.floating_layer.unmaximize_request(window);
            return self.unfullscreen_request(window);
        }
    }

    pub fn fullscreen_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(output) {
            return;
        }

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Fullscreen);
                state.states.unset(xdg_toplevel::State::Maximized);
            });
        }

        self.set_fullscreen(window, output)
    }

    fn set_fullscreen(&mut self, window: &Window, output: &Output) {
        if let Some(mapped) = self
            .mapped()
            .find(|m| m.windows().any(|(w, _)| &w == window))
        {
            mapped.set_active(window);
        }

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.with_pending_state(|state| {
                state.size = Some(
                    output
                        .current_mode()
                        .map(|m| m.size)
                        .unwrap_or((0, 0).into())
                        .to_f64()
                        .to_logical(output.current_scale().fractional_scale())
                        .to_i32_round(),
                );
            });

            xdg.send_configure();
        }
        self.fullscreen.insert(output.clone(), window.clone());
    }

    pub fn unfullscreen_request(&mut self, window: &Window) {
        if self.fullscreen.values().any(|w| w == window) {
            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(xdg) = &window.toplevel() {
                xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Fullscreen);
                    state.states.unset(xdg_toplevel::State::Maximized);
                    state.size = None;
                });
                self.floating_layer.refresh();
                self.tiling_layer.refresh();
                xdg.send_configure();
            }
            self.fullscreen.retain(|_, w| w != window);
        }
    }

    pub fn fullscreen_toggle(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(output) {
            self.unfullscreen_request(window)
        } else {
            self.fullscreen_request(window, output)
        }
    }

    pub fn get_fullscreen(&self, output: &Output) -> Option<&Window> {
        self.fullscreen.get(output).filter(|w| w.alive())
    }

    pub fn resize_request(
        &mut self,
        mapped: &CosmicMapped,
        seat: &Seat<State>,
        serial: Serial,
        start_data: PointerGrabStartData<State>,
        edges: ResizeEdge,
    ) -> Option<ResizeSurfaceGrab> {
        if mapped.is_fullscreen() || mapped.is_maximized() {
            return None;
        }
        if self.floating_layer.mapped().any(|m| m == mapped) {
            self.floating_layer
                .resize_request(mapped, seat, serial, start_data.clone(), edges)
        } else if self.tiling_layer.mapped().any(|(_, m, _)| m == mapped) {
            //self.tiling_layer.resize_request(mapped, seat, serial, start_data, edges)
            None
        } else {
            None
        }
    }

    pub fn toggle_tiling(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            for window in self
                .tiling_layer
                .mapped()
                .map(|(_, m, _)| m.clone())
                .collect::<Vec<_>>()
                .into_iter()
            {
                self.tiling_layer.unmap(&window);
                self.floating_layer.map(window, seat, None);
            }
            self.tiling_enabled = false;
        } else {
            let focus_stack = self.focus_stack.get(seat);
            for window in self
                .floating_layer
                .mapped()
                .cloned()
                .collect::<Vec<_>>()
                .into_iter()
            {
                self.floating_layer.unmap(&window);
                self.tiling_layer.map(window, seat, focus_stack.iter())
            }
            self.tiling_enabled = true;
        }
    }

    pub fn toggle_floating_window(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            if let Some(window) = self.focus_stack.get(seat).iter().next().cloned() {
                if self.tiling_layer.mapped().any(|(_, m, _)| m == &window) {
                    self.tiling_layer.unmap(&window);
                    self.floating_layer.map(window, seat, None);
                } else if self.floating_layer.mapped().any(|w| w == &window) {
                    let focus_stack = self.focus_stack.get(seat);
                    self.floating_layer.unmap(&window);
                    self.tiling_layer.map(window, seat, focus_stack.iter())
                }
            }
        }
    }

    pub fn mapped(&self) -> impl Iterator<Item = &CosmicMapped> {
        self.floating_layer
            .mapped()
            .chain(self.tiling_layer.mapped().map(|(_, w, _)| w))
    }

    pub fn windows(&self) -> impl Iterator<Item = Window> + '_ {
        self.floating_layer
            .windows()
            .chain(self.tiling_layer.windows().map(|(_, w, _)| w))
    }

    pub fn render_output<R>(
        &self,
        output: &Output,
    ) -> Result<Vec<WorkspaceRenderElement<R>>, OutputNotMapped>
    where
        R: Renderer + ImportAll,
        <R as Renderer>::TextureId: 'static,
    {
        let mut render_elements = Vec::new();

        let output_scale = output.current_scale().fractional_scale();
        let layer_map = layer_map_for_output(output);

        if let Some(fullscreen) = self.fullscreen.get(output) {
            // overlay layer surfaces
            render_elements.extend(
                layer_map
                    .layers()
                    .rev()
                    .filter(|s| s.layer() == Layer::Overlay)
                    .filter_map(|surface| {
                        layer_map
                            .layer_geometry(surface)
                            .map(|geo| (geo.loc, surface))
                    })
                    .flat_map(|(loc, surface)| {
                        AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                            surface,
                            loc.to_physical_precise_round(output_scale),
                            Scale::from(output_scale),
                        )
                    }),
            );

            // fullscreen window
            render_elements.extend(AsRenderElements::<R>::render_elements::<
                WorkspaceRenderElement<R>,
            >(fullscreen, (0, 0).into(), output_scale.into()));
        } else {
            // TODO: Handle modes like
            // - keyboard window swapping
            // - resizing / moving in tiling

            // overlay and top layer surfaces
            let lower = {
                let (upper, lower): (Vec<&LayerSurface>, Vec<&LayerSurface>) = layer_map
                    .layers()
                    .rev()
                    .partition(|s| matches!(s.layer(), Layer::Background | Layer::Bottom));

                render_elements.extend(
                    upper
                        .into_iter()
                        .filter_map(|surface| {
                            layer_map
                                .layer_geometry(surface)
                                .map(|geo| (geo.loc, surface))
                        })
                        .flat_map(|(loc, surface)| {
                            AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                                surface,
                                loc.to_physical_precise_round(output_scale),
                                Scale::from(output_scale),
                            )
                        }),
                );

                lower
            };

            // floating surfaces
            render_elements.extend(
                self.floating_layer
                    .render_output::<R>(output)?
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            //tiling surfaces
            render_elements.extend(
                self.tiling_layer
                    .render_output::<R>(output)?
                    .into_iter()
                    .map(WorkspaceRenderElement::from),
            );

            // bottom and background layer surfaces
            {
                render_elements.extend(
                    lower
                        .into_iter()
                        .filter_map(|surface| {
                            layer_map
                                .layer_geometry(surface)
                                .map(|geo| (geo.loc, surface))
                        })
                        .flat_map(|(loc, surface)| {
                            AsRenderElements::<R>::render_elements::<WorkspaceRenderElement<R>>(
                                surface,
                                loc.to_physical_precise_round(output_scale),
                                Scale::from(output_scale),
                            )
                        }),
                );
            }
        }

        Ok(render_elements)
    }
}

impl FocusStacks {
    pub fn get<'a>(&'a self, seat: &Seat<State>) -> FocusStack<'a> {
        FocusStack(self.0.get(seat))
    }

    pub fn get_mut<'a>(&'a mut self, seat: &Seat<State>) -> FocusStackMut<'a> {
        FocusStackMut(self.0.entry(seat.clone()).or_default())
    }
}

pub struct OutputNotMapped;

render_elements! {
    pub WorkspaceRenderElement<R> where R: ImportAll;
    Wayland=WaylandSurfaceRenderElement,
    Floating=FloatingRenderElement<R>,
    Tiling=TilingRenderElement<R>,
}
