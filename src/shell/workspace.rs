use crate::{
    shell::layout::{floating::FloatingLayout, tiling::TilingLayout},
    state::State,
    wayland::protocols::workspace::WorkspaceHandle,
};

use smithay::{
    desktop::{Kind, Space, Window},
    reexports::{
        wayland_protocols::xdg::shell::server::xdg_toplevel::{self, ResizeEdge},
        wayland_server::DisplayHandle,
    },
    utils::IsAlive,
    wayland::{
        output::Output,
        seat::{PointerGrabStartData, Seat},
        Serial,
    },
};
use std::collections::HashMap;

pub struct Workspace {
    pub idx: u8,
    pub space: Space,
    pub tiling_layer: TilingLayout,
    pub floating_layer: FloatingLayout,
    tiling_enabled: bool,
    pub fullscreen: HashMap<String, Window>,
    pub handle: WorkspaceHandle,
}

impl Workspace {
    pub fn new(idx: u8, handle: WorkspaceHandle) -> Workspace {
        Workspace {
            idx,
            space: Space::new(None),
            tiling_layer: TilingLayout::new(),
            floating_layer: FloatingLayout::new(),
            tiling_enabled: true,
            fullscreen: HashMap::new(),
            handle,
        }
    }

    pub fn refresh(&mut self, dh: &DisplayHandle) {
        let outputs = self.space.outputs().collect::<Vec<_>>();
        let dead_output_windows = self
            .fullscreen
            .iter()
            .filter(|(name, _)| !outputs.iter().any(|o| o.name() == **name))
            .map(|(_, w)| w)
            .cloned()
            .collect::<Vec<_>>();
        for window in dead_output_windows {
            self.unfullscreen_request(&window);
        }
        self.fullscreen.retain(|_, w| w.alive());
        self.floating_layer.refresh(&mut self.space);
        self.tiling_layer.refresh(&mut self.space);
        self.space.refresh(dh);
    }

    pub fn maximize_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.values().any(|w| w == window) {
            return;
        }
        if self.floating_layer.windows.contains(window) {
            self.floating_layer
                .maximize_request(&mut self.space, window, output);
        }
    }

    pub fn unmaximize_request(&mut self, window: &Window) {
        if self.fullscreen.values().any(|w| w == window) {
            return self.unfullscreen_request(window);
        }
        if self.floating_layer.windows.contains(window) {
            self.floating_layer
                .unmaximize_request(&mut self.space, window);
        }
    }

    pub fn resize_request(
        &mut self,
        window: &Window,
        seat: &Seat<State>,
        serial: Serial,
        start_data: PointerGrabStartData,
        edges: ResizeEdge,
    ) {
        if self.fullscreen.values().any(|w| w == window) {
            return;
        }
        if self.floating_layer.windows.contains(window) {
            self.floating_layer.resize_request(
                &mut self.space,
                window,
                seat,
                serial,
                start_data.clone(),
                edges,
            )
        }
        if self.tiling_layer.windows.contains(window) {
            self.tiling_layer.resize_request(
                &mut self.space,
                window,
                seat,
                serial,
                start_data,
                edges,
            )
        }
    }

    pub fn fullscreen_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(&output.name()) {
            return;
        }

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            xdg.with_pending_state(|state| {
                state.states.set(xdg_toplevel::State::Fullscreen);
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
            self.fullscreen.insert(output.name(), window.clone());
        }
    }

    pub fn unfullscreen_request(&mut self, window: &Window) {
        if self.fullscreen.values().any(|w| w == window) {
            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(xdg) = &window.toplevel() {
                xdg.with_pending_state(|state| {
                    state.states.unset(xdg_toplevel::State::Fullscreen);
                    state.size = None;
                });
                self.floating_layer.refresh(&mut self.space);
                self.tiling_layer.refresh(&mut self.space);
                xdg.send_configure();
            }
            self.fullscreen.retain(|_, w| w != window);
        }
    }

    pub fn fullscreen_toggle(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(&output.name()) {
            self.unfullscreen_request(window)
        } else {
            self.fullscreen_request(window, output)
        }
    }

    pub fn get_fullscreen(&self, output: &Output) -> Option<&Window> {
        if !self.space.outputs().any(|o| o == output) {
            return None;
        }
        self.fullscreen.get(&output.name()).filter(|w| w.alive())
    }

    pub fn toggle_tiling(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            for window in self.tiling_layer.windows.clone().into_iter() {
                self.tiling_layer.unmap_window(&mut self.space, &window);
                self.floating_layer
                    .map_window(&mut self.space, window, seat, None);
            }
            self.tiling_enabled = false;
        } else {
            let focus_stack = self.focus_stack(seat);
            for window in self.floating_layer.windows.clone().into_iter() {
                self.floating_layer.unmap_window(&mut self.space, &window);
                self.tiling_layer
                    .map_window(&mut self.space, window, seat, focus_stack.iter())
            }
            self.tiling_enabled = true;
        }
    }

    pub fn toggle_floating_window(&mut self, seat: &Seat<State>) {
        if self.tiling_enabled {
            if let Some(window) = self.focus_stack(seat).iter().next().cloned() {
                if self.tiling_layer.windows.contains(&window) {
                    self.tiling_layer.unmap_window(&mut self.space, &window);
                    self.floating_layer
                        .map_window(&mut self.space, window, seat, None);
                } else if self.floating_layer.windows.contains(&window) {
                    let focus_stack = self.focus_stack(seat);
                    self.floating_layer.unmap_window(&mut self.space, &window);
                    self.tiling_layer
                        .map_window(&mut self.space, window, seat, focus_stack.iter())
                }
            }
        }
    }
}
