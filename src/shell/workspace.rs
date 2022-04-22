use super::{layout, Layout};
use indexmap::IndexSet;
use smithay::{
    desktop::{LayerSurface, Space, Window, Kind},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::{self, ResizeEdge},
    wayland::{
        output::Output,
        seat::{PointerGrabStartData, Seat},
        Serial,
    },
};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
};

pub struct FocusStack<'a>(Ref<'a, IndexSet<Window>>);
pub struct FocusStackMut<'a>(RefMut<'a, IndexSet<Window>>);

impl<'a> FocusStack<'a> {
    pub fn last(&self) -> Option<Window> {
        self.0.iter().rev().find(|w| w.toplevel().alive()).cloned()
    }

    pub fn iter<'b>(&'b self) -> Box<dyn Iterator<Item = &'b Window> + 'b> {
        //working around object-safety constraints for trait Layout
        Box::new(self.0.iter().rev().filter(|w| w.toplevel().alive()))
    }
}

impl<'a> FocusStackMut<'a> {
    pub fn append(&mut self, window: &Window) {
        self.0.retain(|w| w.toplevel().alive());
        self.0.shift_remove(window);
        self.0.insert(window.clone());
    }

    pub fn last(&self) -> Option<Window> {
        self.0.iter().rev().find(|w| w.toplevel().alive()).cloned()
    }

    pub fn iter<'b>(&'b self) -> Box<dyn Iterator<Item = &'b Window> + 'b> {
        //working around object-safety constraints for trait Layout
        Box::new(self.0.iter().rev().filter(|w| w.toplevel().alive()))
    }
}

type FocusStackData = RefCell<(HashMap<u8, IndexSet<Window>>, IndexSet<Window>)>;

pub struct Workspace {
    pub(super) idx: u8,
    pub space: Space,
    pub(super) layout: Box<dyn Layout>,
    pub(super) pending_windows: Vec<(Window, Seat)>,
    pub(super) pending_layers: Vec<(LayerSurface, Output, Seat)>,
    pub fullscreen: HashMap<String, Window>,
}

impl Workspace {
    pub fn new(idx: u8) -> Workspace {
        Workspace {
            idx,
            space: Space::new(None),
            layout: layout::new_default_layout(),
            pending_windows: Vec::new(),
            pending_layers: Vec::new(),
            fullscreen: HashMap::new(),
        }
    }

    pub fn focus_stack<'a>(&'a self, seat: &'a Seat) -> FocusStack<'a> {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        FocusStack(Ref::map(
            seat.user_data().get::<FocusStackData>().unwrap().borrow(),
            |map| map.0.get(&self.idx).unwrap_or(&map.1), //TODO: workaround until Ref::filter_map goes stable
        ))
    }

    pub fn focus_stack_mut<'a>(&'a self, seat: &'a Seat) -> FocusStackMut<'a> {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        FocusStackMut(RefMut::map(
            seat.user_data()
                .get::<FocusStackData>()
                .unwrap()
                .borrow_mut(),
            |map| map.0.entry(self.idx).or_insert_with(|| IndexSet::new()),
        ))
    }

    pub fn pending_window(&mut self, window: Window, seat: &Seat) {
        self.pending_windows.push((window, seat.clone()));
    }

    pub fn pending_layer(&mut self, layer: LayerSurface, output: &Output, seat: &Seat) {
        self.pending_layers
            .push((layer, output.clone(), seat.clone()));
    }

    pub(super) fn map_window(&mut self, window: &Window, seat: &Seat) {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        let focus_stack = FocusStackMut(RefMut::map(
            seat.user_data()
                .get::<FocusStackData>()
                .unwrap()
                .borrow_mut(),
            |map| map.0.entry(self.idx).or_insert_with(|| IndexSet::new()),
        ));
        self.layout
            .map_window(&mut self.space, window, seat, focus_stack.iter())
    }

    pub(super) fn unmap_window(&mut self, window: &Window) {
        self.layout.unmap_window(&mut self.space, window)
    }

    pub fn refresh(&mut self) {
        let outputs = self.space.outputs().collect::<Vec<_>>();
        let dead_output_windows = self.fullscreen
            .iter()
            .filter(|(name, _)|
                !outputs.iter().any(|o| o.name() == **name)
            )
            .map(|(_, w)| w)
            .cloned()
            .collect::<Vec<_>>();
        for window in dead_output_windows {
            self.unfullscreen_request(&window);
        }
        self.layout.refresh(&mut self.space);
        self.space.refresh();
    }

    pub fn update_orientation(&mut self, seat: &Seat, orientation: layout::Orientation) {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        let focus_stack = FocusStackMut(RefMut::map(
            seat.user_data()
                .get::<FocusStackData>()
                .unwrap()
                .borrow_mut(),
            |map| map.0.entry(self.idx).or_insert_with(|| IndexSet::new()),
        ));
        self.layout
            .update_orientation(orientation, seat, &mut self.space, focus_stack.iter())
    }

    pub fn move_focus(&mut self, seat: &Seat, focus: layout::FocusDirection) -> Option<Window> {
        seat.user_data()
            .insert_if_missing(|| FocusStackData::new((HashMap::new(), IndexSet::new())));
        let focus_stack = FocusStackMut(RefMut::map(
            seat.user_data()
                .get::<FocusStackData>()
                .unwrap()
                .borrow_mut(),
            |map| map.0.entry(self.idx).or_insert_with(|| IndexSet::new()),
        ));
        self.layout
            .move_focus(focus, seat, &mut self.space, focus_stack.iter())
    }

    pub fn maximize_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.values().any(|w| w == window) {
            return;
        }
        self.layout
            .maximize_request(&mut self.space, window, output)
    }

    pub fn move_request(
        &mut self,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
    ) {
        if self.fullscreen.values().any(|w| w == window) {
            return;
        }
        self.layout
            .move_request(&mut self.space, window, seat, serial, start_data)
    }

    pub fn resize_request(
        &mut self,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
        edges: ResizeEdge,
    ) {
        if self.fullscreen.values().any(|w| w == window) {
            return;
        }
        self.layout
            .resize_request(&mut self.space, window, seat, serial, start_data, edges)
    }

    pub fn fullscreen_request(&mut self, window: &Window, output: &Output) {
        if self.fullscreen.contains_key(&output.name()) {
            return;
        }

        #[allow(irrefutable_let_patterns)]
        if let Kind::Xdg(xdg) = &window.toplevel() {
            if xdg.get_surface().is_some() {
                let ret = xdg.with_pending_state(|state| {
                    state.states.set(xdg_toplevel::State::Fullscreen);
                    state.size = Some(
                        output
                            .current_mode()
                            .map(|m| m.size)
                            .unwrap_or((0, 0).into())
                            .to_logical(output.current_scale().integer_scale()),
                    );
                });

                if ret.is_ok() {
                    xdg.send_configure();
                    self.fullscreen.insert(output.name(), window.clone());
                }
            }
        }
    }
    
    pub fn unfullscreen_request(&mut self, window: &Window) {
        if self.fullscreen.values().any(|w| w == window) {
            #[allow(irrefutable_let_patterns)]
            if let Kind::Xdg(xdg) = &window.toplevel() {
                if xdg.get_surface().is_some() {
                    let ret = xdg.with_pending_state(|state| {
                        state.states.unset(xdg_toplevel::State::Fullscreen);
                        state.size = None;
                    });
                    if ret.is_ok() {
                        self.layout.refresh(&mut self.space);
                        xdg.send_configure();
                    } 
                }
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
        self.fullscreen.get(&output.name()).filter(|w| w.toplevel().get_surface().is_some())
    }
}
