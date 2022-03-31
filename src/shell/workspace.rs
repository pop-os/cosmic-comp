use super::{layout, Layout};
use indexmap::IndexSet;
use smithay::{
    desktop::{Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::ResizeEdge,
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
}

impl Workspace {
    pub fn new(idx: u8) -> Workspace {
        Workspace {
            idx,
            space: Space::new(None),
            layout: layout::new_default_layout(),
            pending_windows: Vec::new(),
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
        self.layout
            .resize_request(&mut self.space, window, seat, serial, start_data, edges)
    }
}
