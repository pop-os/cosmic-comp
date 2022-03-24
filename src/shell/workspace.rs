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
pub struct FocusStack(IndexSet<Window>);

impl FocusStack {
    pub fn new() -> FocusStack {
        FocusStack(IndexSet::new())
    }

    pub fn append(&mut self, window: &Window) {
        self.0.retain(|w| w.toplevel().alive());
        self.0.shift_remove(window);
        self.0.insert(window.clone());
    }

    pub fn last(&self) -> Option<Window> {
        self.0.iter().rev().find(|w| w.toplevel().alive()).cloned()
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Window> + 'a> {
        //working around object-safety constraints for trait Layout
        Box::new(self.0.iter().rev().filter(|w| w.toplevel().alive()))
    }
}

pub struct Workspace {
    pub space: Space,
    pub(super) layout: Box<dyn Layout>,
    pub focus_stack: FocusStack,
    pub(super) pending_windows: Vec<(Window, Seat)>,
}

impl Workspace {
    pub fn new() -> Workspace {
        Workspace {
            space: Space::new(None),
            layout: layout::new_default_layout(),
            focus_stack: FocusStack::new(),
            pending_windows: Vec::new(),
        }
    }

    pub fn pending_window(&mut self, window: Window, seat: &Seat) {
        self.pending_windows.push((window, seat.clone()));
    }

    pub(super) fn map_window<'a>(&mut self, window: &Window, seat: &Seat) {
        self.layout
            .map_window(&mut self.space, window, seat, self.focus_stack.iter())
    }

    pub fn refresh(&mut self) {
        self.layout.refresh(&mut self.space);
        self.space.refresh();
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
