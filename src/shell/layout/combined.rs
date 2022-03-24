use super::Layout;
use smithay::{
    desktop::{Space, Window},
    reexports::wayland_protocols::xdg_shell::server::xdg_toplevel::ResizeEdge,
    wayland::{
        compositor::with_states,
        seat::{PointerGrabStartData, Seat},
        shell::xdg::XdgToplevelSurfaceRoleAttributes,
        Serial,
    },
};
use std::sync::Mutex;

struct Filtered;

pub struct Combined<A: Layout, B: Layout> {
    first: A,
    second: B,
    windows_a: Vec<Window>,
    windows_b: Vec<Window>,
    filter: Box<dyn Fn(Option<&str>, Option<&str>) -> bool>,
}

impl<A: Layout, B: Layout> Combined<A, B> {
    pub fn new(
        first: A,
        second: B,
        filter: impl Fn(Option<&str>, Option<&str>) -> bool + 'static,
    ) -> Combined<A, B> {
        Combined {
            first,
            second,
            windows_a: Vec::new(),
            windows_b: Vec::new(),
            filter: Box::new(filter),
        }
    }
}

impl<A: Layout, B: Layout> Layout for Combined<A, B> {
    fn map_window<'a>(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        focus_stack: Box<dyn Iterator<Item = &'a Window> + 'a>,
    ) {
        if let Some(surface) = window.toplevel().get_surface() {
            let filtered = with_states(surface, |states| {
                let attrs = states
                    .data_map
                    .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                    .unwrap()
                    .lock()
                    .unwrap();
                if (self.filter)(attrs.app_id.as_deref(), attrs.title.as_deref()) {
                    window.user_data().insert_if_missing(|| Filtered);
                    true
                } else {
                    false
                }
            })
            .unwrap_or(false);
            if filtered {
                self.windows_b.push(window.clone());
                return self.second.map_window(
                    space,
                    window,
                    seat,
                    Box::new(focus_stack.filter(|w| w.user_data().get::<Filtered>().is_some())),
                );
            }
        }

        self.windows_a.push(window.clone());
        return self.first.map_window(
            space,
            window,
            seat,
            Box::new(focus_stack.filter(|w| w.user_data().get::<Filtered>().is_none())),
        );
    }

    fn refresh(&mut self, space: &mut Space) {
        self.first.refresh(space);
        self.second.refresh(space);
        self.windows_a.retain(|w| w.toplevel().alive());
        self.windows_b.retain(|w| w.toplevel().alive());
    }
    //fn unmap_window(&mut self, space: &mut Space, window: &Window);

    fn move_request(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
    ) {
        if self.windows_a.contains(window) {
            self.first
                .move_request(space, window, seat, serial, start_data)
        } else if self.windows_b.contains(window) {
            self.second
                .move_request(space, window, seat, serial, start_data)
        }
    }

    fn resize_request(
        &mut self,
        space: &mut Space,
        window: &Window,
        seat: &Seat,
        serial: Serial,
        start_data: PointerGrabStartData,
        edges: ResizeEdge,
    ) {
        if self.windows_a.contains(window) {
            self.first
                .resize_request(space, window, seat, serial, start_data, edges)
        } else if self.windows_b.contains(window) {
            self.second
                .resize_request(space, window, seat, serial, start_data, edges)
        }
    }
}
