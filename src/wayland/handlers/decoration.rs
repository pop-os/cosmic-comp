use std::{cell::RefCell, sync::Mutex};

use smithay::{
    delegate_kde_decoration, delegate_xdg_decoration,
    desktop::Window,
    reexports::{
        wayland_protocols::xdg::decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as XdgMode,
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration::{
            Mode as KdeMode, OrgKdeKwinServerDecoration,
        },
        wayland_server::protocol::wl_surface::WlSurface,
    },
    wayland::{
        compositor::with_states,
        seat::WaylandFocus,
        shell::{
            kde::decoration::{KdeDecorationHandler, KdeDecorationState},
            xdg::{decoration::XdgDecorationHandler, ToplevelSurface},
        },
    },
};
use wayland_backend::protocol::WEnum;

use crate::state::State;

pub struct PreferredDecorationMode(RefCell<Option<XdgMode>>);

impl PreferredDecorationMode {
    pub fn is_unset(window: &Window) -> bool {
        window
            .user_data()
            .get::<PreferredDecorationMode>()
            .is_none()
    }

    pub fn mode(window: &Window) -> Option<XdgMode> {
        let user_data = window.user_data();
        user_data.insert_if_missing(|| PreferredDecorationMode(RefCell::new(None)));
        user_data
            .get::<PreferredDecorationMode>()
            .unwrap()
            .0
            .borrow()
            .clone()
    }

    pub fn update(window: &Window, update: Option<XdgMode>) {
        let user_data = window.user_data();
        user_data.insert_if_missing(|| PreferredDecorationMode(RefCell::new(None)));
        *user_data
            .get::<PreferredDecorationMode>()
            .unwrap()
            .0
            .borrow_mut() = update;
    }
}

pub type KdeDecorationData = Mutex<KdeDecorationSurfaceState>;
#[derive(Debug)]
pub struct KdeDecorationSurfaceState {
    pub mode: Option<KdeMode>,
    pub objs: Vec<OrgKdeKwinServerDecoration>,
}

impl Default for KdeDecorationSurfaceState {
    fn default() -> Self {
        KdeDecorationSurfaceState {
            mode: None,
            objs: Vec::new(),
        }
    }
}

impl XdgDecorationHandler for State {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface()) {
            let mode = if mapped.is_stack() {
                XdgMode::ServerSide
            } else {
                XdgMode::ClientSide
            };

            if let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
            {
                if let Some(toplevel) = window.0.toplevel() {
                    toplevel.with_pending_state(|state| {
                        state.decoration_mode = Some(mode);
                    });
                    toplevel.send_configure();
                }
            }
        }
    }

    fn request_mode(&mut self, toplevel: ToplevelSurface, mode: XdgMode) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface()) {
            if let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
            {
                if let Some(toplevel) = window.0.toplevel() {
                    PreferredDecorationMode::update(&window.0, Some(mode));
                    toplevel.with_pending_state(|state| {
                        state.decoration_mode = Some(mode);
                    });
                    toplevel.send_configure();
                }
            }
        } else {
            toplevel.with_pending_state(|state| state.decoration_mode = Some(mode));
        }
    }

    fn unset_mode(&mut self, toplevel: ToplevelSurface) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface()) {
            if let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
            {
                if let Some(toplevel) = window.0.toplevel() {
                    PreferredDecorationMode::update(&window.0, None);
                    toplevel.with_pending_state(|state| {
                        state.decoration_mode = None;
                    });
                    toplevel.send_configure();
                }
            }
        }
    }
}

impl KdeDecorationHandler for State {
    fn kde_decoration_state(&self) -> &KdeDecorationState {
        &self.common.kde_decoration_state
    }

    fn new_decoration(&mut self, surface: &WlSurface, decoration: &OrgKdeKwinServerDecoration) {
        let mode = if let Some(mapped) = self.common.shell.read().element_for_surface(surface) {
            if mapped.is_stack() {
                KdeMode::Server
            } else {
                KdeMode::Client
            }
        } else {
            KdeMode::Client
        };

        with_states(surface, |states| {
            let mut state = states
                .data_map
                .get_or_insert_threadsafe::<KdeDecorationData, _>(Default::default)
                .lock()
                .unwrap();

            state.objs.push(decoration.clone());
            if state.mode.is_none() {
                state.mode = Some(mode)
            }
        });

        decoration.mode(mode);
    }

    fn request_mode(
        &mut self,
        surface: &WlSurface,
        decoration: &OrgKdeKwinServerDecoration,
        mode: WEnum<KdeMode>,
    ) {
        if let WEnum::Value(mode) = mode {
            with_states(surface, |states| {
                states
                    .data_map
                    .get_or_insert_threadsafe::<KdeDecorationData, _>(Default::default)
                    .lock()
                    .unwrap()
                    .mode = Some(mode);
            });
            decoration.mode(mode);
        }
    }

    fn release(&mut self, decoration: &OrgKdeKwinServerDecoration, surface: &WlSurface) {
        with_states(surface, |states| {
            let mut state = states
                .data_map
                .get_or_insert_threadsafe::<KdeDecorationData, _>(Default::default)
                .lock()
                .unwrap();

            state.objs.retain(|obj| obj != decoration);
            if state.objs.is_empty() {
                state.mode.take();
            }
        });
    }
}

delegate_xdg_decoration!(State);
delegate_kde_decoration!(State);
