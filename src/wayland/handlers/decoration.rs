use std::{cell::RefCell, sync::Mutex};

use cosmic_comp_config::DecorationPreference;
use smithay::{
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
            xdg::{ToplevelSurface, decoration::XdgDecorationHandler},
        },
    },
};
use wayland_backend::protocol::WEnum;

use crate::{shell::CosmicMapped, state::State};

pub struct PreferredDecorationMode(RefCell<Option<XdgMode>>);

impl PreferredDecorationMode {
    pub fn is_unset(window: &Window) -> bool {
        window
            .user_data()
            .get::<PreferredDecorationMode>()
            .is_none_or(|mode| mode.0.borrow().is_none())
    }

    pub fn mode(window: &Window) -> Option<XdgMode> {
        let user_data = window.user_data();
        user_data.insert_if_missing(|| PreferredDecorationMode(RefCell::new(None)));
        *user_data
            .get::<PreferredDecorationMode>()
            .unwrap()
            .0
            .borrow()
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

impl State {
    pub fn default_decoration(&self) -> DecorationPreference {
        self.common.config.cosmic_conf.decoration_preference
    }

    pub fn update_decorations(&self) {
        let shell = self.common.shell.read();

        let update = |mapped: &CosmicMapped| {
            let mode = if mapped.is_stack() {
                XdgMode::ServerSide
            } else {
                XdgMode::from_preference(self.default_decoration())
            };
            for (window, _) in mapped.windows() {
                if PreferredDecorationMode::is_unset(&window.0) {
                    if let Some(toplevel) = window.0.toplevel() {
                        if toplevel.with_committed_state(|state| {
                            state.is_some_and(|state| state.decoration_mode.is_some())
                        }) {
                            toplevel.with_pending_state(|state| {
                                state.decoration_mode = Some(mode);
                            });
                            toplevel.send_configure();
                        }
                    }
                }
            }
        };

        for set in shell.workspaces.sets.values() {
            set.sticky_layer.mapped().for_each(update);
        }

        for space in shell.workspaces.spaces() {
            space.mapped().for_each(update);
            space
                .minimized_windows
                .iter()
                .filter_map(|m| m.mapped())
                .for_each(update);
        }
    }
}

trait FromDecorationPreference {
    fn from_preference(preference: DecorationPreference) -> Self;
}

impl FromDecorationPreference for XdgMode {
    fn from_preference(preference: DecorationPreference) -> Self {
        match preference {
            DecorationPreference::ClientSide => XdgMode::ClientSide,
            DecorationPreference::ServerSide => XdgMode::ServerSide,
        }
    }
}

impl FromDecorationPreference for KdeMode {
    fn from_preference(preference: DecorationPreference) -> Self {
        match preference {
            DecorationPreference::ClientSide => KdeMode::Client,
            DecorationPreference::ServerSide => KdeMode::Server,
        }
    }
}

pub type KdeDecorationData = Mutex<KdeDecorationSurfaceState>;
#[derive(Debug, Default)]
pub struct KdeDecorationSurfaceState {
    pub mode: Option<KdeMode>,
    pub objs: Vec<OrgKdeKwinServerDecoration>,
}

impl XdgDecorationHandler for State {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface()) {
            let mode = if mapped.is_stack() {
                XdgMode::ServerSide
            } else {
                XdgMode::from_preference(self.default_decoration())
            };

            if let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
                && let Some(toplevel) = window.0.toplevel()
            {
                toplevel.with_pending_state(|state| {
                    state.decoration_mode = Some(mode);
                });
                toplevel.send_configure();
            }
        } else {
            toplevel.with_pending_state(|state| {
                state.decoration_mode = Some(XdgMode::from_preference(self.default_decoration()))
            })
        }
    }

    fn request_mode(&mut self, toplevel: ToplevelSurface, mode: XdgMode) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface()) {
            if let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
                && let Some(toplevel) = window.0.toplevel()
            {
                PreferredDecorationMode::update(&window.0, Some(mode));
                toplevel.with_pending_state(|state| {
                    state.decoration_mode = Some(mode);
                });
                toplevel.send_configure();
            }
        } else {
            toplevel.with_pending_state(|state| state.decoration_mode = Some(mode));
            if let Some(pending) = shell
                .pending_windows
                .iter()
                .find(|pending| pending.surface.0.toplevel().is_some_and(|t| t == &toplevel))
            {
                PreferredDecorationMode::update(&pending.surface.0, Some(mode));
            }
        }
    }

    fn unset_mode(&mut self, toplevel: ToplevelSurface) {
        let shell = self.common.shell.read();
        if let Some(mapped) = shell.element_for_surface(toplevel.wl_surface())
            && let Some((window, _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_deref() == Some(toplevel.wl_surface()))
            && let Some(toplevel) = window.0.toplevel()
        {
            let mode = if mapped.is_stack() {
                XdgMode::ServerSide
            } else {
                XdgMode::from_preference(self.default_decoration())
            };

            PreferredDecorationMode::update(&window.0, None);
            toplevel.with_pending_state(|state| {
                state.decoration_mode = Some(mode);
            });
            toplevel.send_configure();
        } else {
            toplevel.with_pending_state(|state| {
                state.decoration_mode = Some(XdgMode::from_preference(self.default_decoration()))
            });
            if let Some(pending) = shell
                .pending_windows
                .iter()
                .find(|pending| pending.surface.0.toplevel().is_some_and(|t| t == &toplevel))
            {
                PreferredDecorationMode::update(&pending.surface.0, None);
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
                KdeMode::from_preference(self.default_decoration())
            }
        } else {
            KdeMode::from_preference(self.default_decoration())
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
