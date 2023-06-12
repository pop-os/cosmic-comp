use smithay::{
    delegate_kde_decoration, delegate_xdg_decoration,
    reexports::{
        wayland_protocols::xdg::decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as XdgMode,
        wayland_protocols_misc::server_decoration::server::org_kde_kwin_server_decoration::{
            Mode as KdeMode, OrgKdeKwinServerDecoration,
        },
        wayland_server::protocol::wl_surface::WlSurface,
    },
    wayland::{
        seat::WaylandFocus,
        shell::{
            kde::decoration::{KdeDecorationHandler, KdeDecorationState},
            xdg::{decoration::XdgDecorationHandler, ToplevelSurface},
        },
    },
};
use wayland_backend::protocol::WEnum;

use crate::{
    shell::{CosmicMapped, CosmicSurface},
    state::State,
};

impl State {
    pub fn new_decoration(mapped: &CosmicMapped, surface: &WlSurface) -> KdeMode {
        if mapped.is_stack() {
            if let Some((CosmicSurface::Wayland(window), _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_ref() == Some(surface))
            {
                window
                    .toplevel()
                    .with_pending_state(|state| state.decoration_mode = Some(XdgMode::ServerSide));
                window.toplevel().send_configure();
            }
            KdeMode::Server
        } else {
            if let Some((CosmicSurface::Wayland(window), _)) = mapped
                .windows()
                .find(|(window, _)| window.wl_surface().as_ref() == Some(surface))
            {
                window
                    .toplevel()
                    .with_pending_state(|state| state.decoration_mode = Some(XdgMode::ClientSide));
                window.toplevel().send_configure();
            }
            KdeMode::Client
        }
    }

    pub fn request_mode(mapped: &CosmicMapped, surface: &WlSurface, mode: XdgMode) {
        if let Some((CosmicSurface::Wayland(window), _)) = mapped
            .windows()
            .find(|(window, _)| window.wl_surface().as_ref() == Some(surface))
        {
            window.toplevel().with_pending_state(|state| {
                state.decoration_mode = Some(mode);
            });
            window.toplevel().send_configure();
        }
    }

    pub fn unset_mode(mapped: &CosmicMapped, surface: &WlSurface) {
        if let Some((CosmicSurface::Wayland(window), _)) = mapped
            .windows()
            .find(|(window, _)| window.wl_surface().as_ref() == Some(surface))
        {
            window.toplevel().with_pending_state(|state| {
                state.decoration_mode = None;
            });
            window.toplevel().send_configure();
        }
    }
}

impl XdgDecorationHandler for State {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_wl_surface(toplevel.wl_surface())
        {
            State::new_decoration(mapped, toplevel.wl_surface());
        }
    }

    fn request_mode(&mut self, toplevel: ToplevelSurface, mode: XdgMode) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_wl_surface(toplevel.wl_surface())
        {
            State::request_mode(mapped, toplevel.wl_surface(), mode);
        } else {
            toplevel.with_pending_state(|state| state.decoration_mode = Some(mode));
        }
    }

    fn unset_mode(&mut self, toplevel: ToplevelSurface) {
        if let Some(mapped) = self
            .common
            .shell
            .element_for_wl_surface(toplevel.wl_surface())
        {
            State::unset_mode(mapped, toplevel.wl_surface())
        }
    }
}

impl KdeDecorationHandler for State {
    fn kde_decoration_state(&self) -> &KdeDecorationState {
        &self.common.kde_decoration_state
    }

    fn new_decoration(&mut self, surface: &WlSurface, decoration: &OrgKdeKwinServerDecoration) {
        if let Some(mapped) = self.common.shell.element_for_wl_surface(surface) {
            let mode = State::new_decoration(mapped, surface);
            decoration.mode(mode);
        }
    }

    fn request_mode(
        &mut self,
        surface: &WlSurface,
        decoration: &OrgKdeKwinServerDecoration,
        mode: WEnum<KdeMode>,
    ) {
        if let WEnum::Value(mode) = mode {
            // TODO: We need to store this value until it gets mapped and apply it then, if it is not mapped yet.
            if let Some(mapped) = self.common.shell.element_for_wl_surface(surface) {
                State::request_mode(
                    mapped,
                    surface,
                    match mode {
                        KdeMode::Server => XdgMode::ServerSide,
                        _ => XdgMode::ClientSide,
                    },
                );
                decoration.mode(mode);
            }
        }
    }

    fn release(&mut self, _decoration: &OrgKdeKwinServerDecoration, surface: &WlSurface) {
        if let Some(mapped) = self.common.shell.element_for_wl_surface(surface) {
            State::unset_mode(mapped, surface)
        }
    }
}

delegate_xdg_decoration!(State);
delegate_kde_decoration!(State);
