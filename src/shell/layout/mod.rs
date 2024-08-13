// SPDX-License-Identifier: GPL-3.0-only

use cosmic_settings_config::shortcuts::action::Orientation;
use regex::RegexSet;
use smithay::{
    desktop::WindowSurface,
    wayland::{compositor::with_states, shell::xdg::XdgToplevelSurfaceData},
    xwayland::xwm::WmWindowType,
};

use super::CosmicSurface;

pub mod floating;
pub mod tiling;

lazy_static::lazy_static! {
    static ref EXCEPTIONS_APPID: RegexSet = RegexSet::new(&[
        r"Authy Desktop",
        r"Com.github.amezin.ddterm",
        r"Com.github.donadigo.eddy",
        r".*",
        r"Enpass",
        r"Gjs",
        r"Gnome-initial-setup",
        r"Gnome-terminal",
        r"Guake",
        r"Io.elementary.sideload",
        r"KotatogramDesktop",
        r"Mozilla VPN",
        r"update-manager",
        r"Solaar",
        r"Steam",
        r"",
        r"TelegramDesktop",
        r"Zotero",
        r"gjs",
        r"gnome-screenshot",
        r"ibus-.*",
        r"jetbrains-toolbox",
        r"jetbrains-webstorm",
        r"jetbrains-webstorm",
        r"jetbrains-webstorm",
        r"krunner",
        r"pritunl",
        r"re.sonny.Junction",
        r"system76-driver",
        r"tilda",
        r"zoom",
        r"^.*?action=join.*$",
        r"",
    ]).unwrap();
    static ref EXCEPTIONS_TITLE: RegexSet = RegexSet::new(&[
        r".*",
        r".*",
        r".*",
        r"Discord Updater",
        r"Enpass Assistant",
        r"Settings",
        r".*",
        r"Preferences – General",
        r".*",
        r".*",
        r"Media viewer",
        r".*",
        r"Software Updater",
        r".*",
        r"^.*?(Guard|Login).*",
        r"Steam",
        r"Media viewer",
        r"Quick Format Citation",
        r".*",
        r".*",
        r".*",
        r".*",
        r"Customize WebStorm",
        r"License Activation",
        r"Welcome to WebStorm",
        r".*",
        r".*",
        r".*",
        r".*",
        r".*",
        r".*",
        r".*",
        r"wl-clipboard",
    ]).unwrap();
}

pub fn is_dialog(window: &CosmicSurface) -> bool {
    // Check "window type"
    match window.0.underlying_surface() {
        WindowSurface::Wayland(toplevel) => {
            if with_states(toplevel.wl_surface(), |states| {
                let attrs = states
                    .data_map
                    .get::<XdgToplevelSurfaceData>()
                    .unwrap()
                    .lock()
                    .unwrap();
                attrs.parent.is_some()
            }) {
                return true;
            }
        }
        WindowSurface::X11(surface) => {
            if surface.is_override_redirect()
                || surface.is_popup()
                || !matches!(
                    surface.window_type(),
                    None | Some(WmWindowType::Normal) | Some(WmWindowType::Utility)
                )
            {
                return true;
            }
        }
    };

    // Check if sizing suggest dialog
    let max_size = window.max_size();
    let min_size = window.min_size();

    if min_size.is_some() && min_size == max_size {
        return true;
    }

    false
}

pub fn has_floating_exception(window: &CosmicSurface) -> bool {
    // else take a look at our exceptions
    let appid_matches = EXCEPTIONS_APPID.matches(&window.app_id());
    let title_matches = EXCEPTIONS_TITLE.matches(&window.title());
    for idx in appid_matches.into_iter() {
        if title_matches.matched(idx) {
            return true;
        }
    }

    false
}
