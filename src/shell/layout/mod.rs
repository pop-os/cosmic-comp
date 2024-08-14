// SPDX-License-Identifier: GPL-3.0-only

use cosmic_settings_config::shortcuts::action::Orientation;
use regex::RegexSet;
use smithay::{
    desktop::WindowSurface,
    wayland::{compositor::with_states, shell::xdg::XdgToplevelSurfaceData},
    xwayland::xwm::WmWindowType,
};

use crate::config::Config;

use super::CosmicSurface;

pub mod floating;
pub mod tiling;

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

#[derive(Debug, Clone)]
pub struct TilingExceptions {
    app_ids: RegexSet,
    titles: RegexSet,
}

impl TilingExceptions {
    pub fn new(config: &Config) -> Self {
        let mut app_ids = Vec::new();
        let mut titles = Vec::new();

        for app in &config.cosmic_conf.tiling_exceptions {
            for title in &app.titles {
                app_ids.push(app.appid.clone());
                titles.push(title.clone());
            }
        }

        Self {
            app_ids: RegexSet::new(app_ids).unwrap(),
            titles: RegexSet::new(titles).unwrap(),
        }
    }
}

pub fn has_floating_exception(exceptions: &TilingExceptions, window: &CosmicSurface) -> bool {
    // else take a look at our exceptions
    let appid_matches = exceptions.app_ids.matches(&window.app_id());
    let title_matches = exceptions.titles.matches(&window.title());
    for idx in appid_matches.into_iter() {
        if title_matches.matched(idx) {
            return true;
        }
    }

    false
}
