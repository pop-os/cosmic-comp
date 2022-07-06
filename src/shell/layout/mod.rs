// SPDX-License-Identifier: GPL-3.0-only

use crate::{input::ActiveOutput, state::State};
use regex::RegexSet;
use smithay::{
    desktop::{Space, Window},
    wayland::{
        compositor::with_states, output::Output, seat::Seat,
        shell::xdg::XdgToplevelSurfaceRoleAttributes,
    },
};
use std::sync::Mutex;

pub mod floating;
pub mod tiling;

#[derive(Debug, serde::Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

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
    ]).unwrap();
}

pub fn should_be_floating(window: &Window) -> bool {
    let surface = window.toplevel().wl_surface();
    with_states(surface, |states| {
        let attrs = states
            .data_map
            .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
            .unwrap()
            .lock()
            .unwrap();

        // simple heuristic taken from
        // sway/desktop/xdg_shell.c:188 @ 0ee54a52
        if attrs.parent.is_some()
            || (attrs.min_size.w != 0 && attrs.min_size.h != 0 && attrs.min_size == attrs.max_size)
        {
            return true;
        }

        // else take a look at our exceptions
        let appid_matches = EXCEPTIONS_APPID.matches(attrs.app_id.as_deref().unwrap_or(""));
        let title_matches = EXCEPTIONS_TITLE.matches(attrs.app_id.as_deref().unwrap_or(""));
        for idx in appid_matches.into_iter() {
            if title_matches.matched(idx) {
                return true;
            }
        }

        false
    })
}

fn output_from_seat(seat: Option<&Seat<State>>, space: &Space) -> Option<Output> {
    seat.and_then(|seat| {
        seat.user_data()
            .get::<ActiveOutput>()
            .map(|active| active.0.borrow().clone())
            .or_else(|| {
                seat.get_pointer()
                    .map(|ptr| space.output_under(ptr.current_location()).next().unwrap())
                    .cloned()
            })
    })
    .or_else(|| space.outputs().next().cloned())
}
