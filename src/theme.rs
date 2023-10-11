// insert into the event loop, a watcher for the theme & theme mode for changes

// update a Arc<Mutex<Theme>> in the state on change of the theme and mark all interfaces for a redraw.

use std::sync::{Arc, RwLock};

use calloop::LoopHandle;
use cosmic::cosmic_theme::{palette, Theme, ThemeMode};
use lazy_static::lazy_static;

use crate::state::State;

lazy_static! {
    pub static ref THEME: Arc<RwLock<cosmic::Theme>> =
        Arc::new(RwLock::new(cosmic::theme::system_preference()));
}

pub(crate) fn clear_color() -> [f32; 4] {
    let theme_guard = THEME.read().unwrap();
    let neutral_2 = theme_guard.cosmic().palette.neutral_2;
    [
        neutral_2.red,
        neutral_2.green,
        neutral_2.blue,
        neutral_2.alpha,
    ]
}

pub(crate) fn group_color() -> [f32; 3] {
    let theme_guard = THEME.read().unwrap();
    let neutral_8 = theme_guard.cosmic().palette.neutral_8;
    [neutral_8.red, neutral_8.green, neutral_8.blue]
}

pub(crate) fn accent_color() -> palette::Srgba {
    let theme_guard = THEME.read().unwrap();
    theme_guard.cosmic().accent_color()
}

pub(crate) fn active_hint() -> u8 {
    let theme_guard = THEME.read().unwrap();
    theme_guard.cosmic().active_hint as u8
}

pub(crate) fn gaps() -> (u8, u8) {
    let theme_guard = THEME.read().unwrap();
    let gaps = theme_guard.cosmic().gaps;
    (gaps.0 as u8, gaps.1 as u8)
}

pub(crate) fn active_window_hint() -> palette::Srgba {
    let theme_guard = THEME.read().unwrap();
    let t = theme_guard.cosmic();
    if let Some(hint) = t.window_hint {
        palette::Srgba::from(hint)
    } else {
        t.accent_color()
    }
}

pub fn watch_theme(handle: LoopHandle<'_, State>) -> Result<(), cosmic_config::Error> {
    let (ping_tx, ping_rx) = calloop::ping::make_ping().unwrap();
    let config_mode_helper = ThemeMode::config()?;
    let config_dark_helper = Theme::<palette::Srgba>::dark_config()?;
    let config_light_helper = Theme::<palette::Srgba>::light_config()?;

    if let Err(e) = handle.insert_source(ping_rx, move |_, _, state| {
        let new_theme = cosmic::theme::system_preference();
        let mut theme = THEME.write().unwrap();

        if theme.theme_type != new_theme.theme_type {
            *theme = new_theme;
            let gaps = theme.cosmic().gaps;
            state.common.shell.set_gaps((gaps.0 as u8, gaps.1 as u8));
            drop(theme);
            state.common.shell.workspaces.spaces().for_each(|s| {
                s.mapped().for_each(|m| {
                    m.set_theme();
                    m.force_redraw();
                })
            });
        }
    }) {
        tracing::error!("{e}");
    };

    let ping_tx_clone = ping_tx.clone();
    let theme_watcher_mode = config_mode_helper.watch(move |_, _keys| {
        ping_tx_clone.ping();
    })?;
    let ping_tx_clone = ping_tx.clone();
    let theme_watcher_light = config_light_helper.watch(move |_, _keys| {
        ping_tx_clone.ping();
    })?;
    let theme_watcher_dark = config_dark_helper.watch(move |_, _keys| {
        ping_tx.ping();
    })?;

    std::mem::forget(theme_watcher_dark);
    std::mem::forget(theme_watcher_light);
    std::mem::forget(theme_watcher_mode);

    Ok(())
}
