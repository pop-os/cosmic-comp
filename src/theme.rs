// insert into the event loop, a watcher for the theme & theme mode for changes

// update a Arc<Mutex<Theme>> in the state on change of the theme and mark all interfaces for a redraw.

use calloop::LoopHandle;
use cosmic::cosmic_theme::{Theme, ThemeMode, palette};

use crate::state::State;

pub(crate) fn _group_color(theme: &Theme) -> [f32; 3] {
    let neutral_8 = theme.palette.neutral_8;
    [neutral_8.red, neutral_8.green, neutral_8.blue]
}

pub(crate) fn active_window_hint(theme: &Theme) -> palette::Srgba {
    if let Some(hint) = theme.window_hint {
        palette::Srgba::from(hint)
    } else {
        theme.accent_color()
    }
}

pub fn watch_theme(handle: LoopHandle<'_, State>) -> Result<(), cosmic_config::Error> {
    let theme = cosmic::theme::system_preference();
    cosmic::icon_theme::set_default(theme.cosmic().icon_theme.clone());

    let (ping_tx, ping_rx) = calloop::ping::make_ping().unwrap();
    let config_mode_helper = ThemeMode::config()?;
    let config_dark_helper = Theme::dark_config()?;
    let config_light_helper = Theme::light_config()?;

    if let Err(e) = handle.insert_source(ping_rx, move |_, _, state| {
        let new_theme = cosmic::theme::system_preference();

        let cursor_theme_changed =
            state.common.theme.cosmic().cursor_theme != new_theme.cosmic().cursor_theme;
        let icon_theme_changed =
            state.common.theme.cosmic().icon_theme != new_theme.cosmic().icon_theme;

        if state.common.theme.theme_type != new_theme.theme_type {
            state.common.theme = new_theme;
            let theme = state.common.theme.clone();
            let mut workspace_guard = state.common.workspace_state.update();
            state.common.shell.write().set_theme(
                theme,
                &state.common.xdg_activation_state,
                &mut workspace_guard,
            );
        }

        if cursor_theme_changed {
            crate::backend::render::cursor::reload_cursors(state);
        }

        if icon_theme_changed {
            cosmic::icon_theme::set_default(state.common.theme.cosmic().icon_theme.clone());
            state.common.update_xwayland_settings();
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
