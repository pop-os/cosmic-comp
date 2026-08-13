// Theme watching — monitors color mode and brand changes via inotify,
// reloads CompTheme and propagates to the shell.

use std::path::PathBuf;

use calloop::LoopHandle;
use tracing::{info, warn};

use crate::comp_theme::CompTheme;
use crate::state::State;

/// Describes what changed on disk.
#[derive(Debug, Clone)]
pub enum ThemeEvent {
    /// Color mode or brand theme file changed — reload theme.
    Changed,
}

/// Start watching theme-related config files and reload `CompTheme` on changes.
///
/// Watches:
/// - `~/.config/cosmic/com.system76.CosmicTheme.Mode/v1/` (color mode)
/// - `~/.config/icetron/` (brand theme symlink)
/// - the active theme's installed directory (RON content, via the brand symlink)
///
/// When any changes, builds a new `CompTheme` and calls `shell.set_theme()`.
pub fn watch_theme(handle: LoopHandle<'_, State>) -> Result<(), Box<dyn std::error::Error>> {
    use notify::{Config, EventKind, RecommendedWatcher, RecursiveMode, Watcher};

    let color_mode_dir =
        dirs::config_dir().map(|d| d.join("cosmic/com.system76.CosmicTheme.Mode/v1"));
    let theme_dir = dirs::config_dir().map(|d| d.join("icetron"));

    if color_mode_dir.is_none() && theme_dir.is_none() {
        warn!("Cannot determine config paths; theme watching disabled");
        return Ok(());
    }

    // Ensure dirs exist
    if let Some(ref dir) = color_mode_dir {
        std::fs::create_dir_all(dir).ok();
    }
    if let Some(ref dir) = theme_dir {
        std::fs::create_dir_all(dir).ok();
    }

    // Also watch the active theme's installed directory so RON *content* updates
    // (theme-package reinstalls) hot-reload, not just brand/color-mode switches.
    let active_theme_dir = resolve_active_theme_dir();

    let (tx, rx) = calloop::channel::channel::<ThemeEvent>();

    // Spawn a background OS thread for the notify watcher
    let color_dir_clone = color_mode_dir.clone();
    let theme_dir_clone = theme_dir.clone();
    let active_theme_dir_clone = active_theme_dir.clone();

    std::thread::Builder::new()
        .name("theme-watcher".into())
        .spawn(move || {
            let (notify_tx, notify_rx) = std::sync::mpsc::channel();

            let Ok(mut watcher) = RecommendedWatcher::new(notify_tx, Config::default()) else {
                warn!("Failed to create theme file watcher");
                return;
            };

            if let Some(ref dir) = color_dir_clone
                && let Err(e) = watcher.watch(dir, RecursiveMode::NonRecursive)
            {
                warn!("Failed to watch color mode dir {}: {e}", dir.display());
            }
            if let Some(ref dir) = theme_dir_clone
                && let Err(e) = watcher.watch(dir, RecursiveMode::NonRecursive)
            {
                warn!("Failed to watch theme config dir {}: {e}", dir.display());
            }
            if let Some(ref dir) = active_theme_dir_clone
                && let Err(e) = watcher.watch(dir, RecursiveMode::NonRecursive)
            {
                warn!("Failed to watch active theme dir {}: {e}", dir.display());
            }

            info!("Theme file watcher started");

            for e in notify_rx.into_iter().flatten() {
                if matches!(
                    e.kind,
                    EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_)
                ) && tx.send(ThemeEvent::Changed).is_err()
                {
                    break;
                }
            }
        })?;

    // Register the calloop channel source — callback runs on compositor main loop
    handle
        .insert_source(rx, |event, _, state| {
            if let calloop::channel::Event::Msg(ThemeEvent::Changed) = event {
                reload_theme(state);
            }
        })
        .map_err(|e| format!("Failed to insert theme watcher source: {e}"))?;

    // Also load the real theme at startup (replacing the hardcoded dark default)
    // We do this after watcher setup so we don't miss events during load.
    // Note: state isn't available here — initial load happens via State::new().

    Ok(())
}

/// Resolve the active theme's installed directory by following the brand symlink
/// (e.g. `~/.config/icetron/current-theme -> <datadir>/icetron/themes/playtron`),
/// falling back to the system-wide symlink.
///
/// The system-wide candidates come from the XDG data directories rather than a
/// hardcoded `/usr/share`, so an installation outside the FHS layout resolves
/// too; `/usr/local/share:/usr/share` is the spec default, so FHS systems keep
/// finding exactly what they found before.
///
/// Watching this directory makes installed RON *content* updates (e.g. a
/// `go-task install` of a rebuilt theme) trigger a live reload. Brand switches
/// are already covered by the `~/.config/icetron/` watch; after one the stale
/// directory keeps being watched until the next compositor start, which is
/// acceptable for this dev/install-time convenience.
fn resolve_active_theme_dir() -> Option<PathBuf> {
    dirs::config_dir()
        .map(|d| d.join("icetron/current-theme"))
        .into_iter()
        .chain(crate::utils::xdg_dirs::data_dirs("icetron/current-theme"))
        .find_map(|cand| std::fs::canonicalize(cand).ok().filter(|p| p.is_dir()))
}

/// Reload the theme from disk and propagate to the shell.
fn reload_theme(state: &mut State) {
    use icetron_themes::color_mode::{Mode, get_color_mode};
    use icetron_themes::dynamic::DynamicTheme;

    let is_dark = matches!(get_color_mode(), Mode::Dark);
    let theme_name = DynamicTheme::current_theme_name();
    info!(
        "Theme change detected — reloading (theme={:?}, dark={})",
        theme_name, is_dark
    );

    let new_theme = match theme_name {
        Some(ref name) => CompTheme::from_file(name, is_dark),
        None => {
            // No brand configured — use fallback with correct color mode
            let fallback = CompTheme::default();
            if fallback.is_dark == is_dark {
                fallback
            } else {
                CompTheme::from_file("playtron", is_dark)
            }
        }
    };

    state.common.theme = new_theme.clone();
    let shell = state.common.shell.clone();
    let mut workspace_guard = state.common.workspace_state.update();
    shell.write().set_theme(
        new_theme,
        &state.common.xdg_activation_state,
        &mut workspace_guard,
    );
}
