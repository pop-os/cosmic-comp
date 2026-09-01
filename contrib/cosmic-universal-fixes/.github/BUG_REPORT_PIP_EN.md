# [Feature/Bug] Picture-in-Picture PiP should stay on top — Global "Always on Top" + Sticky

**Related:** `pop-os/cosmic-epoch#1526` `pop-os/cosmic-epoch#1302` `pop-os/cosmic-epoch#2192` `pop-os/cosmic-epoch#2895` (duplicate) `pop-os/cosmic-epoch#1073` `pop-os/cosmic-comp#934` `pop-os/cosmic-comp#1960` (XWayland `_NET_WM_STATE_ABOVE` ignored) `pop-os/cosmic-comp#534`

## Environment
- **OS:** Pop!_OS 24.04 LTS, `cosmic-comp 1.0.0` (`a830784`) Wayland (`XDG_SESSION_TYPE=wayland`)
- **Browsers:** Brave `152.1.94.117` (`--ozone-platform=wayland` + `--ozone-platform=x11` tested), Firefox (Wayland `MOZ_ENABLE_WAYLAND=1`), Chrome/Chromium
- **GPU:** AMD Lucienne `0x164C` (also affects Intel/NVIDIA, not hardware-specific)

## Problem
I'm frustrated when using Picture-in-Picture (PiP) mode in browsers like Firefox or Brave. Currently, the PiP video window behaves like a standard window: it gets buried behind other applications and disappears when I switch to a different workspace. This defeats the purpose of PiP, which is to keep a video visible while multi-tasking.

In `pop-os/cosmic-epoch#1526` `Drakulix` said: *“There is no indicator for windows that want to stay on top in wayland. You can use the sticky-window keybinding to force this.”* But in COSMIC:
- PiP has no titlebar/CSD (`xdg_toplevel` without decoration) → right-click shows *Firefox* context menu, not compositor menu → can't click `Sticky window` / `Always on Top`.
- `Settings → Input → Keyboard → Sticky window` shortcut is **unbound by default** → user must manually assign `Super+/` or `Super+Right-Click` (issue `cosmic-epoch#909`).
- `Tiling` → PiP is tiled, not floating, so `sticky` doesn't work unless user manually makes it floating.
- `XWayland` `._NET_WM_STATE_ABOVE` is ignored in COSMIC (`cosmic-comp#1960` — `Works correctly on KDE Plasma`).

**Expected (like GNOME):** COSMIC should automatically recognize `Picture-in-Picture` windows and apply:
1.  **Always on Top** (`ABOVE`): floating above tiled/floating
2.  **Sticky / Global Presence** (`STICKY`): visible on all workspaces

## Root Cause

**Wayland by design:** client cannot set itself `ABOVE`/`STICKY` (security). Only compositor knows stacking (`discourse.gnome.org/t/8372` `gtk_window_stick` no-op on Wayland).

**GNOME solution (reference):** `Rafostar/gnome-shell-extension-pip-on-top` (115★, 80k downloads) — privileged `gnome-shell` extension:
```js
// extension.js
global.display.get_tab_list(Meta.TabList.NORMAL, workspace)
workspace.connect('window-added', onWindowAdded)
window.connect_after('notify::title', checkTitle)
checkTitle(window) {
  isPipWin = title == 'Picture-in-Picture' || title == 'Picture in picture' ||
             title.endsWith(' - PiP') || title == 'TelegramDesktop'
  if(isPipWin) { window.make_above(); window.stick(); }
}
```
Event-driven `window-created` + `notify::title`, `<50ms`, 0 polling.

**COSMIC current:**
- No hint: `cosmic-comp` `src/shell/mod.rs` `WorkspaceSet { sticky_layer: FloatingLayout }` — `Sticky(AtomicBool)` in `src/shell/element/surface.rs:207` is compositor-internal, only exposed via `zcosmic_toplevel_info_v1` `sticky=4` (v3) and `zcosmic_toplevel_management_v1::set_sticky` (v4, privileged). No auto-detection by title.
- `wayland-protocols` `xx-pip-v1` (MR132, `KWin 6.6+` `KWIN_WAYLAND_SUPPORT_XX_PIP_V1=1`) not yet in `wayland-protocols 1.49` / `wayland.app`, so COSMIC correctly waits. `xdg_toplevel_tag_v1` (`staging` since `1.43` 2025-04-08) already in `Mutter 50.4`/`KWin`/`Hyprland` — Chrome already sends `tag="pip"`.
- Workaround polling exists: `MagnetosphereLabs/cosmic-firefox-pip-fix` (`cosmic-ext-window-helper sticky true "title = 'Picture-in-Picture' and app_id ~= 'firefox'i and not is_sticky"` every 2s) — works but polling, 2s latency.

## Hardware-Agnostic Fix (Rust, XDG, event-driven like GNOME)

**`cosmic-pip-fix` (`cosmic-pip-fix/src/main.rs`):**

1.  **Make PiP floating via WindowRules** (`XDG_CONFIG_HOME` compliant, `0700`):
    ```ron
    // ${XDG_CONFIG_HOME:-$HOME/.config}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_custom
    [
      (appid: ".*", titles: ["Picture in picture", "Picture-in-Picture", ".*Picture.*", ".*PiP.*"]),
      (appid: "brave.*", titles: [".*Picture.*", ".*PiP.*", ".*Mini.*player.*"]),
      (appid: "chrome.*", titles: [".*Picture.*", ".*PiP.*"]),
      (appid: "chromium.*", titles: [".*Picture.*", ".*PiP.*"]),
      (appid: "firefox.*", titles: [".*Picture.*", ".*PiP.*"]),
      (appid: "org.mozilla.firefox.*", titles: [".*Picture.*", ".*PiP.*"]),
      (appid: "com.brave.Browser.*", titles: [".*Picture.*"])
    ]
    ```
    Enables `Sticky window` menu for PiP (tiling → floating).

2.  **Event-driven sticky** (GNOME-style, 0% idle, <100ms vs 2000ms polling):
    - `xprop -spy -root _NET_CLIENT_LIST` (XWayland, `select()` blocking, like `window-created`)
    - `busctl --user monitor --match interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'` (Wayland)
    - On new window, `is_pip_title()` (`OnceLock` regex `(?i)picture\s*in\s*picture|picture-in-picture|imagen\s*dentro|pip|min(i) player` — bilingual) → `wmctrl -i -r <id> -b add,sticky,above` + `xprop` check `_NET_WM_STATE_STICKY/ABOVE` to avoid churn
    - `is_already_sticky()` guard + `map_while(Result::ok)` + `OnceLock` for `RE_WMCTRL`

**Verification:**
```
tiling_exception_custom → 7 appids, count Picture==1, no ,, (valid RON)
systemctl --user is-active cosmic-pip-sticky.service → active (Rust 3.6M, 3 switches)
journalctl → xprop -spy _NET_CLIENT_LIST activo + busctl monitor PropertiesChanged activo (event-driven)
Brave → YouTube → right-click 2x → Picture in picture → sticky+above <100ms
```

## Proposed Upstream Fix

**Short-term (COSMIC, no protocol):** Auto-detect by title like GNOME extension, but in Rust compositor:
- In `cosmic-comp` `src/shell/mod.rs` `on_window_added` + `notify::title`, if `title.contains("Picture in picture")` (case-insensitive) or `title.ends_with(" - PiP")`, call `window.set_sticky(true)` + `set_above(true)` (use existing `zcosmic_toplevel_manager_v1::set_sticky` logic, but for non-privileged PiP). Add `XDG_CONFIG_HOME` helper with `HOST_XDG_*` Flatpak support.

**Medium-term:** Implement `xdg_toplevel_tag_v1` (already in `wayland-protocols 1.43`) — Chrome already sends `tag="pip"` → COSMIC can persist `always on top` rule per tag without polling.

**Long-term:** Implement `xx-pip-v1` → `ext-pip-v1` (`KWin` already `ext-foreign-toplevel` + `xx-pip`), gated by `COSMIC_SUPPORT_XX_PIP_V1=1` like KWin, for overlay layer above fullscreen (spec: *“These windows are placed in the overlay layer above all windows, including fullscreen”*).

**XDG note:** Use `dirs::config_dir()` + `is_absolute` + `empty` → default + `HOST_XDG_*` + `chmod 0700` (spec 0.8), not `format!("/home/diez/.config")`.

## Repro
1.  Brave (Wayland) → YouTube → right-click video 2× → `Picture in picture`
2.  Try `Right-click titlebar → Sticky window` → fails (tiling) or no titlebar
3.  Switch workspace → PiP disappears

## Attachments
- `cosmic-pip-fix` Rust (event-driven) + `Cargo.toml` workspace
- `tiling_exception_custom` RON + `systemd` units (`cosmic-pip-sticky.service`)

**References:** `Rafostar/gnome-shell-extension-pip-on-top` `MagnetosphereLabs/cosmic-firefox-pip-fix` `wayland-protocols MR132` `wayland.app/protocols/xdg-toplevel-tag-v1` `pop-os/cosmic-comp#1960`

