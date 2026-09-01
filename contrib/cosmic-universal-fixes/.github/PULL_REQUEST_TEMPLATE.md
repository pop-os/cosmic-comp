## Description

Fixes hardware-agnostic dual-monitor flicker (100Hz+60Hz mixed refresh) + PiP sticky, Rust, XDG, event-driven (GNOME-style).

**Fixes:** `pop-os/cosmic-comp#2336` `pop-os/cosmic-comp#1039` `pop-os/cosmic-comp#683` `pop-os/cosmic-comp#2384` `pop-os/cosmic-comp#2413` `pop-os/cosmic-comp#1960` `pop-os/cosmic-epoch#1526` `pop-os/cosmic-epoch#1302` `pop-os/cosmic-epoch#2192` `pop-os/cosmic-epoch#2895`

## Root Cause
- Direct Scanout + Overlay Planes without `TEST_ONLY` + `modifier/GPU node` gating → `amdgpu DC 3.2.378` `flip_done timed out` + `vendor infoframe -22` on DP-1 with HDMI VSDB
- EDID preferred 100Hz forced to 60Hz → stale watermarks, `pp_dpm_sclk 200*`
- PiP: Wayland no `ABOVE`/`STICKY` hint, `tiling_exception` missing, `XWayland _NET_WM_STATE_ABOVE` ignored

## Solution
- **Display:** EDID parser (`/sys/class/drm/card*/edid`) → compare `current` vs `preferred` → auto-correct to native + `VRR off` + `COSMIC_DISABLE_*=1` + `inotify` on `.../status` + `XDG_CONFIG_HOME/cosmic` + `udevadm HOTPLUG` fallback, 0% idle (Mutter `GUdevClient`)
- **PiP:** `tiling_exception_custom` 7 appids (bilingual) + `xprop -spy` + `busctl monitor` event-driven <100ms + `OnceLock` regex + `wmctrl sticky+above` + `is_already_sticky`

## Testing
- `cargo test` `cargo fmt --check` `cargo clippy -- -D warnings` pass
- `cosmic-randr list` DP-1 100Hz + HDMI 60Hz `current==preferred`
- `systemctl --user is-active` active Rust 4.0M/3.6M, 20/20 `test_professional.sh`
- Cable ruled out (GNOME Wayland same hardware 100Hz no flicker)

## Checklist
- [x] XDG Base Directory 0.8 (`is_absolute` + `empty` + `HOST_XDG_*` + `0700`)
- [x] No hardcode `/home/diez` (`dirs::config_dir()` + `XDG_CONFIG_HOME`)
- [x] Event-driven, no polling
- [x] Rust `COSMIC` (`cargo 1.95` `rustc 1.95` `clippy` `rustfmt`)

## Repro
See `.github/BUG_REPORT_*.md`

## Screenshots
Before: 1-frame corruption on `x=1920` edge, `dmesg -22`
After: stable 100Hz+60Hz, PiP sticky on all workspaces
