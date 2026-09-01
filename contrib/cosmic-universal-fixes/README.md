# Cosmic Universal Fixes — Rust, Hardware-Agnostic, Event-Driven (GNOME-style)

Fixes two COSMIC issues on Pop!_OS 24.04 without depending on brand/model/cable. **Analyzed vs GNOME Mutter** — 0% CPU idle, XDG-compliant.

## Why GNOME had no noise and COSMIC did

**GNOME Mutter (correct):** primary plane only + `drmModeAtomicCommit` test + fallback, mailbox 0% block, VRR per-CRTC `determine_deadline`, EDID preferred + `monitors.xml`, `GUdevClient HOTPLUG` → `MetaKms::resources-changed` → `poll(-1)` 0 wakeups.

**COSMIC bug:** aggressive `overlay planes` without validating `modifier/GPU node` → `flip_done timed out`, FIFO 50% block, `60Hz` forced on `100Hz` native → `vendor infoframe -22`. Fix: disable overlay (like Mutter) + `inotify/udev` event-driven.

## 1. Dual-Monitor Noise — Rust `cosmic-display-fix/src/main.rs`

**Problem:** 2 displays (e.g. LG 100Hz + Samsung 60Hz) 1-frame noise. Log: `Failed to setup vendor infoframe on connector DP-1: -22`.

**Fix:**
- Pure Rust EDID parser (`parse_edid()` reads `/sys/class/drm/card*/edid`, no hardcoded brand)
- `cosmic-randr list` `current` vs `preferred` → corrects to native if `>5Hz` or vendor error
- `VRR off` multi-monitor, `COSMIC_DISABLE_DIRECT_SCANOUT=1` + `OVERLAY=1` in `${XDG_CONFIG_HOME:-$HOME/.config}/environment.d/99-cosmic-disable-scanout.conf` and `/etc/profile.d/cosmic-disable-scanout.sh` (XDG)
- **Event-driven:** `notify` (inotify) on `/sys/class/drm/card*/status` + `${XDG_CONFIG_HOME}/cosmic` + `udevadm HOTPLUG` fallback, `select(-1)` 0% idle (was 2880 wakeups/day)

```bash
cargo build --release
bash cosmic-display-fix-install.sh
systemctl --user status cosmic-display-fix.service
RUST_LOG=info cosmic-display-fix --check
```

## 2. PiP Always on Top — Rust `cosmic-pip-fix/src/main.rs`

**Problem:** Brave/Chrome/Firefox PiP can't be marked “Always on Top” (Sticky).

**Why:** COSMIC `Sticky(AtomicBool)` in `surface.rs:207` doesn't auto-assign; `tiling_exception` missing; `ext-pip-v1` pending.

**Fix:**
- WindowRules XDG: `${XDG_CONFIG_HOME}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_custom` with 7 patterns `Picture in picture`, `PiP`, `Mini player` (bilingual, `dirs::config_dir()` XDG)
- **Event-driven** (like GNOME `window-created` + `notify::title`): `xprop -spy -root _NET_CLIENT_LIST` + `busctl --user monitor PropertiesChanged`, `<100ms` vs `2000ms` polling, `wmctrl -i -r <id> -b add,sticky,above`, `is_already_sticky()` avoids churn

```bash
bash cosmic-pip-fix-install.sh
systemctl --user status cosmic-pip-sticky.service
# Test: Brave → YouTube → PiP → sticky auto
```

## Build

```bash
cargo build --release
cargo test
cargo clippy -- -D warnings
cargo fmt --check
bash install_all.sh
```

## Structure
```
Cargo.toml (workspace dirs, notify, tokio, regex, anyhow, log, env_logger, zbus, clap)
cosmic-display-fix/Cargo.toml + src/main.rs  (dirs 5.0, XDG)
cosmic-pip-fix/Cargo.toml + src/main.rs      (dirs 5.0, regex)
cosmic-display-fix-install.sh (XDG, PROJECT_DIR="$(cd "$(dirname "$0")")")
cosmic-pip-fix-install.sh (XDG)
install_all.sh (cargo build --release)
target/release/cosmic-display-fix (4.0M, Rust)
target/release/cosmic-pip-fix (3.6M, Rust)
```

## Tests
```bash
cargo test
RUST_LOG=info cargo run -p cosmic-display-fix -- --check
RUST_LOG=info cargo run -p cosmic-pip-fix -- --check
```

## Requirements
- Rust 1.75+, COSMIC 1.0+, `wmctrl`, `xprop` (x11-utils), `busctl`

## References
- pop-os/cosmic-comp#2336, #2413, #2504
- GNOME mutter!73, !3177, Rafostar/pip-on-top, wayland-protocols!132
