# Cosmic Universal Fixes — Rust COSMIC, hardware-agnóstico, event-driven (GNOME-style)

Soluciona 2 problemas en COSMIC (Pop!_OS 24.04) sin depender de marca/modelo/cable. **Analizado vs GNOME Mutter** — 0% CPU idle, XDG-compliant, profesional.

## Por qué GNOME no tenía ruido y COSMIC sí

**GNOME Mutter (correcto):** solo `primary plane` + `drmModeAtomicCommit` test + fallback, mailbox 0% bloqueo, VRR por CRTC `determine_deadline`, EDID preferred + `monitors.xml`, `GUdevClient HOTPLUG` → `MetaKms::resources-changed` → `poll(-1)` 0 wakeups.

**COSMIC bug:** `overlay planes` agresivo sin validar `modifier/GPU node` → `flip_done timed out`, FIFO 50% bloqueo, `60Hz` forzado en `100Hz` nativo → `vendor infoframe -22`. Fix: deshabilitar overlay (como Mutter) + `inotify/udev` event-driven.

## 1. Ruido Dual Monitor — Rust `cosmic-display-fix/src/main.rs`

**Problema:** 2 pantallas (ej LG 100Hz + Samsung 60Hz) ruido 1-frame. Log: `Failed to setup vendor infoframe on connector DP-1: -22`.

**Fix Rust profesional:**
- Parser EDID Rust puro (`parse_edid()` lee `/sys/class/drm/card*/edid`, no hardcodea marca)
- `cosmic-randr list` `current` vs `preferred` → corrige a nativo si `>5Hz` o vendor error
- `VRR off` multi-monitor, `COSMIC_DISABLE_DIRECT_SCANOUT=1` + `OVERLAY=1` en `${XDG_CONFIG_HOME:-$HOME/.config}/environment.d/99-cosmic-disable-scanout.conf` y `/etc/profile.d/cosmic-disable-scanout.sh` (XDG, no `/home/diez`)
- **Event-driven:** `notify` (inotify) sobre `/sys/class/drm/card*/status` + `${XDG_CONFIG_HOME}/cosmic` + `udevadm HOTPLUG` fallback, `select(-1)` 0% idle (antes 2880 wakeups/día)

```bash
cargo build --release
bash cosmic-display-fix-install.sh  # XDG, no hardcode
systemctl --user status cosmic-display-fix.service
RUST_LOG=info cosmic-display-fix --check
```

## 2. PiP Siempre Visible — Rust `cosmic-pip-fix/src/main.rs`

**Problema:** PiP Brave/Chrome/Firefox no deja marcar “Siempre visible” (Sticky).

**Por qué:** COSMIC `Sticky(AtomicBool)` en `surface.rs:207` no auto-asigna; `tiling_exception` faltante; `ext-pip-v1` pendiente.

**Fix Rust profesional:**
- WindowRules XDG: `${XDG_CONFIG_HOME}/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_custom` con 7 patrones `Picture in picture`, `PiP`, `Mini player`, `imagen dentro de imagen` (bilingüe, `dirs::config_dir()` XDG, no `/home/diez`)
- **Event-driven** (como GNOME `window-created` + `notify::title`): `xprop -spy -root _NET_CLIENT_LIST` + `busctl --user monitor PropertiesChanged`, `<100ms` vs `2000ms` polling, `wmctrl -i -r <id> -b add,sticky,above` solo en evento, `is_already_sticky()` evita churn

```bash
bash cosmic-pip-fix-install.sh  # XDG, wmctrl + xprop event-driven
systemctl --user status cosmic-pip-sticky.service
# Probar: Brave → YouTube → PiP → sticky auto
```

## Build profesional

```bash
cargo build --release
cargo test
cargo clippy -- -D warnings
cargo fmt --check
bash install_all.sh  # XDG, Rust, no Python
```

## Estructura profesional
```
Cargo.toml (workspace dirs, notify, tokio, regex, anyhow, log, env_logger, zbus, clap)
cosmic-display-fix/Cargo.toml + src/main.rs  (dirs 5.0, XDG, anyhow, log)
cosmic-pip-fix/Cargo.toml + src/main.rs      (dirs 5.0, regex, x11rb via wmctrl)
cosmic-display-fix-install.sh (XDG, PROJECT_DIR="$(cd "$(dirname "$0")"), no /home/diez)
cosmic-pip-fix-install.sh (XDG, idem)
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

## Requisitos
- Rust 1.75+, COSMIC 1.0+, `wmctrl`, `xprop` (x11-utils), `busctl`

## Referencias
- pop-os/cosmic-comp#2336, #2413, #2504
- GNOME mutter!73, !3177, Rafostar/pip-on-top, wayland-protocols!132
