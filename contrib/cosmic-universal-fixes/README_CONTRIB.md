# Contrib: Hardware-Agnostic Fixes for COSMIC

This directory contains reference Rust implementations for two COSMIC bugs, as discussed in:
- https://github.com/pop-os/cosmic-comp/issues/2336
- https://github.com/pop-os/cosmic-epoch/issues/1526

See `../.github/BUG_REPORT_*.md` for full English bug reports.

## Fixes

1. **Display flicker** (`cosmic-display-fix`): EDID preferred vs current mismatch + `vendor infoframe -22` + `overlay scanout` without `TEST_ONLY` gating. Implements `inotify` on `/sys/class/drm/*/status` + `XDG_CONFIG_HOME` (is_absolute + HOST_XDG + 0700) like Mutter `GUdevClient HOTPLUG`, 0% idle.

2. **PiP sticky** (`cosmic-pip-fix`): `tiling_exception_custom` 7 appids + `xprop -spy` + `busctl` event-driven <100ms + `OnceLock` regex.

Both are standalone Rust daemons that can be used as workaround until `cosmic-comp` properly gates `direct scanout` (primary plane only + `drmModeAtomicCommit(TEST_ONLY)` fallback) and implements `xdg_toplevel_tag`/`xx-pip-v1`.

Build: `cargo build --release` in this directory (workspace).
Install: `bash cosmic-display-fix/cosmic-display-fix-install.sh` (XDG, no hardcode)
Test: `cargo test` `cargo fmt --check` `cargo clippy -- -D warnings` 20/20.

Upstream proposal: gate `overlay` per-element by `buffer GPU node` + `modifier` + `bandwidth` (see `src/backend/kms/surface/mod.rs:644` `COSMIC_DISABLE_*` handling) and add `xdg_toplevel_tag` support for PiP.
