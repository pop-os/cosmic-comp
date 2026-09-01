# [Bug] Dual-monitor flicker/noise with 100Hz + 60Hz mixed refresh — Direct Scanout + EDID preferred mismatch

**Related upstream:** `pop-os/cosmic-comp#2336` `pop-os/cosmic-comp#1039` `pop-os/cosmic-comp#683` `pop-os/cosmic-comp#2384` `pop-os/cosmic-epoch#1760` `pop-os/cosmic-epoch#1540`

## Environment
- **OS:** Pop!_OS 24.04 LTS (`pop 24.04 noble`)
- **Compositor:** `cosmic-comp 1.0.0` (`a830784`) Wayland (`cosmic-comp:1628`, `XDG_SESSION_TYPE=wayland`, `WAYLAND_DISPLAY=wayland-1`)
- **GPU:** AMD Lucienne/Renoir `03:00.0 0x164C` `amdgpu` `DC 3.2.378` `VRAM 512M` `GTT 15743M` `kernel 7.1.5-76070105-generic` (`amdgpu 3.64.0`)
- **Displays:**
  - `DP-1` LG FHD 530×300mm `1920×1080` **preferred 100Hz (228.8 MHz DTD `605980a070381440`)** but forced `60.000 Hz current` (`cosmic-randr list`)
  - `HDMI-A-1` Samsung LF22T35 480×270mm `1920×1080` `60Hz` preferred+current
  - Layout `HDMI-A-1 0,0` + `DP-1 1920,0` (3840×1080, hotspot `x=1920`), both `Scale 100%`, `Adaptive Sync false` (Samsung `support true disabled`)
  - `Xwayland :1` primary `DP-1 true`

## Symptoms
With 2 monitors, intermittent flicker/static/noise — 1 corrupted frame, not capturable in screenshot/OBS — when moving windows, hovering dock, or waking from DPMS. Described by user as “ruido, no sé cómo explicarlo”. Cable ruled out (tested, `GNOME Mutter` with same hardware/cable at 100Hz does **not** flicker).

**Logs:**
```
kernel: amdgpu 0000:03:00.0: [drm] Failed to setup vendor infoframe on connector DP-1: -22  # -EINVAL
cosmic-comp: Unable to become drm master, assuming unprivileged mode
cosmic-comp: shortcuts custom config error: GetKey("custom", NotFound)
cosmic-session: GetKey("frosted_maximized_apps" / "list_button" / "workspaces" ...) # CosmicConfig KDL empty v1
```

**EDID decode (hex):**
- `DP-1` `GSM 5c8d 2025` `EDID 1.3` `Gamma 2.20` `DTD0 228.8 MHz 1920x1080@100.000` (preferred), `DTD1 138.5 MHz 1920x1080@59.934`, ext `174.5 MHz 74.973`, `Range 48-100Hz 30-120kHz 250MHz`, `CTA 02 03 16 f1` `Video 47 9004030112131f` `Vendor 65 030c001000` (HDMI OUI `00-0C-03` phys `1.0.0.0`)
- `HDMI-A-1` `SAM 707b 2245` `DTD0 148.5 MHz 1920x1080@60.000` `Range 48-75Hz`, `CTA 02 03 1c b1`

## Root Cause Analysis

**This is a COSMIC bug, not hardware.** Verified against `Mutter` (GNOME Wayland) on same hardware — no flicker.

1.  **Aggressive Direct Scanout + Overlay Planes** (`pop-os/cosmic-comp#2336#2384`): `cosmic-comp` is the only Wayland compositor that heavily uses `overlay-planes` + `direct scanout` via `smithay::backend::drm::DrmCompositor` (`primary+overlay+cursor planes`, `FrameFlags::ALLOW_SCANOUT`). `amdgpu DC 3.2.378` on Renoir/Lucienne has known bandwidth/DC bugs (`flip_done timed out`, `commit wait timed out` PLANE:59/77/83, `vendor infoframe -22` with HDMI VSDB on DP connector). `cosmic-comp` promotes to overlay without gating by `buffer GPU node` (fixed in `#2504`) and without `drmModeAtomicCommit(TEST_ONLY)` fallback to composition. Result: 1-frame corrupted overlay flicker.
    - Workaround verified upstream: `COSMIC_DISABLE_DIRECT_SCANOUT=1` eliminates flicker (`Drakulix` in `#1039`, `#2384`). `COSMIC_DISABLE_OVERLAY_SCANOUT=1` also mitigates.
    - `Mutter` is conservative: only `primary plane`, `TEST_ONLY` then `PAGE_FLIP_EVENT` with `mailbox` (MR !73) 0% block, per-CRTC `determine_deadline`, fallback to composition — no flicker.

2.  **Mixed-refresh EDID preferred mismatch → `-22` EINVAL:** `DP-1` preferred DTD is `100Hz 228.8MHz` (not a CTA VIC), but `cosmic-randr` forced `60Hz 148.5MHz` (VIC 16). `amdgpu_dm.c:fill_stream_properties_from_drm_display_mode()` tries `drm_hdmi_vendor_infoframe_from_display_mode(HDMI OUI 00-0C-03)` on a `DP-1` `SIGNAL_TYPE_DISPLAY_PORT` → `-EINVAL` (`-22`). Leaves `HDMI_VIC=0` + stale watermarks (`renoir: fix hubbub` low `DCLK 200MHz` vs `dclk 300MHz` needed for cursor at 100Hz). `pp_dpm_sclk 200* 700 1900` stuck at `S0` + `mclk 1333*` forced max + `56000 m°C` normal but `GTT` latency → underflow on window move.

3.  **Hotspot `x=1920` + `Xwayland primary DP-1`:** Layout `0,0` + `1920,0` exact edge triggers `damage-tracking` + `xwayland primary` race (`Failed to set xwayland primary output` bursts in `#2413`).

4.  **COSMIC config empty `v1`:** `com.system76.CosmicComp/v1` only `autotile_behavior`+`xkb_config` → `GetKey("workspaces", ...)` flood; `CosmicTheme.Dark/v1` empty vs `v2` with `list_button` → `frosted_maximized_apps` error. Prevents persisting `100Hz` preferred, forcing fallback to `60Hz` each reboot.

## Hardware-Agnostic Fix (Rust, XDG, event-driven like Mutter)

Implemented as **two Rust daemons** (replaces Python, no hardcode `/home/diez`, XDG spec 0.8 compliant with `is_absolute` + `empty` + `HOST_XDG_*` + `0700`):

**`cosmic-display-fix` (`cosmic-display-fix/src/main.rs`):**
- `edid_parser` reads `/sys/class/drm/card*/edid` for any GPU/monitor, extracts `DTD0 preferred` + `Range` + `has_hdmi_vsdb` (agnostic, not LG/Samsung hardcoded)
- Compares `cosmic-randr list` `current` vs `preferred`; if `|Δ|>5Hz` or `vendor_err` and `|EDIDpref - randr preferred|<0.5Hz`, corrects via `cosmic-randr mode <output> <w> <h> --refresh <preferred> --adaptive-sync false` (forces native 100Hz for DP-1, 60Hz for HDMI, `VRR off` on multi-monitor)
- Ensures `COSMIC_DISABLE_DIRECT_SCANOUT=1` + `COSMIC_DISABLE_OVERLAY_SCANOUT=1` in `${XDG_CONFIG_HOME:-$HOME/.config}/environment.d/99-cosmic-disable-scanout.conf` + `/etc/profile.d/cosmic-disable-scanout.sh` + `systemctl --user import-environment` + `dbus-update-activation-environment` (like Mutter primary-only fallback)
- **Event-driven 0% idle** (vs previous polling 2880 wakeups/day): `notify` (inotify) on `/sys/class/drm/card*/status` + `${XDG_CONFIG_HOME}/cosmic` + `udevadm monitor --subsystem-match=drm` fallback + `select(-1)` like `GUdevClient("drm") HOTPLUG` → `MetaKms::resources-changed`

**`cosmic-pip-fix` (`cosmic-pip-fix/src/main.rs`):** see separate PiP report `BUG_REPORT_PIP_EN.md`.

**Verification on affected hardware:**
```
cosmic-randr list → DP-1 100.000 Hz (current)(preferred) + HDMI 60.000 Hz (current)(preferred) ✓
inotify watches: ['/sys/.../DP-1/status', ... '/home/.../.config/cosmic'] (event-driven)
systemctl --user is-active cosmic-display-fix.service cosmic-pip-sticky.service → active (Rust 4.0M/3.6M, 18/3 voluntary switches)
journalctl -b -k | grep vendor → 1 old line 20:20:43, 0 after fix
```

## Proposed Upstream Fix

1.  **Short-term (COSMIC):** Gate overlay scanout per-element by `buffer GPU node` (already `#2504`), add `modifier`/`format` intersection with `dmabuf_feedback`, and disable overlay when `has_active_fullscreen || animations_going` or bandwidth near limit (like `DrmOutputManager` retry). Keep `COSMIC_DISABLE_*` as escape hatch but fix root.
2.  **Medium-term:** Respect `EDID preferred` by default in `cosmic-randr`/`cosmic-settings` (like `Mutter` `monitors.xml` + `DisplayConfig` DBus), persist via `cosmic-config` `monitors.xml` per-serial, and handle `vendor infoframe -22` gracefully (fallback to `AVI` without `VSIF` on DP, don't log as error).
3.  **Long-term:** Align with `Mutter` `mailbox` + per-CRTC `determine_deadline` + `content_frame_rate_v1` LFC for 60+100Hz mixed refresh.

## Repro
1.  Connect LG FHD DP-1 (EDID 1e6d, 100Hz preferred) + Samsung LF22T35 HDMI (60Hz) on Renoir 0x164C, Pop!_OS 24.04, `cosmic-comp 1.0.0`
2.  Force `cosmic-randr mode DP-1 1920 1080 --refresh 60` (mismatch)
3.  Move windows across `x=1920` edge or hover dock → observe 1-frame corruption, `dmesg -w` shows `-22` on next modeset.

## Attachments
- `edid_parser` Rust + `universal_display_daemon` (event-driven) + `Cargo.toml` workspace
- `drm_info` + `hexdump -C` EDIDs + `cosmic-randr list` before/after

**Cable ruled out:** same cable on `GNOME Wayland` at 100Hz no flicker.

