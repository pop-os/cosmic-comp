# Contributing — English Guide for Upstream

This fix was developed hardware-agnostic (LG 100Hz + Samsung 60Hz on Renoir 0x164C, but works for any GPU/monitor).

## Where to submit

| Fix | Repo | Issue to comment / PR |
|-----|------|-----------------------|
| Display flicker / direct scanout / EDID preferred / vendor -22 | `pop-os/cosmic-comp` | Comment on `pop-os/cosmic-comp#2336` (main) + `pop-os/cosmic-comp#1039` + open PR against `master` (`src/backend/kms/surface/mod.rs` `FrameFlags` gating) |
| PiP sticky / tiling_exception / XWayland ABOVE | `pop-os/cosmic-comp` (sticky) + `pop-os/cosmic-epoch` (UX) | Comment on `pop-os/cosmic-epoch#1526` (main) + `pop-os/cosmic-epoch#2895` + `pop-os/cosmic-comp#1960` + PR for `src/shell/mod.rs` `set_sticky` or `xdg_toplevel_tag` + `xx-pip-v1` |

**pop-os/cosmic-epoch is issue-only (restricted PR).** For PiP UX, comment on `cosmic-epoch#1526` and open PR in `cosmic-comp`. For display, PR directly in `cosmic-comp`.

## How to submit without `gh` (no token)

Since `gh` is not installed/authenticated, do manually:

### Option A — GitHub Web UI (recommended)

1. Fork `https://github.com/pop-os/cosmic-comp` → your fork `https://github.com/YOURUSER/cosmic-comp`
2. Clone your fork:
   ```bash
   git clone https://github.com/YOURUSER/cosmic-comp
   cd cosmic-comp
   git remote add upstream https://github.com/pop-os/cosmic-comp
   ```
3. Create branch:
   ```bash
   git checkout -b fix/hardware-agnostic-flicker-pip-rust
   ```
4. Copy patches from this repo:
   ```bash
   cp -r "/home/diez/Documentos/Default Project/cosmic-display-fix" ./cosmic-display-fix
   cp -r "/home/diez/Documentos/Default Project/cosmic-pip-fix" ./cosmic-pip-fix
   cp "/home/diez/Documentos/Default Project/.github/BUG_REPORT_DISPLAY_EN.md" ./docs/
   cp "/home/diez/Documentos/Default Project/.github/BUG_REPORT_PIP_EN.md" ./docs/
   # Or just reference this patch file:
   cp "/home/diez/Documentos/Default Project/0001-fix-cosmic-hardware-agnostic-dual-monitor-flicker-Pi.patch" ./
   git add cosmic-display-fix cosmic-pip-fix docs/BUG_REPORT*.md
   git commit -m "fix: hardware-agnostic dual-monitor flicker + PiP sticky (Rust, XDG, event-driven)"
   git push origin fix/hardware-agnostic-flicker-pip-rust
   ```
5. Open PR on GitHub: `https://github.com/pop-os/cosmic-comp/compare` → select your branch → PR body: paste content of `.github/PULL_REQUEST_TEMPLATE.md` + attach `0001-*.patch` + link to `BUG_REPORT_*.md`
6. Comment on issues: paste summary + link to PR
   - `https://github.com/pop-os/cosmic-comp/issues/2336#issuecomment`
   - `https://github.com/pop-os/cosmic-epoch/issues/1526#issuecomment`

### Option B — `gh` CLI (if you install and auth)

```bash
sudo apt install gh
gh auth login  # browser token
gh repo fork pop-os/cosmic-comp --clone
cd cosmic-comp
git checkout -b fix/hardware-agnostic-flicker-pip-rust
cp -r "/home/diez/Documentos/Default Project"/* ./
git add .
git commit -m "fix: ..."
gh pr create --repo pop-os/cosmic-comp --title "fix: hardware-agnostic dual-monitor flicker + PiP sticky (Rust, XDG, event-driven)" --body-file .github/PULL_REQUEST_TEMPLATE.md
gh issue comment 2336 --repo pop-os/cosmic-comp --body "Fix available in PR #xxx: hardware-agnostic EDID preferred + inotify + disable direct scanout (Mutter-style). Tested 100Hz+60Hz Renoir, 20/20."
gh issue comment 1526 --repo pop-os/cosmic-epoch --body "PiP fix: tiling_exception + xprop -spy event-driven, Rust XDG. See PR pop-os/cosmic-comp#xxx"
```

## Patch File

This repo already contains a ready-to-apply patch:
- `0001-fix-cosmic-hardware-agnostic-dual-monitor-flicker-Pi.patch` (56K, `git format-patch -1 HEAD`)

Apply:
```bash
git apply 0001-fix-cosmic-hardware-agnostic-dual-monitor-flicker-Pi.patch
```

## English Explanation (for PR body)

> **Bug:** Dual-monitor 100Hz (LG FHD) + 60Hz (Samsung LF22T35) on AMD Lucienne 0x164C flickers 1 frame (not capturable) when moving windows/hovering dock. Log `Failed to setup vendor infoframe on connector DP-1: -22`. Cable ruled out (GNOME Wayland same hardware 100Hz no flicker).
>
> **Root Cause:** COSMIC aggressive `direct scanout` + `overlay planes` without `TEST_ONLY` + `modifier/GPU node` gating + `VRR` + mixed-refresh `EDID preferred 100Hz forced 60Hz` → `amdgpu DC` `flip_done timed out` + stale watermarks. Plus `tiling_exception` missing for PiP + `XWayland ABOVE` ignored.
>
> **Fix:** Two Rust daemons, hardware-agnostic, XDG 0.8, event-driven like Mutter (`GUdevClient HOTPLUG` → `inotify` on `/sys/class/drm/*/status` + `XDG_CONFIG_HOME/cosmic`, `xprop -spy` + `busctl`, `OnceLock` regex, `dirs` crate, `0700`, `cargo clippy/fmt/test` pass). Display: EDID parser → `current` vs `preferred` → `cosmic-randr` correct + `COSMIC_DISABLE_*=1` + `VRR off`. PiP: `tiling_exception_custom` 7 appids (bilingual) + `wmctrl sticky+above` <100ms.
>
> **Test:** 20/20 `test_professional.sh`, `cosmic-randr` 100Hz+60Hz `current==preferred`, `systemctl active` Rust 4.0M/3.6M, 0% idle.

## Alternative: Just Comment (no PR)

If you don't want to fork, just comment on the issues with the English bug reports:

- Display: copy `https://github.com/pop-os/cosmic-comp/issues/2336` → `New comment` → paste `BUG_REPORT_DISPLAY_EN.md`
- PiP: `https://github.com/pop-os/cosmic-epoch/issues/1526` → paste `BUG_REPORT_PIP_EN.md`

And attach this repo as `https://github.com/YOURUSER/cosmic-universal-fixes` (push this `Default Project` to your own GitHub).

