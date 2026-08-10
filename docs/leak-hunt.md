# Compositor anon-heap leak hunt (CRD / HumainOS)

Context: on 0.0.8.61 a CRD leaked ~35 GB of cosmic-comp anonymous heap over a
weekend (report-2026-08-10-1786379007; second occurrence, first was 0.0.8.39 on
2026-07-02), ending in a zram reclaim livelock and a frozen desktop. This
branch adds instrumentation to attribute the leak with certainty.

## Fixes in this branch

Four confirmed bugs, all matching the leak/never-idle profile:

1. **Per-rect blur state** (`src/backend/render/wayland/blur_effect.rs`) — all
   blur rects of a surface shared one `BlurState` (one element `Id`, one
   `CommitCounter`), so with ≥2 rects each rect compared against its
   neighbour's values: the commit advanced every frame forever (damage never
   empty → the compositor could never submit an empty frame and go idle), the
   duplicated `Id` tripped smithay's forced-recapture-every-frame path (the
   `Duplicated FramebufferEffect element` warning the CRD logged 1 s after
   session start), and all rects fought over one cached capture texture,
   reallocating two GL textures per rect per frame. Now per-rect
   `BlurRectState` with stable distinct `Id`s. Also: a renderer error no
   longer wipes the fade `seen` list (which re-armed the fade deadline — and
   its forced redraws — on every failing frame).
2. **`wl_region` op-list growth** (smithay fork,
   `src/wayland/compositor/{mod,handlers}.rs`) — `wl_region` has no clear
   request, so blur clients reuse one region and "clear" it by subtracting a
   huge rect before re-adding every update; smithay appended every op forever
   and cosmic-comp's blur handler cloned the whole list per update. A subtract
   that covers everything now compacts the list to empty (semantically
   identical).
3. **SVG icon `Box::leak` per refresh** (`src/shell/element/window.rs`) —
   client-set named toplevel icons were rebuilt (and their SVG bytes leaked)
   on every `SpaceElement::refresh` (~6.6 Hz), because the change-fingerprint
   was compared only after building. Now fingerprint-first, and leaked bytes
   are memoized per path.
4. **`LayerShadowCache` eviction** (`src/backend/render/shadow.rs`) — keyed by
   `ObjectId` with no removal; every popup/layer surface ever shadowed left a
   permanent entry. Now retains on surface liveness like its sibling cache.

**Upstream sweep (2026-08-10):** pop-os/cosmic-comp master is only 2
(translation) commits past our fork point — nothing relevant upstream.
Smithay/smithay master has 72 commits past our fork's merge-base; four
leak/correctness fixes are cherry-picked onto our smithay branch:
`4e8da008` (multigpu per-surface texture cache freed on surface destroy),
`d1da9512` (orphaned EGLImage freed on import failure), `9479cd6c` (PBO freed
on ExportMem error — matters for screencopy/OBS), `e76b1eec` (shm damage
clamped to buffer size, stops recurring GL upload errors). Upstream's
`0753341c` drm_syncobj `import_sync_file` fix was evaluated and skipped: the
function has no call sites in our stack. Upstream has NO fix for the
`wl_region` accumulation — our compaction fix is a candidate to send up.

Known candidates deliberately NOT fixed yet (the heap profile will say if
they're live on the CRD): smithay transaction-queue growth behind a stuck
dmabuf sync-point blocker (Chrome + explicit sync), and
`frame_callbacks`/`damage` accumulation on minimized surfaces drained with
`throttle: None`. Also noted for follow-up: the dock binds toplevel-info ~13×
(client bug, amplifies title-churn fan-out), and several `animations_going()`
predicates that can pin rendering (stuck slide-crossfade snapshots, undrained
`pending_layer_*`, voice orb scale).

## What's in this branch (instrumentation)

- `utils::memlog` — always-on periodic memory self-report (default every
  300 s, `COSMIC_MEMLOG=<secs>` to change, `0` to disable). Logs
  `VmSize/VmRSS/RssAnon/RssShmem/VmSwap` + thread count, and with
  `heap-profile` also jemalloc allocated/resident/mapped.
- `heap-profile` cargo feature — whole-process jemalloc (C-side mallocs too)
  with the sampling heap profiler compiled in; periodic `prof.dump` into
  `COSMIC_HEAP_PROFILE_DIR`.

## Building the test binary

```bash
cargo build --release --features heap-profile
```

The release profile should carry debug info for later symbolization; if it
doesn't, build with `CARGO_PROFILE_RELEASE_DEBUG=true`. Keep the exact binary —
`jeprof` needs it to symbolize the dumps.

## Running on the CRD

Install the binary the usual way, then make the session set (e.g. via a
systemd drop-in or the greetd environment):

```
MALLOC_CONF=prof:true,prof_active:true,lg_prof_sample:19
_RJEM_MALLOC_CONF=prof:true,prof_active:true,lg_prof_sample:19
COSMIC_HEAP_PROFILE_DIR=/var/tmp/cosmic-heap
COSMIC_MEMLOG=300
```

`mkdir -p /var/tmp/cosmic-heap` first (must be writable by the session user;
do NOT use /tmp — it's tmpfs and the dumps would add to memory pressure).
`lg_prof_sample:19` = one sampled stack per ~512 KiB allocated; low overhead,
plenty of resolution for a multi-GB leak.

Then reproduce QA's setup: ~10 windows incl. a browser with Slack (title
churn), chat windows with blur, and leave it running for a few hours minimum
(overnight is better). No interaction needed.

## Reading the results

- Growth curve without any tooling:
  `journalctl --user -t cosmic-comp | grep memlog` — `rss_anon_mb` +
  `vm_swap_mb` rising monotonically confirms the leak; `je_allocated_mb`
  rising in step says it's malloc-visible (attributable); if `rss_anon_mb`
  rises but `je_allocated_mb` doesn't, the leak is raw-mmap outside malloc.
- Attribution: copy `/var/tmp/cosmic-heap/*.heap` and the exact binary to a
  dev machine, then diff first vs last dump:

```bash
jeprof --text   ./cosmic-comp --base=cosmic-comp.<pid>.00001.heap cosmic-comp.<pid>.<last>.heap
jeprof --svg    ./cosmic-comp --base=cosmic-comp.<pid>.00001.heap cosmic-comp.<pid>.<last>.heap > leak.svg
```

The top of that diff IS the leak call stack.

## Recovery guidance for QA (until fixed)

After a freeze: switch to tty2 and `pkill cosmic-comp` (session restarts).
Do NOT switch back to the desktop VT while it's frozen — leaving the
compositor's VT again needs the stalled compositor to ack (VT_PROCESS), which
wedges the whole box.
