// SPDX-License-Identifier: GPL-3.0-only

//! Periodic process-memory self-report for leak hunting.
//!
//! Logs a compact `memlog` line (VmSize/VmRSS/RssAnon/RssShmem/VmSwap) at a
//! fixed interval on a dedicated thread, so a stalled event loop still
//! reports. Interval comes from `COSMIC_MEMLOG` (seconds, `0` disables);
//! defaults to 300.
//!
//! With the `heap-profile` feature the same thread also reports jemalloc's
//! allocated/resident/mapped totals — separating "malloc heap grew" from
//! "mmap outside the allocator" — and, when `COSMIC_HEAP_PROFILE_DIR` is set,
//! writes a numbered jemalloc heap profile there each tick. Profiling must be
//! active at startup: run with `MALLOC_CONF=prof:true,prof_active:true` (and
//! the same value in `_RJEM_MALLOC_CONF`; the effective name depends on the
//! symbol prefix jemalloc was built with).

use std::{thread, time::Duration};

use tracing::{info, warn};

const DEFAULT_INTERVAL: u64 = 300;

pub fn spawn() {
    let interval = std::env::var("COSMIC_MEMLOG")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .unwrap_or(DEFAULT_INTERVAL);
    if interval == 0 {
        return;
    }

    if let Err(err) = thread::Builder::new()
        .name("memlog".into())
        .spawn(move || run(Duration::from_secs(interval)))
    {
        warn!(?err, "Failed to spawn memlog thread");
    }
}

fn run(interval: Duration) {
    let mut seq = 0u64;
    loop {
        report(seq);
        seq += 1;
        thread::sleep(interval);
    }
}

fn report(seq: u64) {
    let Ok(status) = std::fs::read_to_string("/proc/self/status") else {
        return;
    };
    let field = |name: &str| -> u64 {
        status
            .lines()
            .find_map(|l| l.strip_prefix(name))
            .and_then(|rest| {
                rest.trim_start_matches(':')
                    .split_whitespace()
                    .next()
                    .and_then(|v| v.parse::<u64>().ok())
            })
            .unwrap_or(0)
    };

    info!(
        target: "memlog",
        seq,
        vm_size_mb = field("VmSize") / 1024,
        vm_rss_mb = field("VmRSS") / 1024,
        rss_anon_mb = field("RssAnon") / 1024,
        rss_shmem_mb = field("RssShmem") / 1024,
        vm_swap_mb = field("VmSwap") / 1024,
        threads = field("Threads"),
        "process memory"
    );

    #[cfg(feature = "heap-profile")]
    jemalloc::report(seq);
}

#[cfg(feature = "heap-profile")]
mod jemalloc {
    use std::ffi::CString;

    use tracing::{info, warn};

    pub(super) fn report(seq: u64) {
        // Advancing the epoch refreshes jemalloc's cached stats.
        let _ = tikv_jemalloc_ctl::epoch::advance();
        let allocated = tikv_jemalloc_ctl::stats::allocated::read().unwrap_or(0);
        let resident = tikv_jemalloc_ctl::stats::resident::read().unwrap_or(0);
        let mapped = tikv_jemalloc_ctl::stats::mapped::read().unwrap_or(0);
        info!(
            target: "memlog",
            seq,
            je_allocated_mb = allocated / (1024 * 1024),
            je_resident_mb = resident / (1024 * 1024),
            je_mapped_mb = mapped / (1024 * 1024),
            "jemalloc stats"
        );

        if let Ok(dir) = std::env::var("COSMIC_HEAP_PROFILE_DIR") {
            dump(&dir, seq);
        }
    }

    fn dump(dir: &str, seq: u64) {
        let path = format!("{dir}/cosmic-comp.{}.{seq:05}.heap", std::process::id());
        let Ok(c_path) = CString::new(path.clone()) else {
            return;
        };
        // "prof.dump" takes a *const c_char path as its write value. No typed
        // wrapper exists in tikv-jemalloc-ctl, so go through raw mallctl.
        let name = c"prof.dump";
        let mut ptr = c_path.as_ptr();
        let ret = unsafe {
            tikv_jemalloc_sys::mallctl(
                name.as_ptr(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut ptr as *mut _ as *mut std::ffi::c_void,
                size_of::<*const std::ffi::c_char>(),
            )
        };
        if ret == 0 {
            info!(target: "memlog", %path, "heap profile dumped");
        } else {
            // ENOENT/EFAULT here usually means prof:true was not set at startup.
            warn!(target: "memlog", ret, %path, "heap profile dump failed (is MALLOC_CONF=prof:true set?)");
        }
    }
}
