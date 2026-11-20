// SPDX-License-Identifier: GPL-3.0-only

use std::{
    sync::{
        OnceLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    thread,
    time::{Duration, Instant},
};

use calloop::{
    LoopHandle,
    timer::{TimeoutAction, Timer},
};
use tracing::error;

const INTERVAL: Duration = Duration::from_secs(1);
const TIMEOUT: Duration = Duration::from_secs(60);

static TURNED_AT: AtomicU64 = AtomicU64::new(0);
static ARMED: AtomicBool = AtomicBool::new(false);

fn elapsed_ms() -> u64 {
    static START: OnceLock<Instant> = OnceLock::new();
    START.get_or_init(Instant::now).elapsed().as_millis() as u64
}

fn held_by_kernel(tid: u32) -> bool {
    std::fs::read_to_string(format!("/proc/self/task/{tid}/stat"))
        .ok()
        .and_then(|stat| stat.rsplit_once(')')?.1.trim_start().chars().next())
        .is_some_and(|state| matches!(state, 'D' | 'T' | 't'))
}

pub fn start<D>(handle: &LoopHandle<'static, D>) {
    if crate::utils::env::bool_var("COSMIC_DISABLE_WATCHDOG").unwrap_or(false) {
        return;
    }

    TURNED_AT.store(elapsed_ms(), Ordering::Relaxed);
    let timer = handle.insert_source(Timer::from_duration(INTERVAL), |_, _, _| {
        TURNED_AT.store(elapsed_ms(), Ordering::Relaxed);
        TimeoutAction::ToDuration(INTERVAL)
    });
    if let Err(err) = timer {
        error!(?err, "Failed to start the watchdog");
        return;
    }

    ARMED.store(true, Ordering::Relaxed);
    let main_thread = std::process::id();
    let thread = thread::Builder::new()
        .name("watchdog".into())
        .spawn(move || {
            let mut woke_at = elapsed_ms();
            while ARMED.load(Ordering::Relaxed) {
                thread::sleep(INTERVAL);
                let now = elapsed_ms();
                if now.saturating_sub(woke_at) > 5 * INTERVAL.as_millis() as u64
                    || held_by_kernel(main_thread)
                {
                    TURNED_AT.store(now, Ordering::Relaxed);
                }
                woke_at = now;

                let stuck_for = now.saturating_sub(TURNED_AT.load(Ordering::Relaxed));
                if stuck_for > TIMEOUT.as_millis() as u64 && ARMED.load(Ordering::Relaxed) {
                    thread::spawn(move || {
                        error!("Main loop stuck for {} s, aborting", stuck_for / 1000);
                    });
                    thread::sleep(Duration::from_millis(500));
                    std::process::abort();
                }
            }
        });
    if let Err(err) = thread {
        error!(?err, "Failed to start the watchdog");
    }
}

pub fn stop() {
    ARMED.store(false, Ordering::Relaxed);
}
