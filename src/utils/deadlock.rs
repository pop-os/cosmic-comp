// SPDX-License-Identifier: GPL-3.0-only

//! Runtime deadlock detection for `parking_lot` locks.
//!
//! The compositor shares most of its state — most importantly `Common::shell` —
//! between the event-loop thread and the KMS surface render threads via
//! `parking_lot::RwLock`. Two properties of that lock make a silent, total hang
//! easy to write by accident:
//!
//!   * it is **not reentrant**, so taking a second `read()` while still holding
//!     one on the same thread deadlocks if a writer queues in between (the lock
//!     is write-preferring, so the nested `read()` waits behind the writer, and
//!     the writer waits behind the outer guard). `read_recursive()` is the
//!     recursion-safe variant.
//!   * a lock-order inversion between two locks (thread A takes X then Y while
//!     thread B takes Y then X) wedges both threads.
//!
//! Either way the compositor stops servicing the Wayland socket and stops
//! rendering, with no panic, no exit and nothing in the journal — the process is
//! simply gone. This watchdog turns that into a logged backtrace.
//!
//! Enabled by the off-by-default `deadlock-detection` feature, since
//! `parking_lot`'s detection adds bookkeeping to *every* lock acquisition:
//!
//! ```text
//! cargo build --features deadlock-detection
//! ```
//!
//! Note this only sees `parking_lot` locks. `std::sync::Mutex` (used by, among
//! others, `IcedElement`'s interior) is invisible to it, so a clean report does
//! not by itself prove the hang was not a lock.

/// How often to scan for cycles. Cheap relative to the per-lock bookkeeping the
/// feature already imposes, and a hung compositor is not getting better on its
/// own, so a short interval just means a faster report.
#[cfg(feature = "deadlock-detection")]
const CHECK_INTERVAL: std::time::Duration = std::time::Duration::from_secs(5);

/// Start the background deadlock watchdog.
///
/// No-op unless built with the `deadlock-detection` feature.
pub fn spawn_watchdog() {
    #[cfg(feature = "deadlock-detection")]
    {
        let spawned = std::thread::Builder::new()
            .name("deadlock-detector".into())
            .spawn(|| {
                loop {
                    std::thread::sleep(CHECK_INTERVAL);

                    let deadlocks = parking_lot::deadlock::check_deadlock();
                    if deadlocks.is_empty() {
                        continue;
                    }

                    tracing::error!(
                        count = deadlocks.len(),
                        "parking_lot deadlock detected; the compositor is wedged"
                    );
                    for (i, threads) in deadlocks.iter().enumerate() {
                        for thread in threads {
                            tracing::error!(
                                cycle = i,
                                thread_id = ?thread.thread_id(),
                                "deadlocked thread backtrace:\n{:#?}",
                                thread.backtrace()
                            );
                        }
                    }
                }
            });

        match spawned {
            Ok(_) => {
                tracing::info!("Deadlock detection enabled (checking every {CHECK_INTERVAL:?})")
            }
            Err(err) => tracing::warn!(?err, "Failed to spawn deadlock detector"),
        }
    }
}
