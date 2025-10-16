use rustix::process::{Resource, Rlimit, getrlimit, setrlimit};
use std::sync::atomic::{AtomicU64, Ordering};

static OLD_LIMIT: AtomicU64 = AtomicU64::new(0);
static MAX_LIMIT: AtomicU64 = AtomicU64::new(0);

pub fn increase_nofile_limit() {
    let mut limits = getrlimit(Resource::Nofile);

    OLD_LIMIT.store(limits.current.unwrap_or(0), Ordering::SeqCst);
    MAX_LIMIT.store(limits.maximum.unwrap_or(0), Ordering::SeqCst);
    limits.current = limits.maximum;

    if let Err(err) = setrlimit(Resource::Nofile, limits) {
        tracing::warn!("Failed to raise nofile soft limit: {:?}", err);
    }
}

pub fn restore_nofile_limit() {
    let current = OLD_LIMIT.load(Ordering::SeqCst);
    let maximum = MAX_LIMIT.load(Ordering::SeqCst);
    let limits = Rlimit {
        current: (current > 0).then_some(current),
        maximum: (maximum > 0).then_some(maximum),
    };

    if let Err(err) = setrlimit(Resource::Nofile, limits) {
        // Use `eprintln!` instead of `tracing` since this is used in `pre_exec`.
        eprintln!("Failed to restore nofile soft limit: {:?}", err);
    }
}
