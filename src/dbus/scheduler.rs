//! # DBus interface proxy for: `com.system76.Scheduler`
//!
//! [`SchedulerProxy`] allows cosmic-comp to inform system76-scheduler of the focused process
//! so system76-scheduler can prioritize it over other processes.

/// A type alias for the DBus proxy to system76-scheduler.
pub type Sys76Scheduler = SchedulerProxy<'static>;

/// Based on the [example client](https://github.com/pop-os/system76-scheduler/blob/20fe9020b48efcce0cb790f35402914c6cabc669/daemon/src/dbus.rs#L41) from system76-scheduler.
#[zbus::proxy(
    interface = "com.system76.Scheduler",
    default_service = "com.system76.Scheduler",
    default_path = "/com/system76/Scheduler"
)]
pub trait Scheduler {
    /// Replaces the active foreground process with the given process.
    ///
    /// The given process and its children will be prioritized over background processes
    /// based on the foreground and background config provided to system76-scheduler.
    fn set_foreground_process(&self, pid: u32) -> zbus::Result<()>;
}
