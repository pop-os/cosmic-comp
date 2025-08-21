use futures_executor::ThreadPool;
use futures_util::StreamExt;
use std::{
    collections::HashMap,
    future::{poll_fn, Future},
    sync::{Arc, Mutex, Weak},
    task::{Context, Poll, Waker},
};
use zbus::{
    fdo,
    names::{BusName, UniqueName, WellKnownName},
};

#[derive(Debug)]
struct Inner {
    name_owners: HashMap<WellKnownName<'static>, UniqueName<'static>>,
    stream: fdo::NameOwnerChangedStream,
    // Waker from `update_task` is stored, so that task will still be woken after
    // polling elsewhere.
    waker: Waker,
    enforce: bool,
}

impl Drop for Inner {
    fn drop(&mut self) {
        // XXX shouldn't wake until Arc has no refs? Race.
        self.waker.wake_by_ref();
    }
}

impl Inner {
    /// Process all events so far on `stream`, and update `name_owners`.
    fn update_if_needed(&mut self) {
        let mut context = Context::from_waker(&self.waker);
        while let Poll::Ready(val) = self.stream.poll_next_unpin(&mut context) {
            let val = val.unwrap();
            let args = val.args().unwrap();
            if let BusName::WellKnown(name) = args.name {
                if let Some(owner) = &*args.new_owner {
                    self.name_owners.insert(name.to_owned(), owner.to_owned());
                } else {
                    self.name_owners.remove(&name.to_owned());
                }
            }
        }
    }
}

/// This task polls the steam regularly, to make sure events on the stream aren't just
/// buffered indefinitely if `check_owner` is never called.
fn update_task(inner: Weak<Mutex<Inner>>) -> impl Future<Output = ()> {
    poll_fn(move |context| {
        if let Some(inner) = inner.upgrade() {
            let mut inner = inner.lock().unwrap();
            inner.waker = context.waker().clone();
            inner.update_if_needed();
            // Nothing to do now until waker is invoked
            Poll::Pending
        } else {
            // All strong references have been dropped, so task has nothing left to do.
            Poll::Ready(())
        }
    })
}

/// Track which DBus unique names own which well-known names, so protocols can be restricted to
/// only certain names.
///
/// Enforcement can be disabled by setting `COSMIC_ENFORCE_DBUS_OWNERS`.
#[derive(Clone, Debug)]
pub struct NameOwners(Arc<Mutex<Inner>>);

impl NameOwners {
    pub async fn new(connection: &zbus::Connection, executor: &ThreadPool) -> zbus::Result<Self> {
        let dbus = fdo::DBusProxy::new(connection).await?;
        let stream = dbus.receive_name_owner_changed().await?;

        let enforce = crate::utils::env::bool_var("COSMIC_ENFORCE_DBUS_OWNERS").unwrap_or(true);

        let inner = Arc::new(Mutex::new(Inner {
            name_owners: HashMap::new(),
            stream,
            waker: Waker::noop().clone(),
            enforce,
        }));

        if enforce {
            executor.spawn_ok(update_task(Arc::downgrade(&inner)));
        }

        Ok(NameOwners(inner))
    }

    /// Check if the unique name `name` owns at least one of the well-known names in `allowed_names`.
    pub fn check_owner(&self, name: &UniqueName<'_>, allowed_names: &[WellKnownName<'_>]) -> bool {
        let mut inner = self.0.lock().unwrap();
        if !inner.enforce {
            return true;
        }
        // Make sure latest events from stream have been processed
        inner.update_if_needed();

        allowed_names
            .iter()
            .any(|n| inner.name_owners.get(n) == Some(name))
    }
}
