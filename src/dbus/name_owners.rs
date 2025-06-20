//! Helper for tracking if a caller of a DBus method owns a name on a bus
//!
//! Compare to Mutter's `MetaDbusAccessChecker`

use futures_executor::ThreadPool;
use futures_util::{StreamExt, stream::FuturesUnordered};
use std::{
    collections::{HashMap, HashSet},
    future::{Future, poll_fn},
    sync::{Arc, Mutex, Weak},
    task::{Context, Poll, Waker},
};
use zbus::{
    fdo,
    names::{BusName, UniqueName, WellKnownName},
};

#[derive(Debug)]
struct Inner {
    dbus: fdo::DBusProxy<'static>,
    name_owners: HashMap<WellKnownName<'static>, Option<UniqueName<'static>>>,
    unique_names: HashSet<UniqueName<'static>>,
    stream: fdo::NameOwnerChangedStream,
    // Waker from `update_task` is stored, so that task will still be woken after
    // polling elsewhere.
    waker: Waker,
    enforce: bool,
}

impl Drop for Inner {
    fn drop(&mut self) {
        // Wake `update_task` so it can terminate
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
            match args.name {
                BusName::Unique(name) => {
                    if args.new_owner.is_some() {
                        self.unique_names.insert(name.to_owned());
                    } else {
                        self.unique_names.remove(&name.to_owned());
                    }
                }
                BusName::WellKnown(name) => {
                    if let Some(owner) = &*args.new_owner {
                        self.name_owners
                            .insert(name.to_owned(), Some(owner.to_owned()));
                    } else {
                        self.name_owners.remove(&name.to_owned());
                    }
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

        let names = dbus.list_names().await?;
        let unique_names = names
            .iter()
            .filter_map(|n| match n.inner() {
                BusName::Unique(name) => Some(name.to_owned()),
                BusName::WellKnown(_) => None,
            })
            .collect();
        let name_owners = names
            .iter()
            .filter_map(|n| match n.inner() {
                BusName::Unique(_) => None,
                BusName::WellKnown(name) => Some((name.to_owned(), None)),
            })
            .collect();

        let inner = Arc::new(Mutex::new(Inner {
            dbus,
            name_owners,
            unique_names,
            stream,
            waker: Waker::noop().clone(),
            enforce,
        }));

        if enforce {
            executor.spawn_ok(update_task(Arc::downgrade(&inner)));
        }

        Ok(NameOwners(inner))
    }

    #[allow(dead_code)]
    pub fn has_unique_name(&self, name: &UniqueName<'_>) -> bool {
        let mut inner = self.0.lock().unwrap();
        inner.update_if_needed();
        inner.unique_names.contains(name)
    }

    /// Check if the unique name `name` owns at least one of the well-known names in `allowed_names`.
    ///
    /// Does not poll with `GetNameOwner` for well-known names that were already on the bus,
    /// but have not already been populated by an earlier `check_owner()` call.
    pub fn check_owner_no_poll(
        &self,
        name: &UniqueName<'_>,
        allowed_names: &[WellKnownName<'_>],
    ) -> bool {
        let mut inner = self.0.lock().unwrap();

        // Make sure latest events from stream have been processed
        inner.update_if_needed();

        if !inner.unique_names.contains(name) {
            // If unique is no longer on bus, no longer a valid client
            false
        } else if !inner.enforce {
            // If client exists, and we aren't enforcing owners, nothing
            // more to check.
            true
        } else {
            allowed_names
                .iter()
                .any(|n| inner.name_owners.get(n).map(|x| x.as_ref()).flatten() == Some(name))
        }
    }

    /// Lazily populate `name_owenrs` with owners of well known names
    /// from `names` that were advertised by `ListNames`.
    ///
    /// This way we avoid calling `GetNameOwner` for every single well-known
    /// name on the bus. Since there don't seem to be a way to populate that
    /// without a call per name.
    async fn poll_name_owners(&self, names: &[WellKnownName<'_>]) {
        let mut futures = {
            let inner = self.0.lock().unwrap();
            names
                .iter()
                .filter(|n| inner.name_owners.get(*n) == Some(&None))
                .map(|n| {
                    let dbus = inner.dbus.clone();
                    async move { (n, dbus.get_name_owner(BusName::WellKnown(n.as_ref())).await) }
                })
                .collect::<FuturesUnordered<_>>()
        };
        while let Some((n, owner)) = futures.next().await {
            let mut inner = self.0.lock().unwrap();
            if let Ok(owner) = owner {
                // If values is no longer `None`, this has raced against the
                // name owner changed stream. In which case, keep that value.
                if inner.name_owners.get(n) == Some(&None) {
                    inner.name_owners.insert(n.to_owned(), Some(owner.into()));
                }
            }
        }
    }

    /// Check if the unique name `name` owns at least one of the well-known names in `allowed_names`.
    pub async fn check_owner(
        &self,
        name: &UniqueName<'_>,
        allowed_names: &[WellKnownName<'_>],
    ) -> bool {
        self.poll_name_owners(allowed_names).await;
        self.check_owner_no_poll(name, allowed_names)
    }
}
