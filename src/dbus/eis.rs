// SPDX-License-Identifier: GPL-3.0-only

//! D-Bus interface for accepting EIS socket connections from the RemoteDesktop
//! portal. The portal creates a UNIX socket pair and sends the server-side fd
//! to the compositor via this interface.

use calloop::channel;
use futures_executor::ThreadPool;
use std::os::unix::net::UnixStream;
use tracing::{error, info, warn};
use zbus::message::Header;

/// Channel sender for delivering EIS sockets to the compositor's calloop.
#[derive(Clone)]
pub struct EisSocketSender {
    tx: channel::Sender<UnixStream>,
}

impl EisSocketSender {
    pub fn new(tx: channel::Sender<UnixStream>) -> Self {
        Self { tx }
    }
}

/// Allowed D-Bus well-known names that may call `AcceptEisSocket`.
const ALLOWED_CALLERS: &[&str] = &["org.freedesktop.impl.portal.desktop.cosmic"];

/// D-Bus interface for the compositor to accept EIS socket fds.
pub struct CosmicCompEis {
    sender: EisSocketSender,
}

impl CosmicCompEis {
    pub fn new(sender: EisSocketSender) -> Self {
        Self { sender }
    }
}

#[zbus::interface(name = "com.system76.CosmicComp.RemoteDesktop")]
impl CosmicCompEis {
    /// Accept an EIS socket fd from the RemoteDesktop portal.
    /// The portal sends the server-side of a UNIX socket pair; the compositor
    /// will run an EIS receiver on it to accept emulated input events.
    ///
    /// Only callers that own an allowed D-Bus well-known name (currently the
    /// COSMIC portal) may invoke this method.
    async fn accept_eis_socket(
        &self,
        #[zbus(header)] header: Header<'_>,
        #[zbus(connection)] connection: &zbus::Connection,
        fd: zbus::zvariant::OwnedFd,
    ) -> zbus::fdo::Result<()> {
        // Verify caller identity: resolve sender's unique name to well-known names
        let sender = header
            .sender()
            .ok_or_else(|| zbus::fdo::Error::AccessDenied("no sender in D-Bus message".into()))?;

        let dbus_proxy = zbus::fdo::DBusProxy::new(connection)
            .await
            .map_err(|e| zbus::fdo::Error::Failed(format!("D-Bus proxy error: {e}")))?;

        // Check if the sender owns any of the allowed well-known names
        let mut authorized = false;
        for allowed in ALLOWED_CALLERS {
            let bus_name: zbus::names::BusName<'_> = (*allowed)
                .try_into()
                .map_err(|e| zbus::fdo::Error::Failed(format!("invalid bus name: {e}")))?;
            if let Ok(owner) = dbus_proxy.get_name_owner(bus_name).await
                && owner.as_str() == sender.as_str()
            {
                authorized = true;
                break;
            }
        }

        if !authorized {
            warn!(
                sender = sender.as_str(),
                "Rejected AcceptEisSocket call from unauthorized D-Bus sender"
            );
            return Err(zbus::fdo::Error::AccessDenied(
                "caller is not an authorized portal process".into(),
            ));
        }

        // Verify the fd is a UNIX stream socket (not a file, pipe, etc.)
        let raw_fd = std::os::fd::OwnedFd::from(fd);
        {
            use std::os::fd::AsRawFd;
            let mut sock_type: libc::c_int = 0;
            let mut len = std::mem::size_of::<libc::c_int>() as libc::socklen_t;
            let ret = unsafe {
                libc::getsockopt(
                    raw_fd.as_raw_fd(),
                    libc::SOL_SOCKET,
                    libc::SO_TYPE,
                    std::ptr::addr_of_mut!(sock_type).cast(),
                    std::ptr::addr_of_mut!(len),
                )
            };
            if ret != 0 || sock_type != libc::SOCK_STREAM {
                warn!(
                    sender = sender.as_str(),
                    "Rejected AcceptEisSocket: fd is not a SOCK_STREAM socket"
                );
                return Err(zbus::fdo::Error::InvalidArgs(
                    "fd must be a SOCK_STREAM Unix socket".into(),
                ));
            }
        }

        let stream = UnixStream::from(raw_fd);
        info!(sender = sender.as_str(), "Accepted EIS socket via D-Bus");
        self.sender
            .tx
            .send(stream)
            .map_err(|_| zbus::fdo::Error::Failed("Compositor EIS channel closed".to_string()))
    }
}

/// Initialize the EIS D-Bus interface and register it on the session bus.
///
/// Sets up a calloop channel to deliver EIS socket connections to the
/// compositor's event loop, and spawns async D-Bus registration via the
/// executor.
pub fn init(
    evlh: &calloop::LoopHandle<'static, crate::state::State>,
    executor: &ThreadPool,
) -> anyhow::Result<()> {
    let (socket_tx, socket_rx) = channel::channel::<UnixStream>();

    // Register the socket receiver with calloop - when the portal sends
    // an EIS fd, this will deliver it to the compositor
    evlh.insert_source(socket_rx, |event, _, state| {
        if let channel::Event::Msg(stream) = event {
            // Initialize EIS state if needed, then add connection
            if state.common.eis_state.is_none() {
                match crate::input::eis::EisState::new(&state.common.event_loop_handle) {
                    Ok(eis_state) => {
                        state.common.eis_state = Some(eis_state);
                    }
                    Err(err) => {
                        error!("Failed to initialize EIS state: {}", err);
                        return;
                    }
                }
            }
            if let Some(eis_state) = &state.common.eis_state {
                eis_state.add_connection(stream);
            }
        }
    })
    .map_err(|e| anyhow::anyhow!("Failed to insert EIS socket channel: {}", e.error))?;

    // Spawn async D-Bus registration via the executor (same pattern as a11y)
    let sender = EisSocketSender::new(socket_tx);
    executor.spawn_ok(async move {
        match register_dbus(sender).await {
            Ok(()) => info!("EIS D-Bus interface registered"),
            Err(err) => error!("Failed to register EIS D-Bus interface: {}", err),
        }
    });

    Ok(())
}

async fn register_dbus(sender: EisSocketSender) -> anyhow::Result<()> {
    let connection = zbus::Connection::session().await?;
    let eis_interface = CosmicCompEis::new(sender);

    connection
        .object_server()
        .at("/com/system76/CosmicComp", eis_interface)
        .await?;

    connection
        .request_name("com.system76.CosmicComp.RemoteDesktop")
        .await?;

    // Keep the connection alive
    std::future::pending::<()>().await;
    Ok(())
}
