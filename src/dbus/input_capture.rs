//! Private compositor-side API used by the COSMIC XDG portal InputCapture
//! implementation.
//!
//! The public portal deliberately remains in `xdg-desktop-portal-cosmic`.
//! This interface only transfers the EIS socket and the small amount of
//! compositor-owned state that must be checked on the event-loop thread.

use std::{
    os::unix::net::UnixStream,
    sync::{Arc, Mutex},
};

use futures_channel::oneshot;
use smithay::reexports::calloop;
use zbus::{
    message::Header,
    names::{UniqueName, WellKnownName},
    object_server::SignalEmitter,
    zvariant::OwnedFd,
};

use super::name_owners::NameOwners;

pub(crate) const PATH: &str = "/com/system76/CosmicComp/InputCapture";

// Input capture is only mediated by the portal. The OSK may inject input via
// its separate EI interface, but must never receive physical input here.
static ALLOWED_NAMES: &[WellKnownName] = &[WellKnownName::from_static_str_unchecked(
    "org.freedesktop.impl.portal.desktop.cosmic",
)];

type InputCaptureSender =
    Arc<Mutex<Option<calloop::channel::Sender<crate::input_capture::Request>>>>;

pub(crate) struct InputCapture {
    sender: InputCaptureSender,
    name_owners: NameOwners,
}

impl InputCapture {
    async fn check_sender_allowed(&self, sender: &UniqueName<'_>) -> zbus::fdo::Result<()> {
        if self
            .name_owners
            .check_owner_strict(sender, ALLOWED_NAMES)
            .await
        {
            Ok(())
        } else {
            Err(zbus::fdo::Error::AccessDenied("Access denied".to_string()))
        }
    }

    fn sender(&self) -> zbus::fdo::Result<calloop::channel::Sender<crate::input_capture::Request>> {
        self.sender
            .lock()
            .unwrap()
            .as_ref()
            .cloned()
            .ok_or_else(|| {
                zbus::fdo::Error::Failed("InputCapture event source not available".to_string())
            })
    }

    async fn authorize(&self, header: &Header<'_>) -> zbus::fdo::Result<()> {
        let sender = header.sender().ok_or_else(|| {
            zbus::fdo::Error::AccessDenied("InputCapture requires a named caller".to_string())
        })?;
        self.check_sender_allowed(sender).await
    }

    async fn send_and_wait<T>(
        &self,
        request: crate::input_capture::Request,
        reply: oneshot::Receiver<Result<T, String>>,
    ) -> zbus::fdo::Result<T> {
        self.sender()?.send(request).map_err(|err| {
            zbus::fdo::Error::Failed(format!("Failed to hand off request: {err}"))
        })?;
        reply
            .await
            .map_err(|err| {
                zbus::fdo::Error::Failed(format!("InputCapture request canceled: {err}"))
            })?
            .map_err(zbus::fdo::Error::Failed)
    }
}

#[zbus::interface(name = "com.system76.CosmicComp.InputCapture")]
impl InputCapture {
    /// Return an EIS receiver socket for one public InputCapture session.
    async fn get_receiver_socket(
        &self,
        session_handle: &str,
        device_types: u32,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<OwnedFd> {
        self.authorize(&header).await?;
        let (comp_stream, client_stream) = UnixStream::pair().map_err(|err| {
            zbus::fdo::Error::Failed(format!("Failed to create InputCapture socket pair: {err}"))
        })?;
        let (reply_tx, reply_rx) = oneshot::channel();
        self.sender()?
            .send(crate::input_capture::Request::Connect {
                session_handle: session_handle.to_string(),
                portal_owner: header.sender().unwrap().to_string(),
                device_types,
                stream: comp_stream,
                reply: reply_tx,
            })
            .map_err(|err| {
                zbus::fdo::Error::Failed(format!("Failed to hand off InputCapture socket: {err}"))
            })?;
        reply_rx
            .await
            .map_err(|err| {
                zbus::fdo::Error::Failed(format!("InputCapture connection canceled: {err}"))
            })?
            .map_err(zbus::fdo::Error::Failed)?;
        Ok(std::os::fd::OwnedFd::from(client_stream).into())
    }

    async fn get_zones(
        &self,
        _session_handle: &str,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<(u32, Vec<crate::input_capture::Zone>)> {
        self.authorize(&header).await?;
        let (reply_tx, reply_rx) = oneshot::channel();
        self.send_and_wait(
            crate::input_capture::Request::GetZones { reply: reply_tx },
            reply_rx,
        )
        .await
    }

    async fn set_pointer_barriers(
        &self,
        session_handle: &str,
        zone_set: u32,
        barriers: Vec<(u32, (i32, i32, i32, i32))>,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<Vec<u32>> {
        self.authorize(&header).await?;
        let (reply_tx, reply_rx) = oneshot::channel();
        let barriers = barriers
            .into_iter()
            .map(|(id, position)| crate::input_capture::Barrier::new(id, position))
            .collect();
        self.send_and_wait(
            crate::input_capture::Request::SetPointerBarriers {
                session_handle: session_handle.to_string(),
                zone_set,
                barriers,
                reply: reply_tx,
            },
            reply_rx,
        )
        .await
    }

    async fn enable(
        &self,
        session_handle: &str,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        self.authorize(&header).await?;
        let (reply_tx, reply_rx) = oneshot::channel();
        self.send_and_wait(
            crate::input_capture::Request::Enable {
                session_handle: session_handle.to_string(),
                reply: reply_tx,
            },
            reply_rx,
        )
        .await
    }

    async fn disable(
        &self,
        session_handle: &str,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        self.authorize(&header).await?;
        let (reply_tx, reply_rx) = oneshot::channel();
        self.send_and_wait(
            crate::input_capture::Request::Disable {
                session_handle: session_handle.to_string(),
                reply: reply_tx,
            },
            reply_rx,
        )
        .await
    }

    async fn release(
        &self,
        session_handle: &str,
        activation_id: zbus::zvariant::Optional<u32>,
        cursor_position: zbus::zvariant::Optional<(f64, f64)>,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        self.authorize(&header).await?;
        let (reply_tx, reply_rx) = oneshot::channel();
        self.send_and_wait(
            crate::input_capture::Request::Release {
                session_handle: session_handle.to_string(),
                activation_id: activation_id.into(),
                cursor_position: cursor_position.into(),
                reply: reply_tx,
            },
            reply_rx,
        )
        .await
    }

    async fn close(
        &self,
        session_handle: &str,
        #[zbus(header)] header: Header<'_>,
    ) -> zbus::fdo::Result<()> {
        self.authorize(&header).await?;
        self.sender()?
            .send(crate::input_capture::Request::Close {
                session_handle: session_handle.to_string(),
            })
            .map_err(|err| {
                zbus::fdo::Error::Failed(format!("Failed to close InputCapture session: {err}"))
            })?;
        Ok(())
    }

    #[zbus(signal)]
    pub(crate) async fn activated(
        emitter: &SignalEmitter<'_>,
        session_handle: &str,
        activation_id: u32,
        barrier_id: u32,
        cursor_position: (f64, f64),
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    pub(crate) async fn deactivated(
        emitter: &SignalEmitter<'_>,
        session_handle: &str,
        activation_id: u32,
        cursor_position: (f64, f64),
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    pub(crate) async fn disabled(
        emitter: &SignalEmitter<'_>,
        session_handle: &str,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    pub(crate) async fn zones_changed(
        emitter: &SignalEmitter<'_>,
        session_handle: &str,
        zone_set: u32,
    ) -> zbus::Result<()>;
}

/// Register the private interface on COSMIC's existing session-bus name.
pub(crate) async fn init(
    conn: &zbus::Connection,
    name_owners: &NameOwners,
    sender: InputCaptureSender,
) -> zbus::Result<()> {
    conn.object_server()
        .at(
            PATH,
            InputCapture {
                sender,
                name_owners: name_owners.clone(),
            },
        )
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok(())
}
