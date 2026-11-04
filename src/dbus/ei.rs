use std::{
    os::unix::net::UnixStream,
    sync::{Arc, Mutex},
};

use smithay::reexports::calloop;
use smithay::reexports::wayland_protocols::wp::text_input::zv3::server::zwp_text_input_v3::{
    ContentHint, ContentPurpose,
};
use zbus::{
    names::{UniqueName, WellKnownName},
    object_server::SignalEmitter,
};

use super::name_owners::NameOwners;

static ALLOWED_NAMES: &[WellKnownName] = &[
    WellKnownName::from_static_str_unchecked("org.freedesktop.impl.portal.desktop.cosmic"),
    WellKnownName::from_static_str_unchecked("com.system76.CosmicOSK"),
];

/// Channel for handing the EI socketpair (and requested device types)
/// It's `None` until the EI sender side has been set up
type EiSender = Arc<Mutex<Option<calloop::channel::Sender<crate::libei::EiRequest>>>>;

struct Ei {
    ei_sender: EiSender,
    name_owners: NameOwners,
}

impl Ei {
    async fn check_sender_allowed(&self, sender: &UniqueName<'_>) -> zbus::fdo::Result<()> {
        if self.name_owners.check_owner(sender, ALLOWED_NAMES).await {
            Ok(())
        } else {
            Err(zbus::fdo::Error::AccessDenied("Access denied".to_string()))
        }
    }
}

#[zbus::interface(name = "com.system76.CosmicComp.Ei")]
impl Ei {
    /// Create a new EI sender context
    async fn get_sender_socket(
        &self,
        device_types: u32,
        #[zbus(header)] header: zbus::message::Header<'_>,
    ) -> zbus::fdo::Result<zbus::zvariant::OwnedFd> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
        }

        let (comp_stream, client_stream) = UnixStream::pair().map_err(|err| {
            zbus::fdo::Error::Failed(format!("Failed to create socket pair: {err}"))
        })?;

        {
            let guard = self.ei_sender.lock().unwrap();
            let sender = guard
                .as_ref()
                .ok_or_else(|| zbus::fdo::Error::Failed("EI sender not available".to_string()))?;
            sender.send((comp_stream, device_types)).map_err(|err| {
                zbus::fdo::Error::Failed(format!("Failed to hand off EI socket: {err}"))
            })?;
        }

        Ok(std::os::fd::OwnedFd::from(client_stream).into())
    }

    #[zbus(signal)]
    async fn activated(
        ctx: SignalEmitter<'_>,
        content_hint: u32,
        content_purpose: u32,
    ) -> zbus::Result<()>;

    #[zbus(signal)]
    async fn deactivated(ctx: SignalEmitter<'_>) -> zbus::Result<()>;
}

#[derive(Debug)]
pub struct EiState {
    conn: zbus::Connection,
    executor: calloop::futures::Scheduler<()>,
}

impl EiState {
    /// Register the `com.system76.CosmicComp.Ei` interface on the shared session connection.
    pub async fn new(
        conn: &zbus::Connection,
        name_owners: &NameOwners,
        ei_sender: EiSender,
        executor: &calloop::futures::Scheduler<()>,
    ) -> zbus::Result<Self> {
        let ei = Ei {
            ei_sender,
            name_owners: name_owners.clone(),
        };
        conn.object_server()
            .at("/com/system76/CosmicComp/Ei", ei)
            .await?;
        conn.request_name("com.system76.CosmicComp").await?;
        Ok(EiState {
            conn: conn.clone(),
            executor: executor.clone(),
        })
    }

    pub fn activated(&self, content_type: Option<(ContentHint, ContentPurpose)>) {
        let signal_context = SignalEmitter::new(&self.conn, "/com/system76/CosmicComp/Ei").unwrap();
        let (content_hint, content_purpose) =
            content_type.unwrap_or((ContentHint::None, ContentPurpose::Normal));
        let future = Ei::activated(signal_context, content_hint.bits(), content_purpose.into());
        let _ = self.executor.schedule(async {
            let _ = future.await;
        });
    }

    pub fn deactivated(&self) {
        let signal_context = SignalEmitter::new(&self.conn, "/com/system76/CosmicComp/Ei").unwrap();
        let future = Ei::deactivated(signal_context);
        let _ = self.executor.schedule(async {
            let _ = future.await;
        });
    }
}
