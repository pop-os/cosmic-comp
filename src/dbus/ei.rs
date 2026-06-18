use std::{
    os::unix::net::UnixStream,
    sync::{Arc, Mutex},
};

use smithay::reexports::calloop;
use zbus::names::{UniqueName, WellKnownName};

use super::name_owners::NameOwners;

static ALLOWED_NAMES: &[WellKnownName] = &[
    WellKnownName::from_static_str_unchecked("org.freedesktop.impl.portal.desktop.cosmic"),
    WellKnownName::from_static_str_unchecked("com.system76.CosmicRemoteDesktop"),
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
}

/// Register the `com.system76.CosmicComp.Ei` interface on the shared session connection.
pub async fn init(
    conn: &zbus::Connection,
    name_owners: &NameOwners,
    ei_sender: EiSender,
) -> zbus::Result<()> {
    let ei = Ei {
        ei_sender,
        name_owners: name_owners.clone(),
    };
    conn.object_server()
        .at("/com/system76/CosmicComp/Ei", ei)
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok(())
}
