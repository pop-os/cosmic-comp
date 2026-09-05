use std::{
    os::unix::net::UnixStream,
    sync::{Arc, Mutex},
};

use smithay::reexports::calloop;
use tracing::warn;
use zbus::names::UniqueName;

use super::{client_allow_list::EiAllowList, name_owners::NameOwners};
use crate::libei::DEVICE_TYPE_ALL;

/// Channel for handing the EI socketpair (and requested device types)
/// It's `None` until the EI sender side has been set up
type EiSender = Arc<Mutex<Option<calloop::channel::Sender<crate::libei::EiRequest>>>>;

struct Ei {
    ei_sender: EiSender,
    name_owners: NameOwners,
    allow_list: EiAllowList,
}

impl Ei {
    /// Check `sender` against the allow list, returning the device types it may request.
    async fn check_sender_allowed(&self, sender: &UniqueName<'_>) -> zbus::fdo::Result<u32> {
        if !self
            .name_owners
            .check_owner(sender, self.allow_list.names())
            .await
        {
            return Err(zbus::fdo::Error::AccessDenied("Access denied".to_string()));
        }
        // `check_owner` has already polled the owners, so the second lookup is cache-only.
        Ok(self
            .name_owners
            .matched_name_no_poll(sender, self.allow_list.names())
            .and_then(|name| self.allow_list.device_types_for(&name))
            .unwrap_or(DEVICE_TYPE_ALL))
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
        let mut device_types = device_types;
        if let Some(sender) = header.sender() {
            // Cap the capabilities with the configured list
            let permitted = self.check_sender_allowed(sender).await?;
            let allowed = device_types & permitted;
            if allowed != device_types {
                warn!(
                    requested = format!("{device_types:#b}"),
                    permitted = format!("{permitted:#b}"),
                    "Restricting EI device types for {sender}",
                );
                // The mask covers nothing the client asked for, so there is nothing useful
                // to hand back.
                if allowed == 0 {
                    return Err(zbus::fdo::Error::AccessDenied(
                        "None of the requested device types are permitted for this client"
                            .to_string(),
                    ));
                }
            }
            device_types = allowed;
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
    allow_list: EiAllowList,
) -> zbus::Result<()> {
    let ei = Ei {
        ei_sender,
        name_owners: name_owners.clone(),
        allow_list,
    };
    conn.object_server()
        .at("/com/system76/CosmicComp/Ei", ei)
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok(())
}
