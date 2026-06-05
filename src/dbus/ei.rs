use std::sync::{Arc, Mutex};
use zbus::names::{UniqueName, WellKnownName};

use super::name_owners::NameOwners;

static ALLOWED_NAMES: &[WellKnownName] = &[WellKnownName::from_static_str_unchecked(
    "org.freedesktop.impl.portal.desktop.cosmic",
)];

struct Ei {
    socket_path: Arc<Mutex<Option<String>>>,
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
    async fn get_socket_path(
        &self,
        #[zbus(header)] header: zbus::message::Header<'_>,
    ) -> zbus::fdo::Result<String> {
        if let Some(sender) = header.sender() {
            self.check_sender_allowed(sender).await?;
        }
        self.socket_path
            .lock()
            .unwrap()
            .clone()
            .ok_or_else(|| zbus::fdo::Error::Failed("EIS listener not available".to_string()))
    }
}

/// Register the `com.system76.CosmicComp.Ei` interface on the shared session connection.
pub async fn init(
    conn: &zbus::Connection,
    name_owners: &NameOwners,
    socket_path: Arc<Mutex<Option<String>>>,
) -> zbus::Result<()> {
    let ei = Ei {
        socket_path,
        name_owners: name_owners.clone(),
    };
    conn.object_server()
        .at("/com/system76/CosmicComp/Ei", ei)
        .await?;
    conn.request_name("com.system76.CosmicComp").await?;
    Ok(())
}
