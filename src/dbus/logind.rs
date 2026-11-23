use std::os::fd::OwnedFd;

use anyhow::{Context, Result};
use cosmic_login1_zbus::manager::{InhibitType::HandleLidSwitch, ManagerProxy};

use crate::state::Common;

pub fn inhibit_lid(common: &Common) -> Result<OwnedFd> {
    let fd = zbus::block_on(async {
        let conn = common.dbus_state.system_conn().await?;
        let manager = ManagerProxy::new(conn).await?;
        manager
            .inhibit(
                HandleLidSwitch,
                "cosmic-comp",
                "External output connected",
                "block",
            )
            .await
    })?;

    Ok(fd.into())
}

pub fn lid_closed(common: &Common) -> Result<bool> {
    zbus::block_on(async {
        let conn = common.dbus_state.system_conn().await?;
        let manager = ManagerProxy::new(conn).await?;
        manager
            .lid_closed()
            .await
            .context("Failed to talk to logind")
    })
}
