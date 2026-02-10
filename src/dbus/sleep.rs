// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{Context, Result};
use logind_zbus::manager::ManagerProxy;
use zbus::Connection;

pub async fn init() -> Result<ManagerProxy<'static>> {
    let conn = Connection::system()
        .await
        .context("Failed to connect to system D-Bus")?;
    let proxy = ManagerProxy::new(&conn)
        .await
        .context("Failed to create logind manager proxy")?;
    Ok(proxy)
}
