use std::os::fd::OwnedFd;

use anyhow::{Context, Result};
use logind_zbus::manager::{InhibitType::HandleLidSwitch, ManagerProxyBlocking};
use zbus::blocking::Connection;

pub fn inhibit_lid() -> Result<OwnedFd> {
    let conn = Connection::system()?;
    let proxy = ManagerProxyBlocking::new(&conn)?;
    let fd = proxy.inhibit(
        HandleLidSwitch,
        "cosmic-comp",
        "External output connected",
        "block",
    )?;

    Ok(fd.into())
}

pub fn lid_closed() -> Result<bool> {
    let conn = Connection::system()?;
    let proxy = ManagerProxyBlocking::new(&conn)?;
    proxy.lid_closed().context("Failed to talk to logind")
}
