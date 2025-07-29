use std::os::fd::OwnedFd;

use logind_zbus::manager::{InhibitType::HandleLidSwitch, ManagerProxyBlocking};
use zbus::blocking::Connection;

pub fn inhibit_lid() -> anyhow::Result<OwnedFd> {
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
