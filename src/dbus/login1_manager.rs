use zbus::{zvariant, Connection};

#[zbus::proxy(
    interface = "org.freedesktop.login1",
    default_service = "org.freedesktop.login1.Manager",
    default_path = "/org/freedesktop/login1"
)]
pub trait Login1Manager {
    fn inhibit(
        &self,
        what: &str,
        who: &str,
        why: &str,
        mode: &str,
    ) -> zbus::Result<zvariant::OwnedFd>;
}

pub async fn inhibit_buttons() {
    let conn = Connection::system().await.unwrap();
    let manager = Login1ManagerProxy::new(&conn).await.unwrap();
    // XXX
    let pipe = manager
        .inhibit(
            "handle-power-key",
            "cosmic",
            "cosmic-comp handling power button",
            "block",
        )
        .await
        .unwrap();
    std::mem::forget(pipe); // XXX
}
