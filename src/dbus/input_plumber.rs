use anyhow::Result;
use futures_util::{Stream, StreamExt, stream::SelectAll};
use inputplumber_zbus::{
    composite_device::CompositeDeviceProxy,
    dbus_device::{DBusDeviceProxy, InputEvent},
    input_manager::InputManagerProxy,
};

const DESTINATION: &'static str = "org.shadowblip.InputPlumber";

pub(super) async fn task(conn: zbus::Connection) -> Result<()> {
    let manager = InputManagerProxy::builder(&conn)
        .destination(DESTINATION)?
        .path("/org/shadowblip/InputPlumber/Manager")?
        .interface("org.shadowblip.InputManager")?
        .build()
        .await?;

    let manage_all = manager.manage_all_devices().await?;
    tracing::warn!("manage all devices: {:?}", manage_all);
    if !manage_all {
        manager.set_manage_all_devices(true).await?;
    }

    #[derive(Debug)]
    enum Event {
        GamepadOrderChanged,
        Input(InputEvent),
    }

    let mut select_all = SelectAll::<Box<dyn Stream<Item = Event> + Unpin>>::new();

    let stream = manager
        .receive_gamepad_order_changed()
        .await
        .map(|_| Event::GamepadOrderChanged);
    select_all.push(Box::new(stream));

    for comp_path in manager.gamepad_order().await? {
        tracing::warn!("- composite device: {}", comp_path);
        let comp = CompositeDeviceProxy::builder(&conn)
            .destination(DESTINATION)?
            .path(comp_path)?
            .interface("org.shadowblip.Input.CompositeDevice")?
            .build()
            .await?;
        tracing::warn!("  - name: {}", comp.name().await?);

        let intercept = comp.intercept_mode().await?;
        tracing::warn!("  - intercept mode: {}", intercept);
        // Interecept mode 2 - all
        if intercept != 2 {
            comp.set_intercept_mode(2).await?;
        }

        for dbus_path in comp.dbus_devices().await? {
            tracing::warn!("  - dbus device: {}", dbus_path);
            let dbus = DBusDeviceProxy::builder(&conn)
                .destination(DESTINATION)?
                .path(dbus_path)?
                .interface("org.shadowblip.Input.DBusDevice")?
                .build()
                .await?;
            let stream = dbus
                .receive_input_event()
                .await?
                .map(|event| Event::Input(event));
            select_all.push(Box::new(stream));
        }
    }

    while let Some(event) = select_all.next().await {
        match event {
            Event::GamepadOrderChanged => {
                tracing::warn!("gamepad order changed");
                //TODO: reload dbus devices
            }
            Event::Input(input) => {
                tracing::warn!("input {:?}", input.args());
            }
        }
    }

    Ok(())
}
