use anyhow::Result;
use futures_util::{Stream, StreamExt, stream::SelectAll};
use inputplumber_zbus::{
    composite_device::CompositeDeviceProxy,
    dbus_device::{DBusDeviceProxy, InputEvent},
    debug::{DebugProxy, InputReport},
    input_manager::InputManagerProxy,
    target::TargetProxy,
};
use packed_struct::PackedStruct;
use std::collections::BTreeSet;

use self::unified_gamepad::{
    capability::InputCapability,
    reports::{input_capability_report::InputCapabilityReport, input_data_report::InputDataReport},
    value::Value,
};

// From InputPlumber, license GPLv3
//TODO: break into crate that can be reused from upstream
mod unified_gamepad;

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

    let mut gamepad_stream = manager.receive_gamepad_order_changed().await.fuse();
    let mut select_all =
        SelectAll::<Box<dyn Stream<Item = Vec<(InputCapability, Value)>> + Unpin>>::new();
    loop {
        futures::select! {
            _ = gamepad_stream.next() => {
                tracing::warn!("gamepad order changed");

                select_all.clear();
                for comp_path in manager.gamepad_order().await? {
                    tracing::warn!("- composite device: {}", comp_path);
                    let comp = CompositeDeviceProxy::builder(&conn)
                        .destination(DESTINATION)?
                        .path(comp_path)?
                        .interface("org.shadowblip.Input.CompositeDevice")?
                        .build()
                        .await?;
                    tracing::warn!("  - name: {}", comp.name().await?);

                    //TODO: limit number of tries?
                    let mut debug_path = None;
                    while debug_path.is_none() {
                        let mut target_device_types = BTreeSet::new();
                        for target_path in comp.target_devices().await? {
                            tracing::warn!("  - target device: {}", target_path);
                            let target = TargetProxy::builder(&conn)
                                .destination(DESTINATION)?
                                .path(target_path.clone())?
                                .interface("org.shadowblip.Input.Target")?
                                .build()
                                .await?;
                            tracing::warn!("    - name: {}", target.name().await?);
                            let device_type = target.device_type().await?;
                            tracing::warn!("    - type: {}", device_type);
                            if device_type == "debug" {
                                debug_path = Some(target_path);
                            }
                            target_device_types.insert(device_type);
                        }
                        if debug_path.is_none() {
                            target_device_types.insert("debug".to_string());
                            let target_device_types_str: Vec<&str> = target_device_types.iter().map(|x| x.as_str()).collect();
                            comp.set_target_devices(&target_device_types_str).await?;
                        }
                    }

                    if let Some(debug_path) = debug_path {
                        tracing::warn!("  - debug device: {}", debug_path);
                        let debug_dev = DebugProxy::builder(&conn)
                            .destination(DESTINATION)?
                            .path(debug_path)?
                            .interface("org.shadowblip.Input.Debug")?
                            .build()
                            .await?;
                        //TODO: update on input capability report changes
                        let cap_bytes = debug_dev.input_capability_report().await?;
                        let cap_res = InputCapabilityReport::unpack(&cap_bytes);
                        tracing::warn!("    - input capability: {:X?}", cap_res);
                        if let Ok(cap) = cap_res {
                            let stream = debug_dev.receive_input_report().await?.map(move |x| {
                                let data_bytes = match x.message().body().deserialize::<Vec<u8>>() {
                                    Ok(ok) => ok,
                                    Err(err) => {
                                        tracing::error!("failed to deserialize input data report: {}", err);
                                        return Vec::new();
                                    }
                                };
                                let data_slice = match data_bytes.as_slice().try_into() {
                                    Ok(ok) => ok,
                                    Err(err) => {
                                        tracing::error!("failed to convert input data report to slice: {}", err);
                                        return Vec::new();
                                    }
                                };
                                let data = match InputDataReport::unpack(data_slice) {
                                    Ok(ok) => ok,
                                    Err(err) => {
                                        tracing::error!("failed to unpack input data report: {}", err);
                                        return Vec::new();
                                    }
                                };
                                let values = match cap.decode_data_report(&data) {
                                    Ok(ok) => ok,
                                    Err(err) => {
                                        tracing::error!("failed to decode input data report: {}", err);
                                        return Vec::new();
                                    }
                                };
                                cap.get_capabilities().iter().map(|x| x.capability).zip(values.into_iter()).collect()
                            });
                            select_all.push(Box::new(stream));
                        }
                    }

                    for dbus_path in comp.dbus_devices().await? {
                        tracing::warn!("  - dbus device: {}", dbus_path);
                        let dbus = DBusDeviceProxy::builder(&conn)
                            .destination(DESTINATION)?
                            .path(dbus_path)?
                            .interface("org.shadowblip.Input.DBusDevice")?
                            .build()
                            .await?;
                        /*TODO: testing debug device
                        let stream = dbus.receive_input_event().await?;
                        select_all.push(Box::new(stream));
                        */
                    }
                }
            }
            input_opt = select_all.next() => {
                if let Some(input) = input_opt {
                    tracing::warn!("input {:#?}", input);
                }
            }
        }
    }
}
