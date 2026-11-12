use anyhow::Result;
use futures_util::{Stream, StreamExt, stream::SelectAll};
use inputplumber_zbus::{
    composite_device::CompositeDeviceProxy, debug::DebugProxy, input_manager::InputManagerProxy,
    target::TargetProxy,
};
use packed_struct::{PackedStruct, PrimitiveEnum};
use std::collections::{BTreeMap, BTreeSet};

use self::unified_gamepad::{
    capability::InputCapability,
    reports::{input_capability_report::InputCapabilityReport, input_data_report::InputDataReport},
    value::Value,
};

// From InputPlumber, license GPLv3
//TODO: break into crate that can be reused from upstream
#[allow(unused)]
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
        SelectAll::<Box<dyn Stream<Item = (String, Vec<(InputCapability, Value)>)> + Unpin>>::new();
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

                    /*
                    // Intercept all events
                    if comp.intercept_mode().await? != 2 {
                        comp.set_intercept_mode(2).await?;
                    }
                    */
                    tracing::warn!("  - intercept mode {}", comp.intercept_mode().await?);

                    let mut debug_path = None;
                    for _ in 0..2 {
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
                        } else {
                            break;
                        }
                    }

                    if let Some(debug_path) = debug_path {
                        tracing::warn!("  - debug device: {}", debug_path);
                        let debug_dev = DebugProxy::builder(&conn)
                            .destination(DESTINATION)?
                            .path(debug_path.clone())?
                            .interface("org.shadowblip.Input.Debug")?
                            .build()
                            .await?;
                        //TODO: update on input capability report changes
                        let cap_bytes = debug_dev.input_capability_report().await?;
                        let cap_res = InputCapabilityReport::unpack(&cap_bytes);
                        tracing::warn!("    - input capability: {:X?}", cap_res);
                        if let Ok(cap) = cap_res {
                            let mut state = BTreeMap::new();
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
                                let mut changed = Vec::new();
                                for (capability, value) in cap.get_capabilities().iter().map(|x| x.capability).zip(values.into_iter()) {
                                    let old = state.insert(capability.to_primitive(), value.clone());
                                    if old.as_ref() != Some(&value) {
                                        changed.push((capability, value));
                                    }
                                }
                                changed
                            });
                            {
                                let debug_path = debug_path.clone();
                                select_all.push(Box::new(stream.map(move |changed| (debug_path.clone(), changed))));
                            }
                        }
                    }
                }
            }
            events_opt = select_all.next() => {
                if let Some((debug_path, events)) = events_opt {
                    for (capability, value) in events {
                        // Ignore axes for now, it causes too much noise
                        if matches!(capability, InputCapability::GamepadAxisLeftStick | InputCapability::GamepadAxisRightStick) {
                            continue;
                        }
                        let str = match value {
                            Value::None => "none".to_string(),
                            Value::Bool(v) => v.value.to_string(),
                            Value::UInt8(v) => v.value.to_string(),
                            Value::UInt16(v) => v.value.to_string(),
                            Value::UInt16Vector2(v) => format!("{}, {}", v.x, v.y),
                            Value::Int16Vector3(v) => format!("{}, {}, {}", v.x, v.y, v.z),
                            Value::Touch(v) => format!("{:?}", v),
                        };
                        eprintln!("{}: {:?}: {}", debug_path.trim_start_matches("/org/shadowblip/InputPlumber/devices/target/debug"), capability, str);
                    }
                }
            }
        }
    }
}
