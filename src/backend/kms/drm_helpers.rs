// SPDX-License-Identifier: GPL-3.0-only

use anyhow::{Context, Result, anyhow};
use libdisplay_info::{edid::DisplayDescriptorTag, info::Info};
use smithay::{
    reexports::drm::control::{
        AtomicCommitFlags, Device as ControlDevice, Mode, ModeFlags, PlaneType, ResourceHandle,
        atomic::AtomicModeReq,
        connector::{self, State as ConnectorState},
        crtc,
        dumbbuffer::DumbBuffer,
        property,
    },
    utils::Transform,
};
use std::{collections::HashMap, ops::Range};

pub fn display_configuration(
    device: &mut impl ControlDevice,
    supports_atomic: bool,
) -> Result<HashMap<connector::Handle, Option<crtc::Handle>>> {
    let res_handles = device.resource_handles()?;
    let connectors = res_handles.connectors();

    let mut map = HashMap::new();
    let mut cleanup = Vec::new();

    // We expect the previous running drm master (likely the login mananger)
    // to leave the drm device in a sensible state.
    // That means, to reduce flickering, we try to keep an established mapping.
    for conn in connectors
        .iter()
        .flat_map(|conn| device.get_connector(*conn, true).ok())
    {
        if let Some(enc) = conn.current_encoder() {
            if let Some(crtc) = device.get_encoder(enc)?.crtc() {
                // If is is connected we found a mapping
                if conn.state() == ConnectorState::Connected {
                    map.insert(conn.handle(), Some(crtc));
                // If not, the user just unplugged something,
                // or the drm master did not cleanup?
                // Well, I guess we cleanup after them.
                } else {
                    cleanup.push(crtc);
                }
            }
        }
    }

    // But just in case we try to match all remaining connectors.
    for conn in connectors
        .iter()
        .flat_map(|conn| device.get_connector(*conn, false).ok())
        .filter(|conn| conn.state() == ConnectorState::Connected)
        .filter(|conn| !map.contains_key(&conn.handle()))
        .collect::<Vec<_>>()
        .iter()
    {
        'outer: for encoder_info in conn
            .encoders()
            .iter()
            .flat_map(|encoder_handle| device.get_encoder(*encoder_handle))
        {
            for crtc in res_handles.filter_crtcs(encoder_info.possible_crtcs()) {
                if !map.values().any(|v| *v == Some(crtc)) {
                    map.insert(conn.handle(), Some(crtc));
                    break 'outer;
                }
            }
        }

        map.entry(conn.handle()).or_insert(None);
    }

    // And then cleanup
    if supports_atomic {
        let mut req = AtomicModeReq::new();
        let plane_handles = device.plane_handles()?;

        for conn in connectors
            .iter()
            .flat_map(|conn| device.get_connector(*conn, false).ok())
            .filter(|conn| {
                if let Some(enc) = conn.current_encoder() {
                    if let Ok(enc) = device.get_encoder(enc) {
                        if let Some(crtc) = enc.crtc() {
                            return cleanup.contains(&crtc);
                        }
                    }
                }
                false
            })
            .map(|info| info.handle())
        {
            let crtc_id = get_prop(device, conn, "CRTC_ID")?;
            req.add_property(conn, crtc_id, property::Value::CRTC(None));
        }

        // We cannot just shortcut and use the legacy api for all cleanups because of this.
        // (Technically a device does not need to be atomic for planes to be used, but nobody does this otherwise.)
        for plane in plane_handles {
            let info = device.get_plane(plane)?;
            if let Some(crtc) = info.crtc() {
                let is_primary = get_property_val(device, plane, "type").map(
                    |(val_type, val)| match val_type.convert_value(val) {
                        property::Value::Enum(Some(val)) => {
                            val.value() == PlaneType::Primary as u64
                        }
                        _ => false,
                    },
                )?;
                if cleanup.contains(&crtc) || !is_primary {
                    let crtc_id = get_prop(device, plane, "CRTC_ID")?;
                    let fb_id = get_prop(device, plane, "FB_ID")?;
                    req.add_property(plane, crtc_id, property::Value::CRTC(None));
                    req.add_property(plane, fb_id, property::Value::Framebuffer(None));
                }
            }
        }

        for crtc in cleanup {
            let mode_id = get_prop(device, crtc, "MODE_ID")?;
            let active = get_prop(device, crtc, "ACTIVE")?;
            req.add_property(crtc, active, property::Value::Boolean(false));
            req.add_property(crtc, mode_id, property::Value::Unknown(0));
        }

        device.atomic_commit(AtomicCommitFlags::ALLOW_MODESET, req)?;
    } else {
        for crtc in res_handles.crtcs() {
            #[allow(deprecated)]
            let _ = device.set_cursor(*crtc, Option::<&DumbBuffer>::None);
        }
        for crtc in cleanup {
            // null commit (necessary to trigger removal on the kernel side with the legacy api.)
            let _ = device.set_crtc(crtc, None, (0, 0), &[], None);
        }
    }

    Ok(map)
}

pub fn interface_name(device: &impl ControlDevice, connector: connector::Handle) -> Result<String> {
    let conn_info = device.get_connector(connector, false)?;

    let other_short_name;
    let interface_short_name = match conn_info.interface() {
        connector::Interface::DVII => "DVI-I",
        connector::Interface::DVID => "DVI-D",
        connector::Interface::DVIA => "DVI-A",
        connector::Interface::SVideo => "S-VIDEO",
        connector::Interface::DisplayPort => "DP",
        connector::Interface::HDMIA => "HDMI-A",
        connector::Interface::HDMIB => "HDMI-B",
        connector::Interface::EmbeddedDisplayPort => "eDP",
        other => {
            other_short_name = format!("{:?}", other);
            &other_short_name
        }
    };

    Ok(format!(
        "{}-{}",
        interface_short_name,
        conn_info.interface_id()
    ))
}

pub fn edid_info(device: &impl ControlDevice, connector: connector::Handle) -> Result<Info> {
    let edid_prop = get_prop(device, connector, "EDID")?;
    let edid_info = device.get_property(edid_prop)?;

    let mut edid = None;
    let props = device.get_properties(connector)?;
    let (ids, vals) = props.as_props_and_values();
    for (&id, &val) in ids.iter().zip(vals.iter()) {
        if id == edid_prop {
            if let property::Value::Blob(edid_blob) = edid_info.value_type().convert_value(val) {
                let blob = device.get_property_blob(edid_blob)?;
                edid = Some(Info::parse_edid(&blob).context("Unable to parse edid")?);
            }
            break;
        }
    }

    edid.ok_or(anyhow!("No EDID found"))
}

pub fn get_prop(
    device: &impl ControlDevice,
    handle: impl ResourceHandle,
    name: &str,
) -> Result<property::Handle> {
    let props = device.get_properties(handle)?;
    let (prop_handles, _) = props.as_props_and_values();
    for prop in prop_handles {
        let info = device.get_property(*prop)?;
        if Some(name) == info.name().to_str().ok() {
            return Ok(*prop);
        }
    }
    anyhow::bail!("No prop found for {}", name)
}

pub fn get_property_val(
    device: &impl ControlDevice,
    handle: impl ResourceHandle,
    name: &str,
) -> Result<(property::ValueType, property::RawValue)> {
    let props = device.get_properties(handle)?;
    let (prop_handles, values) = props.as_props_and_values();
    for (&prop, &val) in prop_handles.iter().zip(values.iter()) {
        let info = device.get_property(prop)?;
        if Some(name) == info.name().to_str().ok() {
            let val_type = info.value_type();
            return Ok((val_type, val));
        }
    }
    anyhow::bail!("No prop found for {}", name)
}

// Returns refresh rate in milliherz
pub fn calculate_refresh_rate(mode: Mode) -> u32 {
    let htotal = mode.hsync().2 as u32;
    let vtotal = mode.vsync().2 as u32;
    let mut refresh =
        (mode.clock() as u64 * 1000000_u64 / htotal as u64 + vtotal as u64 / 2) / vtotal as u64;

    if mode.flags().contains(ModeFlags::INTERLACE) {
        refresh *= 2;
    }
    if mode.flags().contains(ModeFlags::DBLSCAN) {
        refresh /= 2;
    }
    if mode.vscan() > 1 {
        refresh /= mode.vscan() as u64;
    }

    refresh as u32
}

pub fn get_minimum_refresh_rate(
    device: &impl ControlDevice,
    connector: connector::Handle,
) -> Result<Option<u32>> {
    let info = edid_info(device, connector)?;
    let edid = info.edid().context("EDID lacking into")?;
    for descriptor in edid.display_descriptors() {
        if descriptor.tag() == DisplayDescriptorTag::RangeLimits {
            return Ok(Some(
                descriptor
                    .range_limits()
                    .context("Invalid range limits descriptor")?
                    .min_vert_rate_hz as u32,
            ));
        }
    }

    Ok(None)
}

pub fn get_max_bpc(
    dev: &impl ControlDevice,
    conn: connector::Handle,
) -> Result<Option<(u32, Range<u32>)>> {
    let Some(handle) = get_prop(dev, conn, "max bpc").ok() else {
        return Ok(None);
    };

    let info = dev.get_property(handle)?;
    let range = match info.value_type() {
        property::ValueType::UnsignedRange(x, y) => (x as u32)..(y as u32),
        _ => return Err(anyhow!("max bpc has wrong value type")),
    };

    let value = get_property_val(dev, conn, "max bpc").map(|(val_type, val)| {
        match val_type.convert_value(val) {
            property::Value::UnsignedRange(res) => res as u32,
            _ => unreachable!(),
        }
    })?;

    Ok(Some((value, range)))
}

pub fn set_max_bpc(dev: &impl ControlDevice, conn: connector::Handle, bpc: u32) -> Result<u32> {
    let (_, range) =
        get_max_bpc(dev, conn)?.ok_or(anyhow!("max bpc does not exist for connector"))?;
    dev.set_property(
        conn,
        get_prop(dev, conn, "max bpc")?,
        property::Value::UnsignedRange(bpc.clamp(range.start, range.end) as u64).into(),
    )
    .map_err(Into::<anyhow::Error>::into)
    .and_then(|_| get_property_val(dev, conn, "max bpc"))
    .map(|(val_type, val)| match val_type.convert_value(val) {
        property::Value::UnsignedRange(val) => val as u32,
        _ => unreachable!(),
    })
}

pub fn panel_orientation(dev: &impl ControlDevice, conn: connector::Handle) -> Result<Transform> {
    let (val_type, val) = get_property_val(dev, conn, "panel orientation")?;
    match val_type.convert_value(val) {
        property::Value::Enum(Some(val)) => match val.value() {
            // "Normal"
            0 => Ok(Transform::Normal),
            // "Upside Down"
            1 => Ok(Transform::_180),
            // "Left Side Up"
            2 => Ok(Transform::_90),
            // "Right Side Up"
            3 => Ok(Transform::_270),
            _ => Err(anyhow!("panel orientation has invalid value '{:?}'", val)),
        },
        _ => Err(anyhow!("panel orientation has wrong value type")),
    }
}
