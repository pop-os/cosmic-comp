// SPDX-License-Identifier: GPL-3.0-only

use smithay::backend::drm::{DrmNode, NodeType};
use tracing::{info, warn};

pub fn bool_var(name: &str) -> Option<bool> {
    let value = std::env::var(name).ok()?.to_lowercase();
    Some(["1", "true", "yes", "y"].contains(&value.as_str()))
}

#[derive(Debug, Clone, Copy)]
pub enum DeviceIdentifier {
    Id { vendor: u32, device: u32 },
    Node(DrmNode),
}

impl DeviceIdentifier {
    pub fn matches(&self, dev_node: &DrmNode) -> bool {
        match self {
            DeviceIdentifier::Node(id_node) => id_node == dev_node,
            DeviceIdentifier::Id { vendor, device } => {
                let (major, minor) = (dev_node.major(), dev_node.minor());
                let Some(dev_vendor) = std::fs::read_to_string(format!(
                    "/sys/dev/char/{}:{}/device/vendor",
                    major, minor
                ))
                .ok()
                .and_then(|ven| u32::from_str_radix(ven[2..].trim(), 16).ok()) else {
                    return false;
                };
                let Some(dev_device) = std::fs::read_to_string(format!(
                    "/sys/dev/char/{}:{}/device/device",
                    major, minor
                ))
                .ok()
                .and_then(|dev| u32::from_str_radix(dev[2..].trim(), 16).ok()) else {
                    return false;
                };
                info!(
                    "{:x}:{:x} == {:x}:{:x}",
                    *vendor, *device, dev_vendor, dev_device
                );
                dev_vendor == *vendor && dev_device == *device
            }
        }
    }
}

pub fn dev_var(name: &str) -> Option<DeviceIdentifier> {
    let value = std::env::var(name).ok()?;
    try_parse_dev_from_str(&value)
}

pub fn dev_list_var(name: &str) -> Option<Vec<DeviceIdentifier>> {
    let value = std::env::var(name).ok()?;
    Some(value.split(',').flat_map(try_parse_dev_from_str).collect())
}

fn try_parse_dev_from_str(val: &str) -> Option<DeviceIdentifier> {
    let val = val.trim();
    if val.starts_with("0x") && val.contains(':') {
        let (vendor, device) = val.split_once(':').unwrap();
        if !device.starts_with("0x") {
            warn!(
                "Failed to parse device entry {}, device id doesn't start with '0x'. Skipping",
                val
            );
            return None;
        }
        let vendor = u32::from_str_radix(&vendor[2..], 16)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, vendor_id is no hex integer: {}. Skipping",
                    val, err
                );
            })
            .ok()?;
        let device = u32::from_str_radix(&device[2..], 16)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, device_id is no hex integer: {}. Skipping",
                    val, err
                );
            })
            .ok()?;
        Some(DeviceIdentifier::Id { vendor, device })
    } else if val.starts_with("pci-") {
        let path = std::fs::read_link(format!("/dev/dri/by-path/{}-render", val))
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, no known pci path: {}. Skipping",
                    val, err
                );
            })
            .ok()?;
        let node = DrmNode::from_path(&path)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, failed to get node from path {}: {}",
                    val,
                    path.display(),
                    err
                )
            })
            .ok()?;

        let node = node
            .node_with_type(NodeType::Render)
            .map(|res| res.ok())
            .flatten()
            .unwrap_or(node);
        Some(DeviceIdentifier::Node(node))
    } else if val.contains(':') {
        let (major, minor) = val.split_once(':').unwrap();
        let major = str::parse::<u32>(major)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, major is no integer: {}. Skipping",
                    val, err
                )
            })
            .ok()?;
        let minor = str::parse::<u32>(minor)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, minor is no integer: {}. Skipping",
                    val, err
                )
            })
            .ok()?;
        let dev = rustix::fs::makedev(major, minor);
        let node = DrmNode::from_dev_id(dev)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, failed to get node from dev_t {}: {}",
                    val, dev, err
                );
            })
            .ok()?;

        let node = node
            .node_with_type(NodeType::Render)
            .map(|res| res.ok())
            .flatten()
            .unwrap_or(node);
        Some(DeviceIdentifier::Node(node))
    } else {
        // try to parse as device path

        let path = format!("/dev/dri/{}", val);
        let node = DrmNode::from_path(&path)
            .inspect_err(|err| {
                warn!(
                    "Failed to parse device entry {}, failed to get node from path {}: {}",
                    val, path, err
                );
            })
            .ok()?;

        let node = node
            .node_with_type(NodeType::Render)
            .map(|res| res.ok())
            .flatten()
            .unwrap_or(node);
        Some(DeviceIdentifier::Node(node))
    }
}
