// SPDX-License-Identifier: GPL-3.0-only

//! Configurable allow lists for the D-Bus interfaces.
//!
//! Alongside the clients compiled in below, you can grant access to further clients by
//! dropping TOML files into [`ETC_DROP_IN_DIR`] (for administrators) or
//! [`VENDOR_DROP_IN_DIR`] (for packages):

use std::{
    collections::{BTreeMap, HashMap},
    ffi::OsString,
    fs,
    os::unix::fs::MetadataExt,
    path::{Path, PathBuf},
};

use serde::Deserialize;
use tracing::{debug, info, warn};
use zbus::names::WellKnownName;

use crate::libei::{
    DEVICE_TYPE_ALL, DEVICE_TYPE_KEYBOARD, DEVICE_TYPE_POINTER, DEVICE_TYPE_TOUCHSCREEN,
};

const VENDOR_DROP_IN_DIR: &str = "/usr/share/cosmic-comp/ei-clients.d";
const ETC_DROP_IN_DIR: &str = "/etc/cosmic-comp/ei-clients.d";

static BUILT_IN_EI_CLIENTS: &[WellKnownName] = &[
    WellKnownName::from_static_str_unchecked("org.freedesktop.impl.portal.desktop.cosmic"),
    WellKnownName::from_static_str_unchecked("com.system76.CosmicOSK"),
];

static BUILT_IN_A11Y_CLIENTS: &[WellKnownName] = &[WellKnownName::from_static_str_unchecked(
    "org.gnome.Orca.KeyboardMonitor",
)];

/// Every allow list, parsed in a single pass over the drop-in files.
#[derive(Debug)]
pub struct ClientAllowLists {
    /// Clients permitted on `com.system76.CosmicComp.Ei`.
    pub ei: EiAllowList,
    /// Clients permitted on `org.freedesktop.a11y.KeyboardMonitor`.
    pub a11y_keyboard_monitor: NameAllowList,
}

impl ClientAllowLists {
    /// Built-in clients plus every valid entry found in the drop-in directories.
    ///
    /// Never fails. The loader logs and skips unreadable, untrusted or malformed files, so
    /// the built-in clients keep working whatever is on disk.
    pub fn load() -> Self {
        Self::load_from(&[Path::new(VENDOR_DROP_IN_DIR), Path::new(ETC_DROP_IN_DIR)])
    }

    fn load_from(dirs: &[&Path]) -> Self {
        let mut ei_device_types: HashMap<WellKnownName<'static>, u32> = BUILT_IN_EI_CLIENTS
            .iter()
            .map(|name| (name.to_owned(), DEVICE_TYPE_ALL))
            .collect();
        let mut a11y_names: Vec<WellKnownName<'static>> = BUILT_IN_A11Y_CLIENTS
            .iter()
            .map(|name| name.to_owned())
            .collect();

        for path in drop_in_files(dirs) {
            let Some(config) = parse_file(&path) else {
                continue;
            };
            for (name, mask) in ei_clients(config.ei, &path) {
                // A name granted by several files gets the union of what they permit,
                // since each file is an independent grant.
                *ei_device_types.entry(name).or_insert(0) |= mask;
            }
            for name in a11y_clients(config.a11y_keyboard_monitor, &path) {
                if !a11y_names.contains(&name) {
                    a11y_names.push(name);
                }
            }
        }

        let mut ei_names: Vec<_> = ei_device_types.keys().cloned().collect();
        ei_names.sort();
        a11y_names.sort();
        debug!(names = ?ei_names, "Loaded EI client allow list");
        debug!(names = ?a11y_names, "Loaded a11y keyboard monitor allow list");

        Self {
            ei: EiAllowList {
                names: ei_names,
                device_types: ei_device_types,
            },
            a11y_keyboard_monitor: NameAllowList { names: a11y_names },
        }
    }
}

/// The set of bus names allowed to open an EI sender socket, and what each may request.
#[derive(Debug)]
pub struct EiAllowList {
    /// Flattened bus names, in the form [`super::name_owners::NameOwners`] wants them.
    names: Vec<WellKnownName<'static>>,
    /// Permitted device-type mask per bus name.
    device_types: HashMap<WellKnownName<'static>, u32>,
}

impl EiAllowList {
    /// Every allowed bus name, for [`super::name_owners::NameOwners::check_owner`].
    pub fn names(&self) -> &[WellKnownName<'static>] {
        &self.names
    }

    /// The device types `name` may request, or `None` if it isn't on the allow list.
    pub fn device_types_for(&self, name: &WellKnownName<'_>) -> Option<u32> {
        self.device_types.get(name).copied()
    }
}

/// A plain list of allowed bus names, for interfaces with no per-client policy.
#[derive(Clone, Debug)]
pub struct NameAllowList {
    names: Vec<WellKnownName<'static>>,
}

impl NameAllowList {
    /// Every allowed bus name, for [`super::name_owners::NameOwners::check_owner`].
    pub fn names(&self) -> &[WellKnownName<'static>] {
        &self.names
    }
}

fn drop_in_files(dirs: &[&Path]) -> Vec<PathBuf> {
    let mut files: BTreeMap<OsString, PathBuf> = BTreeMap::new();
    for dir in dirs {
        match dir.metadata() {
            Ok(metadata) if !is_trusted(dir, &metadata) => continue,
            Ok(_) => {}
            // A missing drop-in directory is the normal case
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to stat client config directory {}",
                    dir.display()
                );
                continue;
            }
        }
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(err) => {
                warn!(
                    ?err,
                    "Failed to read client config directory {}",
                    dir.display()
                );
                continue;
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(entry) => entry,
                Err(err) => {
                    warn!(?err, "Failed to read entry in {}", dir.display());
                    continue;
                }
            };
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "toml") {
                files.insert(entry.file_name(), path);
            }
        }
    }
    files.into_values().collect()
}

fn parse_file(path: &Path) -> Option<ConfigFile> {
    let metadata = match path.metadata() {
        Ok(metadata) => metadata,
        Err(err) => {
            warn!(?err, "Failed to stat {}", path.display());
            return None;
        }
    };
    if !metadata.is_file() || !is_trusted(path, &metadata) {
        return None;
    }

    let contents = match fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) => {
            warn!(?err, "Failed to read {}", path.display());
            return None;
        }
    };
    match toml::from_str(&contents) {
        Ok(config) => Some(config),
        Err(err) => {
            warn!(?err, "Failed to parse {}", path.display());
            None
        }
    }
}

/// Validated EI entries from one file's `[[ei.client]]` tables.
fn ei_clients(section: Section, path: &Path) -> Vec<(WellKnownName<'static>, u32)> {
    section
        .client
        .into_iter()
        .filter_map(|client| {
            let name = parse_bus_name(&client.bus_name, path)?;
            let mask = match &client.device_types {
                Some(types) => device_type_mask(types, path),
                None => DEVICE_TYPE_ALL,
            };
            if mask == 0 {
                warn!(
                    "EI client {} in {} permits no device types and will be denied access",
                    name,
                    path.display()
                );
            }
            info!(
                "Allowing EI client {} (device types {:#b}) from {}",
                name,
                mask,
                path.display()
            );
            Some((name, mask))
        })
        .collect()
}

fn a11y_clients(section: Section, path: &Path) -> Vec<WellKnownName<'static>> {
    section
        .client
        .into_iter()
        .filter_map(|client| {
            let name = parse_bus_name(&client.bus_name, path)?;
            if client.device_types.is_some() {
                warn!(
                    "Ignoring `device_types` on keyboard monitor client {} in {}: the \
                     interface carries no per-device policy",
                    name,
                    path.display()
                );
            }
            info!(
                "Allowing a11y keyboard monitor client {} from {}",
                name,
                path.display()
            );
            Some(name)
        })
        .collect()
}

fn parse_bus_name(raw: &str, path: &Path) -> Option<WellKnownName<'static>> {
    match WellKnownName::try_from(raw.to_owned()) {
        Ok(name) => Some(name),
        Err(err) => {
            warn!(
                ?err,
                "Ignoring invalid bus name {:?} in {}",
                raw,
                path.display()
            );
            None
        }
    }
}

fn device_type_mask(types: &[String], path: &Path) -> u32 {
    let mut mask = 0;
    for device_type in types {
        match device_type.as_str() {
            "keyboard" => mask |= DEVICE_TYPE_KEYBOARD,
            "pointer" => mask |= DEVICE_TYPE_POINTER,
            "touchscreen" => mask |= DEVICE_TYPE_TOUCHSCREEN,
            other => warn!(
                "Ignoring unknown device type {:?} in {}",
                other,
                path.display()
            ),
        }
    }
    mask
}

fn is_trusted(path: &Path, metadata: &fs::Metadata) -> bool {
    if metadata.mode() & 0o022 != 0 {
        warn!(
            "Ignoring client config path {}: it is group- or world-writable",
            path.display()
        );
        return false;
    }
    let uid = metadata.uid();
    if uid != 0 && uid != rustix::process::geteuid().as_raw() {
        warn!(
            "Ignoring client config path {}: it is owned by uid {}",
            path.display(),
            uid
        );
        return false;
    }
    true
}

#[derive(Debug, Default, Deserialize)]
struct ConfigFile {
    #[serde(default)]
    ei: Section,
    #[serde(default)]
    a11y_keyboard_monitor: Section,
}

#[derive(Debug, Default, Deserialize)]
struct Section {
    #[serde(default)]
    client: Vec<ClientEntry>,
}

#[derive(Debug, Deserialize)]
struct ClientEntry {
    bus_name: String,
    /// Only meaningful under `[ei]`
    device_types: Option<Vec<String>>,
}
