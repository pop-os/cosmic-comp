// SPDX-License-Identifier: GPL-3.0-only

pub fn bool_var(name: &str) -> Option<bool> {
    let value = std::env::var(name).ok()?.to_lowercase();
    Some(["1", "true", "yes", "y"].contains(&value.as_str()))
}
