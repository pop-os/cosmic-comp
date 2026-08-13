use std::ffi::OsStr;
use std::path::PathBuf;
use std::sync::LazyLock;

/// Binary directory searched after `PATH`, so an FHS install keeps resolving
/// when the compositor is started with a stripped environment. On layouts where
/// binaries do not live in `/usr/bin`, `PATH` is what carries the answer.
const FALLBACK_BIN_DIR: &str = "/usr/bin";

static WORKSPACES_INSTALLED: LazyLock<bool> =
    LazyLock::new(|| is_binary_installed("cosmic-workspaces"));

static SETTINGS_BINARY: LazyLock<&'static str> = LazyLock::new(|| {
    if is_binary_installed("agentos-settings") {
        "agentos-settings"
    } else {
        "cosmic-settings"
    }
});

/// Returns whether cosmic-workspaces is installed on the system.
pub fn workspaces_enabled() -> bool {
    *WORKSPACES_INSTALLED
}

/// Returns the system settings binary to launch, preferring `agentos-settings`
/// over `cosmic-settings` when it is installed.
pub fn settings_binary() -> &'static str {
    *SETTINGS_BINARY
}

/// Every existing file named `name` reachable through `PATH`, in `PATH` order.
///
/// Used instead of hardcoding an install location: the same binary has to work
/// where programs live in `/usr/bin` and where each one lives in its own prefix.
pub fn which(name: &str) -> Vec<PathBuf> {
    let path = std::env::var_os("PATH");
    binary_candidates(path.as_deref(), name)
        .into_iter()
        .filter(|candidate| candidate.is_file())
        .collect()
}

/// Locations [`which`] checks for `name`, deduplicated and without touching the
/// filesystem -- split out so the search order can be tested without mutating
/// process-global state.
fn binary_candidates(path: Option<&OsStr>, name: &str) -> Vec<PathBuf> {
    let path = path.filter(|p| !p.is_empty());
    let dirs = std::env::split_paths(path.unwrap_or(OsStr::new("")))
        .chain(std::iter::once(PathBuf::from(FALLBACK_BIN_DIR)));

    let mut out: Vec<PathBuf> = Vec::new();
    for dir in dirs {
        if dir.as_os_str().is_empty() {
            continue;
        }
        let candidate = dir.join(name);
        if !out.contains(&candidate) {
            out.push(candidate);
        }
    }
    out
}

fn is_binary_installed(name: &str) -> bool {
    !which(name).is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn paths(list: &[&str]) -> Vec<PathBuf> {
        list.iter().map(PathBuf::from).collect()
    }

    #[test]
    fn fhs_path_still_reaches_usr_bin() {
        assert_eq!(
            binary_candidates(Some(OsStr::new("/usr/local/bin:/usr/bin")), "playserve"),
            paths(&["/usr/local/bin/playserve", "/usr/bin/playserve"])
        );
    }

    #[test]
    fn nix_path_resolves_store_prefixes() {
        let found = binary_candidates(
            Some(OsStr::new(
                "/home/u/.nix-profile/bin:/run/current-system/sw/bin",
            )),
            "playserve",
        );
        assert_eq!(
            found,
            paths(&[
                "/home/u/.nix-profile/bin/playserve",
                "/run/current-system/sw/bin/playserve",
                // Harmless on Nix (it does not exist) and required on FHS.
                "/usr/bin/playserve",
            ])
        );
    }

    #[test]
    fn missing_path_falls_back_to_usr_bin() {
        assert_eq!(
            binary_candidates(None, "playserve"),
            paths(&["/usr/bin/playserve"])
        );
        assert_eq!(
            binary_candidates(Some(OsStr::new("")), "playserve"),
            paths(&["/usr/bin/playserve"])
        );
    }

    #[test]
    fn duplicate_path_entries_collapse() {
        assert_eq!(
            binary_candidates(Some(OsStr::new("/usr/bin::/usr/bin")), "playserve"),
            paths(&["/usr/bin/playserve"])
        );
    }
}
