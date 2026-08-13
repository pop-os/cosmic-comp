// SPDX-License-Identifier: GPL-3.0-only

//! XDG base-directory resolution for installed data files.
//!
//! The compositor is not always installed under `/usr`: on Nix-style systems
//! every package lives in its own prefix and is reached through
//! `XDG_DATA_DIRS`, or relative to the running executable. Hardcoding
//! `/usr/share/...` makes those lookups silently miss.
//!
//! Nothing is lost on FHS systems: the spec default for `XDG_DATA_DIRS` is
//! `/usr/local/share:/usr/share`, so the previously hardcoded directory is
//! still searched -- it is just no longer the only candidate.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};

/// Spec default for `XDG_DATA_DIRS`, used when it is unset or empty.
const DEFAULT_DATA_DIRS: &str = "/usr/local/share:/usr/share";

/// The prefix `exe` was installed into: `<prefix>/bin/foo` gives `<prefix>`.
///
/// `None` when the executable does not sit in a `bin` directory, which is the
/// case for a cargo target directory -- there is no install tree to search.
fn prefix_of_exe(exe: &Path) -> Option<PathBuf> {
    let bin = exe.parent()?;
    (bin.file_name()? == "bin")
        .then(|| bin.parent())?
        .map(Path::to_path_buf)
}

/// The prefix the running executable was installed into.
pub fn install_prefix() -> Option<PathBuf> {
    prefix_of_exe(&std::env::current_exe().ok()?)
}

/// Directories to search for the data sub-path `rel` (e.g. `"pixmaps"`),
/// most specific first and deduplicated.
///
/// Order is the XDG one -- per-user data home, then `XDG_DATA_DIRS` -- with the
/// running executable's own install prefix appended as a final fallback, which
/// is what lets a self-contained prefix resolve with no environment set at all.
pub fn data_dirs(rel: &str) -> Vec<PathBuf> {
    let data_home = std::env::var_os("XDG_DATA_HOME");
    let home = std::env::var_os("HOME");
    let dirs = std::env::var_os("XDG_DATA_DIRS");
    data_dirs_from(
        rel,
        data_home.as_deref(),
        home.as_deref(),
        dirs.as_deref(),
        install_prefix().as_deref(),
    )
}

/// [`data_dirs`] with the environment injected, so the resolution order can be
/// tested without mutating process-global state.
fn data_dirs_from(
    rel: &str,
    data_home: Option<&OsStr>,
    home: Option<&OsStr>,
    data_dirs: Option<&OsStr>,
    install_prefix: Option<&Path>,
) -> Vec<PathBuf> {
    let mut bases: Vec<PathBuf> = Vec::new();

    // Per-user data home: XDG_DATA_HOME, else the spec default ~/.local/share.
    match data_home.filter(|d| !d.is_empty()) {
        Some(dir) => bases.push(PathBuf::from(dir)),
        None => {
            if let Some(home) = home.filter(|h| !h.is_empty()) {
                bases.push(Path::new(home).join(".local/share"));
            }
        }
    }

    // System data dirs: XDG_DATA_DIRS, else the spec default, which is the FHS
    // layout that used to be hardcoded here.
    let system = data_dirs.filter(|d| !d.is_empty());
    bases.extend(std::env::split_paths(
        system.unwrap_or(OsStr::new(DEFAULT_DATA_DIRS)),
    ));

    // Our own install prefix, so `<prefix>/bin/cosmic-comp` finds
    // `<prefix>/share/...` even with no XDG variables at all.
    if let Some(prefix) = install_prefix {
        bases.push(prefix.join("share"));
    }

    let mut out: Vec<PathBuf> = Vec::new();
    for base in bases {
        if base.as_os_str().is_empty() {
            continue;
        }
        let candidate = base.join(rel);
        if !out.contains(&candidate) {
            out.push(candidate);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dirs(paths: &[&str]) -> Vec<PathBuf> {
        paths.iter().map(PathBuf::from).collect()
    }

    #[test]
    fn prefix_of_exe_finds_install_root() {
        assert_eq!(
            prefix_of_exe(Path::new("/usr/bin/cosmic-comp")),
            Some(PathBuf::from("/usr"))
        );
        assert_eq!(
            prefix_of_exe(Path::new(
                "/nix/store/abc123-cosmic-comp-1.0/bin/cosmic-comp"
            )),
            Some(PathBuf::from("/nix/store/abc123-cosmic-comp-1.0"))
        );
    }

    #[test]
    fn prefix_of_exe_rejects_non_bin_parent() {
        // A cargo target directory has no install tree to resolve against.
        assert_eq!(
            prefix_of_exe(Path::new("/home/u/cosmic-comp/target/debug/cosmic-comp")),
            None
        );
    }

    #[test]
    fn fhs_layout_still_searches_usr_share() {
        // Fedora: no XDG_DATA_* set at all, binary in /usr/bin.
        let found = data_dirs_from(
            "icetron/current-theme",
            None,
            Some(OsStr::new("/home/u")),
            None,
            Some(Path::new("/usr")),
        );
        assert_eq!(
            found,
            dirs(&[
                "/home/u/.local/share/icetron/current-theme",
                "/usr/local/share/icetron/current-theme",
                "/usr/share/icetron/current-theme",
            ])
        );
    }

    #[test]
    fn nix_layout_resolves_through_data_dirs_and_prefix() {
        // NixOS: nothing under /usr exists; the answer comes from XDG_DATA_DIRS
        // and from the compositor's own store prefix.
        let found = data_dirs_from(
            "icetron/current-theme",
            None,
            Some(OsStr::new("/home/u")),
            Some(OsStr::new(
                "/home/u/.nix-profile/share:/run/current-system/sw/share",
            )),
            Some(Path::new("/nix/store/abc123-cosmic-comp-1.0")),
        );
        assert_eq!(
            found,
            dirs(&[
                "/home/u/.local/share/icetron/current-theme",
                "/home/u/.nix-profile/share/icetron/current-theme",
                "/run/current-system/sw/share/icetron/current-theme",
                "/nix/store/abc123-cosmic-comp-1.0/share/icetron/current-theme",
            ])
        );
        assert!(!found.iter().any(|p| p.starts_with("/usr")));
    }

    #[test]
    fn data_home_wins_and_duplicates_collapse() {
        let found = data_dirs_from(
            "pixmaps",
            Some(OsStr::new("/home/u/.local/share")),
            Some(OsStr::new("/home/u")),
            Some(OsStr::new("/usr/share:/usr/share")),
            // Same prefix the system dirs already cover.
            Some(Path::new("/usr")),
        );
        assert_eq!(
            found,
            dirs(&["/home/u/.local/share/pixmaps", "/usr/share/pixmaps"])
        );
    }

    #[test]
    fn empty_values_fall_back_to_defaults() {
        let found = data_dirs_from(
            "pixmaps",
            Some(OsStr::new("")),
            Some(OsStr::new("/home/u")),
            Some(OsStr::new("")),
            None,
        );
        assert_eq!(
            found,
            dirs(&[
                "/home/u/.local/share/pixmaps",
                "/usr/local/share/pixmaps",
                "/usr/share/pixmaps",
            ])
        );
    }
}
