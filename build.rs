// SPDX-License-Identifier: GPL-3.0-only
use std::process::Command;

fn main() {
    if let Ok(output) = Command::new("git").args(["rev-parse", "HEAD"]).output() {
        let git_hash = String::from_utf8(output.stdout).unwrap();
        println!("cargo:rustc-env=GIT_HASH={}", git_hash);
    }
    if let Ok(output) = Command::new("git").args(["describe", "--abbrev=0"]).output() {
        let mut git_tag = String::from_utf8(output.stdout).unwrap();
        // Tags may be of the form "epoch-1.0.8", but we only want the version number
        if git_tag.len() >= 6 && &git_tag[..6] == "epoch-" {
            git_tag = String::from(&git_tag[6..]);
        }
        println!("cargo:rustc-env=GIT_TAG={}", git_tag);
    }
}
