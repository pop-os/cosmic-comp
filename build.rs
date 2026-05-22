// SPDX-License-Identifier: GPL-3.0-only
use std::process::Command;

fn main() {
    if let Ok(hash) = std::env::var("SOURCE_GIT_HASH") {
        println!("cargo:rustc-env=GIT_HASH={}", hash);
        return;
    }
    if let Ok(output) = Command::new("git").args(["rev-parse", "HEAD"]).output() {
        let git_hash = String::from_utf8(output.stdout).unwrap();
        println!("cargo:rustc-env=GIT_HASH={}", git_hash);
    }
}
