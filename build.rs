// SPDX-License-Identifier: GPL-3.0-only
use vergen::EmitBuilder;

fn main() {
    EmitBuilder::builder().git_sha(true).emit().unwrap();
}

