// SPDX-License-Identifier: GPL-3.0-only

use anyhow::Result;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    cosmic_comp::run(Default::default())
}
