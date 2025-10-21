// SPDX-License-Identifier: GPL-3.0-only

fn main() {
    if let Err(err) = cosmic_comp::run(Default::default()) {
        tracing::error!("Error occured in main(): {}", err);
        std::process::exit(1);
    }
}
