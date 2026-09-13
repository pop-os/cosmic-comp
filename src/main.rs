// SPDX-License-Identifier: GPL-3.0-only

fn main() {
    // reis exposes an opt-in raw EIS wire dump through REIS_DEBUG. This
    // process handles physical keyboard input, so never allow an inherited
    // environment variable to enable that dump. main is still single-
    // threaded here, which satisfies remove_var's safety requirement.
    unsafe {
        std::env::remove_var("REIS_DEBUG");
    }

    if let Err(err) = cosmic_comp::run(Default::default()) {
        tracing::error!("Error occured in main(): {}", err);
        std::process::exit(1);
    }
}
