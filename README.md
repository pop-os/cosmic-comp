# cosmic-comp
Compositor for the COSMIC desktop environment

## Development

This project uses [just](https://github.com/casey/just) as a command runner.

### Available Commands

#### Building
- `just build-debug` - Compile with debug profile
- `just build-release` - Compile with release profile (default)
- `just build-vendored` - Compile release profile with vendored dependencies
    - Requires vendoring first, which can be done with `just vendor`

#### Testing & Development
- `just mock` - Run the full COSMIC shell in a nested winit window. This starts the locally built cosmic-comp, which spawns cosmic-session, which in turn starts the shell components (panel, background, notifications, etc.). Requires cosmic-session and shell components to be installed.
- `just run` - Run with debug logs (`RUST_LOG=cosmic_comp=debug` and `RUST_BACKTRACE=full`)
- `just run <app>` - Run cosmic-comp with a specific app in kiosk mode (e.g., `just run cosmic-term`)

#### Code Quality
- `just check` - Run clippy linter with pedantic warnings

#### Installation
- `just install` - Install binary and configuration files
- `just install-bare-session` - Install with bare session files (wayland session, systemd units)
- `just uninstall` - Remove installed files
- `just uninstall-bare-session` - Remove installed files including bare session files

#### Cleanup
- `just clean` - Run `cargo clean`
- `just clean-dist` - Run `cargo clean` and remove vendored dependencies

#### Vendoring
- `just vendor` - Vendor dependencies locally and create vendor.tar
- `just vendor-extract` - Extract vendored dependencies from vendor.tar
