name := 'cosmic-comp'

rootdir := ''
prefix := '/usr'
sysconfdir := '/etc'

base-dir := absolute_path(clean(rootdir / prefix))
sysconfdir-dir := absolute_path(clean(rootdir / sysconfdir))

bin-dir := base-dir / 'bin'
lib-dir := base-dir / 'lib'
share-dir := base-dir / 'share'

cargo-target-dir := env('CARGO_TARGET_DIR', 'target')
bin-src := cargo-target-dir / 'release' / name
bin-dst := bin-dir / name

keybindings-src := 'data' / 'keybindings.ron'
keybindings-dst := share-dir / 'cosmic' / 'com.system76.CosmicSettings.Shortcuts' / 'v1' / 'defaults'

tiling-exceptions-src := 'data' / 'tiling-exceptions.ron'
tiling-exceptions-dst := share-dir / 'cosmic' / 'com.system76.CosmicSettings.WindowRules' / 'v1' / 'tiling_exception_defaults'

session-desktop-src := 'data' / 'cosmic.desktop'
session-desktop-dst := share-dir / 'wayland-sessions' / 'cosmic.desktop'

systemd-session-target-src := 'data' / 'cosmic-session.target'
systemd-session-target-dst := lib-dir / 'systemd' / 'user' / 'cosmic-session.target'

systemd-session-pre-target-src := 'data' / 'cosmic-session-pre.target'
systemd-session-pre-target-dst := lib-dir / 'systemd' / 'user' / 'cosmic-session-pre.target'

systemd-comp-service-src := 'data' / 'cosmic-comp.service'
systemd-comp-service-dst := lib-dir / 'systemd' / 'user' / 'cosmic-comp.service'

cosmic-service-src := 'data' / 'cosmic-service'
cosmic-service-dst := bin-dir / 'cosmic-service'

# Default recipe which runs `just build-release`
default: build-release

# Runs `cargo clean`
clean:
    cargo clean

# `cargo clean` and removes vendored dependencies
clean-dist: clean
    rm -rf .cargo vendor vendor.tar

# Compiles with debug profile
build-debug *args:
    cargo build {{args}}

# Compiles with release profile
build-release *args: (build-debug '--release' args)

# Compiles release profile with vendored dependencies
build-vendored *args: vendor-extract (build-release '--frozen --offline' args)

# Runs a clippy check
check *args:
    cargo clippy --all-features {{args}} -- -W clippy::pedantic

# Run the full COSMIC shell in a nested window (requires cosmic-session and shell components installed)
mock:
    cargo build --release
    {{cargo-target-dir}}/release/cosmic-comp cosmic-session {{cargo-target-dir}}/release/cosmic-comp

# Run with debug logs
run *args:
    env RUST_LOG=cosmic_comp=debug RUST_BACKTRACE=full cargo run --release {{args}}

# Installs files
install:
    install -Dm0755 {{bin-src}} {{bin-dst}}
    install -Dm0644 {{keybindings-src}} {{keybindings-dst}}
    install -Dm0644 {{tiling-exceptions-src}} {{tiling-exceptions-dst}}

# Installs files including bare session files
install-bare-session: install
    install -Dm0644 {{session-desktop-src}} {{session-desktop-dst}}
    install -Dm0644 {{systemd-session-target-src}} {{systemd-session-target-dst}}
    install -Dm0644 {{systemd-session-pre-target-src}} {{systemd-session-pre-target-dst}}
    install -Dm0644 {{systemd-comp-service-src}} {{systemd-comp-service-dst}}
    install -Dm0755 {{cosmic-service-src}} {{cosmic-service-dst}}

# Uninstalls installed files
uninstall:
    rm {{bin-dst}} {{keybindings-dst}} {{tiling-exceptions-dst}}

# Uninstalls installed files including bare session files
uninstall-bare-session: uninstall
    rm {{session-desktop-dst}} {{systemd-session-target-dst}} {{systemd-session-pre-target-dst}} {{systemd-comp-service-dst}} {{cosmic-service-dst}}

# Vendor dependencies locally
vendor:
    #!/usr/bin/env sh
    mkdir -p .cargo
    cargo vendor --sync Cargo.toml | head -n -1 > .cargo/config.toml
    echo 'directory = "vendor"' >> .cargo/config.toml
    echo >> .cargo/config.toml
    echo '[env]' >> .cargo/config.toml
    if [ -n "${SOURCE_DATE_EPOCH}" ]
    then
        source_date="$(date -d "@${SOURCE_DATE_EPOCH}" "+%Y-%m-%d")"
        echo "VERGEN_GIT_COMMIT_DATE = \"${source_date}\"" >> .cargo/config.toml
    fi
    if [ -n "${SOURCE_GIT_HASH}" ]
    then
        echo "VERGEN_GIT_SHA = \"${SOURCE_GIT_HASH}\"" >> .cargo/config.toml
    fi
    tar pcf vendor.tar .cargo vendor
    rm -rf .cargo vendor

# Extracts vendored dependencies
vendor-extract:
    rm -rf vendor
    tar pxf vendor.tar
