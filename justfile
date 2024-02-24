name := 'cosmic-comp'
export APPID := 'com.system76.CosmicComp'

rootdir := ''
prefix := '/usr'

base-dir := absolute_path(clean(rootdir / prefix))

export SHARE_DIR := sharedir
export BIN_DIR := base-dir / 'bin'
export LIB_DIR := libdir

bin-src := 'target' / 'release' / name
bin-dst := base-dir / 'bin' / name

libdir := base-dir / 'lib'
sharedir := base-dir / 'share'

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

# Runs a clippy check with JSON message format
check-json: (check '--message-format=json')

# Installs files
install:
    install -Dm0755 {{bin-src}} {{bin-dst}}

install-bare-session: install
    @just data/install-bare-session

# Uninstalls installed files
uninstall:
    rm {{bin-dst}}

uninstall-bare-session: uninstall
    @just data/uninstall-bare-session

# Vendor dependencies locally
vendor:
    mkdir -p .cargo
    cargo vendor --sync Cargo.toml \
        | head -n -1 > .cargo/config
    echo 'directory = "vendor"' >> .cargo/config
    tar pcf vendor.tar vendor
    rm -rf vendor

# Extracts vendored dependencies
vendor-extract:
    #!/usr/bin/env sh
    rm -rf vendor
    tar pxf vendor.tar