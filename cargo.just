cargo-target-dir := env('CARGO_TARGET_DIR', 'target')

# Compile with debug profile
build-debug *args:
    cargo build {{ args }}

# Compile with release profile
build-release *args: (build-debug '--release' args)

# Compile with a vendored tarball
build-vendored *args: vendor-extract (build-release '--frozen --offline' args)

# Check for errors and linter warnings
check *args:
    cargo clippy --all-features {{ args }} -- -W clippy::pedantic

# Remove Cargo build artifacts
[no-cd]
clean:
    cargo clean

# Also remove .cargo and vendored dependencies
[no-cd]
clean-dist: clean
    rm -rf .cargo vendor vendor.tar

# Vendor Cargo dependencies locally
[no-cd]
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
        echo "GIT_HASH = \"${SOURCE_GIT_HASH}\"" >> .cargo/config.toml
    fi
    tar pcf vendor.tar .cargo vendor
    rm -rf .cargo vendor

# Extracts vendored dependencies
[no-cd]
[private]
vendor-extract:
    rm -rf vendor
    tar pxf vendor.tar
