rootdir := ''
prefix := rootdir + '/usr'
clean := '0'
debug := '0'
vendor := '0'
cargo-target-dir := env('CARGO_TARGET_DIR', 'target')
target := if debug == '1' { 'debug' } else { 'release' }
vendor_args := if vendor == '1' { '--frozen --offline' } else { '' }
debug_args := if debug == '1' { '' } else { '--release' }
cargo_args := vendor_args + ' ' + debug_args

bindir := prefix + '/bin'
libdir := prefix + '/lib'
sharedir := prefix + '/share'

all: _extract_vendor build

build:
        cargo build {{cargo_args}}

# Installs files into the system
install:
	install -Dm0755 {{cargo-target-dir}}/release/cosmic-comp {{bindir}}/cosmic-comp
	install -Dm0644 "data/keybindings.ron" {{sharedir}}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults

install-bare-session: install
	install -Dm0644 "data/cosmic.desktop" {{sharedir}}/wayland-sessions/cosmic.desktop
	install -Dm0644 "data/cosmic-session.target" {{libdir}}/systemd/user/cosmic-session.target
	install -Dm0644 "data/cosmic-session-pre.target" {{libdir}}/systemd/user/cosmic-session-pre.target
	install -Dm0644 "data/cosmic-comp.service" {{libdir}}/systemd/user/cosmic-comp.service
	install -Dm0755 "data/cosmic-service" {{bindir}}/cosmic-service

uninstall:
        rm {{bindir}}/cosmic-comp
        rm {{sharedir}}/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults


uninstall-bare-session:
	rm {{sharedir}}/wayland-sessions/cosmic.desktop"

clean_vendor:
	rm -rf vendor vendor.tar .cargo/config

clean: clean_vendor
	cargo clean

# Extracts vendored dependencies if vendor=1
_extract_vendor:
	#!/usr/bin/env sh
	if test {{vendor}} = 1; then
		rm -rf vendor; tar pxf vendor.tar
	fi

