name := 'cosmic-comp'

rootdir := ''
prefix := '/usr'
sysconfdir := '/etc'

base-dir := absolute_path(clean(rootdir / prefix))
sysconfdir-dir := absolute_path(clean(rootdir / sysconfdir))

bin-dir := base-dir / 'bin'
lib-dir := base-dir / 'lib'
share-dir := base-dir / 'share'

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

import 'cargo.just'

# Default recipe which runs `just build-release`
[private]
default: build-release

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
