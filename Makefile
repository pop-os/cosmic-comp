export prefix ?= /usr
sysconfdir ?= /etc
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sharedir = $(prefix)/share

BINARY = cosmic-comp
CARGO_TARGET_DIR ?= target
TARGET = debug
DEBUG ?= 0

.PHONY = all clean install uninstall vendor

ifeq ($(DEBUG),0)
	TARGET = release
	ARGS += --release
endif

VENDOR ?= 0
ifneq ($(VENDOR),0)
	ARGS += --offline --locked
endif

TARGET_BIN="$(DESTDIR)$(bindir)/$(BINARY)"

KEYBINDINGS_CONF="$(DESTDIR)$(sharedir)/cosmic/com.system76.CosmicSettings.Shortcuts/v1/defaults"
TILING_EXCEPTIONS_CONF="$(DESTDIR)$(sharedir)/cosmic/com.system76.CosmicSettings.WindowRules/v1/tiling_exception_defaults"
VOICE_MODE_DIR="$(DESTDIR)$(sharedir)/cosmic/com.playtron.VoiceMode/v1/defaults"

all: extract-vendor
	cargo build $(ARGS)

clean:
	cargo clean

distclean:
	rm -rf .cargo vendor vendor.tar target

vendor:
	mkdir -p .cargo
	cargo vendor | head -n -1 > .cargo/config
	echo 'directory = "vendor"' >> .cargo/config
	[ -n "$(SOURCE_GIT_HASH)" ] && printf '\n[env]\nGIT_HASH = "%s"\n' "$(SOURCE_GIT_HASH)" >> .cargo/config || true
	tar pcf vendor.tar vendor
	rm -rf vendor

extract-vendor:
ifeq ($(VENDOR),1)
	rm -rf vendor; tar pxf vendor.tar
endif

install:
	install -Dm0755 "$(CARGO_TARGET_DIR)/$(TARGET)/$(BINARY)" "$(TARGET_BIN)"
	install -Dm0644 "data/keybindings.ron" "$(KEYBINDINGS_CONF)"
	install -Dm0644 "data/tiling-exceptions.ron" "$(TILING_EXCEPTIONS_CONF)"

install-voice-mode:
	mkdir -p "$(VOICE_MODE_DIR)"
	install -m0644 data/voice-mode/* "$(VOICE_MODE_DIR)/"

uninstall-voice-mode:
	rm -rf "$(VOICE_MODE_DIR)"

# Local user config targets (for development)
LOCAL_VOICE_MODE_DIR = $(HOME)/.config/cosmic/com.playtron.VoiceMode/v1

install-voice-mode-local:
	mkdir -p "$(LOCAL_VOICE_MODE_DIR)"
	install -m0644 data/voice-mode/* "$(LOCAL_VOICE_MODE_DIR)/"

uninstall-voice-mode-local:
	rm -rf "$(LOCAL_VOICE_MODE_DIR)"

install-bare-session: install
	install -Dm0644 "data/cosmic.desktop" "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"
	install -Dm0644 "data/cosmic-session.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session.target"
	install -Dm0644 "data/cosmic-session-pre.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session-pre.target"
	install -Dm0644 "data/cosmic-comp.service" "$(DESTDIR)$(libdir)/systemd/user/cosmic-comp.service"
	install -Dm0755 "data/cosmic-service" "$(DESTDIR)/$(bindir)/cosmic-service"

uninstall:
	rm "$(TARGET_BIN)" "$(KEYBINDINGS_CONF)"
	rm -rf "$(VOICE_MODE_DIR)"

uninstall-bare-session:
	rm "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"

# RPM packaging
VERSION = $(shell grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')
RPM_ROOT = $(CARGO_TARGET_DIR)/rpm-root
RPM_BUILD = $(CARGO_TARGET_DIR)/rpm-build

rpm: all
	@echo "Building RPM for $(BINARY) version $(VERSION)..."
	rm -rf "$(RPM_ROOT)" "$(RPM_BUILD)"
	mkdir -p "$(RPM_ROOT)" "$(RPM_BUILD)"/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
	COSMIC_COMP_SOURCE="$(CURDIR)" COSMIC_COMP_VERSION="$(VERSION)" rpmbuild -bb --nodeps \
		--define "_topdir $(CURDIR)/$(RPM_BUILD)" \
		--define "_binary_payload w2.xzdio" \
		--buildroot "$(CURDIR)/$(RPM_ROOT)" \
		packaging/rpm/cosmic-comp.spec
	mkdir -p dist
	cp -v $(RPM_BUILD)/RPMS/*/*.rpm dist/
	@echo "RPM copied to dist/"

run-debug:
	WAYLAND_DISPLAY=wayland-2 \
	COSMIC_BACKEND=kms \
	COSMIC_COMP_LOG=warn,cosmic_comp::shell::layout=debug,cosmic_comp::shell::layout::floating=debug,cosmic_comp::backend::render=debug,cosmic_comp::wayland::handlers::surface_embed=info,cosmic_comp::backend::kms=debug,cosmic_comp::shell=debug,cosmic_comp::shell::workspace=debug,cosmic_comp::backend::kms::surface=debug,cosmic_comp::xwayland=debug,smithay::xwayland=debug \
	dbus-run-session --config-file=$(CURDIR)/scripts/game-mode-session.conf -- sh -c 'echo "GAMEMODE_BUS=$$DBUS_SESSION_BUS_ADDRESS"; exec stdbuf -oL cargo run --features debug' 2>&1 \
	| stdbuf -oL sed -r 's/\x1B\[[0-9;]*[A-Za-z]//g' \
	| stdbuf -oL grep -v 'smithay::backend::renderer::gles' \
	| tee test.log

# Deploy to a remote device for testing.
# Usage: make deploy HOST=user@hostname
#
# The device's sudo password is read interactively, or taken from SUDO_PASS when
# set -- which is required when stdin is not a terminal (an IDE task runner, CI),
# since the interactive read would otherwise get EOF and send an empty password:
#   SUDO_PASS=... make deploy HOST=user@hostname
# Better still, give the device passwordless sudo for this and skip it entirely.
REMOTE_BIN = /usr/bin/$(BINARY)
HOST ?=

deploy:
ifndef HOST
	$(error HOST is required. Usage: make deploy HOST=user@hostname)
endif
	@if [ -z "$$SUDO_PASS" ]; then \
	  read -s -p "Enter sudo password for $(HOST): " SUDO_PASS || true; echo; \
	fi; \
	if [ -z "$$SUDO_PASS" ]; then \
	  echo "no sudo password: pass SUDO_PASS=... (needed when stdin is not a terminal)" >&2; \
	  exit 1; \
	fi && \
	cargo build && \
	echo "Stripping debug symbols..." && \
	strip -o "$(CARGO_TARGET_DIR)/debug/$(BINARY).stripped" "$(CARGO_TARGET_DIR)/debug/$(BINARY)" && \
	echo "Deploying $(BINARY) to $(HOST):$(REMOTE_BIN)..." && \
	scp -C "$(CARGO_TARGET_DIR)/debug/$(BINARY).stripped" "$(HOST):/tmp/$(BINARY)" && \
	printf '%s\n' "$$SUDO_PASS" | ssh $(HOST) "sudo -S sh -c 'install -m0755 /tmp/$(BINARY) $(REMOTE_BIN) && { command -v restorecon >/dev/null 2>&1 && restorecon -v $(REMOTE_BIN) || true; } && { pkill -x $(BINARY) || true; }'" && \
	echo "Done. Compositor restarted on $(HOST)."

# Deploy a profiling build (release optimizations + debug symbols).
# Uses the "fastdebug" Cargo profile for full optimizations with debuginfo.
# Usage: make deploy-profile HOST=user@hostname
deploy-profile:
ifndef HOST
	$(error HOST is required. Usage: make deploy-profile HOST=user@hostname)
endif
	@if [ -z "$$SUDO_PASS" ]; then \
	  read -s -p "Enter sudo password for $(HOST): " SUDO_PASS || true; echo; \
	fi; \
	if [ -z "$$SUDO_PASS" ]; then \
	  echo "no sudo password: pass SUDO_PASS=... (needed when stdin is not a terminal)" >&2; \
	  exit 1; \
	fi && \
	cargo build --profile fastdebug && \
	echo "Deploying $(BINARY) (fastdebug) to $(HOST):$(REMOTE_BIN)..." && \
	scp -C "$(CARGO_TARGET_DIR)/fastdebug/$(BINARY)" "$(HOST):/tmp/$(BINARY)" && \
	printf '%s\n' "$$SUDO_PASS" | ssh $(HOST) "sudo -S sh -c 'install -m0755 /tmp/$(BINARY) $(REMOTE_BIN) && { command -v restorecon >/dev/null 2>&1 && restorecon -v $(REMOTE_BIN) || true; } && { pkill -x $(BINARY) || true; }'" && \
	echo "Done. Compositor restarted on $(HOST) (fastdebug profile)."

# Deploy a release build (fully optimized, stripped).
# Usage: make deploy-release HOST=user@hostname
deploy-release:
ifndef HOST
	$(error HOST is required. Usage: make deploy-release HOST=user@hostname)
endif
	@if [ -z "$$SUDO_PASS" ]; then \
	  read -s -p "Enter sudo password for $(HOST): " SUDO_PASS || true; echo; \
	fi; \
	if [ -z "$$SUDO_PASS" ]; then \
	  echo "no sudo password: pass SUDO_PASS=... (needed when stdin is not a terminal)" >&2; \
	  exit 1; \
	fi && \
	cargo build --release && \
	echo "Stripping debug symbols..." && \
	strip -o "$(CARGO_TARGET_DIR)/release/$(BINARY).stripped" "$(CARGO_TARGET_DIR)/release/$(BINARY)" && \
	echo "Deploying $(BINARY) (release) to $(HOST):$(REMOTE_BIN)..." && \
	scp -C "$(CARGO_TARGET_DIR)/release/$(BINARY).stripped" "$(HOST):/tmp/$(BINARY)" && \
	printf '%s\n' "$$SUDO_PASS" | ssh $(HOST) "sudo -S sh -c 'install -m0755 /tmp/$(BINARY) $(REMOTE_BIN) && { command -v restorecon >/dev/null 2>&1 && restorecon -v $(REMOTE_BIN) || true; } && { pkill -x $(BINARY) || true; }'" && \
	echo "Done. Compositor restarted on $(HOST) (release profile)."
