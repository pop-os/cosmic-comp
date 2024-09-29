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

install-bare-session: install
	install -Dm0644 "data/cosmic.desktop" "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"
	install -Dm0644 "data/cosmic-session.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session.target"
	install -Dm0644 "data/cosmic-session-pre.target" "$(DESTDIR)$(libdir)/systemd/user/cosmic-session-pre.target"
	install -Dm0644 "data/cosmic-comp.service" "$(DESTDIR)$(libdir)/systemd/user/cosmic-comp.service"
	install -Dm0755 "data/cosmic-service" "$(DESTDIR)/$(bindir)/cosmic-service"

uninstall:
	rm "$(TARGET_BIN)" "$(KEYBINDINGS_CONF)"

uninstall-bare-session:
	rm "$(DESTDIR)$(sharedir)/wayland-sessions/cosmic.desktop"
