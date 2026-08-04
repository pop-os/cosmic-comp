# Keep in sync with rust-toolchain.toml. Drift is no longer fatal -- the rustup step
# below installs the pinned channel regardless -- but a matching tag avoids
# re-downloading a whole toolchain on every image build.
ARG version=1.93
FROM rust:${version}

RUN dpkg --add-architecture arm64
RUN apt-get update && apt-get install -y \
  cmake \
  meson \
  ninja-build \
  python3 \
  hwdata \
  pkg-config \
  git \
  ca-certificates \
  libclang-dev \
  libudev-dev \
  libudev-dev:arm64 \
  libseat-dev \
  libseat-dev:arm64 \
  libinput-dev \
  libinput-dev:arm64 \
  libxkbcommon-dev \
  libxkbcommon-dev:arm64 \
  libgbm-dev \
  libgbm-dev:arm64 \
  libpixman-1-dev \
  libpixman-1-dev:arm64 \
  libssl-dev:arm64

RUN apt-get install -y \
  g++-aarch64-linux-gnu \
  libc6-dev-arm64-cross

# Build libdisplay-info 0.3.0 from source as a STATIC archive for both arches, and
# install ONLY the archive (no shared object). With no libdisplay-info.so present, the
# linker embeds the archive into the cosmic-comp binary, which therefore carries NO
# libdisplay-info.so.N runtime soname dependency -- so a single RPM installs on Fedora 43
# (ships .so.2) AND Fedora 44 (ships .so.3), eliminating the soname divergence that broke
# fc44. Debian only packages 0.2, hence the from-source build. Installed into the multiarch
# libdir so pkg-config resolves it like the other :arm64 dev packages.
# NOTE: the static guarantee is the *absence* of a .so here, not a link-mode override --
# pkg-config-rs deliberately will not statically link libraries found in system paths.
ARG LIBDISPLAY_INFO_VERSION=0.3.0
RUN set -eux; \
  git clone --depth 1 --branch "${LIBDISPLAY_INFO_VERSION}" \
    https://gitlab.freedesktop.org/emersion/libdisplay-info.git /tmp/ldi; \
  cd /tmp/ldi; \
  meson setup build-x86_64 \
    --prefix=/usr --libdir=lib/x86_64-linux-gnu \
    --buildtype=release -Ddefault_library=static; \
  meson compile -C build-x86_64; \
  meson install -C build-x86_64; \
  printf '%s\n' \
    '[binaries]' \
    "c = 'aarch64-linux-gnu-gcc'" \
    "cpp = 'aarch64-linux-gnu-g++'" \
    "ar = 'aarch64-linux-gnu-ar'" \
    "strip = 'aarch64-linux-gnu-strip'" \
    "pkg-config = 'pkg-config'" \
    '[host_machine]' \
    "system = 'linux'" \
    "cpu_family = 'aarch64'" \
    "cpu = 'aarch64'" \
    "endian = 'little'" \
    > /tmp/aarch64-cross.txt; \
  meson setup build-aarch64 --cross-file /tmp/aarch64-cross.txt \
    --prefix=/usr --libdir=lib/aarch64-linux-gnu \
    --buildtype=release -Ddefault_library=static; \
  meson compile -C build-aarch64; \
  meson install -C build-aarch64; \
  rm -rf /tmp/ldi

# Taskfile support
RUN curl -1sLf 'https://dl.cloudsmith.io/public/task/task/setup.deb.sh' | bash
RUN apt-get install -y task

# RPM support
RUN apt-get install -y rpm librpmbuild10 elfutils

# rust-toolchain.toml pins the channel and rustup honours it for every cargo
# invocation in the mounted repo, so the cross std must be installed for THAT
# channel -- not for whatever `version` above happens to be. Resolving it from the
# file keeps the image correct when the pin moves ahead of the base image tag;
# a mismatch surfaces as "can't find crate for `std`" on the aarch64 build only
# (the host std gets auto-downloaded, the cross one does not).
COPY rust-toolchain.toml /tmp/rust-toolchain.toml
RUN set -eux; \
  channel="$(sed -n 's/^ *channel *= *"\([^"]*\)".*/\1/p' /tmp/rust-toolchain.toml)"; \
  test -n "$channel"; \
  rustup toolchain install "$channel"; \
  rustup component add --toolchain "$channel" clippy rust-src; \
  rustup target add --toolchain "$channel" aarch64-unknown-linux-gnu; \
  rustup toolchain install --force-non-host "$channel-aarch64-unknown-linux-gnu"; \
  rm /tmp/rust-toolchain.toml
RUN chmod -R 777 /usr/local/rustup

ENV CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER=aarch64-linux-gnu-gcc
ENV CC_aarch64_unknown_linux_gnu=aarch64-linux-gnu-gcc
ENV CXX_aarch64_unknown_linux_gnu=aarch64-linux-gnu-g++

# Retry transient network failures (crates.io / git deps) instead of failing the build
ENV CARGO_NET_RETRY=10

