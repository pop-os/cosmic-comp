{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  nixConfig.bash-prompt-suffix = "[nix]: "; # shows when inside of nix shell

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      pkgsForSystem =
        system:
        (import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        });

      commonFor =
        system:
        let
          pkgs = (pkgsForSystem system);
          rustToolchain = pkgs.pkgsBuildHost.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          buildInputs = with pkgs; [
            rustToolchain
            libdisplay-info
            libinput
            libgbm
            libxkbcommon
            fontconfig
            pixman
            stdenv.cc.cc.lib
            seatd # For libseat
            systemd # For libudev
            wayland
          ];

          nativeBuildInputs = with pkgs; [
            autoPatchelfHook
            cmake
            pkg-config
          ];

          runtimeDependencies = with pkgs; [
            libglvnd # For libEGL
            wayland # winit->wayland-sys wants to dlopen libwayland-egl.so
            # for running in X11
            libx11
            libxcb
            libxcursor
            libxi
            libxkbcommon
            xrdb
            # for vulkan backend
            vulkan-loader
          ];
        in
        {
          inherit
            pkgs
            rustToolchain
            buildInputs
            nativeBuildInputs
            runtimeDependencies
            ;
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          c = (commonFor system);
          rustPlatform = c.pkgs.makeRustPlatform {
            cargo = c.rustToolchain;
            rustc = c.rustToolchain;
          };
        in
        rec {
          default = cosmic-comp;
          cosmic-comp = rustPlatform.buildRustPackage {
            name = "cosmic-comp";
            src = c.pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = c.pkgs.lib.fileset.unions [
                ./src
                ./build.rs
                ./i18n.toml
                ./Cargo.toml
                ./Cargo.lock
                ./resources
                ./cosmic-comp-config
              ];
            };

            cargoLock = {
              lockFile = ./Cargo.lock;
              allowBuiltinFetchGit = true;
            };

            buildInputs = c.buildInputs;
            nativeBuildInputs = c.nativeBuildInputs;
            runtimeDependencies = c.runtimeDependencies;
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          c = (commonFor system);
        in
        {
          default = c.pkgs.mkShell {
            buildInputs = c.buildInputs;
            inputsFrom = [ self.packages.${system}.cosmic-comp ];

            LD_LIBRARY_PATH = c.pkgs.lib.makeLibraryPath c.runtimeDependencies;
            RUSTFLAGS = "-C link-arg=-Wl,-rpath,${c.pkgs.lib.makeLibraryPath c.runtimeDependencies}";
          };
        }
      );
    };
}
