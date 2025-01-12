{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    parts.url = "github:hercules-ci/flake-parts";
    parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    crane.url = "github:ipetkov/crane";

    rust.url = "github:oxalica/rust-overlay";
    rust.inputs.nixpkgs.follows = "nixpkgs";

    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      parts,
      crane,
      rust,
      nix-filter,
      ...
    }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];

      perSystem =
        {
          self',
          lib,
          system,
          ...
        }:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend rust.overlays.default;
          rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          craneLib = (crane.mkLib pkgs).overrideToolchain rust-toolchain;
          craneArgs = {
            pname = "cosmic-comp";
            version = self.rev or "dirty";

            src = nix-filter.lib.filter {
              root = ./.;
              include = [
                ./src
                ./i18n.toml
                ./Cargo.toml
                ./Cargo.lock
                ./resources
                ./cosmic-comp-config
              ];
            };

            nativeBuildInputs = with pkgs; [
              pkg-config
              autoPatchelfHook
              cmake
            ];

            buildInputs = with pkgs; [
              wayland
              systemd # For libudev
              seatd # For libseat
              libxkbcommon
              libinput
              mesa # For libgbm
              fontconfig
              stdenv.cc.cc.lib
              pixman
              libdisplay-info
            ];

            runtimeDependencies = with pkgs; [
              libglvnd # For libEGL
              wayland # winit->wayland-sys wants to dlopen libwayland-egl.so
              # for running in X11
              xorg.libX11
              xorg.libXcursor
              xorg.libxcb
              xorg.libXi
              libxkbcommon
              # for vulkan backend
              vulkan-loader
            ];
          };

          cargoArtifacts = craneLib.buildDepsOnly craneArgs;
          cosmic-comp = craneLib.buildPackage (craneArgs // { inherit cargoArtifacts; });
        in
        {
          apps.cosmic-comp = {
            type = "app";
            program = lib.getExe self'.packages.default;
          };

          checks.cosmic-comp = cosmic-comp;
          packages.default = cosmic-comp;

          devShells.default = craneLib.devShell {
            LD_LIBRARY_PATH = lib.makeLibraryPath (
              __concatMap (d: d.runtimeDependencies) (__attrValues self'.checks)
            );

            # include build inputs
            inputsFrom = [ cosmic-comp ];
          };
        };
    };
}
