{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    parts.url = "github:hercules-ci/flake-parts";
    crane.url = "github:ipetkov/crane";
    rust.url = "github:oxalica/rust-overlay";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs@{ self, nixpkgs, parts, crane, rust, nix-filter, ... }:
    parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-linux" "x86_64-linux" ];

      perSystem = { self', lib, system, ... }:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend rust.overlays.default;
          rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          craneLib = crane.lib.${system}.overrideToolchain rust-toolchain;
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
            ];

            runtimeDependencies = with pkgs; [
              libglvnd # For libEGL
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

          devShells.default = pkgs.mkShell {
            # Should there be packages here or use Nix purely for CI?
            LD_LIBRARY_PATH = lib.makeLibraryPath (__concatMap (d: d.runtimeDependencies) (__attrValues self'.checks));
          };
        };
    };
}
