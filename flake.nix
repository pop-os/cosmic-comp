{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    # <https://github.com/nix-systems/default-linux>
    systems.url = "github:nix-systems/default-linux";

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane.url = "github:ipetkov/crane";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    crane,
    nixpkgs,
    rust-overlay,
    systems,
    ...
  }: let
    inherit (nixpkgs) lib;

    fs = lib.fileset;
    eachSystem = lib.genAttrs (import systems);
    pkgsFor = eachSystem (system:
      import nixpkgs {
        localSystem.system = system;
        overlays = [(import rust-overlay)];
      });
  in {
    packages =
      lib.mapAttrs (system: pkgs: let
        craneLib = crane.mkLib pkgs;
        craneArgs = {
          pname = "cosmic-comp";
          version = self.rev or "dirty";

          src = fs.toSource {
            root = ./.;
            fileset = fs.unions [
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
      in {
        cosmic-comp = craneLib.buildPackage (craneArgs // {inherit cargoArtifacts;});

        default = self.packages.${system}.cosmic-comp;
      })
      pkgsFor;

    apps = eachSystem (system: {
      cosmic-comp = {
        type = "app";
        program = lib.getExe self.packages.${system}.default;
      };

      default = self.apps.${system}.cosmic-comp;
    });

    checks = eachSystem (system: {
      inherit (self.packages.${system}) cosmic-comp;
    });

    devShells =
      lib.mapAttrs (
        system: pkgs: let
          craneLib = crane.mkLib pkgs;
        in {
          default = craneLib.devShell {
            LD_LIBRARY_PATH = lib.makeLibraryPath (
              builtins.concatMap (d: d.runtimeDependencies) (builtins.attrValues self.checks.${system})
            );

            # include build inputs
            inputsFrom = [self.packages.${system}.cosmic-comp];
          };
        }
      )
      pkgsFor;
  };
}
