{
  description = "Compositor for the COSMIC desktop environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

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
        import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

      commonFor =
        system:
        let
          pkgs = pkgsForSystem system;
          rustToolchain = pkgs.pkgsBuildHost.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        in
        {
          inherit pkgs rustToolchain;

          buildInputs = with pkgs; [
            libdisplay-info
            libinput
            libgbm
            libxkbcommon
            fontconfig
            pixman
            stdenv.cc.cc.lib
            seatd
            systemd
            udev
            wayland
          ];

          nativeBuildInputs = with pkgs; [
            autoPatchelfHook
            cmake
            pkg-config
          ];

          runtimeDependencies = with pkgs; [
            libglvnd
            wayland
            libx11
            libxcb
            libxcursor
            libxi
            libxkbcommon
            xrdb
            vulkan-loader
          ];
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          c = commonFor system;
          rustPlatform = c.pkgs.makeRustPlatform {
            cargo = c.rustToolchain;
            rustc = c.rustToolchain;
          };
        in
        {
          default = self.packages.${system}.cosmic-comp;

          cosmic-comp = rustPlatform.buildRustPackage {
            pname = "cosmic-comp";
            version = "1.0.8-dev";

            src = c.pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = c.pkgs.lib.fileset.unions [
                ./cosmic-comp-config
                ./resources
                ./src
                ./Cargo.toml
                ./Cargo.lock
                ./Makefile
                ./build.rs
                ./i18n.toml
              ];
            };

            cargoLock = {
              lockFile = ./Cargo.lock;
              allowBuiltinFetchGit = true;
            };

            separateDebugInfo = true;

            buildInputs = c.buildInputs;
            nativeBuildInputs = c.nativeBuildInputs;
            runtimeDependencies = c.runtimeDependencies;

            makeFlags = [
              "prefix=${placeholder "out"}"
              "CARGO_TARGET_DIR=target/${c.pkgs.stdenv.hostPlatform.rust.cargoShortTarget}"
            ];

            dontCargoInstall = true;

            meta = with c.pkgs.lib; {
              description = "Compositor for the COSMIC desktop environment";
              homepage = "https://github.com/pop-os/cosmic-comp";
              license = licenses.gpl3Only;
              platforms = platforms.linux;
              mainProgram = "cosmic-comp";
            };
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          c = commonFor system;
        in
        {
          default = c.pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.cosmic-comp ];

            packages = with c.pkgs; [
              c.rustToolchain
              rust-analyzer
              cargo-watch
            ];

            LD_LIBRARY_PATH = c.pkgs.lib.makeLibraryPath c.runtimeDependencies;

            shellHook = ''
              echo "COSMIC Compositor development environment"
              echo "Run 'cargo build' to build, 'cargo test' to test"
            '';
          };
        }
      );

      # Formatter for 'nix fmt'
      formatter = forAllSystems (system: (pkgsForSystem system).nixfmt-rfc-style);

      # Overlay for use in other flakes
      overlays.default = final: prev: {
        cosmic-comp = self.packages.${prev.system}.cosmic-comp;
      };
    };
}
