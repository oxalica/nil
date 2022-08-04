{
  description = "Language Server for Nix Expression Language";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, flake-utils, nixpkgs, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (builtins) substring;
        pkgs = nixpkgs.legacyPackages.${system};
        rustPkgs = rust-overlay.packages.${system};
      in {
        packages = rec {
          default = nil;
          nil = pkgs.rustPlatform.buildRustPackage {
            pname = "nil";
            version = let mtime = self.lastModifiedDate; in
              "unstable-${substring 0 4 mtime}-${substring 4 2 mtime}-${substring 6 2 mtime}";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
            buildAndTestSubdir = "lsp";
          };
        };

        devShells.default = pkgs.mkShell {
          packages = [
            # Override the stable rustfmt.
            rustPkgs.rust-nightly_2022-08-01.availableComponents.rustfmt
            rustPkgs.rust
            pkgs.jq
            (import ./neovim-env.nix { inherit pkgs; })
          ];

          RUST_BACKTRACE = 1;
          NIXPKGS = nixpkgs;

          shellHook = ''
            export NIL_PATH="$(cargo metadata --format-version=1 | jq -r .target_directory)"
          '';
        };
      });
}
