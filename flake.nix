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

        pre-commit = pkgs.writeShellScriptBin "pre-commit" ''
          set -e
          die() { echo "$*" >&2; exit 1; }

          rg --fixed-strings 'dbg!' --glob '*.rs' \
            && die 'Found dbg!()'
          cargo fmt --quiet --check >/dev/null \
            || die 'Format failed'
          cargo clippy --quiet --all-targets --all-features --package '*' -- --deny warnings \
            || die 'Clippy failed'
          cargo test --quiet --package '*' \
            || die 'Test failed'
        '';

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
          packages = with pkgs; with rustPkgs; [
            # Override the stable rustfmt.
            rust-nightly_2022-08-01.availableComponents.rustfmt
            rust
            gdb
            jq
            (import ./neovim-env.nix { inherit pkgs; })
            pre-commit
          ];
          RUST_BACKTRACE = "short";
          NIXPKGS = nixpkgs;

          # bash
          shellHook = ''
            export NIL_PATH="$(cargo metadata --format-version=1 | jq -r .target_directory)/debug/nil"
          '';
        };

        devShells.fuzz = pkgs.mkShell {
          packages = with pkgs; with rustPkgs; [
            rust-nightly_2022-08-01
            cargo-fuzz
            llvmPackages_14.llvm
            jq
            gnugrep
          ];
          RUST_BACKTRACE = "short";

          # bash
          shellHook = ''
            export CARGO_TARGET_DIR=~/.cache/targets-syntax
          '';
        };
      });
}
