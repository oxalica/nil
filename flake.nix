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

          cd "$(git rev-parse --show-toplevel)"
          rg --fixed-strings 'dbg!' --glob '*.rs' \
            && die 'Found dbg!()'
          cargo fmt --quiet --check >/dev/null \
            || die 'Format failed'
          cargo clippy --quiet --all-targets --all-features -- --deny warnings \
            || die 'Clippy failed'
          cargo test --quiet \
            || die 'Test failed'
        '';

      in {
        packages = rec {
          default = nil;
          nil = let
            mtime = self.lastModifiedDate;
            date = "${substring 0 4 mtime}-${substring 4 2 mtime}-${substring 6 2 mtime}";
            rev = self.shortRev or (throw "Git changes are not committed");
          in pkgs.rustPlatform.buildRustPackage {
            pname = "nil";
            version = "unstable-${date}";
            src = self;
            cargoLock.lockFile = self + "/Cargo.lock";
            buildAndTestSubdir = "crates/nil";

            CFG_DATE = date;
            CFG_REV = rev;
          };
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; with rustPkgs; [
            # Override the stable rustfmt.
            rust-nightly_2022-08-01.availableComponents.rustfmt
            rust
            nix # For generation of builtins.
            gdb
            jq
            (import ./dev/neovim-lsp.nix { inherit pkgs; })
            (import ./dev/vim-coc.nix { inherit pkgs; })
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
