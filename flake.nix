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
    let
      mkNil = { rustPlatform, nix, ... }:
        let
          inherit (builtins) substring;
          mtime = self.lastModifiedDate;
          date = "${substring 0 4 mtime}-${substring 4 2 mtime}-${substring 6 2 mtime}";
          rev = self.shortRev or (throw "Git changes are not committed");
        in
        rustPlatform.buildRustPackage {
          pname = "nil";
          version = "unstable-${date}";
          src = self;
          cargoLock.lockFile = self + "/Cargo.lock";

          nativeBuildInputs = [ nix.out ];

          CFG_DATE = date;
          CFG_REV = rev;
        };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          inherit (nixpkgs) lib;

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

          nil = pkgs.callPackage mkNil { };
        in
        {
          packages = {
            inherit nil;
            default = nil;
          };

          devShells.default = pkgs.mkShell {
            packages = with pkgs; with rustPkgs; [
              # Override the stable rustfmt.
              rust-nightly_2022-08-01.availableComponents.rustfmt
              rust
              nix.out # For generation of builtins.
              gdb
              jq
              pre-commit
              (import ./dev/neovim-lsp.nix { inherit pkgs; })
              (import ./dev/vim-coc.nix { inherit pkgs; })
            ] ++ lib.optionals (lib.meta.availableOn stdenv.hostPlatform vscodium) [
              (import ./dev/vscodium.nix { inherit pkgs; })
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
        })
    // rec {
      overlays = {
        nil = final: prev: {
          nil = final.callPackage mkNil { };
        };
        default = overlays.nil;
      };
    };
}
