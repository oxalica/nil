{
  description = "Development Environment for nil";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      inherit (nixpkgs) lib;
      eachSystem = lib.genAttrs lib.systems.flakeExposed;

      # For rustfmt and fuzz.
      nightlyVersion = "2025-06-01";
    in
    {
      devShells = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          rustPkgs = rust-overlay.packages.${system};

        in
        rec {
          without-rust = pkgs.mkShell {
            nativeBuildInputs =
              with pkgs;
              [
                # Override the stable rustfmt.
                rustPkgs."rust-nightly_${nightlyVersion}".availableComponents.rustfmt

                # Don't include `nix` by default. If would override user's (newer
                # or patched) one, cause damage or misbehavior due to version
                # mismatch.
                # If you do want a locked one, use `devShells.full` below.

                # Used by pre-push
                fd
                typos

                nodejs
                watchman # Required by coc.nvim for file watching.

                jq
                nixfmt-rfc-style
                (import ./nvim-lsp.nix { inherit pkgs; })
                (import ./vim-coc.nix { inherit pkgs; })
                (import ./vim-lsp.nix { inherit pkgs; })
              ]
              ++ lib.optionals (lib.meta.availableOn stdenv.hostPlatform vscodium) [
                (import ./vscodium.nix { inherit pkgs; })
              ]
              ++ lib.optionals (lib.meta.availableOn stdenv.hostPlatform gdb) [
                gdb
              ];

            RUST_BACKTRACE = "short";
            NIXPKGS = nixpkgs;

            # bash
            shellHook = ''
              export NIL_PATH="$(cargo metadata --format-version=1 | jq -r .target_directory)/debug/nil"
              export COC_NIL_PATH="$(realpath ./editors/coc-nil)"
            '';
          };

          default = without-rust.overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [
              # Follows nixpkgs's version of rustc.
              (
                let
                  vers = lib.splitVersion pkgs.rustc.version;
                in
                rustPkgs."rust_${lib.elemAt vers 0}_${lib.elemAt vers 1}_${lib.elemAt vers 2}".override {
                  extensions = [ "rust-src" ];
                }
              )
            ];
          });

          # See comments above.
          full = default.overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [
              (pkgs.nixVersions.latest or pkgs.nixVersions.unstable).out
            ];
          });

          fuzz = pkgs.mkShell {
            packages = with pkgs; [
              rustPkgs."rust-nightly_${nightlyVersion}"
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
        }
      );
    };
}
