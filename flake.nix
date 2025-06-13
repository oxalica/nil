# Note: This flake contains only main packages.
# For development environment, see `./dev/flake.nix`.
rec {
  description = "Language Server for Nix Expression Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      inherit (builtins) substring;
      inherit (nixpkgs) lib;

      eachSystem = lib.genAttrs lib.systems.flakeExposed;

      mtime = self.lastModifiedDate;
      date = "${substring 0 4 mtime}-${substring 4 2 mtime}-${substring 6 2 mtime}";
      rev = self.rev or (lib.warn "Git changes are not committed" (self.dirtyRev or "dirty"));

      mkNil =
        { rustPlatform, nixVersions, ... }:
        rustPlatform.buildRustPackage {
          pname = "nil";
          version = "unstable-${date}";
          src = self;

          cargoLock = {
            lockFile = ./Cargo.lock;
            allowBuiltinFetchGit = false;
          };

          nativeBuildInputs = [ (nixVersions.latest or nixVersions.unstable) ];

          CFG_RELEASE = "git-${rev}";

          meta = {
            inherit description;
            homepage = "https://github.com/oxalica/nil";
            license = with lib.licenses; [
              mit
              asl20
            ];
            mainProgram = "nil";
          };
        };

      mkCocNil =
        {
          runCommand,
          nodejs,
          esbuild,
        }:
        runCommand "coc-nil-unstable-${date}"
          {
            nativeBuildInputs = [
              nodejs
              esbuild
            ];
            src = ./editors/coc-nil;
          }
          ''
            cp -r --no-preserve=all $src ./source
            cd source
            npm run build --offline
            mkdir -p $out
            cp -rt $out lib package{,-lock}.json
          '';

    in
    {
      packages = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          default = nil;
          nil = pkgs.callPackage mkNil { };
          coc-nil = pkgs.callPackage mkCocNil { };
        }
      );

      overlays = {
        default = lib.composeExtensions self.overlays.nil self.overlays.coc-nil;
        nil = final: prev: {
          nil = final.callPackage mkNil { };
        };
        coc-nil = final: prev: {
          vimPlugins = prev.vimPlugins or { } // {
            coc-nil = final.callPackage mkCocNil { };
          };
        };
      };
    };
}
