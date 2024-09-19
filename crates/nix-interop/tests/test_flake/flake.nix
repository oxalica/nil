{
  # Just for tests. No need to be up-to-date.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/5ed481943351e9fd354aeb557679624224de38d5";
  inputs.nix = {
    flake = false;
    url = "github:NixOS/nix/2.13.3";
  };
  inputs.non-flake-file = {
    flake = false;
    url = "https://raw.githubusercontent.com/NixOS/nix/7304806241fe4f72c5f33a5a929d675c8342fabd/flake.nix";
  };

  outputs =
    { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      forSystems = lib.genAttrs lib.systems.flakeExposed;
    in
    {
      packages = forSystems (system: {
        hello = derivation rec {
          pname = "hello";
          version = "1.2.3";
          name = "${pname}-${version}";

          inherit system;
          builder = "/bin/sh";
          args = ":";

          meta = {
            description = "A test derivation";
            homepage = "https://example.com";
            license = [
              lib.licenses.mit # OR
              lib.licenses.asl20
            ];
          };
        };
      });
    };
}
