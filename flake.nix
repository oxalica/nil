{
  description = "Language Server for Nix Expression Language";

  inputs = {
    naersk.url = "github:nix-community/naersk";
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    naersk,
  }: let
    supportedSystems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];

    genSystems = nixpkgs.lib.genAttrs supportedSystems;

    pkgsFor = nixpkgs.legacyPackages;
  in {
    packages = genSystems (system: let
      naersk-lib = naersk.lib.${system};
    in {
      nil = naersk-lib.buildPackage {
        pname = "nil";
        root = ./.;
      };
      default = self.packages.${system}.nil;
    });

    devShells = genSystems (system: {
      default = pkgsFor.${system}.mkShell {
        nativeBuildInputs = with pkgsFor.${system}; [
          rustc
          cargo
          gitAndTools.pre-commit
        ];
      };
    });
  };
}
