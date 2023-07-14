{
  # Just for tests. No need to be up-to-date.
  inputs = {
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixos-23-05.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = inputs: { };
}
