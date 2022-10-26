{
  description = "more unicode symbols";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url = "github:sixears/flake-build-utils/r0.0.0.0";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "more-unicode" { };
}
