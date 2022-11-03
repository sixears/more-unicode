{
  description = "more unicode symbols";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.4";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "more-unicode" {
      ghc = p: p.ghc8107; # for tfmt
    };
}
