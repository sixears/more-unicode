{
  description = "more unicode symbols";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
#    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.8";
build-utils.url = "path:/home/martyn/src/flake-build-utils";
  };

  outputs = { self, nixpkgs, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "more-unicode" {
      ghc = p: p.ghc8107; # for tfmt
    };
}
