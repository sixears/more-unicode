{
  description = "more unicode symbols";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.11";
  };

  outputs = { self, nixpkgs, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "more-unicode" {
      ghc = p: p.ghc8107; # for tfmt

      callPackage = { mkDerivation, lib, system
                    , base, base-unicode-symbols, containers, lens
                    , mono-traversable, prettyprinter, tasty-hunit
                    , tasty-quickcheck, text
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "more-unicode";
            version = "0.0.17.8";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols containers lens mono-traversable
              prettyprinter tasty-hunit tasty-quickcheck text
            ];
            testHaskellDepends = [ base ];
            description = "More unicode symbols";
            license = lib.licenses.mit;
          };
     };
}
