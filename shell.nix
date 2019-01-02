{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cereal, creatur, creatur-wains, deepseq
      , mtl, QuickCheck, stdenv
      }:
      mkDerivation {
        pname = "creatur-wains-test-utils";
        version = "1.2.4";
        src = ./.;
        libraryHaskellDepends = [
          base cereal creatur creatur-wains deepseq mtl QuickCheck
        ];
        homepage = "https://github.com/mhwombat/creatur-wains-test-utils#readme";
        description = "Test utilities for users of creatur-wains";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
