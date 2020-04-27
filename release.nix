let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project =
            haskellPackagesNew.callPackage ./project.nix { };
          creatur =
            haskellPackagesNew.callPackage ./creatur.nix { };
          creatur-wains =
            haskellPackagesNew.callPackage ./creatur-wains.nix { };
          som =
            haskellPackagesNew.callPackage ./som.nix { };
          gray-extended =
            haskellPackagesNew.callPackage ./gray-extended.nix { };
          grid =
            haskellPackagesNew.callPackage ./grid.nix { };
          };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }

