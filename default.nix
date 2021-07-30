let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      som = ../som;
      creatur = ../creatur;
      creatur-wains = ../creatur-wains;
      gray-extended = ../gray-extended;
      grid = ../grid;
    };
  }
