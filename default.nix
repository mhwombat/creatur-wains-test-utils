{ mkDerivation, base, cereal, creatur, creatur-wains, deepseq, mtl
, QuickCheck, stdenv
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
}
