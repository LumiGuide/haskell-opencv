{ mkDerivation
, stdenv

, opencv-HEAD

, base
, bindings-DSL
, bytestring
, containers
, inline-c
, inline-c-cpp
, lens
, linear
, lumi-hackage-extended
, primitive
, repa
, template-haskell
, text
, vector

, QuickCheck
, tasty
, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "thea";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    bindings-DSL
    bytestring
    containers
    inline-c
    inline-c-cpp
    lens
    linear
    lumi-hackage-extended
    primitive
    repa
    template-haskell
    text
    vector
  ];
  testHaskellDepends = [
    base
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck
  ];
  libraryPkgconfigDepends = [ opencv-HEAD ];
  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];
  preConfigure = ''
    rm -rf dist
  '';
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.unfree;
}
