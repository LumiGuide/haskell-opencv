{ mkDerivation
, stdenv
, lib

, opencv3_1

# library dependencies
, aeson
, base
, base64-bytestring
, bindings-DSL
, bytestring
, containers
, deepseq
, inline-c
, inline-c-cpp
, linear
, primitive
, repa
, template-haskell
, text
, vector

# test dependencies
, directory
, Glob
, haskell-src-meta
, haskell-src-exts
, QuickCheck
, tasty
, tasty-hunit
, tasty-quickcheck

# benchmark dependencies
, criterion
}:
mkDerivation {
  pname = "thea";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    bindings-DSL
    bytestring
    containers
    deepseq
    inline-c
    inline-c-cpp
    linear
    primitive
    repa
    template-haskell
    text
    vector
  ];
  testHaskellDepends = [
    base
    directory
    Glob
    haskell-src-meta
    haskell-src-exts
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck

    criterion
  ];
  libraryPkgconfigDepends = [ opencv3_1 ];
  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];
  preConfigure = ''
    rm -rf dist
  '';
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.unfree;
  maintainers = [ lib.engineeringAtLumiGuide ];
}
