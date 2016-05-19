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
, transformers
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
  pname = "opencv";
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
    transformers
    vector
  ];
  testHaskellDepends = [
    base
    containers
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
  homepage = "https://github.com/LumiGuide/haskell-opencv";
  license = stdenv.lib.licenses.bsd3;
  maintainers = [ lib.engineeringAtLumiGuide ];
}
