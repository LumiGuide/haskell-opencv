{ mkDerivation
, stdenv
, lib

, opencv3

# library dependencies
, aeson
, base
, base64-bytestring
, bindings-DSL
, bytestring
, containers
, data-default
, deepseq
, inline-c
, inline-c-cpp
, JuicyPixels
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
, haskell-src-exts
, QuickCheck
, tasty
, tasty-hunit
, tasty-quickcheck

# benchmark dependencies
, criterion
}:
mkDerivation ({
  pname = "opencv";
  version = "0.0.0";
  src = lib.sourceByRegex ./. [
    "^src$"
    "^src/.*"
    "^include$"
    "^include/.*"
    "^test$"
    "^test/.*"
    "^bench$"
    "^bench/.*"
    "^data$"
    "^data/.*"
    "^doc$"
    "^doc/.*"
    "^LICENSE$"
    "^opencv.cabal$"
    "^Setup.hs$"
  ];

  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    bindings-DSL
    bytestring
    containers
    data-default
    deepseq
    inline-c
    inline-c-cpp
    JuicyPixels
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
    haskell-src-exts
    JuicyPixels
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck

    criterion
  ];

  libraryPkgconfigDepends = [ opencv3 ];

  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];

  hardeningDisable = [ "bindnow" ];
  shellHook = ''
    export hardeningDisable=bindnow
  '';

  homepage = "https://github.com/LumiGuide/haskell-opencv";
  license = stdenv.lib.licenses.bsd3;
  maintainers = [ "engineering@lumi.guide" ];
})
