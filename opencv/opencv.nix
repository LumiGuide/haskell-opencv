{ mkDerivation, aeson, base, bindings-DSL
, bytestring, Cabal, containers, criterion, data-default, deepseq
, directory, Glob, haskell-src-exts, inline-c, inline-c-cpp
, JuicyPixels, lens, linear, mtl, opencv4, primitive, QuickCheck
, random, repa, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, transformers, vector
}:
mkDerivation {
  pname = "opencv";
  version = "0.0.2.1";
  src = ./opencv;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    aeson base bindings-DSL bytestring containers
    data-default deepseq inline-c inline-c-cpp JuicyPixels linear mtl
    primitive repa template-haskell text transformers vector
  ];
  libraryPkgconfigDepends = [ opencv4 ];
  testHaskellDepends = [
    base bytestring containers data-default deepseq directory Glob
    haskell-src-exts JuicyPixels lens linear primitive QuickCheck
    random repa tasty tasty-hunit tasty-quickcheck template-haskell
    text transformers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion linear repa vector
  ];
  hardeningDisable = [ "bindnow" ];
  homepage = "https://github.com/LumiGuide/haskell-opencv";
  description = "Haskell binding to OpenCV-3.x";
  license = stdenv.lib.licenses.bsd3;
}
