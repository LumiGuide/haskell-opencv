{ mkDerivation
, runCommand
, stdenv
, lib

, opencv3

# library dependencies
, base
, bindings-DSL
, bytestring
, containers
, data-default
, inline-c
, inline-c-cpp
, opencv
, primitive
, template-haskell
, transformers

# test dependencies
, directory
, Glob
, haskell-src-exts
, JuicyPixels
}:
mkDerivation {
  pname = "opencv-extra";
  version = "0.0.0";
  src = runCommand "opencv-extra-src"
    { files = lib.sourceByRegex ./. [
        "^include$"
        "^include/.*"
        "^src$"
        "^src/.*"
        "^opencv-extra.cabal$"
        "^Setup.hs$"
      ];
      doc     = ../doc;
      data    = ../data;
      LICENSE = ../LICENSE;
    } ''
      mkdir -p $out
      cp -r $files/* $out #*/
      cp -r $doc     $out/doc
      cp -r $data    $out/data
      cp $LICENSE    $out/LICENSE
    '';

  libraryHaskellDepends =
  [ base
    bindings-DSL
    bytestring
    containers
    inline-c
    inline-c-cpp
    opencv
    primitive
    template-haskell
    transformers
  ];

  testHaskellDepends = [
    base
    containers
    data-default
    directory
    Glob
    haskell-src-exts
    JuicyPixels
  ];

  libraryPkgconfigDepends = [ opencv3 ];

  configureFlags =
    [ "--with-gcc=${stdenv.cc}/bin/c++"
      "--with-ld=${stdenv.cc}/bin/c++"
    ];

  hardeningDisable = [ "bindnow" ];
  shellHook = ''
    export hardeningDisable=bindnow
  '';

  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.bsd3;
}
