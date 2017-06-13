{ mkDerivation
, stdenv
, lib
, runCommand

, opencv

, base
, bytestring
, file-embed
, filepath
, linear
, template-haskell
, temporary
, transformers
, vector
}:
mkDerivation {
  pname = "opencv-examples";
  version = "0.0.0";
  src = runCommand "opencv-examples-src"
    { files = lib.sourceByRegex ./. [
        "^src$"
        "^src/.*"
        "^lib$"
        "^lib/.*"
        "^opencv-examples.cabal$"
      ];
      data = ../data;
      LICENSE = ../LICENSE;
   } ''
      mkdir -p $out
      cp -r $files/* $out #*/
      cp -r $data    $out/data
      cp $LICENSE    $out/LICENSE
    '';
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring file-embed filepath opencv template-haskell
    temporary
  ];
  executableHaskellDepends = [
    base bytestring linear opencv transformers vector
  ];
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.bsd3;
}
