{ mkDerivation
, stdenv
, lib
, runCommand

, opencv
, opencv-examples
, opencv-extra

, base
, linear
, transformers
, vector
}:
mkDerivation {
  pname = "opencv-extra-examples";
  version = "0.0.0";
  src = runCommand "opencv-extra-examples-src"
    { files = lib.sourceByRegex ./. [
        "^src$"
        "^src/.*"
        "^opencv-extra-examples.cabal$"
      ];
      data = ../../data;
      LICENSE = ../LICENSE;
    } ''
      mkdir -p $out
      cp -r $files/* $out #*/
      cp -r $data    $out/data
      cp $LICENSE    $out/LICENSE
    '';
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base linear opencv opencv-examples opencv-extra transformers vector
  ];
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.bsd3;
}
