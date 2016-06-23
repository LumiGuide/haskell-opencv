{ mkDerivation, base, bytestring, opencv, stdenv }:
mkDerivation {
  pname = "opencv-examples";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring opencv ];
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.bsd3;
}
