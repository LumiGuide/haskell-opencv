{ mkDerivation
, stdenv

, opencv

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
, template-haskell
, text
, vector
}:
mkDerivation {
  pname = "thea";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends =
    [ base
      bindings-DSL
      bytestring
      containers
      inline-c
      inline-c-cpp
      lens
      linear
      lumi-hackage-extended
      primitive
      template-haskell
      text
      vector
    ];
  libraryPkgconfigDepends = [ opencv ];
  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.unfree;
}
