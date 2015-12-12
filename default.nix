{ mkDerivation
, stdenv

, opencv

, attoparsec
, base
, bindings-DSL
, bytestring
, conduit
, conduit-extra
, containers
, exceptions
, http-conduit
, inline-c
, inline-c-cpp
, lumi-hackage-extended
, optparse-applicative
, primitive
, resourcet
, template-haskell
, text
, transformers
, vector
}:
mkDerivation {
  pname = "thea";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends =
    [ attoparsec
      base
      bindings-DSL
      bytestring
      conduit
      conduit-extra
      containers
      exceptions
      http-conduit
      inline-c
      inline-c-cpp
      lumi-hackage-extended
      optparse-applicative
      primitive
      resourcet
      template-haskell
      text
      transformers
      vector
    ];
  executablePkgconfigDepends = [ opencv ];
  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];
  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.unfree;
}
