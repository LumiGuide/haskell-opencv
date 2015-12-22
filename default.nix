{ mkDerivation
, stdenv
, lib

, opencv-HEAD

# library dependencies
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
, repa
, template-haskell
, text
, vector

# test dependencies
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
    base
    bindings-DSL
    bytestring
    containers
    inline-c
    inline-c-cpp
    lens
    linear
    lumi-hackage-extended
    primitive
    repa
    template-haskell
    text
    vector
  ];
  testHaskellDepends = [
    base
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck

    criterion
  ];
  libraryPkgconfigDepends = [ opencv-HEAD ];
  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];
  preConfigure = ''
    rm -rf dist
  '';

  # TODO (BvD): We disable the test-suite for now when installing the package
  # since it fails because it can't find the data-files. We do enable the
  # test-suite when inside nix-shell.
  doCheck = lib.inNixShell;

  homepage = "lumiguide.eu";
  license = stdenv.lib.licenses.unfree;
}
