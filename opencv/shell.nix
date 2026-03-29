{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "opencv" ./. {
    opencv3 = pkgs.opencv4;
  };
  opencv = pkgs.haskell.lib.enableCabalFlag drv "opencv4";
in opencv.env.overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
    pkgs.cabal-install
  ];
  shellHook = ''
    export GHC_ENVIRONMENT=-
  '';
})
