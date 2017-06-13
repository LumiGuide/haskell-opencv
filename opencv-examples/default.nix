{ nixpkgs ? import (import ../nixpkgs.nix) {config = import ../nixpkgs-config.nix;}
, compiler ? "default"
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.opencv-examples;

in if pkgs.lib.inNixShell then drv.env else drv
