{ nixpkgs ? import <nixpkgs> {config = import ../nixpkgs-config.nix;}
, compiler ? "default"
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage (import ./opencv-examples.nix) {};

in if pkgs.lib.inNixShell then drv.env else drv
