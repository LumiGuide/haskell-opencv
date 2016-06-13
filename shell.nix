{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  osx = builtins.currentSystem == "x86_64-darwin";

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage (import ./haskell-opencv.nix) {
    opencv3_1 = pkgs.callPackage ./opencv-3.1.0.nix {
      gtk = pkgs.gtk3;
      qt  = pkgs.qt5;
      enableIpp     = !osx;
      enableContrib = true;
      enableBloat   = !osx;
      enableOpenGL  = true;
      enableQT      = false;
    };
  };

in if pkgs.lib.inNixShell then drv.env else drv
