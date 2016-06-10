{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage (import ./haskell-opencv.nix) {
    opencv3_1 = pkgs.callPackage ./opencv-3.1.0.nix {
      gtk = pkgs.gtk3;
      qt  = pkgs.qt5;
      enableIpp     = true;
      enableContrib = true;
      enableBloat   = true;
      enableOpenGL  = true;
      enableQT      = false;
    };
  };

in if pkgs.lib.inNixShell then drv.env else drv
