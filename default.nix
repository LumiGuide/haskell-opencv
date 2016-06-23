pkgs : haskellPackages :
let osx = builtins.currentSystem == "x86_64-darwin";
in haskellPackages.callPackage (import ./haskell-opencv.nix) {
     opencv3_1 = pkgs.callPackage ./opencv-3.1.0.nix {
       gtk = pkgs.gtk3;
       qt  = pkgs.qt5;
       enableIpp     = !osx;
       enableContrib = true;
       enableBloat   = !osx;
       enableOpenGL  = true;
       enableQT      = false;
     };
   }
