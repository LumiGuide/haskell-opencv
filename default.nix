pkgs : haskellPackages :
haskellPackages.callPackage (import ./haskell-opencv.nix) {
  opencv3_1 = import ./opencv-default.nix pkgs;
}
