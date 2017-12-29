final : previous :
let
  haskellOverrides = {
    overrides = self: super: {
      opencv                = self.callPackage (import ./opencv/opencv.nix) {};
      opencv-examples       = self.callPackage (import ./opencv-examples/opencv-examples.nix) {};
      opencv-extra          = self.callPackage (import ./opencv-extra/opencv-extra.nix) {};
      opencv-extra-examples = self.callPackage (import ./opencv-extra-examples/opencv-extra-examples.nix) {};
    };
  };
  osx = builtins.currentSystem == "x86_64-darwin";
in  {
  haskellPackages = previous.haskell.packages.ghc802.override haskellOverrides;
  haskell = previous.haskell // {
    packages = previous.haskell.packages // {
      ghc802 = previous.haskell.packages.ghc802.override haskellOverrides;
      ghc822 = previous.haskell.packages.ghc822.override haskellOverrides;
    };
  };

  opencv3 = previous.opencv3.override {
    enableIpp       = true;
    enableContrib   = true;
    enableGtk2      = true;
    enableFfmpeg    = !osx;
    enableGStreamer = true;
    enableEigen     = true;
    # enableDocs      = true;
  };
}
