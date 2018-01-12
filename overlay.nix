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
  haskellPackages = previous.haskellPackages.override haskellOverrides;
  haskell = previous.haskell // {
    packages = previous.haskell.packages // {
      ghc802 = previous.haskell.packages.ghc802.override haskellOverrides;
      ghc822 = previous.haskell.packages.ghc822.override haskellOverrides;
    };
  };

  opencv3 = previous.opencv3.override {
    enableIpp       = true;
    enableContrib   = true;
    enableGtk3      = true;
    enableFfmpeg    = !osx;
    enableGStreamer = true;
    enableDocs      = true;
    enableUnfree    = false;
    enableTesseract = true;
    enableOvis      = false; # Currently causes the error: "Missing C library: opencv_ovis"
    enableGPhoto2   = true;
    enableDC1394    = false; # Currently causes the warning: libdc1394 error: Failed to initialize libdc1394
  };
}
