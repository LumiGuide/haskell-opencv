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
      ghc841 = previous.haskell.packages.ghc841.override {
        overrides = self: super:
          haskellOverrides.overrides self super // (with previous.haskell.lib; {
            safe-exceptions = doJailbreak super.safe-exceptions;
            repa = doJailbreak super.repa;
            inline-c = overrideSrc super.inline-c {
              src = (previous.fetchFromGitHub {
                owner  = "basvandijk";
                repo   = "inline-c";
                rev    = "bb8a77b5abb58084de3f3d2b7d3335d6997d9905";
                sha256 = "16n769pkkbjxp2khj7s0i9qya0pigjw666mwvib1ys78j59fpbjb";
              }) + "/inline-c";
            };
            criterion = super.criterion_1_4_0_0;
          });
      };
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
