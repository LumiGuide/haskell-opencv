{
  packageOverrides = pkgs:
    let
      haskellOverrides = {
        overrides = self: super: {
          opencv                = self.callPackage (import ./haskell-opencv.nix) {};
          opencv-examples       = self.callPackage (import ./examples/opencv-examples.nix) {};
          opencv-extra          = self.callPackage (import ./opencv-extra/opencv-extra.nix) {};
          opencv-extra-examples = self.callPackage (import ./opencv-extra/examples/opencv-extra-examples.nix) {};
        };
      };
      osx = builtins.currentSystem == "x86_64-darwin";
    in {
      haskellPackages = pkgs.haskellPackages.override haskellOverrides;

      opencv3 = pkgs.opencv3.override {
        enableIpp       = true;
        enableContrib   = true;
        enableGtk2      = true;
        enableFfmpeg    = true;
        enableGStreamer = true;
        enableEigen     = true;
      };
    };
}
