let
  haskellOverrides = {
    overrides = self: super: {
      inline-c = super.inline-c.override (args: args // {
        mkDerivation = expr: args.mkDerivation (expr // {
          version = "0.5.5.5";
          sha256 = "0c3jijav2iz9b5k1hzzidq7rbavj45rbbyrk93ybd2dagrj45lgk";
        });
      });
      opencv = self.callPackage (import ./haskell-opencv.nix) {};
    };
  };
  osx = builtins.currentSystem == "x86_64-darwin";
in {
  packageOverrides = super: {
    haskellPackages = super.haskellPackages.override haskellOverrides;

    opencv3_1 = super.callPackage ./opencv-3.1.0.nix
                                # ./opencv-HEAD.nix
    { gtk = super.gtk3;
      qt  = super.qt5;
      enableIpp     = !osx;
      enableContrib = true;
      enableBloat   = !osx;
      enableOpenGL  = true;
      enableQT      = false;
    };
  };
}
