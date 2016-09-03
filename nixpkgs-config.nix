{
  packageOverrides = pkgs:
    let
      haskellOverrides = {
        overrides = self: super: {
          inline-c = super.inline-c.override (args: args // {
            mkDerivation = expr: args.mkDerivation (expr // {
              version = "0.5.5.5";
              sha256 = "0c3jijav2iz9b5k1hzzidq7rbavj45rbbyrk93ybd2dagrj45lgk";
            });
          });
          opencv = /* TODO (BvD): Get this to work: pkgs.haskell.lib.buildStrictly */
                   (self.callPackage (import ./haskell-opencv.nix) {});
        };
      };
      osx = builtins.currentSystem == "x86_64-darwin";
    in {
      haskellPackages = pkgs.haskellPackages.override haskellOverrides;

      opencv3_1 = pkgs.callPackage ./opencv-3.1.0.nix
                                  # ./opencv-HEAD.nix
      { gtk = pkgs.gtk3;
        qt  = pkgs.qt5;
        enableIpp     = !osx;
        enableContrib = true;
        enableBloat   = !osx;
        enableOpenGL  = true;
        enableQT      = false;
      };
    };
}
