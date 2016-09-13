{
  packageOverrides = pkgs:
    let
      haskellOverrides = {
        overrides = self: super: {
          inline-c = super.inline-c.override (args: args // {
            mkDerivation = expr: args.mkDerivation (expr // {
              version = "0.5.5.7";
              sha256 = "1falgh6ahyr9yb4f8npz0hai5yy3bf0vza5s90al9ngf6wf5d85p";
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
