{
  packageOverrides = pkgs:
    let
      haskellOverrides = {
        overrides = self: super: {
          opencv = /* TODO (BvD): Get this to work: pkgs.haskell.lib.buildStrictly */
                   (self.callPackage (import ./haskell-opencv.nix) {});
        };
      };
      osx = builtins.currentSystem == "x86_64-darwin";
    in {
      haskellPackages = pkgs.haskellPackages.override haskellOverrides;

      # opencv3 = pkgs.callPackage ./opencv-3.2.0.nix
      # { enableGtk3    = true;
      #   enableContrib = true;
      #   enableIpp     = !osx;
      # };
    };
}
