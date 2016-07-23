let
  haskellOverrides = {
    overrides = self: super: {
      inline-c = super.inline-c.override (args: args // {
        mkDerivation = expr: args.mkDerivation (expr // {
          version = "0.5.5.5";
          sha256 = "0c3jijav2iz9b5k1hzzidq7rbavj45rbbyrk93ybd2dagrj45lgk";
        });
      });
    };
  };
in {
  packageOverrides = super: {
    haskellPackages = super.haskellPackages.override haskellOverrides;
  };
}
