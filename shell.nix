{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  superHaskellPackages = if compiler == "default"
                         then pkgs.haskellPackages
                         else pkgs.haskell.packages.${compiler};

  haskellPackages = superHaskellPackages.override {
    overrides = self: super: {
      inline-c = super.inline-c.override (args: args // {
          mkDerivation = expr: args.mkDerivation (expr // {
            src = pkgs.fetchgit {
              url    = "https://github.com/fpco/inline-c.git";
              rev    = "24020e1cd4d9196fde125ea7e957d633d5b49717";
              sha256 = "0221w2pr136harxdql0649ilbn4yncb324q9jnhdg1xfch4shxbz";
            };
          });
        });
    };
  };

  drv = import ./default.nix pkgs haskellPackages;

in if pkgs.lib.inNixShell then drv.env else drv
