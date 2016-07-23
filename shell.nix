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
            version = "0.5.5.5";
            sha256 = "0c3jijav2iz9b5k1hzzidq7rbavj45rbbyrk93ybd2dagrj45lgk";
          });
        });
    };
  };

  drv = import ./default.nix pkgs haskellPackages;

in if pkgs.lib.inNixShell then drv.env else drv
