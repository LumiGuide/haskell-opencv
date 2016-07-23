{ nixpkgs ? import <nixpkgs> {config = import ./nixpkgs-config.nix;}
, compiler ? "default"
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  drv = import ./default.nix pkgs haskellPackages;

in if pkgs.lib.inNixShell then drv.env else drv
