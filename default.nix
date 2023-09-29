{ nixpkgs ? import ./nixpkgs.nix
, system ? builtins.currentSystem
}:
import nixpkgs {
  inherit system;
  overlays = [ (import ./overlay.nix) ];
}
