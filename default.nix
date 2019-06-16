{ nixpkgs ? import ./nixpkgs.nix, enableOpencv4 ? false
, system ? builtins.currentSystem
}:
import nixpkgs {
  inherit system;
  overlays = [ (import ./overlay.nix enableOpencv4) ];
}
