{ nixpkgs ? import ./nixpkgs.nix, enableOpencv4 ? false }:
import nixpkgs {
  overlays = [ (import ./overlay.nix enableOpencv4) ];
}
