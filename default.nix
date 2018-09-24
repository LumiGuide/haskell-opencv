{ nixpkgs ? import ./nixpkgs.nix }:
import nixpkgs {
  overlays = [ (import ./overlay.nix) ];
}
