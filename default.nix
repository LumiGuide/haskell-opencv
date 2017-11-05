import (import ./nixpkgs.nix) {
  overlays = [ (import ./overlay.nix) ];
}
