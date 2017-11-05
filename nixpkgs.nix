let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "cdbe4671bef1c0ba6345c51270472998bd44e266";
 sha256  = "0g6296b36ig48j785cpfbc0avzk1fjlba1izbagjmk30ssgw6vk9";
}
