let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "0011f9065a1ad1da4db67bec8d535d91b0a78fba";
 sha256  = "0m662mibyxqmp83zdhsi084p2h90268h3i8bfk3b2q8pbjz89yx2";
}
