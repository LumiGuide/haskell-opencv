let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "66b63d2f5a442d642ac2b226d71b4f97bafce5c8";
 sha256  = "0vss6g2gsirl2ds3zaxwv9sc6q6x3zc68431z1wz3wpbhpw190p5";
}
