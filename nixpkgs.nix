let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "89e02c7516ff301d28301926e9d7b373f29836fe";
 sha256  = "0v28hyyn1hw9951w1zvq5s40bbmk3gpjsmhq0cyp7lvs7d1aq0z7";
}
