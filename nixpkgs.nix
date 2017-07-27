let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "229b2492819ff01f1b758d485813b7ebc91914b8";
 sha256  = "1brm9krvj4sf2h9i9w66ishnkclxqvngs6nwj4638y7hy15vhsyi";
}
