let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "797d4d4a3f38244ae5e5b5b55b7304ab6e75fe51";
 sha256  = "0hbrcmyq2dpgynhid702gxlbcbhf4ljjp2rii56lr7fyybnj1fc7";
}
