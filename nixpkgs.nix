let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "0e00bc51eaa3f86c7b435f6f04219dafe51ac52f";
 sha256  = "0ncfr44xrc6r9ylbrh1y7w8qshvnjjlk521pa6wyhnz53dwczkb0";
}
