# This expression returns the nix/store path to our version of nixpkgs.
# It ensures that all engineers use the same revision of nixpkgs.
#
# See: https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgsVersion = import ./nixpkgs-version.nix;

  nixpkgs = fetchNixpkgs nixpkgsVersion;

  pkgs = import nixpkgs {};

  patches = [
  ];

in if builtins.length patches == 0
   then nixpkgs
   else pkgs.runCommand ("nixpkgs-" + builtins.substring 0 6 nixpkgsVersion.rev + "-patched")
          {inherit nixpkgs patches; } ''
          cp -r $nixpkgs $out
          chmod -R +w $out
          for p in $patches ; do
            echo "Applying patch $p"
            patch -d $out -p1 < "$p"
          done
        ''
