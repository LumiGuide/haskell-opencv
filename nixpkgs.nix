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
    # # opencv: 3.3.0 -> 3.3.1
    # (pkgs.fetchpatch {
    #   url = "https://github.com/NixOS/nixpkgs/commit/929d3d43e2252299a790cd9e53f7154db8848bd6.patch";
    #   sha256 = "08vlx44ndfl0l321qd0302p2fy9qf7qma0ach1iwm5vqc8jhlgcr";
    # })
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
