{ rev
, sha256
, owner ? "NixOS"
, repo  ? "nixpkgs"
}:

let url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
in

if 0 <= builtins.compareVersions builtins.nixVersion "1.12"
then builtins.fetchTarball { inherit url sha256; }
else

# `builtins.fetchTarball` only accepts a `sha256` argument in Nix version 1.12 or later
with rec {
  system = builtins.currentSystem;

  tarball = import <nix/fetchurl.nix> { inherit url sha256; };

  builtin-paths = import <nix/config.nix>;

  script = builtins.toFile "nixpkgs-unpacker" ''
    "$coreutils/mkdir" "$out"
    cd "$out"
    "$gzip" --decompress < "$tarball" | "$tar" --extract --strip-components=1
  '';

  nixpkgs = builtins.derivation {
    name = "nixpkgs-${builtins.substring 0 6 rev}";

    builder = builtins.storePath builtin-paths.shell;

    args = [ script ];

    inherit tarball system;

    tar = builtins.storePath builtin-paths.tar;
    gzip = builtins.storePath builtin-paths.gzip;
    coreutils = builtins.storePath builtin-paths.coreutils;
  };
};

nixpkgs
