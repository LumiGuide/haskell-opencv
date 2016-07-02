pkgs :
let osx = builtins.currentSystem == "x86_64-darwin";
in pkgs.stdenv.lib.overrideDerivation
  (pkgs.callPackage ./opencv-3.1.0.nix {
    gtk = pkgs.gtk3;
    qt  = pkgs.qt5;
    enableIpp     = !osx;
    enableContrib = true;
    enableBloat   = !osx;
    enableOpenGL  = true;
    enableQT      = false;
  }) (super : {
    # Uncomment the following to use opencv-HEAD:

    # name = "opencv-HEAD";
    # src = pkgs.fetchgit {
    #   url = "https://github.com/Itseez/opencv.git";
    #   rev = "7dc5332a92f4843321d0b1c2039d881deda48999";
    #   sha256 = "0a94vg4xqx39bym7z8d0sz6c47qgalbzc9lw90dbdcf23k9rrcfv";
    # };
    # contribSrc = pkgs.fetchgit {
    #   url = "https://github.com/Itseez/opencv_contrib.git";
    #   rev = "51a4f6e415b7f7419927acecf9ef8aae8876d6e5";
    #   sha256 = "1shhyp2gzgj0g0875wrhmc93lkkr2faza647l33skxj5rh7n254w";
    # };

  })
