{ lib, stdenv, fetchurl, fetchgit
, cmake, cudatoolkit, gtk, qt, libjpeg, libpng, libtiff, jasper, ffmpeg
, pkgconfig, gstreamer, xineLib, glib, python27, python27Packages, unzip, doxygen, perl
, enableIpp ? false
, enableContrib ? false
, enableBloat   ? false
, enableOpenGL  ? false
, enableQT      ? false
, enableCuda    ? false
}:

let
  v = "HEAD";

  enabled = condition : if condition then "ON" else "OFF";

  contribSrc = fetchgit {
    url = "https://github.com/Itseez/opencv_contrib.git";
    rev = "159534a272955f04ad2bacf5afb4e1a17c5d82c3";
    sha256 = "1gl2d7kvfh2ls3zj5wk1zz97gps368n9bdhapdnbbvwni9q1y2vl";
  };
in

stdenv.mkDerivation rec {
  name = "opencv-${v}";
  src = fetchgit {
    url = "https://github.com/Itseez/opencv.git";
    rev = "1001b05def6d09051629755d9c9a2ec370b48a8d";
    sha256 = "1rh753r2dgwdjw3mla18cblvs6jgdcyzf4qqiigng72nrfhfjzf3";
  };

  preConfigure =
    let ippicv = fetchurl {
          url = "https://raw.githubusercontent.com/Itseez/opencv_3rdparty/${ippicvBinariesCommit}/ippicv/${ippicvName}";
          md5 = ippicvHash;
        };
        ippicvBinariesCommit = "81a676001ca8075ada498583e4166079e5744668";
        ippicvName           = "ippicv_linux_20151201.tgz";
        ippicvHash           = "808b791a6eac9ed78d32a7666804320e";
        ippicvArchive        = "3rdparty/ippicv/downloads/linux-${ippicvHash}/${ippicvName}";
    in stdenv.lib.optionalString enableIpp
      ''
        mkdir -p $(dirname ${ippicvArchive})
        ln -s ${ippicv}    ${ippicvArchive}
      '';

  # We move ippicv from the public to the private libraries. This ensures that
  # our Haskell binding doesn't try to link with it. This is not fully tested
  # yet so we might get linking errors in the future.
  postInstall = stdenv.lib.optionalString enableIpp ''
    sed -i "s|-lippicv||;s|Libs.private: |Libs.private: -L$out/share/OpenCV/3rdparty/lib -lippicv |" $out/lib/pkgconfig/opencv.pc
  '';

  patchPhase = ''
    sed -i "s|/usr/bin/perl|${perl}/bin/perl|" doc/Doxyfile.in
  '';

  postBuild = ''
    make doxygen
  '';

  buildInputs =
    [ unzip doxygen perl libjpeg libpng libtiff ]
    ++ lib.optionals enableBloat [ gtk glib jasper ffmpeg xineLib gstreamer python27 python27Packages.numpy ]
    ++ lib.optionals enableQT [ qt.base ]
    ++ lib.optionals enableCuda [ cudatoolkit ];

  nativeBuildInputs = [ cmake pkgconfig ];

  cmakeFlags = [
    "-DWITH_IPP=${enabled enableIpp}"
    "-DWITH_OPENGL=${enabled enableOpenGL}"
    "-DWITH_QT=${enabled enableQT}"
    "-DWITH_CUDA=${enabled enableCuda}"
  ] ++ stdenv.lib.optionals enableContrib [ "-DOPENCV_EXTRA_MODULES_PATH=${contribSrc}/modules" ];

  enableParallelBuilding = true;

  meta = {
    description = "Open Computer Vision Library with more than 500 algorithms";
    homepage = http://opencv.org/;
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [viric flosse];

    # TODO (BvD): Lets see if we can get this to built on OS X.
    #platforms = with stdenv.lib.platforms; linux;
  };
}
