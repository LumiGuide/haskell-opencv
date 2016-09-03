{ lib, stdenv, fetchurl, fetchzip
, cmake, gtk, qt, libjpeg, libpng, libtiff, jasper, ffmpeg
, pkgconfig, gstreamer, xineLib, glib, python27, python27Packages, unzip, doxygen, perl
, enableIpp ? false
, enableContrib ? false
, enableBloat   ? false
, enableOpenGL  ? false
, enableQT      ? false
}:

let
  v = "3.1.0";

  enabled = condition : if condition then "ON" else "OFF";

  contribSrc = fetchzip {
    url = "https://github.com/Itseez/opencv_contrib/archive/${v}.tar.gz";
    sha256 = "153yx62f34gl3zd6vgxv0fj3wccwmq78lnawlda1f6xhrclg9bax";
    name = "opencv-contrib-${v}-src";
  };
in

stdenv.mkDerivation rec {
  name = "opencv-${v}";
  src = fetchurl {
    url = "https://github.com/Itseez/opencv/archive/${v}.zip";
    sha256 = "1912wrsb6nfl9fp7w9z3n0x04jcrv6k6zsa0zx7q10nvkwj90s8z";
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
    ++ lib.optionals enableQT [ qt.base ];

  nativeBuildInputs = [ cmake pkgconfig ];

  cmakeFlags = [
    "-DWITH_IPP=${enabled enableIpp}"
    "-DWITH_OPENGL=${enabled enableOpenGL}"
    "-DWITH_QT=${enabled enableQT}"
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
