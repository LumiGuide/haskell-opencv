enableOpencv4 : final : previous : with final.lib; with final.haskell.lib;
let
  handleOpencv4 = drv : if enableOpencv4 then enableCabalFlag drv "opencv4" else drv;

  useOpencvHighgui = drv : overrideCabal drv (_drv: {
    libraryPkgconfigDepends = [
      (if enableOpencv4
       then final.opencv4_highgui
       else final.opencv3_highgui)
    ];
  });

  haskellOverrides = self: super:
    let
      addBuildToolsInShell = drv : overrideCabal drv (drv : optionalAttrs inNixShell {
        buildTools = (drv.buildTools or []) ++ (with self; [
          cabal-install
          stack
        ]);
      });
    in {
      opencv = (handleOpencv4 (addBuildToolsInShell (doBenchmark (overrideCabal (super.callCabal2nix "opencv" ./opencv {}) (drv : {
        src = final.runCommand "opencv-src"
          { files = final.lib.sourceByRegex ./opencv [
              "^src$"
              "^src/.*"
              "^include$"
              "^include/.*"
              "^test$"
              "^test/.*"
              "^bench$"
              "^bench/.*"
              "^opencv.cabal$"
              "^Setup.hs$"
            ];
            doc     = ./doc;
            data    = ./data;
            LICENSE = ./LICENSE;
          } ''
            mkdir -p $out
            cp -r $files/* $out
            cp -r $doc     $out/doc
            cp -r $data    $out/data
            cp $LICENSE    $out/LICENSE
          '';
        shellHook = ''
          export hardeningDisable=bindnow
        '';
      } // optionalAttrs enableOpencv4 {
        libraryPkgconfigDepends = [ final.opencv4 ];
      }))))).overrideAttrs (_oldAttrs: {
        # The following is to fix the following test-suite error:
        # ImgCodecs
        #   imencode . imdecode
        #     OutputJpeg2000:   FAIL
        #       Exception: BindingException OpenCV(3.4.6)
        #       /build/source/modules/imgcodecs/src/grfmt_jpeg2000.cpp:103:
        #       error: (-213:The function/feature is not implemented) imgcodecs:
        #       Jasper (JPEG-2000) codec is disabled.
        #       You can enable it via 'OPENCV_IO_ENABLE_JASPER' option.
        #       Refer for details and cautions here:
        #       https://github.com/opencv/opencv/issues/14058 in function 'initJasper'
        OPENCV_IO_ENABLE_JASPER = true;
      });

      opencv_highgui = useOpencvHighgui self.opencv;

      opencv-examples =
        (addBuildToolsInShell (overrideCabal (super.callCabal2nix "opencv-examples" ./opencv-examples {}) (_drv : {
          src = final.runCommand "opencv-examples-src"
            { files = final.lib.sourceByRegex ./opencv-examples [
                "^src$"
                "^src/.*"
                "^lib$"
                "^lib/.*"
                "^opencv-examples.cabal$"
              ];
              data = ./data;
              LICENSE = ./LICENSE;
           } ''
              mkdir -p $out
              cp -r $files/* $out
              cp -r $data    $out/data
              cp $LICENSE    $out/LICENSE
            '';
        }))).override { opencv = self.opencv_highgui; };

      opencv-extra =
        handleOpencv4 (addBuildToolsInShell (overrideCabal (super.callCabal2nix "opencv-extra" ./opencv-extra {}) (_drv : {
          src = final.runCommand "opencv-extra-src"
            { files = final.lib.sourceByRegex ./opencv-extra [
                "^include$"
                "^include/.*"
                "^src$"
                "^src/.*"
                "^opencv-extra.cabal$"
                "^Setup.hs$"
              ];
              doc     = ./doc;
              data    = ./data;
              LICENSE = ./LICENSE;
            } ''
              mkdir -p $out
              cp -r $files/* $out
              cp -r $doc     $out/doc
              cp -r $data    $out/data
              cp $LICENSE    $out/LICENSE
            '';
          shellHook = ''
            export hardeningDisable=bindnow
          '';
          # TODO (BvD): This should be added by cabal2nix. Fix this upstream.
          libraryPkgconfigDepends = [
            (if enableOpencv4
             then final.opencv4
             else final.opencv3)
          ];
        })));

      opencv-extra_highgui = (useOpencvHighgui self.opencv-extra).override {
        opencv = self.opencv_highgui;
      };

      opencv-extra-examples =
        (addBuildToolsInShell (overrideCabal (super.callCabal2nix "opencv-extra-examples" ./opencv-extra-examples {}) (_drv : {
          src = final.runCommand "opencv-extra-examples-src"
            { files = final.lib.sourceByRegex ./opencv-extra-examples [
                "^src$"
                "^src/.*"
                "^opencv-extra-examples.cabal$"
              ];
              data = ./data;
              LICENSE = ./LICENSE;
            } ''
              mkdir -p $out
              cp -r $files/* $out
              cp -r $data    $out/data
              cp $LICENSE    $out/LICENSE
            '';
        }))).override {
          opencv = self.opencv_highgui;
          opencv-extra = self.opencv-extra_highgui;
        };
  };

in  {
  haskell = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskellOverrides self super;
  };

  opencv3_highgui = previous.opencv3.override {
    enableGtk3 = !final.stdenv.isDarwin;
  };
  opencv4_highgui = previous.opencv4.override {
    enableGtk3 = !final.stdenv.isDarwin;
  };
}
