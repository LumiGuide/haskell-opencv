final : previous : with final.haskell.lib;
let
  haskellOverrides = self: super: {
      opencv = disableLibraryProfiling (doBenchmark (overrideCabal (super.callPackage ./opencv/opencv.nix {}) (drv : {
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
        libraryPkgconfigDepends = [ final.opencv4 ];
        shellHook = ''
          export hardeningDisable=bindnow
        '';
        # TODO: cabal2nix automatically adds:
        #
        #   configureFlags = ["--with-gcc=${stdenv.cc}/bin/c++" "--with-ld=${stdenv.cc}/bin/c++"];
        #
        # This is not needed anymore and will actually break the build.
        # So lets remove this from cabal2nix or ask @peti to do it.
        configureFlags = [];
        buildTools = (drv.buildTools or []) ++ [self.cabal-install self.stack];
      })));

      opencv-examples =
        overrideCabal (super.callCabal2nix "opencv-examples" ./opencv-examples {}) (_drv : {
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
        });

      opencv-extra =
        overrideCabal (super.callCabal2nix "opencv-extra" ./opencv-extra {}) (_drv : {
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
          configureFlags = [];
        });

      opencv-extra-examples =
        overrideCabal (super.callCabal2nix "opencv-extra-examples" ./opencv-extra-examples {}) (_drv : {
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
        });

      # TODO Remove when https://github.com/fpco/inline-c/pull/78 is available
      inline-c =
        overrideCabal super.inline-c (drv : {
          src = final.fetchgit {
            url = "https://github.com/fpco/inline-c.git";
            rev = "b5f93c71161891a901f48aea8db80417b057bc67";
            sha256 = "0lbrvscbhpbgcsfjfq4mm162s0yxdmwx9h024n8i1riliqpfxw56";
          };
          preCompileBuildDriver = "cd inline-c";
        });
  };
in  {
  haskell = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskellOverrides self super;
  };
}
