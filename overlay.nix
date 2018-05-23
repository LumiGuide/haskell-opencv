final : previous : with final.haskell.lib;
let
  haskellOverrides = {
    overrides = self: super: {
      opencv = overrideCabal (super.callCabal2nix "opencv" ./opencv {}) (_drv : {
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
        libraryPkgconfigDepends = [ final.opencv3 ];
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
      });

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
          libraryPkgconfigDepends = [ final.opencv3 ];
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

    };
  };
in  {
  haskellPackages = previous.haskellPackages.override haskellOverrides;
  haskell = previous.haskell // {
    packages = previous.haskell.packages // {
      ghc802 = previous.haskell.packages.ghc802.override haskellOverrides;
      ghc822 = previous.haskell.packages.ghc822.override haskellOverrides;
      ghc842 = previous.haskell.packages.ghc842.override {
        overrides = self: super:
          haskellOverrides.overrides self super // (with previous.haskell.lib; {
            criterion = super.criterion_1_4_1_0;
            base-compat-batteries = doJailbreak super.base-compat-batteries;
            base-compat = super.base-compat_0_10_1;
          });
      };
    };
  };
}
