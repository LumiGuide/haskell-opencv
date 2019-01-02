enableOpencv4 : final : previous : with final.lib; with final.haskell.lib;
let
  handleOpencv4 = drv : if enableOpencv4 then enableCabalFlag drv "opencv4" else drv;

  haskellOverrides = self: super:
    let
      addBuildToolsInShell = drv : overrideCabal drv (drv : optionalAttrs inNixShell {
        buildTools = (drv.buildTools or []) ++ (with self; [
          cabal-install
          stack
        ]);
      });
    in {
      opencv = handleOpencv4 (addBuildToolsInShell (doBenchmark (overrideCabal (super.callCabal2nix "opencv" ./opencv {}) (drv : {
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
      }))));

      opencv-examples =
        addBuildToolsInShell (overrideCabal (super.callCabal2nix "opencv-examples" ./opencv-examples {}) (_drv : {
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
        }));

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
          libraryPkgconfigDepends =
            if enableOpencv4
            then [ final.opencv4 ]
            else [ final.opencv3 ];
        })));

      opencv-extra-examples =
        addBuildToolsInShell (overrideCabal (super.callCabal2nix "opencv-extra-examples" ./opencv-extra-examples {}) (_drv : {
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
        }));
  };
in  {
  haskell = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskellOverrides self super;
  };
}
