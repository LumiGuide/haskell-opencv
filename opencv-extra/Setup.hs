import Distribution.Simple ( defaultMainWithHooksArgs, simpleUserHooks )
import System.Environment ( getArgs )


-- Source copied from: https://hackage.haskell.org/package/cabal-pkg-config-version-hook-0.1.0.1/docs/src/Distribution.PkgConfigVersionHook.html#addHook
-- TODO: Import this via `setup-depends` instead once it's on Stackage.

import Control.Lens ((%~), (^.))
import Control.Monad (when)
import qualified Data.Char as C
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as L
import Distribution.Simple (UserHooks (confHook))
import Distribution.Simple.Setup (ConfigFlags, configConfigurationsFlags)
import Distribution.Types.BuildInfo.Lens (ccOptions, cppOptions, cxxOptions)
import Distribution.Types.Flag (flagName, mkFlagAssignment, mkFlagName, unFlagName)
import Distribution.Types.GenericPackageDescription.Lens
  ( GenericPackageDescription,
    condBenchmarks,
    condExecutables,
    condForeignLibs,
    condLibrary,
    condSubLibraries,
    condTestSuites,
    genPackageFlags,
  )
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import qualified Text.ParserCombinators.ReadP as P
import Prelude hiding (log)

-- | Hook into Cabal to provide pkg-config metadata. Can be applied multiple
-- times to support multiple packages.
addHook :: Settings -> UserHooks -> UserHooks
addHook settings hooks = hooks {confHook = composeConfHook settings (confHook hooks)}

-- | How the metadata for a pkg-config package should be made available to the
-- cabal file.
data Settings = Settings
  { -- | Name of the package; used for querying pkg-config.
    pkgConfigName :: String,
    -- | Name to use in the Haskell CPP and C/C++ preprocessor macros.
    --
    -- For example, `pkgConfigName = "FOO"` will set the macros
    --
    --  * @FOO_MAJOR@
    --
    --  * @FOO_MINOR@
    --
    --  * @FOO_PATCH@
    --
    --  * @FOO_IS_AT_LEAST(major, minor, patch)@
    macroName :: String,
    -- | Name to use when setting flag values in the cabal file.
    --
    -- Flags named with this prefix, followed by a dash, followed by a major version number, an underscore and a minor version number will be set when the detected package is at least that version.
    flagPrefixName :: String
  }

-- | Derive a default 'Settings' value from just a pkg-config package name.
mkSettings :: String -> Settings
mkSettings name =
  Settings
    { pkgConfigName = name,
      macroName = map (\c -> case c of '-' -> '_'; x -> x) name,
      flagPrefixName = name
    }

-- | Extend the value of 'confHook'. It's what powers 'addHook'.
composeConfHook ::
  Settings ->
  ((GenericPackageDescription, a) -> ConfigFlags -> IO b) ->
  (GenericPackageDescription, a) ->
  Distribution.Simple.Setup.ConfigFlags ->
  IO b
composeConfHook settings origHook = \(genericPackageDescription, hookedBuildInfo) confFlags -> do
  (actualMajor, actualMinor, actualPatch) <- getPkgConfigPackageVersion (pkgConfigName settings)

  let defines =
        [ "-D" <> macroName settings <> "_MAJOR=" <> show actualMajor,
          "-D" <> macroName settings <> "_MINOR=" <> show actualMinor,
          "-D" <> macroName settings <> "_PATCH=" <> show actualPatch,
          "-D" <> macroName settings <> "_IS_AT_LEAST(a,b,c)=(" <> show actualMajor <> ">a||(" <> show actualMajor <> "==a&&(" <> show actualMinor <> ">b||(" <> show actualMinor <> "==b&&" <> show actualPatch <> ">=c))))"
        ]
      extraFlags =
        [ (mkFlagName (flagPrefixName settings ++ "-" ++ show major ++ "_" ++ show minor), (actualMajor, actualMinor) >= (major, minor))
          | declaredFlag <- genericPackageDescription ^. genPackageFlags,
            let rawName = unFlagName $ flagName declaredFlag,
            rawVersion <- L.stripPrefix (flagPrefixName settings ++ "-") rawName & toList,
            [major, minor] <- unambiguously parseFlagVersion rawVersion & toList
        ]
      setDefines comp x =
        x
          & comp . cppOptions %~ (<> defines)
          & comp . ccOptions %~ (<> defines)
          & comp . cxxOptions %~ (<> defines)
      genericPackageDescription' =
        genericPackageDescription
          & setDefines (condLibrary . traverse . traverse)
          & setDefines (condSubLibraries . traverse . traverse . traverse)
          & setDefines (condForeignLibs . traverse . traverse . traverse)
          & setDefines (condExecutables . traverse . traverse . traverse)
          & setDefines (condTestSuites . traverse . traverse . traverse)
          & setDefines (condBenchmarks . traverse . traverse . traverse)

      configConfigurationsFlags' = configConfigurationsFlags confFlags `mappend` mkFlagAssignment extraFlags
      confFlags' =
        confFlags
          { configConfigurationsFlags = configConfigurationsFlags'
          }
  origHook (genericPackageDescription', hookedBuildInfo) confFlags'

parseVersion :: P.ReadP [Int]
parseVersion = do
  map read <$> do
    P.many1 (P.satisfy C.isDigit) `P.sepBy` P.char '.'

parseFlagVersion :: P.ReadP [Int]
parseFlagVersion =
  map read <$> do
    P.many1 (P.satisfy C.isDigit) `P.sepBy` P.char '_'

unambiguously :: P.ReadP a -> String -> Maybe a
unambiguously p s =
  case filter (\(_a, x) -> x == "") $ P.readP_to_S p s of
    [(v, _)] -> Just v
    _ -> Nothing

getPkgConfigPackageVersion :: String -> IO (Int, Int, Int)
getPkgConfigPackageVersion pkgName = do
  s <- readProcess "pkg-config" ["--modversion", pkgName] ""
  case L.sortOn (\(_, remainder) -> length remainder) $ P.readP_to_S parseVersion s of
    [] -> error ("Could not parse version " ++ show s ++ " returned by pkg-config for package " ++ pkgName)
    (v, r) : _ -> do
      when (L.dropWhile C.isSpace r /= "") $ do
        log ("ignoring trailing text " ++ show r ++ " in version " ++ show s ++ " of pkg-config package " ++ pkgName)
      let v' = v ++ L.repeat 0
      pure (v' L.!! 0, v' L.!! 1, v' L.!! 2)

-- Should probably use a Cabal function?
log :: String -> IO ()
log = hPutStrLn stderr

-- End of source copied from: https://hackage.haskell.org/package/cabal-pkg-config-version-hook

hooks =
  simpleUserHooks &
    addHook
      (mkSettings "opencv4")
        { macroName = "SETUP_HS_OPENCV4_VERSION",
          flagPrefixName = "setup-hs-opencv4-version"
        }


main = do
    args <- getArgs
    let args' | "configure" `elem` args = args ++ ["--with-gcc","c++"]
              | otherwise               = args
    defaultMainWithHooksArgs hooks args'
