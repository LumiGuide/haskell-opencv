import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.PackageDescription ( PackageDescription )
import Distribution.Types.LocalBuildInfo ( LocalBuildInfo )
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let args' | "configure" `elem` args = args ++ ["--with-gcc","c++", "--with-ld","c++"]
              | otherwise               = args
    defaultMainWithHooksArgs userHooks args'
  where
    userHooks :: UserHooks
    userHooks = simpleUserHooks{ haddockHook = myHaddockHook }

    myHaddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
    myHaddockHook pkgDesc lbi uk haddockFlags =
        haddockHook simpleUserHooks pkgDesc lbi uk myHaddockFlags
      where
        myHaddockFlags :: HaddockFlags
        myHaddockFlags = haddockFlags{ haddockProgramArgs = myHaddockProgramArgs}

        myHaddockProgramArgs :: [(String, [String])]
        myHaddockProgramArgs = haddockProgramArgs haddockFlags ++
          [ ("--optghc", ["-pgmc=c++"]) ]
