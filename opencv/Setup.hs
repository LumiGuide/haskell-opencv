import Distribution.Simple ( defaultMainArgs )
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let args' | "configure" `elem` args = args ++ ["--with-gcc","c++" ]
              | otherwise               = args
    defaultMainArgs args'
