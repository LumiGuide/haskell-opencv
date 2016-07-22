import Distribution.Simple ( defaultMainArgs )
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let args' | "configure" `elem` args = args ++ ["--with-gcc","g++", "--with-ld","g++"]
              | otherwise               = args
    defaultMainArgs args'
