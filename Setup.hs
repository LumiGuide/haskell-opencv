import Distribution.Simple
import System.Environment
import System.IO

main =
    do args <- getArgs
       let args' = if "configure" `elem` args then args ++ ["--with-gcc","g++","--with-ld","g++"] else args
       defaultMainArgs args'
