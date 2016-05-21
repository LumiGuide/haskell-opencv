{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module ExampleExtractor ( render, extractExampleImages ) where

import "base" Control.Arrow
import "base" Control.Monad
import "base" Data.Either
import "base" Data.Maybe
import "base" Data.Monoid
import qualified "containers" Data.Map.Strict as M
import "directory" System.Directory ( canonicalizePath )
import qualified "haskell-src-exts" Language.Haskell.Exts.Extension as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Parser as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Syntax as Hse
import qualified "Glob" System.FilePath.Glob as G
import qualified "opencv" OpenCV as CV
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import qualified "bytestring" Data.ByteString as B ( writeFile )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import "this" Language.Haskell.Meta.Syntax.Translate ( toDecs )

--------------------------------------------------------------------------------

render
    :: FilePath
    -> CV.Mat ('CV.S [height, width]) channels depth
    -> IO ()
render fp img = do
    let bs = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
        dest = "doc/generated/" <> fp
    putStr $ "Writing file " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"

--------------------------------------------------------------------------------

data SrcLoc
   = SrcLoc
     { locFile :: !FilePath
     , locLine :: !Int
     }

-- | Haskell source code containing 0, 1 or more examples.
data ExampleSrc
   = ExampleSrc
     { exsLoc :: !SrcLoc
     , exsSrc :: !T.Text
     }

data ParsedExampleSrc
   = ParsedExampleSrc
     { pexsLoc   :: !SrcLoc
     , pexsDecls :: ![Dec]
     }

-- | A single line of Haskell source code.
data SrcLine
   = SrcLine
     { srcLoc  :: !SrcLoc
     , srcLine :: !T.Text
     }

data SymbolType
   = SymImage
   | SymImageAction
     deriving (Show, Eq)

data RenderTarget
   = RenderTarget
     { rtDestination :: !FilePath
       -- ^ Relative path where the symbol must be rendered as an image file.
     , rtSymbolName :: !Name
       -- ^ Name of a top level symbol (function or CAF) that is either an image
       -- or an IO action that yields an image.
     , rtSymbolIsIO :: !Bool
     } deriving Show

--------------------------------------------------------------------------------

extractExampleImages :: FilePath -> Q [Dec]
extractExampleImages srcDir = do
    haskellPaths <- runIO $ findHaskellPaths srcDir
    mapM_ (addDependentFile <=< runIO . canonicalizePath) haskellPaths

    ((exampleSrcs, renderTargets) :: ([ExampleSrc], [RenderTarget])) <- runIO $ do
      xs <- mapM findExamples haskellPaths
      pure $ (concat *** concat) $ unzip xs

    let parseErrors       :: [String]
        parsedExampleSrcs :: [ParsedExampleSrc]
        (parseErrors, parsedExampleSrcs) = partitionEithers $ map parseExampleSrc exampleSrcs

        examplesTH :: [Dec]
        examplesTH = concatMap (\pexs -> parsedExampleLinePragma pexs : pexsDecls pexs)
                               parsedExampleSrcs

        exampleMap :: M.Map Name Bool
        exampleMap = M.map typeIsIO $ M.fromList $ mapMaybe asSigD examplesTH

        renderTargets' :: [RenderTarget]
        renderTargets' = do
            renderTarget <- renderTargets
            let isIO = M.findWithDefault False (rtSymbolName renderTarget) exampleMap
            pure renderTarget {rtSymbolIsIO = isIO}

    unless (null parseErrors) $
      error $ show parseErrors

    mdecs <- mkRenderExampleImages renderTargets'
    pure $ examplesTH <> mdecs

parsedExampleLinePragma :: ParsedExampleSrc -> Dec
parsedExampleLinePragma pexs =
    PragmaD $ LineP (locLine loc) (locFile loc)
  where
    loc = pexsLoc pexs

parseExampleSrc :: ExampleSrc -> Either String ParsedExampleSrc
parseExampleSrc exs =
    case parseDecsHse (locFile $ exsLoc exs) $ T.unpack $ haddockToHaskell $ exsSrc exs of
      Left errMsg -> Left $ (locFile $ exsLoc exs) <> ": " <> errMsg
      Right decls -> Right
                     ParsedExampleSrc
                     { pexsLoc   = exsLoc exs
                     , pexsDecls = toDecs decls
                     }


asSigD :: Dec -> Maybe (Name, Type)
asSigD (SigD n t) = Just (n, t)
asSigD _ = Nothing

-- Really hacky way of determining whether some type has IO on a left most
-- position.
typeIsIO :: Type -> Bool
typeIsIO (ForallT _ _ t) = typeIsIO t
typeIsIO (AppT t1 _)     = typeIsIO t1
typeIsIO (VarT _)        = False
typeIsIO (ConT n) | nameBase n == nameBase ''IO = True
typeIsIO (PromotedT _)   = False
typeIsIO _               = False

parseDecsHse :: String -> String -> Either String [Hse.Decl]
parseDecsHse fileName str =
    case Hse.parseModuleWithMode (parseMode fileName) str of
      Hse.ParseFailed _srcLoc err -> Left err
      Hse.ParseOk (Hse.Module _ _ _ _ _ _ decls) -> Right decls

parseMode :: String -> Hse.ParseMode
parseMode fileName =
    Hse.ParseMode
    { Hse.parseFilename         = fileName
    , Hse.baseLanguage          = Hse.Haskell2010
    , Hse.extensions            = map Hse.EnableExtension exts
    , Hse.ignoreLanguagePragmas = False
    , Hse.ignoreLinePragmas     = False
    , Hse.fixities              = Nothing
    , Hse.ignoreFunctionArity   = False
    }
  where
    exts :: [Hse.KnownExtension]
    exts =
      [ Hse.BangPatterns
      , Hse.DataKinds
      , Hse.FlexibleContexts
      , Hse.LambdaCase
      , Hse.OverloadedStrings
      , Hse.PackageImports
      , Hse.PolyKinds
      , Hse.ScopedTypeVariables
      , Hse.TupleSections
      , Hse.TypeFamilies
      , Hse.TypeOperators
      , Hse.PostfixOperators
      , Hse.QuasiQuotes
      , Hse.UnicodeSyntax
      , Hse.MagicHash
      , Hse.PatternSignatures
      , Hse.MultiParamTypeClasses
      , Hse.RankNTypes
      ]

-- | Generate code for every render target
--
-- Executing the generated code will actually render the target.
mkRenderExampleImages :: [RenderTarget] -> Q [Dec]
mkRenderExampleImages renderTargets = [d|
    renderExampleImages :: IO ()
    renderExampleImages = $(pure doRender)
    |]
  where
    doRender :: Exp
    doRender = DoE [ if rtSymbolIsIO rt
                     then NoBindS $ VarE '(>>=) `AppE` sym `AppE` (VarE 'render `AppE` fp)
                     else NoBindS $ VarE 'render `AppE` fp `AppE` sym
                   | rt <- renderTargets
                   , let sym = VarE $ rtSymbolName rt
                         fp  = LitE $ StringL $ "examples/" <> rtDestination rt
                   ]

findHaskellPaths :: FilePath -> IO [FilePath]
findHaskellPaths srcDir = do
  (paths, _) <- G.globDir [G.compile "**/*.hs", G.compile "**/*.hsc"] srcDir
  pure $ concat paths

haddockToHaskell :: T.Text -> T.Text
haddockToHaskell =
    T.replace "\\`" "`"
  . T.replace "\\<" "<"
  . T.replace "\\/" "/"

findExamples :: FilePath -> IO ([ExampleSrc], [RenderTarget])
findExamples fp = ((parseExamples &&& parseGeneratedImages) . textToSource fp) <$> T.readFile fp

textToSource :: FilePath -> T.Text -> [SrcLine]
textToSource fp txt = zipWith lineToSource [1..] (T.lines txt)
  where
    lineToSource :: Int -> T.Text -> SrcLine
    lineToSource n line =
        SrcLine
        { srcLoc  = SrcLoc {locFile = fp, locLine = n}
        , srcLine = line
        }

parseExamples :: [SrcLine] -> [ExampleSrc]
parseExamples = findStart
  where
    findStart :: [SrcLine] -> [ExampleSrc]
    findStart      []  = []
    findStart   (_:[]) = []
    findStart (_:_:[]) = []
    findStart (a:b:c:ls)
              |    srcLine a == "Example:"
                && srcLine b == ""
                && srcLine c == "@"
                     = findEnd [] ls
    findStart (_:ls) = findStart ls

    findEnd :: [SrcLine] -> [SrcLine] -> [ExampleSrc]
    findEnd _acc [] = []
    findEnd acc (l:ls)
        | srcLine l == "@" =
            case reverse acc of
              []    -> findStart ls
              revAcc@(firstLine:_) ->
                  let exs = ExampleSrc
                            { exsLoc = srcLoc firstLine
                            , exsSrc = T.unlines (map srcLine revAcc)
                            }
                  in exs : findStart ls
        | otherwise = findEnd (l:acc) ls

parseGeneratedImages :: [SrcLine] -> [RenderTarget]
parseGeneratedImages = concatMap $ parseLine . srcLine
  where
    parseLine :: T.Text -> [RenderTarget]
    parseLine line = maybeToList $ do
      let fromPrefix = snd $ T.breakOn prefix line
      rest <- T.stripPrefix prefix fromPrefix
      case take 2 $ T.words rest of
        [fp, funcName] ->
            pure RenderTarget
                 { rtDestination = T.unpack $ fp
                 , rtSymbolName = mkName $ T.unpack $ fromMaybe funcName (T.stripSuffix ">>" funcName)
                   -- Later on we will check whether the symbol is actually (or likely) IO.
                 , rtSymbolIsIO = False
                 }
        _ -> Nothing

    prefix = "<<doc/generated/examples/"
