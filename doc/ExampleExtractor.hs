{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExampleExtractor ( render, extractExampleImages ) where

import "base" Control.Exception
import "base" Control.Arrow
import "base" Control.Monad
import "base" Data.Either
import "base" Data.Maybe
import "base" Data.Monoid
import qualified "containers" Data.Map.Strict as M
import "directory" System.Directory ( canonicalizePath )
import "haskell-src-meta" Language.Haskell.Meta.Syntax.Translate ( toDecs )
import qualified "haskell-src-exts" Language.Haskell.Exts.Extension as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Parser as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Syntax as Hse
import qualified "Glob" System.FilePath.Glob as G
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import qualified "thea" OpenCV as CV
import qualified "bytestring" Data.ByteString as B ( writeFile )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------

render
    :: FilePath
    -> CV.Mat ('CV.S [height, width]) channels depth
    -> IO ()
render fp img = do
    let bs = either throw id $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
        dest = "doc/generated/" <> fp
    putStr $ "Writing file " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"

--------------------------------------------------------------------------------

-- | Haskell source code containing 0, 1 or more examples.
type ExampleSrc = T.Text

-- | A single line of Haskell source code.
type SrcLine = T.Text

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
    (extractedStuff :: [(FilePath, ([ExampleSrc], [RenderTarget]))]) <-
        runIO $ mapM (\p -> (p, ) <$> findExamples p) =<< haskellPaths srcDir

    mapM_ (\(fp, _) -> addDependentFile =<< runIO (canonicalizePath fp)) extractedStuff

    let perFileExampleSrcs :: [(FilePath, [ExampleSrc])]
        perFileExampleSrcs = [(fp, xs) | (fp, (xs, _)) <- extractedStuff]

        perFileExampleDecls :: [(FilePath, ([String], [[Hse.Decl]]))]
        perFileExampleDecls = map (second $ partitionEithers . map (parseDecsHse . T.unpack . haddockToHaskell)) perFileExampleSrcs

        perFileErrors :: [(FilePath, [String])]
        perFileErrors = mapMaybe (\(fp, (errs, _)) -> if null errs then Nothing else Just (fp, errs)) perFileExampleDecls

        perFileExamples :: [(FilePath, [Hse.Decl])]
        perFileExamples = map (\(fp, (_, ds)) -> (fp, concat ds)) perFileExampleDecls

        examplesHSE :: [Hse.Decl]
        examplesHSE = concatMap snd perFileExamples

        examplesTH :: [Dec]
        examplesTH = toDecs examplesHSE

        exampleMap :: M.Map Name Bool
        exampleMap = M.map typeIsIO $ M.fromList $ catMaybes $ map asSigD examplesTH

        renderTargets :: [RenderTarget]
        renderTargets = do
            (_, (_, xs)) <- extractedStuff
            renderTarget <- xs
            let isIO = M.findWithDefault False (rtSymbolName renderTarget) exampleMap
            pure renderTarget {rtSymbolIsIO = isIO}

    unless (null perFileErrors) $
      error $ show perFileErrors

    mdecs <- mkRenderExampleImages renderTargets
    pure $ examplesTH <> mdecs

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

parseDecsHse :: String -> Either String [Hse.Decl]
parseDecsHse str =
    case Hse.parseModuleWithMode parseMode str of
      Hse.ParseFailed _srcLoc err -> Left err
      Hse.ParseOk (Hse.Module _ _ _ _ _ _ decls) -> Right decls

parseMode :: Hse.ParseMode
parseMode =
    Hse.ParseMode
    { Hse.parseFilename         = ""
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

haskellPaths :: FilePath -> IO [FilePath]
haskellPaths srcDir = do
  (paths, _) <- G.globDir [G.compile "**/*.hs", G.compile "**/*.hsc"] srcDir
  pure $ concat paths

haddockToHaskell :: T.Text -> T.Text
haddockToHaskell =
    T.replace "\\`" "`"
  . T.replace "\\<" "<"

findExamples :: FilePath -> IO ([ExampleSrc], [RenderTarget])
findExamples fp = ((parseExamples &&& parseGeneratedImages) . T.lines) <$> T.readFile fp

parseExamples :: [SrcLine] -> [ExampleSrc]
parseExamples = findStart
  where
    findStart :: [SrcLine] -> [ExampleSrc]
    findStart      []  = []
    findStart   (_:[]) = []
    findStart (_:_:[]) = []
    findStart ("Example:":"":"@":ls) = findEnd [] ls
    findStart (_:ls) = findStart ls

    findEnd :: [SrcLine] -> [SrcLine] -> [ExampleSrc]
    findEnd _acc [] = []
    findEnd acc ("@":ls) = T.unlines (reverse acc) : findStart ls
    findEnd acc (l  :ls) = findEnd (l:acc) ls

parseGeneratedImages :: [SrcLine] -> [RenderTarget]
parseGeneratedImages = concatMap parseLine
  where
    parseLine :: SrcLine -> [RenderTarget]
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
