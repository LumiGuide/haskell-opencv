{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExampleExtractor ( render, extractExampleImages ) where

import "base" Control.Exception ( throw )
import "base" Control.Arrow ( (&&&), second )
import "base" Control.Monad ( unless )
import "base" Data.Either ( partitionEithers )
import "base" Data.Maybe ( mapMaybe, maybeToList, fromMaybe )
import "base" Data.Monoid ( (<>) )
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

render
    :: CV.Mat ('CV.S [height, width]) channels depth
    -> FilePath
    -> IO ()
render img fp = do
    let bs = either throw id $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
        dest = "doc/generated/" <> fp
    putStr $ "Writing file " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"

extractExampleImages :: FilePath -> Q [Dec]
extractExampleImages srcDir = do
    (extractedStuff :: [(FilePath, ([T.Text], [(T.Text, T.Text)]))]) <- runIO $ mapM (\p -> (p, ) <$> findExamples p) =<< haskellPaths srcDir

    mapM_ (\(fp, _) -> addDependentFile =<< runIO (canonicalizePath fp)) extractedStuff

    let perFileExamples :: [(FilePath, [T.Text])]
        perFileExamples = [(fp, xs) | (fp, (xs, _)) <- extractedStuff]

        renderTargets :: [(FilePath, Name)]
        renderTargets = do
            (_, (_, xs)) <- extractedStuff
            (dst, func) <- xs
            pure (T.unpack dst, mkName $ T.unpack func)

        perFileDecls :: [(FilePath, ([String], [[Hse.Decl]]))]
        perFileDecls = map (second $ partitionEithers . map (parseDecsHse . T.unpack . haddockToHaskell)) perFileExamples

        perFileErrors :: [(FilePath, [String])]
        perFileErrors = mapMaybe (\(fp, (errs, _)) -> if null errs then Nothing else Just (fp, errs)) perFileDecls

        perFileResults :: [(FilePath, [Hse.Decl])]
        perFileResults = map (\(fp, (_, ds)) -> (fp, concat ds)) perFileDecls

        results :: [Hse.Decl]
        results = concatMap snd perFileResults

        resultsTH :: [Dec]
        resultsTH = toDecs results

    unless (null perFileErrors) $
      error $ show perFileErrors

    -- -- runIO $ mapM_ (\r -> print r >> putStrLn "") $ filter isSig $ results

    mdecs <- mkRenderExampleImages renderTargets
    pure $ resultsTH <> mdecs

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

mkRenderExampleImages :: [(FilePath, Name)] -> Q [Dec]
mkRenderExampleImages renderTargets = [d|
    renderExampleImages :: IO ()
    renderExampleImages = $(pure doRender)
    |]
  where
    doRender :: Exp
    doRender = DoE [ NoBindS $ VarE 'render `AppE` img `AppE` fp
                   | (dst, n) <- renderTargets
                   , let img = VarE n
                         fp  = LitE $ StringL $ "examples/" <> dst
                   ]

haskellPaths :: FilePath -> IO [FilePath]
haskellPaths srcDir = do
  (paths, _) <- G.globDir [G.compile "**/*.hs", G.compile "**/*.hsc"] srcDir
  pure $ concat paths

haddockToHaskell :: T.Text -> T.Text
haddockToHaskell = T.replace "\\`" "`"

findExamples :: FilePath -> IO ([T.Text], [(T.Text, T.Text)])
findExamples fp = ((parseExamples &&& parseGeneratedImages) . T.lines) <$> T.readFile fp

parseExamples :: [T.Text] -> [T.Text]
parseExamples = findStart
  where
    findStart :: [T.Text] -> [T.Text]
    findStart      []  = []
    findStart   (_:[]) = []
    findStart (_:_:[]) = []
    findStart ("Example:":"":"@":ls) = findEnd [] ls
    findStart (_:ls) = findStart ls

    findEnd :: [T.Text] -> [T.Text] -> [T.Text]
    findEnd _acc [] = []
    findEnd acc ("@":ls) = T.unlines (reverse acc) : findStart ls
    findEnd acc (l  :ls) = findEnd (l:acc) ls

parseGeneratedImages :: [T.Text] -> [(T.Text, T.Text)]
parseGeneratedImages = concatMap parseLine
  where
    parseLine :: T.Text -> [(T.Text, T.Text)]
    parseLine line = maybeToList $ do
      let fromPrefix = snd $ T.breakOn prefix line
      rest <- T.stripPrefix prefix fromPrefix
      case take 2 $ T.words rest of
        [fp, funcName] -> pure ( fp
                               , fromMaybe funcName (T.stripSuffix ">>" funcName)
                               )
        _ -> Nothing

    prefix = "<<doc/generated/examples/"
