{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleExtractor ( extractExampleImages ) where

import "base" Control.Exception ( throw )
import "base" Control.Arrow ( second )
import "base" Control.Monad ( unless )
import "base" Data.Maybe ( mapMaybe )
import "base" Data.Monoid ( (<>) )
import "base" Data.Either ( partitionEithers )
import "directory" System.Directory ( canonicalizePath )
import "haskell-src-meta" Language.Haskell.Meta ( parseDecs )
import qualified "Glob" System.FilePath.Glob as G
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import qualified "thea" OpenCV as CV
import qualified "bytestring" Data.ByteString as B ( writeFile )
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax

render :: FilePath -> CV.Mat -> FilePath -> IO ()
render destDir img fp = do
    let bs = either throw id $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
        dest = destDir <> "/" <> fp <> ".png"
    putStr $ "Writing file " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"

extractExampleImages :: FilePath -> Q [Dec]
extractExampleImages srcDir = do
    (perFileExamples :: [(FilePath, [T.Text])]) <- runIO $ mapM (\p -> (p, ) <$> findExamples p) =<< haskellPaths srcDir

    mapM_ (\(fp, _) -> addDependentFile =<< runIO (canonicalizePath fp)) perFileExamples

    let perFileDecls :: [(FilePath, ([String], [[Dec]]))]
        perFileDecls = map (second $ partitionEithers . map (parseDecs . T.unpack . haddockToHaskell)) perFileExamples

        perFileErrors :: [(FilePath, [String])]
        perFileErrors = mapMaybe (\(fp, (errs, _)) -> if null errs then Nothing else Just (fp, errs)) perFileDecls

        perFileResults :: [(FilePath, [Dec])]
        perFileResults = map (\(fp, (_, ds)) -> (fp, concat ds)) perFileDecls

    unless (null perFileErrors) $
      error $ show perFileErrors

    let results = concatMap snd perFileResults
        imgNames = findImgDecls results

    mdecs <- mkMain imgNames
    pure $ results <> mdecs

mkMain :: [Name] -> Q [Dec]
mkMain names = [d|
    main :: IO ()
    main = mapM_ (\(img, fp) -> render "doc/generated" img fp) $(pure images)
    |]
  where
    images :: Exp
    images = ListE $ [TupE [VarE n, LitE $ StringL $ nameBase n] | n <- names]

findImgDecls :: [Dec] -> [Name]
findImgDecls = mapMaybe maybeImgDeclName
  where
    maybeImgDeclName :: Dec -> Maybe Name
    maybeImgDeclName (SigD n (ConT tn)) | tn == mkName "Mat" = Just n
    maybeImgDeclName _ = Nothing

haskellPaths :: FilePath -> IO [FilePath]
haskellPaths srcDir = do
  (paths, _) <- G.globDir [G.compile "**/*.hs", G.compile "**/*.hsc"] srcDir
  pure $ concat paths

haddockToHaskell :: T.Text -> T.Text
haddockToHaskell = T.filter (/= '\'') . T.replace "\\`" "`"

findExamples :: FilePath -> IO [T.Text]
findExamples = fmap parseExamples . T.readFile

parseExamples :: T.Text -> [T.Text]
parseExamples = findStart . T.lines
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
