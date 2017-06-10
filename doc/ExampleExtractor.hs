{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module ExampleExtractor
  ( Animation

  , renderImage
  , renderAnimation

  , extractExampleImages
  ) where

import "base" Control.Arrow ( (***), (&&&) )
import "base" Control.Monad
import "base" Data.Bifunctor
import "base" Data.Either
import "base" Data.Maybe
import "base" Data.Monoid
import "base" Data.Word
import qualified "containers" Data.Map.Strict as M
import "directory" System.Directory ( canonicalizePath )
import qualified "haskell-src-exts" Language.Haskell.Exts.Extension as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Parser as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.Syntax as Hse
import qualified "haskell-src-exts" Language.Haskell.Exts.SrcLoc as Hse
import qualified "Glob" System.FilePath.Glob as G
import "JuicyPixels" Codec.Picture.ColorQuant as JP
import "JuicyPixels" Codec.Picture.Gif        as JP
import "JuicyPixels" Codec.Picture.Types      as JP
import qualified "opencv" OpenCV as CV
import qualified "opencv" OpenCV.Juicy as CVJ
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Lazy as BL
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import "this" Language.Haskell.Meta.Syntax.Translate ( toDecs )

--------------------------------------------------------------------------------

-- An animation is a list of images. Each image has a duration
-- specified in hundreths of a second.
type Animation shape channels depth = [(Int, CV.Mat shape channels depth)]

--------------------------------------------------------------------------------

renderImage
    :: FilePath
    -> CV.Mat ('CV.S [height, width]) channels depth
    -> IO ()
renderImage fp img = do
    let bs = CV.exceptError $ CV.imencode (CV.OutputPng CV.defaultPngParams) img
    putStr $ "Writing image " <> dest <> " ..."
    B.writeFile dest bs
    putStrLn " OK"
  where
    dest = mkDestPath fp

renderAnimation
    :: FilePath
    -> Animation ('CV.S [height, width]) ('CV.S 3) ('CV.S Word8)
    -> IO ()
renderAnimation fp imgs = do
    putStr $ "Writing animation " <> dest <> " ..."
    case gif of
      Left errMsg -> putStrLn $ " " <> errMsg
      Right bs -> BL.writeFile dest bs
    putStrLn " OK"
  where
    gif :: Either String BL.ByteString
    gif = JP.encodeGifImages JP.LoopingForever palImgs

    palImgs :: [(JP.Palette, JP.GifDelay, JP.Image JP.Pixel8)]
    palImgs =
        map (\(delay, img) ->
              let (img8, pal) = JP.palettize JP.defaultPaletteOptions img
              in (pal, delay, img8)
            )
            jpImgs

    jpImgs :: [(JP.GifDelay, JP.Image JP.PixelRGB8)]
    jpImgs = map (second CVJ.toImage) imgs

    dest = mkDestPath fp

mkDestPath :: FilePath -> FilePath
mkDestPath fp = "doc/generated/" <> fp

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

data ExampleProps
   = ExampleProps
     { exPropIO        :: !Bool
     , exPropAnimation :: !Bool
     } deriving Show

data RenderTarget
   = RenderTarget
     { rtDestination :: !FilePath
       -- ^ Relative path where the symbol must be rendered as an image file.
     , rtSymbolName :: !Name
       -- ^ Name of a top level symbol (function or CAF) that is either an image
       -- or an IO action that yields an image.
     , rtSymbolProps :: !ExampleProps
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

        exampleTypes :: M.Map Name Type
        exampleTypes = M.fromList $ mapMaybe asSigD examplesTH

        renderTargets' :: [RenderTarget]
        renderTargets' =
            mapMaybe
              (\rt -> do
                 exampleType <- M.lookup (rtSymbolName rt) exampleTypes
                 pure rt {rtSymbolProps = classifyExample exampleType}
              )
              renderTargets

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

-- Really hacky way of determining the properties of an example based
-- on its type.
classifyExample :: Type -> ExampleProps
classifyExample (ForallT _ _ t) = classifyExample t
classifyExample (AppT (ConT n) t2) | nameBase n == nameBase ''IO = checkIOAnimation t2
classifyExample (AppT t1 _)     = classifyExample t1
classifyExample (VarT _)        = ExampleProps False False
classifyExample (ConT n) | nameBase n == nameBase ''Animation = ExampleProps False True
classifyExample (PromotedT _)   = ExampleProps False False
classifyExample _               = ExampleProps False False

checkIOAnimation :: Type -> ExampleProps
checkIOAnimation (ForallT _ _ t) = checkIOAnimation t
checkIOAnimation (AppT t1 _)     = checkIOAnimation t1
checkIOAnimation (VarT _)        = ExampleProps True False
checkIOAnimation (ConT n) | nameBase n == nameBase ''Animation = ExampleProps True True
checkIOAnimation (PromotedT _)   = ExampleProps True False
checkIOAnimation _               = ExampleProps True False

parseDecsHse :: String -> String -> Either String [Hse.Decl Hse.SrcSpanInfo]
parseDecsHse fileName str =
    case Hse.parseModuleWithMode (parseMode fileName) str of
      Hse.ParseFailed _srcLoc err -> Left err
      Hse.ParseOk (Hse.Module _ _ _ _  decls) -> Right decls
      Hse.ParseOk _ -> Left "Invalid module"

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
    doRender =
        DoE $ do
          rt <- renderTargets
          let sym = VarE $ rtSymbolName rt
              fp  = LitE $ StringL $ "examples/" <> rtDestination rt
              props = rtSymbolProps rt
              render | exPropAnimation props = 'renderAnimation
                     | otherwise             = 'renderImage
          pure $ NoBindS $
            if exPropIO props
            then VarE '(>>=) `AppE` sym `AppE` (VarE render `AppE` fp)
            else VarE render `AppE` fp `AppE` sym

findHaskellPaths :: FilePath -> IO [FilePath]
findHaskellPaths srcDir = do
  (paths, _) <- G.globDir [G.compile "**/*.hs", G.compile "**/*.hsc"] srcDir
  pure $ concat paths

haddockToHaskell :: T.Text -> T.Text
haddockToHaskell =
    T.replace "\\\\" "\\"
  . T.replace "\\`" "`"
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
              | srcLine a == "Example:"
              , srcLine b == ""
              , srcLine c == "@"
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
                   -- Later on we will determine the actual properties.
                 , rtSymbolProps = ExampleProps False False
                 }
        _ -> Nothing

    prefix = "<<doc/generated/examples/"
