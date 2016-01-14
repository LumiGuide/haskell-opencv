{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import "base" Control.Exception ( throw )
import "base" Control.Monad.ST ( runST )
import "base" Data.Functor ( void )
import "base" Data.Foldable ( forM_ )
import "base" Data.Int
import "base" Data.Word
import "base" System.IO.Unsafe ( unsafePerformIO )
import "linear" Linear.Vector ( zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import qualified "text" Data.Text as T
import "thea" OpenCV
import "thea" OpenCV.Unsafe
import qualified "vector" Data.Vector as V
import qualified "bytestring" Data.ByteString as B

import "this" ExampleExtractor ( render, extractExampleImages )

--------------------------------------------------------------------------------

transparent, white, black, blue, red :: V4 Double
transparent = V4 255 255 255   0
white       = V4 255 255 255 255
black       = V4   0   0   0 255
blue        = V4 255   0   0 255
red         = V4   0   0 255 255

birds_768x512, birds_512x341, lambda :: Mat
birds_768x512 = unsafePerformIO $ imdecode ImreadColor <$> B.readFile "data/kodim23.png"
birds_512x341 = either throw id $
                resize (ResizeAbs $ toSize2i (V2 512 341 :: V2 Int32))
                       InterArea
                       birds_768x512

lambda = unsafePerformIO $ imdecode ImreadGrayscale <$> B.readFile "data/lambda.png"

--------------------------------------------------------------------------------

-- We use some padding around the small image in which we draw the
-- lines. This is because antialiasing doesn't seem to work near the
-- edges of an image.
lineTypeImg :: LineType -> Mat
lineTypeImg lineType = runST $ do
    imgM <- mkMatM (V.fromList [h + 2 * p, w + 2 * p]) MatDepth_8U 3 white
    line imgM (pure p + V2 0 h) (pure p + V2 w 0) black 1 lineType 0
    img <- freeze imgM
    pure $ either throw id
         $ resize (ResizeRel $ pure zoom) InterNearest
           =<< matSubRect img (mkRect (pure p) (V2 w h))
  where
    w, h, p :: Int32
    w = h * 3
    h = 5
    p = 20 -- padding
    zoom = 8


lineType8Img, lineType4Img, lineTypeAAImg :: Mat
lineType8Img  = lineTypeImg LineType_8
lineType4Img  = lineTypeImg LineType_4
lineTypeAAImg = lineTypeImg LineType_AA

--------------------------------------------------------------------------------

extractExampleImages "src"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    renderExampleImages
    render lineType8Img  "lineType8Img"
    render lineType4Img  "lineType4Img"
    render lineTypeAAImg "lineTypeAAImg"
