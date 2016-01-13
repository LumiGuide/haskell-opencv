{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import "base" Control.Exception ( throw )
import "base" Data.Functor ( void )
import "base" Data.Foldable ( forM_ )
import "base" Data.Int
import "base" Data.Word
import "base" System.IO.Unsafe ( unsafePerformIO )
import "linear" Linear.Matrix ( M23 )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import qualified "text" Data.Text as T
import "thea" OpenCV
import "thea" OpenCV.Unsafe
import qualified "vector" Data.Vector as V
import qualified "bytestring" Data.ByteString as B

import "this" ExampleExtractor ( extractExampleImages )

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

extractExampleImages "src"
