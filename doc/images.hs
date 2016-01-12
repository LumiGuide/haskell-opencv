{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import "base" Control.Exception ( throw )
import "base" Data.Functor ( void )
import "base" Data.Foldable ( forM_ )
import "base" Data.Int ( Int32 )
import "base" System.IO.Unsafe ( unsafePerformIO )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import qualified "text" Data.Text as T
import "thea" OpenCV
import qualified "vector" Data.Vector as V
import qualified "bytestring" Data.ByteString as B

import "this" ExampleExtractor ( extractExampleImages )


transparent, white, black, blue, red :: V4 Double
transparent = V4 255 255 255   0
white       = V4 255 255 255 255
black       = V4   0   0   0 255
blue        = V4 255   0   0 255
red         = V4   0   0 255 255

lenna :: Mat
lenna = unsafePerformIO $ imdecode ImreadColor <$> B.readFile "data/Lenna.png"

lambda :: Mat
lambda = unsafePerformIO $ imdecode ImreadGrayscale <$> B.readFile "data/lambda.png"

extractExampleImages "src"
