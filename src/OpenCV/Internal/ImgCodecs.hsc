{-# language CPP #-}
{-# language OverloadedLists #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgCodecs
    ( ImreadMode(..)
    , marshalImreadMode
    , JpegParams(..)
    , defaultJpegParams
    , marshalJpegParams
    , PngStrategy(..)
    , marshalPngStrategy
    , PngParams(..)
    , defaultPngParams
    , marshalPngParams
    , OutputFormat(..)
    , marshalOutputFormat
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "vector" Data.Vector.Storable as VS


#include <stdint.h>
#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgcodecs.hpp"

#include "namespace.hpp"

data ImreadMode
   = ImreadUnchanged
   | ImreadGrayscale
   | ImreadColor
   | ImreadAnyDepth
   | ImreadAnyColor
   | ImreadLoadGdal
     deriving (Show)

#num IMREAD_UNCHANGED
#num IMREAD_GRAYSCALE
#num IMREAD_COLOR
#num IMREAD_ANYDEPTH
#num IMREAD_ANYCOLOR
#num IMREAD_LOAD_GDAL

marshalImreadMode :: ImreadMode -> Int32
marshalImreadMode = \case
    ImreadUnchanged -> c'IMREAD_UNCHANGED
    ImreadGrayscale -> c'IMREAD_GRAYSCALE
    ImreadColor     -> c'IMREAD_COLOR
    ImreadAnyDepth  -> c'IMREAD_ANYDEPTH
    ImreadAnyColor  -> c'IMREAD_ANYCOLOR
    ImreadLoadGdal  -> c'IMREAD_LOAD_GDAL

data JpegParams =
     JpegParams
     { jpegParamQuality         :: Int -- ^ \[0..100\]
     , jpegParamProgressive     :: Bool
     , jpegParamOptimize        :: Bool
     , jpegParamRestartInterval :: Word16
     , jpegParamLumaQuality     :: Int
     , jpegParamChromaQuality   :: Int
     } deriving Show

defaultJpegParams :: JpegParams
defaultJpegParams =
    JpegParams
    { jpegParamQuality         = 95
    , jpegParamProgressive     = False
    , jpegParamOptimize        = False
    , jpegParamRestartInterval = 0
    , jpegParamLumaQuality     = -1
    , jpegParamChromaQuality   = -1
    }

#num IMWRITE_JPEG_QUALITY
#num IMWRITE_JPEG_PROGRESSIVE
#num IMWRITE_JPEG_OPTIMIZE
#num IMWRITE_JPEG_RST_INTERVAL
#num IMWRITE_JPEG_LUMA_QUALITY
#num IMWRITE_JPEG_CHROMA_QUALITY

marshalJpegParams :: JpegParams -> VS.Vector CInt
marshalJpegParams params =
    [ c'IMWRITE_JPEG_QUALITY       , fromIntegral $ jpegParamQuality         params
    , c'IMWRITE_JPEG_PROGRESSIVE   , fromBool     $ jpegParamProgressive     params
    , c'IMWRITE_JPEG_OPTIMIZE      , fromBool     $ jpegParamOptimize        params
    , c'IMWRITE_JPEG_RST_INTERVAL  , fromIntegral $ jpegParamRestartInterval params
    , c'IMWRITE_JPEG_LUMA_QUALITY  , fromIntegral $ jpegParamLumaQuality     params
    , c'IMWRITE_JPEG_CHROMA_QUALITY, fromIntegral $ jpegParamChromaQuality   params
    ]

data PngStrategy
   = PngStrategyDefault
   | PngStrategyFiltered
   | PngStrategyHuffmanOnly
   | PngStrategyRLE
   | PngStrategyFixed
     deriving Show

#num IMWRITE_PNG_STRATEGY_DEFAULT
#num IMWRITE_PNG_STRATEGY_FILTERED
#num IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY
#num IMWRITE_PNG_STRATEGY_RLE
#num IMWRITE_PNG_STRATEGY_FIXED

marshalPngStrategy :: PngStrategy -> CInt
marshalPngStrategy = \case
   PngStrategyDefault     -> c'IMWRITE_PNG_STRATEGY_DEFAULT
   PngStrategyFiltered    -> c'IMWRITE_PNG_STRATEGY_FILTERED
   PngStrategyHuffmanOnly -> c'IMWRITE_PNG_STRATEGY_HUFFMAN_ONLY
   PngStrategyRLE         -> c'IMWRITE_PNG_STRATEGY_RLE
   PngStrategyFixed       -> c'IMWRITE_PNG_STRATEGY_FIXED

data PngParams =
     PngParams
     { pngParamCompression :: Int
     , pngParamStrategy    :: PngStrategy
     , pngParamBinaryLevel :: Bool
     } deriving Show

defaultPngParams :: PngParams
defaultPngParams =
    PngParams
    { pngParamCompression = 3
    , pngParamStrategy    = PngStrategyDefault
    , pngParamBinaryLevel = False
    }

#num IMWRITE_PNG_COMPRESSION
#num IMWRITE_PNG_STRATEGY
#num IMWRITE_PNG_BILEVEL

marshalPngParams :: PngParams -> VS.Vector CInt
marshalPngParams params =
    [ c'IMWRITE_PNG_COMPRESSION, fromIntegral        $ pngParamCompression params
    , c'IMWRITE_PNG_STRATEGY   , marshalPngStrategy $ pngParamStrategy    params
    , c'IMWRITE_PNG_BILEVEL    , fromBool            $ pngParamBinaryLevel params
    ]

data OutputFormat
   = OutputBmp
   | OutputExr
   | OutputHdr Bool -- ^ Compression (run length encoding)
   | OutputJpeg JpegParams
   | OutputJpeg2000
   | OutputPng PngParams
   | OutputPxm Bool -- ^ Binary
   | OutputSunras
   | OutputTiff
   | OutputWebP Int -- ^ Quality [1..100], > 100 == lossless
     deriving Show

#num IMWRITE_PXM_BINARY
#num IMWRITE_WEBP_QUALITY

marshalOutputFormat :: OutputFormat -> (String, VS.Vector CInt)
marshalOutputFormat = \case
    OutputBmp          -> (".bmp" , [])
    OutputExr          -> (".exr" , [])
    OutputHdr comp     -> (".hdr" , [fromBool comp])
    OutputJpeg params  -> (".jpeg", marshalJpegParams params)
    OutputJpeg2000     -> (".jp2" , [])
    OutputPng params   -> (".png" , marshalPngParams params)
    OutputPxm binary   -> (".pxm" , [c'IMWRITE_PXM_BINARY, fromBool binary])
    OutputSunras       -> (".sr"  , [])
    OutputTiff         -> (".tiff", [])
    OutputWebP quality -> (".webp", [c'IMWRITE_WEBP_QUALITY, fromIntegral quality])
