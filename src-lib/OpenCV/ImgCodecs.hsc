{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgCodecs
    ( ImreadMode(..)
    , imdecode
    , imdecodeM

    , OutputFormat(..)
    , JpegParams(..), defaultJpegParams
    , PngParams(..), defaultPngParams
    , imencode
    ) where

import "base" Foreign.C.String ( withCString )
import "base" Foreign.C.Types ( CInt )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr, nullPtr, castPtr )
import "base" Foreign.Storable ( peek )
import "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString.Unsafe as BU
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Internal
import "this" OpenCV.Unsafe
import qualified "vector" Data.Vector.Storable as VS


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "<vector>"
C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"
C.using "namespace cv"

#include <stdint.h>
#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgcodecs.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

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

marshallImreadMode :: ImreadMode -> CInt
marshallImreadMode = \case
    ImreadUnchanged -> c'IMREAD_UNCHANGED
    ImreadGrayscale -> c'IMREAD_GRAYSCALE
    ImreadColor     -> c'IMREAD_COLOR
    ImreadAnyDepth  -> c'IMREAD_ANYDEPTH
    ImreadAnyColor  -> c'IMREAD_ANYCOLOR
    ImreadLoadGdal  -> c'IMREAD_LOAD_GDAL

-- | Reads an image from a buffer in memory.
--
-- The function reads an image from the specified buffer in the
-- memory. If the buffer is too short or contains invalid data, the
-- empty matrix/image is returned.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgcodecs/doc/reading_and_writing_images.html#imdecode OpenCV Sphinx doc>
imdecode :: ByteString
         -> ImreadMode
         -> Mat
imdecode hbuf imreadMode = unsafePerformIO $ matFromPtr
    [C.block|Mat * {
      cv::_InputArray cbuf = cv::_InputArray($bs-ptr:hbuf, $bs-len:hbuf);
      return new cv::Mat(cv::imdecode(cbuf, $(int c'imreadMode)));
    }|]
  where
    c'imreadMode = marshallImreadMode imreadMode

imdecodeM :: (PrimMonad m) => ByteString -> ImreadMode -> m (MutMat (PrimState m))
imdecodeM hbuf imreadMode = unsafeThaw $ imdecode hbuf imreadMode

--------------------------------------------------------------------------------

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

marshallJpegParams :: JpegParams -> VS.Vector C.CInt
marshallJpegParams params =
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

marshallPngStrategy :: PngStrategy -> C.CInt
marshallPngStrategy = \case
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

marshallPngParams :: PngParams -> VS.Vector C.CInt
marshallPngParams params =
    [ c'IMWRITE_PNG_COMPRESSION, fromIntegral        $ pngParamCompression params
    , c'IMWRITE_PNG_STRATEGY   , marshallPngStrategy $ pngParamStrategy    params
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

marshallOutputFormat :: OutputFormat -> (String, VS.Vector C.CInt)
marshallOutputFormat = \case
    OutputBmp          -> (".bmp" , [])
    OutputExr          -> (".exr" , [])
    OutputHdr comp     -> (".hdr" , [fromBool comp])
    OutputJpeg params  -> (".jpeg", marshallJpegParams params)
    OutputJpeg2000     -> (".jp2" , [])
    OutputPng params   -> (".png" , marshallPngParams params)
    OutputPxm binary   -> (".pxm" , [c'IMWRITE_PXM_BINARY, fromBool binary])
    OutputSunras       -> (".sr"  , [])
    OutputTiff         -> (".tiff", [])
    OutputWebP quality -> (".webp", [c'IMWRITE_WEBP_QUALITY, fromIntegral quality])

-- | Encodes an image into a memory buffer.
--
-- __WARNING:__ This function is not thread safe!
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgcodecs/doc/reading_and_writing_images.html#imencode OpenCV Sphinx doc>
imencode :: Mat
         -> OutputFormat
         -> Either CvException ByteString
imencode mat format = unsafePerformIO $
    withMatPtr mat $ \matPtr ->
    withCString ext $ \extPtr ->
    alloca $ \(bufPtrPtr :: Ptr (Ptr C.CUChar)) ->
    alloca $ \(vecPtrPtr :: Ptr (Ptr ())) ->
    alloca $ \(c'bufSizePtr :: Ptr C.CInt) -> mask_ $ do
      ptrException <- [cvExcept|
        const int * const paramsPtr = $vec-ptr:(int * params);
        std::vector<uchar> * vec = new std::vector<uchar>();
        *$(void * * vecPtrPtr) = reinterpret_cast<void *>(vec);
        std::vector<int> params(paramsPtr, paramsPtr + $vec-len:params);
        cv::imencode( $(char * extPtr)
                    , *$(Mat * matPtr)
                    , *vec
                    , params
                    );
        *$(int * c'bufSizePtr) = vec->size();
        *$(unsigned char * * bufPtrPtr) = &((*vec)[0]);
      |]
      vecPtr <- peek vecPtrPtr
      if ptrException /= nullPtr
      then do
        freeVec vecPtr
        Left <$> cvExceptionFromPtr (pure ptrException)
      else do
        bufSize <- peek c'bufSizePtr
        bufPtr  <- peek bufPtrPtr
        bs <- BU.unsafePackCStringFinalizer
                (castPtr bufPtr)
                (fromIntegral bufSize)
                (freeVec vecPtr)
        pure $ Right bs
  where
    (ext, params) = marshallOutputFormat format

    freeVec :: Ptr () -> IO ()
    freeVec vecPtr = [C.exp|void { delete reinterpret_cast< std::vector<uchar> * >($(void * vecPtr)) }|]
