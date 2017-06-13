{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgCodecs
    ( ImreadMode(..)
    , imdecode
    , imdecodeM

    , OutputFormat(..)
    , JpegParams(..), defaultJpegParams
    , PngStrategy(..)
    , PngParams(..), defaultPngParams
    , imencode
    , imencodeM
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int ( Int32 )
import "base" Foreign.C.String ( withCString )
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Ptr ( Ptr, nullPtr, castPtr )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString.Unsafe as BU
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.ImgCodecs
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "<vector>"
C.include "opencv2/core.hpp"
C.include "opencv2/imgcodecs.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------

-- | Reads an image from a buffer in memory.
--
-- The function reads an image from the specified buffer in the
-- memory. If the buffer is too short or contains invalid data, the
-- empty matrix/image is returned.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgcodecs/doc/reading_and_writing_images.html#imdecode OpenCV Sphinx doc>
imdecode
    :: ImreadMode
    -> ByteString
    -> Mat ('S ['D, 'D]) 'D 'D
imdecode imreadMode hbuf = unsafeCoerceMat $ unsafePerformIO $ fromPtr
    [C.block|Mat * {
      cv::_InputArray cbuf = cv::_InputArray($bs-ptr:hbuf, $bs-len:hbuf);
      return new cv::Mat(cv::imdecode(cbuf, $(int32_t c'imreadMode)));
    }|]
  where
    c'imreadMode = marshalImreadMode imreadMode

imdecodeM
    :: (PrimMonad m)
    => ImreadMode
    -> ByteString
    -> m (Mut (Mat ('S ['D, 'D]) 'D 'D) (PrimState m))
imdecodeM imreadMode hbuf = unsafeThaw $ imdecode imreadMode hbuf

--------------------------------------------------------------------------------

-- | Encodes an image into a memory buffer.
--
-- __WARNING:__ This function is not thread safe!
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgcodecs/doc/reading_and_writing_images.html#imencode OpenCV Sphinx doc>
imencode
    :: OutputFormat
    -> Mat shape channels depth
    -> CvExcept ByteString
imencode format mat = unsafeWrapException $
    withPtr mat $ \matPtr ->
    withCString ext $ \extPtr ->
    alloca $ \(bufPtrPtr :: Ptr (Ptr CUChar)) ->
    alloca $ \(vecPtrPtr :: Ptr (Ptr ())) ->
    alloca $ \(c'bufSizePtr :: Ptr Int32) -> mask_ $ do
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
        *$(int32_t * c'bufSizePtr) = vec->size();
        *$(unsigned char * * bufPtrPtr) = &((*vec)[0]);
      |]
      vecPtr <- peek vecPtrPtr
      if ptrException /= nullPtr
      then do
        freeVec vecPtr
        Left . BindingException <$> fromPtr (pure ptrException)
      else do
        bufSize <- peek c'bufSizePtr
        bufPtr  <- peek bufPtrPtr
        bs <- BU.unsafePackCStringFinalizer
                (castPtr bufPtr)
                (fromIntegral bufSize)
                (freeVec vecPtr)
        pure $ Right bs
  where
    (ext, params) = marshalOutputFormat format

    freeVec :: Ptr () -> IO ()
    freeVec vecPtr = [C.exp|void { delete reinterpret_cast< std::vector<uchar> * >($(void * vecPtr)) }|]

-- | Encodes an image into a memory buffer.
--
-- See 'imencode'
imencodeM
    :: (PrimMonad m)
    => OutputFormat
    -> Mut (Mat shape channels depth) (PrimState m)
    -> CvExceptT m ByteString
imencodeM format matM =
    ExceptT . pure . runExcept =<< (imencode format <$> unsafeFreeze matM)
