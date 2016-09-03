{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.VideoIO.VideoCapture
  ( VideoCapture
  , VideoCaptureSource(..)

  , newVideoCapture
  , videoCaptureOpen
  , videoCaptureRelease
  , videoCaptureIsOpened
  , videoCaptureGrab
  , videoCaptureRetrieve
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.String ( withCString )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Internal
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except ( ExceptT(ExceptT) )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/videoio.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

newtype VideoCapture = VideoCapture {unVideoCapture :: ForeignPtr (C VideoCapture)}

type instance C VideoCapture = C'VideoCapture

instance WithPtr VideoCapture where withPtr = withForeignPtr . unVideoCapture

instance FromPtr VideoCapture where
    fromPtr = objFromPtr VideoCapture $ \ptr ->
                [CU.exp| void { delete $(VideoCapture * ptr) }|]

data VideoCaptureSource
   = VideoFileSource   !FilePath -- TODO (RvD): optional API preference
   | VideoDeviceSource !Int32

newVideoCapture :: IO VideoCapture
newVideoCapture = fromPtr $
    [CU.exp|VideoCapture * {
      new cv::VideoCapture()
    }|]

videoCaptureOpen :: VideoCapture -> VideoCaptureSource -> CvExceptT IO ()
videoCaptureOpen videoCapture src =
    ExceptT $
    handleCvException (pure ()) $
    withPtr videoCapture $ \videoCapturePtr ->
      case src of
        VideoFileSource filePath ->
          withCString filePath $ \c'filePath ->
            [cvExcept|
              $(VideoCapture * videoCapturePtr)->open(cv::String($(const char * c'filePath)));
            |]
        VideoDeviceSource device ->
          [cvExcept|
            $(VideoCapture * videoCapturePtr)->open($(int32_t device));
          |]

videoCaptureRelease :: VideoCapture -> CvExceptT IO ()
videoCaptureRelease videoCapture =
    ExceptT $
    handleCvException (pure ()) $
    withPtr videoCapture $ \videoCapturePtr ->
      [cvExcept|
        $(VideoCapture * videoCapturePtr)->release();
      |]

videoCaptureIsOpened :: VideoCapture -> IO Bool
videoCaptureIsOpened videoCapture =
    fmap toBool $
    withPtr videoCapture $ \videoCapturePtr ->
      [CU.exp| bool {
        $(VideoCapture * videoCapturePtr)->isOpened()
      }|]

videoCaptureGrab :: VideoCapture -> IO Bool
videoCaptureGrab videoCapture =
    fmap toBool $
    withPtr videoCapture $ \videoCapturePtr ->
      [C.exp| bool {
        $(VideoCapture * videoCapturePtr)->grab()
      }|]

videoCaptureRetrieve :: VideoCapture -> IO (Maybe (Mat ('S ['D, 'D]) 'D 'D))
videoCaptureRetrieve videoCapture = do
    frame <- newEmptyMat
    ok <- withPtr frame $ \framePtr ->
      withPtr videoCapture $ \videoCapturePtr ->
        [C.exp| bool {
          $(VideoCapture * videoCapturePtr)->retrieve(*$(Mat * framePtr), 0)
        }|]
    pure $ case toBool ok of
      False -> Nothing
      True  -> Just $ unsafeCoerceMat frame
