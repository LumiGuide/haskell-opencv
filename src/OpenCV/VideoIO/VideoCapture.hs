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
  , videoCaptureGetD
  , videoCaptureGetI
  , videoCaptureSetD
  , videoCaptureSetI
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
import "this" OpenCV.Internal.VideoIO.Types
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
   = VideoFileSource      !FilePath !(Maybe VideoCaptureAPI)
        -- ^ VideoFile and backend
   | VideoDeviceSource    !Int32    !(Maybe VideoCaptureAPI)
        -- ^ VideoDevice and backend

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
        VideoFileSource filePath api ->
          withCString filePath $ \c'filePath ->
            [cvExcept|
              $(VideoCapture * videoCapturePtr)->open(cv::String($(const char * c'filePath)), $(int32_t c'api));
            |]
           where
             c'api = maybe 0 marshalVideoCaptureAPI api
        VideoDeviceSource device api ->
          [cvExcept|
            $(VideoCapture * videoCapturePtr)->open($(int32_t c'device ));
          |]
            where
              c'device = device + maybe 0 marshalVideoCaptureAPI api

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

videoCaptureGetD :: VideoCapture -> VideoCaptureProperties -> IO Double
videoCaptureGetD videoCapture prop =
    fmap realToFrac $
    withPtr videoCapture $ \videoCapturePtr ->
      [CU.exp| double {
        $(VideoCapture * videoCapturePtr)->get( $(int32_t c'prop) )
      }|]
   where
     c'prop = marshalCaptureProperties prop

videoCaptureGetI :: VideoCapture -> VideoCaptureProperties -> IO Int32
videoCaptureGetI videoCapture prop =
    withPtr videoCapture $ \videoCapturePtr ->
      [CU.exp| int32_t {
        $(VideoCapture * videoCapturePtr)->get( $(int32_t c'prop) )
      }|]
   where
     c'prop = marshalCaptureProperties prop

videoCaptureSetD :: VideoCapture -> VideoCaptureProperties -> Double -> IO Bool
videoCaptureSetD videoCapture prop val =
    fmap toBool $
    withPtr videoCapture $ \videoCapturePtr ->
      [CU.exp| bool {
        $(VideoCapture * videoCapturePtr)->set( $(int32_t c'prop)
                                              , $(double c'val)
                                              )
      }|]
   where
     c'prop = marshalCaptureProperties prop
     c'val = realToFrac val


videoCaptureSetI :: VideoCapture -> VideoCaptureProperties -> Int32 -> IO Bool
videoCaptureSetI videoCapture prop val =
    fmap toBool $
    withPtr videoCapture $ \videoCapturePtr ->
      [CU.exp| bool {
        $(VideoCapture * videoCapturePtr)->set( $(int32_t c'prop)
                                              , $(int32_t val)
                                              )
      }|]
   where
     c'prop = marshalCaptureProperties prop
