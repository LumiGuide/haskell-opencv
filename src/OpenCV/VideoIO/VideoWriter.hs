{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module OpenCV.VideoIO.VideoWriter
  ( VideoWriter
  , VideoWriterSink(..)

  , videoWriterOpen
  , videoWriterRelease
  , videoWriterIsOpened
  , videoWriterWrite
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.String ( withCString )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( toBool )
import "linear" Linear.V2 ( V2(..) )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Core.Types
import "this" OpenCV.VideoIO.Types
import "this" OpenCV.Internal
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except ( ExceptT(ExceptT) )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/videoio.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

newtype VideoWriter = VideoWriter {unVideoWriter :: ForeignPtr (C VideoWriter)}

type instance C VideoWriter = C'VideoWriter

instance WithPtr VideoWriter where withPtr = withForeignPtr . unVideoWriter

instance FromPtr VideoWriter where
    fromPtr = objFromPtr VideoWriter $ \ptr ->
                [CU.exp| void { delete $(VideoWriter * ptr) }|]

data VideoWriterSink
   = VideoFileSink
      { vfsFilePath  :: !FilePath
      , vfsFourCC    :: !FourCC
      , vfsFps       :: !Double
      , vfsFrameDims :: !(Int32, Int32)
      }

{- |
 The API might change in the future, but currently we can:

 Open/create a new file:
     wr <- CV.videoWriterOpen
        (CV.VideoFileSink
              "tst.MOV"
              "avc1"
              30
              (3840, 2160)
        )

 Now, we can write some frames, but they need to have exactly the same size
  as the one we have opened with:
   CV.exceptErrorIO $ CV.videoWriterWrite wr img'

 We need to close at the end or it will not finalize the file:
   CV.exceptErrorIO $ CV.videoWriterRelease wr

-}

videoWriterOpen :: VideoWriterSink -> IO VideoWriter
videoWriterOpen sink =
    fromPtr $
      case sink of
        VideoFileSink {..} ->
          withCString vfsFilePath $ \c'filePath ->
          withPtr (toSize $ uncurry V2 vfsFrameDims) $ \frameDimsPtr   ->
            [CU.exp|VideoWriter * {
              new cv::VideoWriter( cv::String($(const char * c'filePath))
                                 , $(int32_t c'fourCC)
                                 , $(double c'fps)
                                 , *$(Size2i * frameDimsPtr)
                                 )
            }|]
          where
            c'fps = realToFrac vfsFps
            c'fourCC = unFourCC vfsFourCC

videoWriterRelease :: VideoWriter -> CvExceptT IO ()
videoWriterRelease videoWriter =
    ExceptT $
    handleCvException (pure ()) $
    withPtr videoWriter $ \videoWriterPtr ->
      [cvExcept|
        $(VideoWriter * videoWriterPtr)->release();
      |]

videoWriterIsOpened :: VideoWriter -> IO Bool
videoWriterIsOpened videoWriter =
    fmap toBool $
    withPtr videoWriter $ \videoWriterPtr ->
      [CU.exp| bool {
        $(VideoWriter * videoWriterPtr)->isOpened()
      }|]

videoWriterWrite :: VideoWriter -> Mat ('S ['D, 'D]) 'D 'D -> CvExceptT IO ()
videoWriterWrite videoWriter frame =
    ExceptT $
    handleCvException (pure ()) $
    withPtr frame $ \framePtr ->
    withPtr videoWriter $ \videoWriterPtr ->
        [cvExcept|
          $(VideoWriter * videoWriterPtr)->write(*$(Mat *framePtr));
        |]
