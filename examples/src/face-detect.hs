{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}

import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as CV
import OpenCV.VideoIO.Types
import GHC.Word (Word8)
import OpenCV.Example
import OpenCV.TypeLevel
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..)  )
import Data.Int
import GHC.TypeLits ()
import Data.Proxy
import Control.Monad
import Control.Monad.Trans.Class


green,blue ,white:: CV.Scalar
green  = CV.toScalar (V4   0 255   0 255 :: V4 Double)
blue   = CV.toScalar (V4 255   0   0 255 :: V4 Double)
white  = CV.toScalar (V4 255 255 255 255 :: V4 Double)

main :: IO ()
main = do
    cap <- createCaptureArg
    Just ccFrontal <- $(withEmbededFile "../data/haarcascade_frontalface_default.xml") CV.newCascadeClassifier
    Just ccEyes    <- $(withEmbededFile "../data/haarcascade_eye.xml") CV.newCascadeClassifier
    w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
    h <- CV.videoCaptureGetI cap VideoCapPropFrameHeight
    CV.withWindow "video" $ loop cap (ccFrontal, ccEyes, w, h)
  where
    ccDetectMultiscale cc = CV.cascadeClassifierDetectMultiScale cc Nothing Nothing minSize maxSize

    minSize = Nothing :: Maybe (V2 Int32)
    maxSize = Nothing :: Maybe (V2 Int32)

    loop cap param@(ccFrontal, ccEyes, w, h) window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
              img' = CV.exceptError $ CV.coerceMat img
              imgGray = CV.exceptError $ CV.cvtColor CV.bgr CV.gray img'
              faces = ccDetectMultiscale ccFrontal imgGray
              -- eyes have to be on the face ! fix
              eyes = ccDetectMultiscale ccEyes     imgGray
          print faces

          let box = CV.exceptError $
                CV.withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
                  void $ CV.matCopyToM imgM (V2 0 0) (CV.unsafeCoerceMat img) Nothing
                  forM_ eyes  $ \eyeRect  -> lift $ CV.rectangle imgM eyeRect green 2 CV.LineType_8 0
                  forM_ faces $ \faceRect -> lift $ CV.rectangle imgM faceRect blue 2 CV.LineType_8 0
          CV.imshow window ( CV.unsafeCoerceMat box )

          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap param window

        -- Out of frames, stop looping.
        Nothing -> pure ()
