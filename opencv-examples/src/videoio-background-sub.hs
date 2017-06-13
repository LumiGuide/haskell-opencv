{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.Video.MotionAnalysis
import OpenCV.Example

main :: IO ()
main = do
    cap <- createCaptureArg
    bs <- newBackgroundSubtractorMOG2 Nothing Nothing Nothing
--     bs <- newBackgroundSubtractorKNN Nothing Nothing Nothing

    CV.withWindow "video" $ loop cap bs
  where
    loop cap bs window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) 'D 'D
              img' = CV.exceptError $ CV.coerceMat img
          _fgMask <- bgSubApply bs 0.01 img'

          CV.imshow window =<< getBackgroundImage bs

          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap bs window
        -- Out of frames, stop looping.
        Nothing -> pure ()
