{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.Photo
import OpenCV.Video.MotionAnalysis

main :: IO ()
main = do
    cap <- CV.newVideoCapture
    -- Open the first available video capture device. Usually the
    -- webcam if run on a laptop.
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    isOpened <- CV.videoCaptureIsOpened cap

--     bs <- newBackgroundSubtractorMOG2 Nothing Nothing Nothing
    bs <- newBackgroundSubtractorKNN Nothing Nothing Nothing

    case isOpened of
      False -> putStrLn "Couldn't open video capture device"
      True -> CV.withWindow "video" $ \window -> do
                loop cap window bs
  where
    loop cap window bs = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) 'D 'D
              img' = CV.exceptError $ CV.coerceMat img
          bgSubApply bs 0.01 img'

          CV.imshow window =<< getBackgroundImage bs

          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window bs
        -- Out of frames, stop looping.
        Nothing -> pure ()
