{-# language DataKinds #-}
{-# language FlexibleInstances #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel

main :: IO ()
main = do
    cap <- CV.newVideoCapture
    -- Open the first available video capture device. Usually the
    -- webcam if run on a laptop.
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    isOpened <- CV.videoCaptureIsOpened cap
    case isOpened of
      False -> putStrLn "Couldn't open video capture device"
      True -> CV.withWindow "video" $ \window -> do
                loop cap window
  where
    loop cap window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) 'D 'D
              img' = CV.exceptError $ CV.coerceMat img
          CV.imshow window img'
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window
        -- Out of frames, stop looping.
        Nothing -> pure ()
