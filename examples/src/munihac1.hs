{-# language DataKinds #-}

import Control.Monad ( unless )
import qualified OpenCV as CV

main :: IO ()
main = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    CV.withWindow "video" $ \window -> loop cap window
  where
    loop cap window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          CV.imshow window img
          key <- CV.waitKey 20
          unless (key == 27) $ loop cap window
        Nothing -> pure ()
