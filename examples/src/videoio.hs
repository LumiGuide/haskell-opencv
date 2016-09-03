{-# language DataKinds #-}

import Control.Monad ( unless )
import Data.Monoid ( (<>) )
import Data.Word ( Word8 )
import qualified OpenCV as CV
import OpenCV.TypeLevel

main :: IO ()
main = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    isOpened <- CV.videoCaptureIsOpened cap
    CV.withWindow "video" $ \window -> do
      loop cap window
  where
    loop cap window = do
      -- putStrLn "loop"
      ok <- CV.videoCaptureGrab cap
      -- putStrLn $ "grab: " <> show ok
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          let img' :: CV.Mat ('S ['D, 'D]) 'D ('S Word8)
              img' = CV.exceptError $ CV.coerceMat img
              edges = CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 img'
          CV.imshow window edges
          key <- CV.waitKey 20
          unless (key == 27) $ loop cap window
        Nothing -> pure ()
