{-# language DataKinds #-}

import Control.Monad ( unless )
import Data.Word ( Word8 )
import qualified OpenCV as CV
import OpenCV.TypeLevel

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
          result <- CV.exceptErrorIO $ process img
          CV.imshow window result
          key <- CV.waitKey 20
          unless (key == 27) $ loop cap window
        Nothing -> pure ()

process :: CV.Mat ('S ['D, 'D]) 'D 'D
        -> CV.CvExceptT IO (CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8))
process inputImg = CV.pureExcept $ CV.coerceMat inputImg
