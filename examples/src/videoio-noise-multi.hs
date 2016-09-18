{-# language DataKinds #-}
{-# language FlexibleInstances #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.Photo
import GHC.Word (Word8)
import qualified Data.Vector as V

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
                loop cap window V.empty
  where
    wind = 11
    loop cap window vect = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
              img' = CV.exceptError $ CV.coerceMat img
          let vect' = (if V.length vect >= wind then V.tail vect else vect) `V.snoc` img'

          CV.imshow window $ CV.exceptError $ fastNlMeansDenoisingColoredMulti 1 3 5 11 vect'

          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window vect'
        -- Out of frames, stop looping.
        Nothing -> pure ()
