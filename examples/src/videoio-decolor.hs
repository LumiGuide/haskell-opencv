{-# language DataKinds #-}
{-# language FlexibleInstances #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.Photo
import GHC.Word (Word8)
import OpenCV.Example

main :: IO ()
main = do
    cap <- createCaptureArg
    CV.withWindow "gray" $ \w1 -> CV.withWindow "boost" $ loop cap w1
  where
    loop cap grayWindow boostWindow = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
              img' = CV.exceptError $ CV.coerceMat img

          let (grayImg, boostImg) = CV.exceptError $ decolor img'

          CV.imshow grayWindow grayImg
          CV.imshow boostWindow boostImg

          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap grayWindow boostWindow
        -- Out of frames, stop looping.
        Nothing -> pure ()
