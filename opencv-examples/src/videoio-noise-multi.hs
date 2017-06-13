{-# language DataKinds #-}
{-# language FlexibleInstances #-}

import Control.Monad ( unless )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import OpenCV.Photo
import GHC.Word (Word8)
import qualified Data.Vector as V
import OpenCV.Example

main :: IO ()
main = do
    cap <- createCaptureArg
    CV.withWindow "video" $ loop cap V.empty
  where
    wind = 11
    loop cap vect window = do
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
          unless (key == 27) $ loop cap vect' window
        -- Out of frames, stop looping.
        Nothing -> pure ()
