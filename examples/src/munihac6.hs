{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}

import Control.Monad ( unless )
import Data.Foldable ( forM_ )
import Data.Proxy
import Data.Word ( Word8 )
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..) )
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
process inputImg = do
    (inputImg' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)) <- CV.pureExcept $ CV.coerceMat inputImg

    blurImg <- CV.pureExcept $ CV.gaussianBlur (V2 3 3) 0 0 inputImg'
    edgeImg <- CV.pureExcept $ CV.canny 30 200 Nothing CV.CannyNormL1 blurImg
    edgeImgBgr <- CV.pureExcept $ CV.cvtColor CV.gray CV.bgr edgeImg
    edgeImgM <- CV.thaw edgeImg
    lineSegments <- CV.houghLinesP 1 (pi / 180) 100 (Just 30) (Just 10) edgeImgM

    CV.pureExcept $ CV.withMatM
          (h ::: w ::: Z)
          (Proxy :: Proxy 3)
          (Proxy :: Proxy Word8)
          white $ \outImgM -> do
      CV.matCopyToM outImgM (V2 0 0) inputImg' Nothing
      CV.matCopyToM outImgM (V2 0 0) edgeImgBgr (Just edgeImg)

      forM_ lineSegments $ \lineSegment -> do
        CV.line outImgM
                (CV.lineSegmentStart lineSegment)
                (CV.lineSegmentStop  lineSegment)
                red 2 CV.LineType_8 0
  where
    [h, w] = CV.miShape info
    info = CV.matInfo inputImg

white, red :: CV.Scalar
white = CV.toScalar (V4 255 255 255 255 :: V4 Double)
red   = CV.toScalar (V4   0   0 255 255 :: V4 Double)
