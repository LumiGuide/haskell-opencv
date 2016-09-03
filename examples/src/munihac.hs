{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}

import Control.Monad ( unless )
import Data.Foldable ( forM_ )
import Data.Functor ( void )
import Data.Proxy
import Data.Word ( Word8 )
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..) )
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
          result <- CV.exceptErrorIO $ fancy img
          CV.imshow window result
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window
        -- Out of frames, stop looping.
        Nothing -> pure ()

fancy :: CV.Mat ('S ['D, 'D]) 'D 'D
      -> CV.CvExceptT IO (CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8))
fancy inputImg = do
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
      void $ CV.matCopyToM outImgM (V2 0 0) inputImg' Nothing
      void $ CV.matCopyToM outImgM (V2 0 0) edgeImgBgr (Just edgeImg)

      -- Draw the lines found by houghLinesP
      forM_ lineSegments $ \lineSegment -> do
        CV.line outImgM
                (CV.lineSegmentStart lineSegment)
                (CV.lineSegmentStop  lineSegment)
                red 2 CV.LineType_8 0
  where
    [h, w] = CV.miShape info -- evil!
    info = CV.matInfo inputImg

white, red :: CV.Scalar
white = CV.toScalar (V4 255 255 255 255 :: V4 Double)
red   = CV.toScalar (V4   0   0 255 255 :: V4 Double)
