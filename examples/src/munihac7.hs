{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}

import Control.Monad ( unless )
import Data.Foldable ( forM_ )
import Data.Proxy
import Data.Int ( Int32 )
import Data.Word ( Word8 )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..) )
import qualified OpenCV as CV
import OpenCV.TypeLevel
import Control.Monad.IO.Class ( liftIO )

main :: IO ()
main = do
    cap <- CV.newVideoCapture
    CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource 0
    CV.withWindow "video" $ \window -> do
      lowRef  <- newIORef  30
      highRef <- newIORef 200
      CV.createTrackbar window "low"   30 255 $ \pos -> writeIORef lowRef  pos
      CV.createTrackbar window "high" 200 255 $ \pos -> writeIORef highRef pos
      loop cap window lowRef highRef
  where
    loop cap window lowRef highRef = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          result <- CV.exceptErrorIO $ process lowRef highRef img
          CV.imshow window result
          key <- CV.waitKey 20
          unless (key == 27) $ loop cap window lowRef highRef
        Nothing -> pure ()

process :: IORef Int32 -- low ref
        -> IORef Int32 -- high ref
        -> CV.Mat ('S ['D, 'D]) 'D 'D
        -> CV.CvExceptT IO (CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8))
process lowRef highRef inputImg = do
    (inputImg' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)) <- CV.pureExcept $ CV.coerceMat inputImg

    blurImg <- CV.pureExcept $ CV.gaussianBlur (V2 3 3) 0 0 inputImg'

    low  <- liftIO $ readIORef lowRef
    high <- liftIO $ readIORef highRef
    edgeImg <- CV.pureExcept $
               CV.canny (realToFrac low) (realToFrac high) Nothing CV.CannyNormL1 blurImg
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
