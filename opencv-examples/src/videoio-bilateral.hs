{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Main where

import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as CV
import qualified OpenCV.VideoIO.VideoWriter as CVW
import OpenCV.ImgProc.ImgFiltering
import OpenCV.TypeLevel
import OpenCV.Example
import OpenCV.VideoIO.Types as T
import System.Environment
import Data.Word
import Control.Monad
import Text.Read

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ print ("videoio-bilateral input d output.mov " :: String)
    let (_:d:output:_) = args ++ ["5", "/dev/null"]
    cap <- createCaptureArg

    fps <- CV.videoCaptureGetD cap VideoCapPropFps
    w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
    h <- CV.videoCaptureGetI cap VideoCapPropFrameHeight
    print (fps, w, h)

    isOpened <- CV.videoCaptureIsOpened cap

    if not isOpened
      then putStrLn "Couldn't open video capture device"
      else CV.withWindow "video" $ \window ->
              CV.withWindow "input video" $ \windowlv -> do
                wr <- CVW.videoWriterOpen $ CVW.VideoFileSink' $ CVW.VideoFileSink output "avc1" fps (w , h)
                loop (readMaybe d) cap wr window windowlv 0
                CV.exceptErrorIO $ CVW.videoWriterRelease wr
  where
    loop d cap wr window windowlv i = do
      _ok <- CV.videoCaptureGrab cap
      print (i :: Int)
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          let img' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
              img' = CV.exceptError (CV.coerceMat img)
          let dnImg = CV.exceptError $ bilateralFilter d (Just 20) (Just 500) Nothing img'
          CV.exceptErrorIO $ CVW.videoWriterWrite wr $ CV.unsafeCoerceMat dnImg

          CV.imshow window   dnImg
          CV.imshow windowlv img'
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop d cap wr window windowlv (i + 1)
        Nothing -> pure ()
