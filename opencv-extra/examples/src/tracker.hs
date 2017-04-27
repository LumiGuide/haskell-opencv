{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as CV
import OpenCV.VideoIO.Types
import GHC.Word (Word8)
import OpenCV.Example
import OpenCV.Extra
import qualified Data.Vector as V
import OpenCV.TypeLevel
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..)  )
import Data.Int
import Data.Maybe
import GHC.TypeLits ()
import Data.Proxy
import Control.Monad
import Control.Monad.Trans.Class

import System.IO.Unsafe

class LoopEnum a where
  next :: a -> a

instance (Eq a, Bounded a, Enum a) => LoopEnum a where
  next x | x == maxBound = minBound
         | otherwise = succ x

blue ,white:: CV.Scalar
blue   = CV.toScalar (V4 255   0   0 255 :: V4 Double)
white  = CV.toScalar (V4 255 255 255 255 :: V4 Double)

ccCommon :: ((FilePath -> IO (Maybe CV.CascadeClassifier)) -> IO (Maybe a)) -> a
ccCommon f = unsafePerformIO $ fromJust <$> f CV.newCascadeClassifier

ccFrontal :: CV.CascadeClassifier
ccFrontal = ccCommon $(withEmbededFile "data/haarcascade_frontalface_default.xml")

main :: IO ()
main = do
    cap <- createCaptureArg

    w <- CV.videoCaptureGetI cap VideoCapPropFrameWidth
    h <- CV.videoCaptureGetI cap VideoCapPropFrameHeight
    let trType = BOOSTING
    tr <- newTracker trType -- BOOSTING MIL KFC MEDIANFLOW TLD
    CV.withWindow "video1" $ \w1 ->
        loop cap (w, h, tr, trType) w1
  where
    ccDetectMultiscale cc = CV.cascadeClassifierDetectMultiScale cc Nothing Nothing minSize maxSize
    minSize = Nothing :: Maybe (V2 Int32)
    maxSize = Nothing :: Maybe (V2 Int32)

    loop cap param@(w, h, tr, trType) window = do
      _ok1 <- CV.videoCaptureGrab cap
      mbImg1 <- CV.videoCaptureRetrieve cap
      case mbImg1 of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
              img' = CV.exceptError $ CV.coerceMat img
              imgGray = CV.exceptError $ CV.cvtColor CV.bgr CV.gray img'

              faces = ccDetectMultiscale ccFrontal imgGray
          mbTrac <- updateTracker tr $ CV.unsafeCoerceMat img
          let box = CV.exceptError $
                CV.withMatM (h ::: w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white $ \imgM -> do
                   void $ CV.matCopyToM imgM (V2 0 0) (CV.unsafeCoerceMat img) Nothing
                   forM_ faces $ \faceRect -> lift $ CV.rectangle imgM faceRect blue 2 CV.LineType_8 0
                   forM_ mbTrac $ \trac ->
                     lift $ CV.rectangle
                              imgM
                              (CV.fmapRect round trac)
                              white
                              2
                              CV.LineType_8
                              0
          CV.imshow window ( CV.unsafeCoerceMat box )
          key <- CV.waitKey 20
          -- Loop unless the escape key is pr sed.
          when (key == 32 && not (null faces)) $ do
              print trType
              tr1 <- newTracker trType
              print =<< initTracker tr1 img (CV.fmapRect fromIntegral $ V.head faces)
              loop cap (w, h, tr1, next trType) window
          unless (key == 27) $ loop cap param window

        -- Out of frames, stop looping.
        _ -> pure ()
