{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.FeatureDetection
    ( canny
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Data.Maybe
import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Feature Detection
--------------------------------------------------------------------------------

{- |

Finds edges in an image using the
<http://docs.opencv.org/2.4/modules/imgproc/doc/feature_detection.html#canny86 Canny86>
algorithm.

Example:

@
cannyImg
    :: forall shape channels depth
     . (Mat shape channels depth ~ Lambda)
    => Mat shape ('S 1) depth
cannyImg = exceptError $
  canny 30 200 Nothing Nothing lambda
@

<<doc/generated/examples/cannyImg.png cannyImg>>

-}
canny
    :: Double
       -- ^ First threshold for the hysteresis procedure.
    -> Double
       -- ^ Second threshold for the hysteresis procedure.
    -> Maybe Int32
       -- ^ Aperture size for the @Sobel()@ operator. If not specified defaults
       -- to @3@.
    -> Maybe Bool
       -- ^ A flag, indicating whether a more accurate L2 norm should be used.
       -- If 'False' or 'Nothing' the default L1 norm will be used.
    -> Mat ('S [w, h]) ('S 1) ('S Word8)
       -- ^ Single-channel 8-bit input image.
    -> CvExcept (Mat ('S [w, h]) ('S 1) ('S Word8))
canny threshold1 threshold2 apertureSize l2gradient src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
        withPtr src $ \srcPtr ->
        withPtr dst $ \dstPtr ->
          [cvExcept|
            cv::Canny
            ( *$(Mat * srcPtr)
            , *$(Mat * dstPtr)
            , $(double c'threshold1)
            , $(double c'threshold2)
            , $(int32_t c'apertureSize)
            , $(bool c'l2Gradient)
            );
          |]
  where
    c'threshold1 = realToFrac threshold1
    c'threshold2 = realToFrac threshold2
    c'apertureSize = fromMaybe 3 apertureSize
    c'l2Gradient = fromBool (fromMaybe False l2gradient)
