{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Video
    ( -- * Motion Analysis and Object Tracking
      estimateRigidTransform
    ) where

import "base" Foreign.Marshal.Utils ( fromBool )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Exception
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/video.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/video.hpp"

--------------------------------------------------------------------------------

-- | Computes an optimal affine transformation between two 2D point sets
--
-- <http://docs.opencv.org/3.0-last-rst/modules/video/doc/motion_analysis_and_object_tracking.html#estimaterigidtransform OpenCV Sphinx doc>
estimateRigidTransform
    :: ( Convert srcPoint2i Point2i
       , Convert dstPoint2i Point2i
       )
    => V.Vector srcPoint2i -- ^ Source
    -> V.Vector dstPoint2i -- ^ Destination
    -> Bool -- ^ Full affine
    -> Either CvException (Maybe (Mat (ShapeT [2, 3]) ('S 1) ('S Double)))
estimateRigidTransform src dst fullAffine =
    checkResult <$> c'estimateRigidTransform
  where
    c'estimateRigidTransform = unsafePerformIO $ do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
        withArrayPtr (V.map convert src :: V.Vector Point2i) $ \srcPtr ->
        withArrayPtr (V.map convert dst :: V.Vector Point2i) $ \dstPtr ->
        withPtr matOut $ \matOutPtr ->
          [cvExcept|
            Mat * matOutPtr = $(Mat * matOutPtr);
            *matOutPtr =
               cv::estimateRigidTransform
               ( cv::_InputArray($(Point2i * srcPtr), $(int32_t c'srcLen))
               , cv::_InputArray($(Point2i * dstPtr), $(int32_t c'dstLen))
               , $(bool c'fullAffine)
               );
          |]

    c'srcLen     = fromIntegral $ V.length src
    c'dstLen     = fromIntegral $ V.length dst
    c'fullAffine = fromBool fullAffine

    checkResult = either (const Nothing) Just . coerceMat
