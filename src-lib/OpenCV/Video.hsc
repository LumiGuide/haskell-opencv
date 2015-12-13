{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Video
    ( -- * Motion Analysys and Object Tracking
      estimateRigidTransform
    ) where

import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude hiding ( shift )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Internal
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
    :: V.Vector Point2i -- ^ Source
    -> V.Vector Point2i -- ^ Destination
    -> Bool -- ^ Full affine
    -> Either CvException Mat
estimateRigidTransform src dst fullAffine = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException matOut $
      withPoints src $ \srcPtr ->
      withPoints dst $ \dstPtr ->
      withMatPtr matOut $ \matOutPtr ->
        [cvExcept|
          *$(Mat * matOutPtr) =
             cv::estimateRigidTransform
             ( _InputArray($(Point2i * srcPtr), $(int c'srcLen))
             , _InputArray($(Point2i * dstPtr), $(int c'dstLen))
             , $(bool c'fullAffine)
             );
        |]
  where
    c'srcLen     = fromIntegral $ V.length src
    c'dstLen     = fromIntegral $ V.length dst
    c'fullAffine = fromBool fullAffine
