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
import "this" OpenCV.Core
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
    -> Either CvException (Maybe Mat)
estimateRigidTransform src dst fullAffine = checkResult <$> c'estimateRigidTransform
  where
    c'estimateRigidTransform = unsafePerformIO $ do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
        withPoint2is src $ \srcPtr ->
        withPoint2is dst $ \dstPtr ->
        withMatPtr matOut $ \matOutPtr ->
          [cvExcept|
            Mat * matOutPtr = $(Mat * matOutPtr);
            *matOutPtr =
               cv::estimateRigidTransform
               ( cv::_InputArray($(Point2i * srcPtr), $(int c'srcLen))
               , cv::_InputArray($(Point2i * dstPtr), $(int c'dstLen))
               , $(bool c'fullAffine)
               );
          |]

    c'srcLen     = fromIntegral $ V.length src
    c'dstLen     = fromIntegral $ V.length dst
    c'fullAffine = fromBool fullAffine

    checkResult m
        | miShape (matInfo m) == [2, 3] = Just m
        | otherwise = Nothing
