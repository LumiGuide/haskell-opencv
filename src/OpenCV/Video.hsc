{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Video
    ( -- * Motion Analysis and Object Tracking
      estimateRigidTransform
    ) where

import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Exception.Internal
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except
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
    -> CvExcept (Maybe (Mat (ShapeT [2, 3]) ('S 1) ('S Double)))
estimateRigidTransform src dst fullAffine = do
    result <- c'estimateRigidTransform
    -- If the c++ function can't estimate a rigid transform it will
    -- return an empty matrix. We check for this case by trying to
    -- coerce the result to the desired type.
    catchE (Just <$> coerceMat result)
           (\case CoerceMatError _msgs -> pure Nothing
                  otherError -> throwE otherError
           )
  where
    c'estimateRigidTransform = unsafeWrapException $ do
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
