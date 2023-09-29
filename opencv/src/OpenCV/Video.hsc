{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Video
    ( -- * Motion Analysis and Object Tracking
      estimateRigidTransform
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.Marshal.Utils ( fromBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "mtl" Control.Monad.Error.Class ( MonadError, catchError, throwError )
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
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

{-# DEPRECATED estimateRigidTransform "Use estimateAffine2D or estimateAffinePartial2D instead" #-}

-- | Computes an optimal affine transformation between two 2D point sets
--
-- <http://docs.opencv.org/3.0-last-rst/modules/video/doc/motion_analysis_and_object_tracking.html#estimaterigidtransform OpenCV Sphinx doc>
estimateRigidTransform
    :: ( IsPoint2 srcPoint2 Int32
       , IsPoint2 dstPoint2 Int32
       , MonadError CvException m
       )
    => V.Vector (srcPoint2 Int32) -- ^ Source
    -> V.Vector (dstPoint2 Int32) -- ^ Destination
    -> Bool -- ^ Full affine
    -> m (Maybe (Mat (ShapeT [2, 3]) ('S 1) ('S Double)))
estimateRigidTransform src dst fullAffine
    | V.null src || V.null dst = pure Nothing
    | otherwise = do
        result <- c'estimateRigidTransform
        -- If the c++ function can't estimate a rigid transform it will
        -- return an empty matrix. We check for this case by trying to
        -- coerce the result to the desired type.
        catchError
          (Just <$> coerceMat result)
          (\case CoerceMatError _msgs -> pure Nothing
                 otherError -> throwError otherError
          )
  where
    c'estimateRigidTransform = unsafeWrapException $ do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
        unsafeWithArrayPtr (V.map toPoint src) $ \srcPtr ->
        unsafeWithArrayPtr (V.map toPoint dst) $ \dstPtr ->
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
