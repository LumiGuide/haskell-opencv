{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Internal where

import "base" Foreign.C.Types
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

{-# SPECIALIZE newPoint2i :: V2 Int  -> IO Point2i #-}
{-# SPECIALIZE newPoint2i :: V2 CInt -> IO Point2i #-}
newPoint2i :: (Integral a) => V2 a -> IO Point2i
newPoint2i (V2 x y) = point2iFromPtr $
    [CU.exp|Point2i * { new cv::Point2i($(int c'x), $(int c'y)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y

{-# SPECIALIZE newPoint2f :: V2 Float  -> IO Point2f #-}
{-# SPECIALIZE newPoint2f :: V2 CFloat -> IO Point2f #-}
newPoint2f :: (Real a) => V2 a -> IO Point2f
newPoint2f (V2 x y) = point2fFromPtr $
    [CU.exp|Point2f * { new cv::Point2f($(float c'x), $(float c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y

{-# SPECIALIZE newPoint2d :: V2 Double  -> IO Point2d #-}
{-# SPECIALIZE newPoint2d :: V2 CDouble -> IO Point2d #-}
newPoint2d :: (Real a) => V2 a -> IO Point2d
newPoint2d (V2 x y) = point2dFromPtr $
    [CU.exp|Point2d * { new cv::Point2d($(double c'x), $(double c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y

{-# SPECIALIZE newPoint3i :: V3 Int  -> IO Point3i #-}
{-# SPECIALIZE newPoint3i :: V3 CInt -> IO Point3i #-}
newPoint3i :: (Integral a) => V3 a -> IO Point3i
newPoint3i (V3 x y z) = point3iFromPtr $
    [CU.exp|Point3i * { new cv::Point3i($(int c'x), $(int c'y), $(int c'z)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y
    c'z = fromIntegral z

{-# SPECIALIZE newPoint3f :: V3 Float  -> IO Point3f #-}
{-# SPECIALIZE newPoint3f :: V3 CFloat -> IO Point3f #-}
newPoint3f :: (Real a) => V3 a -> IO Point3f
newPoint3f (V3 x y z) = point3fFromPtr $
    [CU.exp|Point3f * { new cv::Point3f($(float c'x), $(float c'y), $(float c'z)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y
    c'z = realToFrac z

{-# SPECIALIZE newPoint3d :: V3 Double  -> IO Point3d #-}
{-# SPECIALIZE newPoint3d :: V3 CDouble -> IO Point3d #-}
newPoint3d :: (Real a) => V3 a -> IO Point3d
newPoint3d (V3 x y z) = point3dFromPtr $
    [CU.exp|Point3d * { new cv::Point3d($(double c'x), $(double c'y), $(double c'z)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y
    c'z = realToFrac z


--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

newSize2i :: (Integral a) => V2 a -> IO Size2i
newSize2i (V2 x y) = size2iFromPtr $
    [CU.exp|Size2i * { new cv::Size2i($(int c'x), $(int c'y)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y

newSize2f :: (Real a) => V2 a -> IO Size2f
newSize2f (V2 x y) = size2fFromPtr $
    [CU.exp|Size2f * { new cv::Size2f($(float c'x), $(float c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y


--------------------------------------------------------------------------------
--  Scalar
--------------------------------------------------------------------------------

{-# SPECIALIZE newScalar :: V4 Float   -> IO Scalar #-}
{-# SPECIALIZE newScalar :: V4 CFloat  -> IO Scalar #-}
{-# SPECIALIZE newScalar :: V4 Double  -> IO Scalar #-}
{-# SPECIALIZE newScalar :: V4 CDouble -> IO Scalar #-}
newScalar :: (Real a) => V4 a -> IO Scalar
newScalar (V4 x y z w) = scalarFromPtr $
    [CU.exp|Scalar * { new cv::Scalar( $(double c'x)
                                     , $(double c'y)
                                     , $(double c'z)
                                     , $(double c'w)
                                     )
                     }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y
    c'z = realToFrac z
    c'w = realToFrac w
