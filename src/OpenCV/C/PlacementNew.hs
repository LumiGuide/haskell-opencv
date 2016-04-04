{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.C.PlacementNew ( PlacementNew(..) ) where

import "base" Foreign.Ptr ( Ptr )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.C.Types
import "this" OpenCV.C.Inline ( openCvCtx )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

-- | Copy source to destination using C++'s placement new feature
class PlacementNew a where
    -- | Copy source to destination using C++'s placement new feature
    --
    -- This method is intended for types that are proxies for actual
    -- types in C++.
    --
    -- > new(dst) CType(*src)
    --
    -- The copy should be performed by constructing a new object in
    -- the memory pointed to by @dst@. The new object is initialised
    -- using the value of @src@. This design allow underlying
    -- structures to be shared depending on the implementation of
    -- @CType@.
    placementNew
        :: Ptr a -- ^ Source
        -> Ptr a -- ^ Destination
        -> IO ()

    placementDelete
        :: Ptr a
        -> IO ()

instance PlacementNew C'Point2i where
    placementNew src dst =
        [C.exp| void { new($(Point2i * dst)) cv::Point2i(*$(Point2i * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point2i * ptr)->~Point2i() }|]

instance PlacementNew C'Point2f where
    placementNew src dst =
        [C.exp| void { new($(Point2f * dst)) cv::Point2f(*$(Point2f * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point2f * ptr)->~Point2f() }|]

instance PlacementNew C'Point2d where
    placementNew src dst =
        [C.exp| void { new($(Point2d * dst)) cv::Point2d(*$(Point2d * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point2d * ptr)->~Point2d() }|]

instance PlacementNew C'Point3i where
    placementNew src dst =
        [C.exp| void { new($(Point3i * dst)) cv::Point3i(*$(Point3i * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point3i * ptr)->~Point3i() }|]

instance PlacementNew C'Point3f where
    placementNew src dst =
        [C.exp| void { new($(Point3f * dst)) cv::Point3f(*$(Point3f * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point3f * ptr)->~Point3f() }|]

instance PlacementNew C'Point3d where
    placementNew src dst =
        [C.exp| void { new($(Point3d * dst)) cv::Point3d(*$(Point3d * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Point3d * ptr)->~Point3d() }|]

instance PlacementNew C'Size2i where
    placementNew src dst =
        [C.exp| void { new($(Size2i * dst)) cv::Size2i(*$(Size2i * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Size2i * ptr)->~Size2i() }|]

instance PlacementNew C'Size2f where
    placementNew src dst =
        [C.exp| void { new($(Size2f * dst)) cv::Size2f(*$(Size2f * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Size2f * ptr)->~Size2f() }|]

instance PlacementNew C'Scalar where
    placementNew src dst =
        [C.exp| void { new($(Scalar * dst)) cv::Scalar(*$(Scalar * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Scalar * ptr)->~Scalar() }|]

instance PlacementNew C'Mat where
    placementNew src dst =
        [C.exp| void { new($(Mat * dst)) cv::Mat(*$(Mat * src)) }|]
    placementDelete ptr =
        [C.exp| void { $(Mat * ptr)->~Mat() }|]
