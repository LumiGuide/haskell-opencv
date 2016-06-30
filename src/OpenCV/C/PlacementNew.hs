{-# language CPP #-}
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

#define PLACEMENT_NEW(TYPE)                                             \
instance PlacementNew C'TYPE where {                                    \
    placementNew src dst =                                              \
        [C.exp| void { new($(TYPE * dst)) cv::TYPE(*$(TYPE * src)) }|]; \
    placementDelete ptr =                                               \
        [C.exp| void { $(TYPE * ptr)->~TYPE() }|];                      \
}

PLACEMENT_NEW(Point2i)
PLACEMENT_NEW(Point2f)
PLACEMENT_NEW(Point2d)
PLACEMENT_NEW(Point3i)
PLACEMENT_NEW(Point3f)
PLACEMENT_NEW(Point3d)
PLACEMENT_NEW(Vec4i)
PLACEMENT_NEW(Size2i)
PLACEMENT_NEW(Size2f)
PLACEMENT_NEW(Scalar)
PLACEMENT_NEW(Mat)
