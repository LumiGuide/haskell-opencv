module OpenCV.C.PlacementNew ( PlacementNew(..) ) where

import "base" Foreign.Ptr ( Ptr )

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
