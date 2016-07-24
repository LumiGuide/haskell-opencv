{-# language CPP             #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

{-# options_ghc -fno-warn-orphans #-}

module OpenCV.Core.Types.Matx.Internal
    ( Matx(..)
    , MatxDepth
    , MatxDimM
    , MatxDimN
    , ToMatx(..), FromMatx(..)

    , Vec(..)
    , VecDepth
    , VecDim
    , ToVec(..), FromVec(..)

    , Vec2i, Vec2f, Vec2d
    , Vec3i, Vec3f, Vec3d
    , Vec4i, Vec4f, Vec4d

    , newVec2i, newVec2f, newVec2d
    , newVec3i, newVec3f, newVec3d
    , newVec4i, newVec4f, newVec4d
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.PlacementNew.TH
import "this" OpenCV.C.Types
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------

newtype Matx (depth :: *) (m :: Nat) (n :: Nat)
      = Matx {unMatx :: ForeignPtr (C'Matx depth m n)}

type instance C (Matx depth m n) = C'Matx depth m n

-- | Calculate the depth for types which can be converted to or
-- from 'Matx'
type family MatxDepth (a :: *) :: *

-- | Calculate the first dimension (called @m@) for types which can be converted to or
-- from 'Matx'
type family MatxDimM (a :: *) :: Nat

-- | Calculate the second dimension (called @n@) for types which can be converted to or
-- from 'Matx'
type family MatxDimN (a :: *) :: Nat

class ToMatx a where
    toMatx :: a -> Matx (MatxDepth a) (MatxDimM a) (MatxDimN a)

class FromMatx a where
    fromMatx :: Matx (MatxDepth a) (MatxDimM a) (MatxDimN a)-> a

--------------------------------------------------------------------------------

newtype Vec (depth :: *) (dim :: Nat)
      = Vec {unVec :: ForeignPtr (C'Vec depth dim)}

type instance C (Vec depth dim) = C'Vec depth dim

instance WithPtr (Vec depth dim) where withPtr = withForeignPtr . unVec

-- | Calculate the depth for types which can be converted to or
-- from 'Vec'
type family VecDepth (a :: *) :: *

-- | Calculate the dimension for types which can be converted to or
-- from 'Vec'
type family VecDim (a :: *) :: Nat

type family TNormVecDepth (a :: *) :: * where
    TNormVecDepth CDouble = CDouble
    TNormVecDepth Double  = CDouble

    TNormVecDepth CFloat = CFloat
    TNormVecDepth Float  = CFloat

    TNormVecDepth Int32 = Int32

class ToVec   a where toVec   :: a -> Vec (VecDepth a) (VecDim a)
class FromVec a where fromVec :: Vec (VecDepth a) (VecDim a) -> a

#define VEC_TYPE(NAME, HDEPTH, DIM)                       \
type NAME = Vec HDEPTH DIM;                               \
instance FromPtr NAME where {                             \
    fromPtr = objFromPtr Vec $ \ptr ->                    \
                [CU.exp| void { delete $(NAME * ptr) }|]; \
};

#define VEC2_TYPE(NAME, NEW_NAME, HDEPTH, CDEPTH)                          \
VEC_TYPE(NAME, HDEPTH, 2)                                                  \
instance FromVec (V2 HDEPTH) where {                                       \
    fromVec vec = unsafePerformIO $                                        \
      alloca $ \xPtr ->                                                    \
      alloca $ \yPtr ->                                                    \
      withPtr vec $ \vecPtr -> do {                                        \
        [CU.block| void {                                                  \
          const cv::Vec<CDEPTH, 2> & p = *$(NAME * vecPtr);                \
          *$(CDEPTH * xPtr) = p[0];                                        \
          *$(CDEPTH * yPtr) = p[1];                                        \
        }|];                                                               \
        V2 <$> peek xPtr                                                   \
           <*> peek yPtr;                                                  \
      };                                                                   \
};                                                                         \
NEW_NAME :: V2 HDEPTH -> IO NAME;                                          \
NEW_NAME (V2 x y) = fromPtr $                                              \
    [CU.exp|NAME * { new cv::Vec<CDEPTH, 2>($(CDEPTH x), $(CDEPTH y)) }|];

#define VEC3_TYPE(NAME, NEW_NAME, HDEPTH, CDEPTH)           \
VEC_TYPE(NAME, HDEPTH, 3)                                   \
instance FromVec (V3 HDEPTH) where {                        \
    fromVec vec = unsafePerformIO $                         \
      alloca $ \xPtr ->                                     \
      alloca $ \yPtr ->                                     \
      alloca $ \zPtr ->                                     \
      withPtr vec $ \vecPtr -> do {                         \
        [CU.block| void {                                   \
          const cv::Vec<CDEPTH, 3> & p = *$(NAME * vecPtr); \
          *$(CDEPTH * xPtr) = p[0];                         \
          *$(CDEPTH * yPtr) = p[1];                         \
          *$(CDEPTH * zPtr) = p[2];                         \
        }|];                                                \
        V3 <$> peek xPtr                                    \
           <*> peek yPtr                                    \
           <*> peek zPtr;                                   \
      };                                                    \
};                                                          \
NEW_NAME :: V3 HDEPTH -> IO NAME;                           \
NEW_NAME (V3 x y z) = fromPtr $                             \
    [CU.exp|NAME * { new cv::Vec<CDEPTH, 3>($(CDEPTH x), $(CDEPTH y), $(CDEPTH z)) }|];

#define VEC4_TYPE(NAME, NEW_NAME, HDEPTH, CDEPTH)           \
VEC_TYPE(NAME, HDEPTH, 4)                                   \
instance FromVec (V4 HDEPTH) where {                        \
    fromVec vec = unsafePerformIO $                         \
      alloca $ \xPtr ->                                     \
      alloca $ \yPtr ->                                     \
      alloca $ \zPtr ->                                     \
      alloca $ \wPtr ->                                     \
      withPtr vec $ \vecPtr -> do {                         \
        [CU.block| void {                                   \
          const cv::Vec<CDEPTH, 4> & p = *$(NAME * vecPtr); \
          *$(CDEPTH * xPtr) = p[0];                         \
          *$(CDEPTH * yPtr) = p[1];                         \
          *$(CDEPTH * zPtr) = p[2];                         \
          *$(CDEPTH * wPtr) = p[3];                         \
        }|];                                                \
        V4 <$> peek xPtr                                    \
           <*> peek yPtr                                    \
           <*> peek zPtr                                    \
           <*> peek wPtr;                                   \
      };                                                    \
};                                                          \
NEW_NAME :: V4 HDEPTH -> IO NAME;                           \
NEW_NAME (V4 x y z w) = fromPtr $                           \
    [CU.exp|NAME * { new cv::Vec<CDEPTH, 4>($(CDEPTH x), $(CDEPTH y), $(CDEPTH z), $(CDEPTH w)) }|];

VEC2_TYPE(Vec2i, newVec2i, Int32  , int32_t)
VEC2_TYPE(Vec2f, newVec2f, CFloat , float  )
VEC2_TYPE(Vec2d, newVec2d, CDouble, double )

VEC3_TYPE(Vec3i, newVec3i, Int32  , int32_t)
VEC3_TYPE(Vec3f, newVec3f, CFloat , float  )
VEC3_TYPE(Vec3d, newVec3d, CDouble, double )

VEC4_TYPE(Vec4i, newVec4i, Int32  , int32_t)
VEC4_TYPE(Vec4f, newVec4f, CFloat , float  )
VEC4_TYPE(Vec4d, newVec4d, CDouble, double )

type instance VecDepth (Vec depth dim) = depth
type instance VecDim   (Vec depth dim) = dim

type instance VecDepth (V2 a) = TNormVecDepth a
type instance VecDepth (V3 a) = TNormVecDepth a
type instance VecDepth (V4 a) = TNormVecDepth a

type instance VecDepth (a, a)       = TNormVecDepth a
type instance VecDepth (a, a, a)    = TNormVecDepth a
type instance VecDepth (a, a, a, a) = TNormVecDepth a

type instance VecDim (V2 a) = 2
type instance VecDim (V3 a) = 3
type instance VecDim (V4 a) = 4

type instance VecDim (a, a)       = 2
type instance VecDim (a, a, a)    = 3
type instance VecDim (a, a, a, a) = 4

instance ToVec (Vec depth dim) where toVec = id

instance ToVec (V2 Int32  ) where toVec = unsafePerformIO . newVec2i
instance ToVec (V2 CFloat ) where toVec = unsafePerformIO . newVec2f
instance ToVec (V2 CDouble) where toVec = unsafePerformIO . newVec2d
instance ToVec (V2 Float  ) where toVec = toVec . fmap (realToFrac :: Float  -> CFloat )
instance ToVec (V2 Double ) where toVec = toVec . fmap (realToFrac :: Double -> CDouble)

instance ToVec (V3 Int32  ) where toVec = unsafePerformIO . newVec3i
instance ToVec (V3 CFloat ) where toVec = unsafePerformIO . newVec3f
instance ToVec (V3 CDouble) where toVec = unsafePerformIO . newVec3d
instance ToVec (V3 Float  ) where toVec = toVec . fmap (realToFrac :: Float  -> CFloat )
instance ToVec (V3 Double ) where toVec = toVec . fmap (realToFrac :: Double -> CDouble)

instance ToVec (V4 Int32  ) where toVec = unsafePerformIO . newVec4i
instance ToVec (V4 CFloat ) where toVec = unsafePerformIO . newVec4f
instance ToVec (V4 CDouble) where toVec = unsafePerformIO . newVec4d
instance ToVec (V4 Float  ) where toVec = toVec . fmap (realToFrac :: Float  -> CFloat )
instance ToVec (V4 Double ) where toVec = toVec . fmap (realToFrac :: Double -> CDouble)

instance ToVec (Int32  , Int32  ) where toVec = toVec . toV2
instance ToVec (CFloat , CFloat ) where toVec = toVec . toV2
instance ToVec (Float  , Float  ) where toVec = toVec . toV2
instance ToVec (CDouble, CDouble) where toVec = toVec . toV2
instance ToVec (Double , Double ) where toVec = toVec . toV2

instance ToVec (Int32  , Int32  , Int32  ) where toVec = toVec . toV3
instance ToVec (CFloat , CFloat , CFloat ) where toVec = toVec . toV3
instance ToVec (Float  , Float  , Float  ) where toVec = toVec . toV3
instance ToVec (CDouble, CDouble, CDouble) where toVec = toVec . toV3
instance ToVec (Double , Double , Double ) where toVec = toVec . toV3

instance ToVec (Int32  , Int32  , Int32  , Int32  ) where toVec = toVec . toV4
instance ToVec (CFloat , CFloat , CFloat , CFloat ) where toVec = toVec . toV4
instance ToVec (Float  , Float  , Float  , Float  ) where toVec = toVec . toV4
instance ToVec (CDouble, CDouble, CDouble, CDouble) where toVec = toVec . toV4
instance ToVec (Double , Double , Double , Double ) where toVec = toVec . toV4

instance FromVec (Vec depth dim) where fromVec = id

instance FromVec (V2 Float) where fromVec = fmap (realToFrac :: CFloat  -> Float ) . fromVec
instance FromVec (V3 Float) where fromVec = fmap (realToFrac :: CFloat  -> Float ) . fromVec
instance FromVec (V4 Float) where fromVec = fmap (realToFrac :: CFloat  -> Float ) . fromVec

instance FromVec (V2 Double) where fromVec = fmap (realToFrac :: CDouble  -> Double ) . fromVec
instance FromVec (V3 Double) where fromVec = fmap (realToFrac :: CDouble  -> Double ) . fromVec
instance FromVec (V4 Double) where fromVec = fmap (realToFrac :: CDouble  -> Double ) . fromVec

instance FromVec (Int32  , Int32  )                   where fromVec = fromV2 . fromVec
instance FromVec (CFloat , CFloat )                   where fromVec = fromV2 . fromVec
instance FromVec (Float  , Float  )                   where fromVec = fromV2 . fromVec
instance FromVec (CDouble, CDouble)                   where fromVec = fromV2 . fromVec
instance FromVec (Double , Double )                   where fromVec = fromV2 . fromVec

instance FromVec (Int32  , Int32  , Int32  )          where fromVec = fromV3 . fromVec
instance FromVec (CFloat , CFloat , CFloat )          where fromVec = fromV3 . fromVec
instance FromVec (Float  , Float  , Float  )          where fromVec = fromV3 . fromVec
instance FromVec (CDouble, CDouble, CDouble)          where fromVec = fromV3 . fromVec
instance FromVec (Double , Double , Double )          where fromVec = fromV3 . fromVec

instance FromVec (Int32  , Int32  , Int32  , Int32  ) where fromVec = fromV4 . fromVec
instance FromVec (CFloat , CFloat , CFloat , CFloat ) where fromVec = fromV4 . fromVec
instance FromVec (Float  , Float  , Float  , Float  ) where fromVec = fromV4 . fromVec
instance FromVec (CDouble, CDouble, CDouble, CDouble) where fromVec = fromV4 . fromVec
instance FromVec (Double , Double , Double , Double ) where fromVec = fromV4 . fromVec

--------------------------------------------------------------------------------

toV2 :: (a, a)       -> V2 a
toV3 :: (a, a, a)    -> V3 a
toV4 :: (a, a, a, a) -> V4 a

toV2 (x, y)       = V2 x y
toV3 (x, y, z)    = V3 x y z
toV4 (x, y, z, w) = V4 x y z w

fromV2 :: V2 a -> (a, a)
fromV3 :: V3 a -> (a, a, a)
fromV4 :: V4 a -> (a, a, a, a)

fromV2 (V2 x y)     = (x, y)
fromV3 (V3 x y z)   = (x, y, z)
fromV4 (V4 x y z w) = (x, y, z, w)

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Vec4i
