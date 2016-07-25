{-# language CPP             #-}
{-# language ConstraintKinds #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Point.Internal
  ( Point(..)
  , PointDepth
  , PointDim
  , ToPoint(..), FromPoint(..)

  , Point2i, Point2f, Point2d
  , Point3i, Point3f, Point3d

  , ToPoint2i, ToPoint2f, ToPoint2d
  , ToPoint3i, ToPoint3f, ToPoint3d

  , FromPoint2i, FromPoint2f, FromPoint2d
  , FromPoint3i, FromPoint3f, FromPoint3d

  , newPoint2i, newPoint2f, newPoint2d
  , newPoint3i, newPoint3f, newPoint3d
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

newtype Point (depth :: *) (dim :: Nat)
      = Point {unPoint :: ForeignPtr (C'Point depth dim)}

type instance C (Point depth dim) = C'Point depth dim

instance WithPtr (Point depth dim) where withPtr = withForeignPtr . unPoint

type family PointDepth (a :: *) :: *

type family PointDim (a :: *) :: Nat

-- TODO (RvD): rename to TNormDepth and move to Depth module
type family TNormPointDepth (a :: *) :: * where
    TNormPointDepth CDouble = CDouble
    TNormPointDepth Double  = CDouble

    TNormPointDepth CFloat = CFloat
    TNormPointDepth Float  = CFloat

    TNormPointDepth Int32 = Int32

class ToPoint   a where toPoint   :: a -> Point (PointDepth a) (PointDim a)
class FromPoint a where fromPoint :: Point (PointDepth a) (PointDim a) -> a

#define POINT_TYPE(NAME, HDEPTH, DIM)                     \
type NAME = Point HDEPTH DIM;                             \
instance FromPtr NAME where {                             \
    fromPtr = objFromPtr Point $ \ptr ->                  \
                [CU.exp| void { delete $(NAME * ptr) }|]; \
};

#define POINT2_TYPE(NAME, TO_NAME, FROM_NAME, NEW_NAME, HDEPTH, CDEPTH)  \
POINT_TYPE(NAME, HDEPTH, 2)                                              \
type TO_NAME   p = (ToPoint   p, PointDim p ~ 2, PointDepth p ~ HDEPTH); \
type FROM_NAME p = (FromPoint p, PointDim p ~ 2, PointDepth p ~ HDEPTH); \
instance FromPoint (V2 HDEPTH) where {                                   \
    fromPoint point = unsafePerformIO $                                  \
      alloca $ \xPtr ->                                                  \
      alloca $ \yPtr ->                                                  \
      withPtr point $ \pointPtr -> do {                                  \
        [CU.block| void {                                                \
          const cv::Point_<CDEPTH> & p = *$(NAME * pointPtr);            \
          *$(CDEPTH * xPtr) = p.x;                                       \
          *$(CDEPTH * yPtr) = p.y;                                       \
        }|];                                                             \
        V2 <$> peek xPtr                                                 \
           <*> peek yPtr;                                                \
      };                                                                 \
};                                                                       \
NEW_NAME :: V2 HDEPTH -> IO NAME;                                        \
NEW_NAME (V2 x y) = fromPtr $                                            \
    [CU.exp|NAME * { new cv::Point_<CDEPTH>($(CDEPTH x), $(CDEPTH y)) }|];

#define POINT3_TYPE(NAME, TO_NAME, FROM_NAME, NEW_NAME, HDEPTH, CDEPTH)  \
POINT_TYPE(NAME, HDEPTH, 3)                                              \
type TO_NAME   p = (ToPoint   p, PointDim p ~ 3, PointDepth p ~ HDEPTH); \
type FROM_NAME p = (FromPoint p, PointDim p ~ 3, PointDepth p ~ HDEPTH); \
instance FromPoint (V3 HDEPTH) where {                                   \
    fromPoint point = unsafePerformIO $                                  \
      alloca $ \xPtr ->                                                  \
      alloca $ \yPtr ->                                                  \
      alloca $ \zPtr ->                                                  \
      withPtr point $ \pointPtr -> do {                                  \
        [CU.block| void {                                                \
          const cv::Point3_<CDEPTH> & p = *$(NAME * pointPtr);           \
          *$(CDEPTH * xPtr) = p.x;                                       \
          *$(CDEPTH * yPtr) = p.y;                                       \
          *$(CDEPTH * zPtr) = p.z;                                       \
        }|];                                                             \
        V3 <$> peek xPtr                                                 \
           <*> peek yPtr                                                 \
           <*> peek zPtr;                                                \
      };                                                                 \
};                                                                       \
NEW_NAME :: V3 HDEPTH -> IO NAME;                                        \
NEW_NAME (V3 x y z) = fromPtr $                                          \
    [CU.exp|NAME * { new cv::Point3_<CDEPTH>($(CDEPTH x), $(CDEPTH y), $(CDEPTH z)) }|];

POINT2_TYPE(Point2i, ToPoint2i, FromPoint2i, newPoint2i, Int32  , int32_t)
POINT2_TYPE(Point2f, ToPoint2f, FromPoint2f, newPoint2f, CFloat , float  )
POINT2_TYPE(Point2d, ToPoint2d, FromPoint2d, newPoint2d, CDouble, double )

POINT3_TYPE(Point3i, ToPoint3i, FromPoint3i, newPoint3i, Int32  , int32_t)
POINT3_TYPE(Point3f, ToPoint3f, FromPoint3f, newPoint3f, CFloat , float  )
POINT3_TYPE(Point3d, ToPoint3d, FromPoint3d, newPoint3d, CDouble, double )

type instance PointDepth (Point depth dim) = depth
type instance PointDim   (Point depth dim) = dim

type instance PointDepth (V2 a) = TNormPointDepth a
type instance PointDepth (V3 a) = TNormPointDepth a
type instance PointDepth (V4 a) = TNormPointDepth a

type instance PointDim (V2 a) = 2
type instance PointDim (V3 a) = 3
type instance PointDim (V4 a) = 4

instance ToPoint (Point depth dim) where toPoint = id

instance ToPoint (V2 Int32  ) where toPoint = unsafePerformIO . newPoint2i
instance ToPoint (V2 CFloat ) where toPoint = unsafePerformIO . newPoint2f
instance ToPoint (V2 CDouble) where toPoint = unsafePerformIO . newPoint2d
instance ToPoint (V2 Float  ) where toPoint = toPoint . fmap (realToFrac :: Float  -> CFloat )
instance ToPoint (V2 Double ) where toPoint = toPoint . fmap (realToFrac :: Double -> CDouble)

instance ToPoint (V3 Int32  ) where toPoint = unsafePerformIO . newPoint3i
instance ToPoint (V3 CFloat ) where toPoint = unsafePerformIO . newPoint3f
instance ToPoint (V3 CDouble) where toPoint = unsafePerformIO . newPoint3d
instance ToPoint (V3 Float  ) where toPoint = toPoint . fmap (realToFrac :: Float  -> CFloat )
instance ToPoint (V3 Double ) where toPoint = toPoint . fmap (realToFrac :: Double -> CDouble)

instance FromPoint (Point depth dim) where fromPoint = id

instance FromPoint (V2 Float) where fromPoint = fmap (realToFrac :: CFloat  -> Float ) . fromPoint
instance FromPoint (V3 Float) where fromPoint = fmap (realToFrac :: CFloat  -> Float ) . fromPoint

instance FromPoint (V2 Double) where fromPoint = fmap (realToFrac :: CDouble  -> Double ) . fromPoint
instance FromPoint (V3 Double) where fromPoint = fmap (realToFrac :: CDouble  -> Double ) . fromPoint

--------------------------------------------------------------------------------

instance Show Point2i where
    showsPrec prec point = showParen (prec >= 10) $
                               showString "fromPoint2i "
                             . shows (fromPoint point :: V2 Int32)

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Point2i
mkPlacementNewInstance ''Point2f
mkPlacementNewInstance ''Point2d

mkPlacementNewInstance ''Point3i
mkPlacementNewInstance ''Point3f
mkPlacementNewInstance ''Point3d
