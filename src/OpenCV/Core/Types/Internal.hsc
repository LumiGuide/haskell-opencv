{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module OpenCV.Core.Types.Internal where

import "base" Data.Bits ( (.|.) )
import "base" Data.Functor ( ($>) )
import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca, allocaBytes )
import "base" Foreign.Marshal.Array ( allocaArray )
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( sizeOf, peek, poke )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"
#include "macros.hpp"

#sizeof Point2i
#sizeof Point2f


--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------

-- | 2D point with 'Int'eger coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2i = Point2i {unPoint2i :: ForeignPtr C'Point2i}
-- | 2D point with 'Float' coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2f = Point2f {unPoint2f :: ForeignPtr C'Point2f}
-- | 2D point with 'Double' coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV doc>
newtype Point2d = Point2d {unPoint2d :: ForeignPtr C'Point2d}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3i = Point3i {unPoint3i :: ForeignPtr C'Point3i}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3f = Point3f {unPoint3f :: ForeignPtr C'Point3f}
-- | 3D point
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV doc>
newtype Point3d = Point3d {unPoint3d :: ForeignPtr C'Point3d}
-- | Size of an image or rectangle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV doc>
newtype Size2i = Size2i {unSize2i :: ForeignPtr C'Size2i}
-- | Size of an image or rectangle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV doc>
newtype Size2f = Size2f {unSize2f :: ForeignPtr C'Size2f}
newtype Scalar = Scalar {unScalar :: ForeignPtr C'Scalar}
-- | 2D rectangles
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rect OpenCV doc>
newtype Rect = Rect {unRect :: ForeignPtr C'Rect}
-- | Rotated (i.e. not up-right) rectangles on a plane. Each rectangle
-- is specified by the center point (mass center), length of each side
-- (represented by 'Size2f') and the rotation angle in degrees.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rotatedrect OpenCV doc>
newtype RotatedRect = RotatedRect {unRotatedRect :: ForeignPtr C'RotatedRect}
-- | Termination criteria for iterative algorithms
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#termcriteria OpenCV doc>
newtype TermCriteria = TermCriteria { unTermCriteria :: ForeignPtr C'TermCriteria }

class ToPoint2i a where toPoint2i :: a -> Point2i
class ToPoint2f a where toPoint2f :: a -> Point2f
class ToPoint2d a where toPoint2d :: a -> Point2d
class ToPoint3i a where toPoint3i :: a -> Point3i
class ToPoint3f a where toPoint3f :: a -> Point3f
class ToPoint3d a where toPoint3d :: a -> Point3d
class ToSize2i  a where toSize2i  :: a -> Size2i
class ToSize2f  a where toSize2f  :: a -> Size2f
class ToScalar  a where toScalar  :: a -> Scalar

class FromPoint2i a where fromPoint2i :: Point2i -> a
class FromPoint2f a where fromPoint2f :: Point2f -> a
class FromPoint2d a where fromPoint2d :: Point2d -> a
class FromPoint3i a where fromPoint3i :: Point3i -> a
class FromPoint3f a where fromPoint3f :: Point3f -> a
class FromPoint3d a where fromPoint3d :: Point3d -> a
class FromSize2i  a where fromSize2i  :: Size2i  -> a
class FromSize2f  a where fromSize2f  :: Size2f  -> a
class FromScalar  a where fromScalar  :: Scalar  -> a

instance ToPoint2i Point2i where toPoint2i = id
instance ToPoint2f Point2f where toPoint2f = id
instance ToPoint2d Point2d where toPoint2d = id
instance ToPoint3i Point3i where toPoint3i = id
instance ToPoint3f Point3f where toPoint3f = id
instance ToPoint3d Point3d where toPoint3d = id
instance ToSize2i  Size2i  where toSize2i  = id
instance ToSize2f  Size2f  where toSize2f  = id
instance ToScalar  Scalar  where toScalar  = id

instance FromPoint2i Point2i where fromPoint2i = id
instance FromPoint2f Point2f where fromPoint2f = id
instance FromPoint2d Point2d where fromPoint2d = id
instance FromPoint3i Point3i where fromPoint3i = id
instance FromPoint3f Point3f where fromPoint3f = id
instance FromPoint3d Point3d where fromPoint3d = id
instance FromSize2i  Size2i  where fromSize2i  = id
instance FromSize2f  Size2f  where fromSize2f  = id
instance FromScalar  Scalar  where fromScalar  = id

instance ToPoint2i (V2 Int32  ) where toPoint2i = unsafePerformIO . newPoint2i
instance ToPoint2f (V2 CFloat ) where toPoint2f = unsafePerformIO . newPoint2f
instance ToPoint2d (V2 CDouble) where toPoint2d = unsafePerformIO . newPoint2d
instance ToPoint3i (V3 Int32  ) where toPoint3i = unsafePerformIO . newPoint3i
instance ToPoint3f (V3 CFloat ) where toPoint3f = unsafePerformIO . newPoint3f
instance ToPoint3d (V3 CDouble) where toPoint3d = unsafePerformIO . newPoint3d
instance ToSize2i  (V2 Int32  ) where toSize2i  = unsafePerformIO . newSize2i
instance ToSize2f  (V2 CFloat ) where toSize2f  = unsafePerformIO . newSize2f
instance ToScalar  (V4 CDouble) where toScalar  = unsafePerformIO . newScalar

instance ToPoint2f (V2 Float ) where toPoint2f = toPoint2f . fmap (realToFrac :: Float  -> CFloat )
instance ToPoint2d (V2 Double) where toPoint2d = toPoint2d . fmap (realToFrac :: Double -> CDouble)
instance ToPoint3f (V3 Float ) where toPoint3f = toPoint3f . fmap (realToFrac :: Float  -> CFloat )
instance ToPoint3d (V3 Double) where toPoint3d = toPoint3d . fmap (realToFrac :: Double -> CDouble)
instance ToSize2f  (V2 Float ) where toSize2f  = toSize2f  . fmap (realToFrac :: Float  -> CFloat )
instance ToScalar  (V4 Double) where toScalar  = toScalar  . fmap (realToFrac :: Double -> CDouble)

instance ToPoint2i (Int32  , Int32  ) where toPoint2i (x, y) = toPoint2i $ V2 x y
instance ToPoint2f (CFloat , CFloat ) where toPoint2f (x, y) = toPoint2f $ V2 x y
instance ToPoint2f (Float  , Float  ) where toPoint2f (x, y) = toPoint2f $ V2 x y
instance ToPoint2d (CDouble, CDouble) where toPoint2d (x, y) = toPoint2d $ V2 x y
instance ToPoint2d (Double , Double ) where toPoint2d (x, y) = toPoint2d $ V2 x y
instance ToSize2i  (Int32  , Int32  ) where toSize2i  (x, y) = toSize2i  $ V2 x y
instance ToSize2f  (CFloat , CFloat ) where toSize2f  (x, y) = toSize2f  $ V2 x y
instance ToSize2f  (Float  , Float  ) where toSize2f  (x, y) = toSize2f  $ V2 x y
instance ToPoint3i (Int32  , Int32  , Int32  ) where toPoint3i (x, y, z) = toPoint3i $ V3 x y z
instance ToPoint3f (CFloat , CFloat , CFloat ) where toPoint3f (x, y, z) = toPoint3f $ V3 x y z
instance ToPoint3f (Float  , Float  , Float  ) where toPoint3f (x, y, z) = toPoint3f $ V3 x y z
instance ToPoint3d (CDouble, CDouble, CDouble) where toPoint3d (x, y, z) = toPoint3d $ V3 x y z
instance ToPoint3d (Double , Double , Double ) where toPoint3d (x, y, z) = toPoint3d $ V3 x y z
instance ToScalar  (CDouble, CDouble, CDouble, CDouble) where toScalar (x, y, z, w) = toScalar $ V4 x y z w
instance ToScalar  (Double , Double , Double , Double ) where toScalar (x, y, z, w) = toScalar $ V4 x y z w

instance FromPoint2i (V2 Int32) where
    fromPoint2i pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2iPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2i * p = $(Point2i * ptPtr);
          *$(int32_t * xPtr) = p->x;
          *$(int32_t * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance FromPoint2f (V2 CFloat) where
    fromPoint2f pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2fPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2f * p = $(Point2f * ptPtr);
          *$(float * xPtr) = p->x;
          *$(float * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance FromPoint2d (V2 CDouble) where
    fromPoint2d pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2dPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2d * p = $(Point2d * ptPtr);
          *$(double * xPtr) = p->x;
          *$(double * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance FromPoint3i (V3 Int32) where
    fromPoint3i pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPoint3iPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3i * p = $(Point3i * ptPtr);
          *$(int32_t * xPtr) = p->x;
          *$(int32_t * yPtr) = p->y;
          *$(int32_t * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance FromPoint3f (V3 CFloat) where
    fromPoint3f pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPoint3fPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3f * p = $(Point3f * ptPtr);
          *$(float * xPtr) = p->x;
          *$(float * yPtr) = p->y;
          *$(float * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance FromPoint3d (V3 CDouble) where
    fromPoint3d pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPoint3dPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3d * p = $(Point3d * ptPtr);
          *$(double * xPtr) = p->x;
          *$(double * yPtr) = p->y;
          *$(double * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance FromSize2i (V2 Int32) where
    fromSize2i s = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withSize2iPtr s $ \sPtr -> do
        [CU.block| void {
          Size2i * s = $(Size2i * sPtr);
          *$(int32_t * wPtr) = s->width;
          *$(int32_t * hPtr) = s->height;
        }|]
        V2 <$> peek wPtr
           <*> peek hPtr

instance FromSize2f (V2 CFloat) where
    fromSize2f s = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withSize2fPtr s $ \sPtr -> do
        [CU.block| void {
          Size2f * s = $(Size2f * sPtr);
          *$(float * wPtr) = s->width;
          *$(float * hPtr) = s->height;
        }|]
        V2 <$> peek wPtr
           <*> peek hPtr

instance FromScalar (V4 CDouble) where
    fromScalar s = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      alloca $ \wPtr ->
      withScalarPtr s $ \sPtr -> do
        [CU.block| void {
          const Scalar & s = *$(Scalar * sPtr);
          *$(double * xPtr) = s[0];
          *$(double * yPtr) = s[1];
          *$(double * zPtr) = s[2];
          *$(double * wPtr) = s[3];
        }|]
        V4 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr
           <*> peek wPtr

instance FromPoint2f (V2 Float ) where fromPoint2f = fmap (realToFrac :: CFloat  -> Float ) . fromPoint2f
instance FromPoint2d (V2 Double) where fromPoint2d = fmap (realToFrac :: CDouble -> Double) . fromPoint2d
instance FromPoint3f (V3 Float ) where fromPoint3f = fmap (realToFrac :: CFloat  -> Float ) . fromPoint3f
instance FromPoint3d (V3 Double) where fromPoint3d = fmap (realToFrac :: CDouble -> Double) . fromPoint3d
instance FromSize2f  (V2 Float ) where fromSize2f  = fmap (realToFrac :: CFloat  -> Float ) . fromSize2f
instance FromScalar  (V4 Double) where fromScalar  = fmap (realToFrac :: CDouble -> Double) . fromScalar

instance FromPoint2i (Int32  , Int32  ) where fromPoint2i p = let V2 x y = fromPoint2i p in (x, y)
instance FromPoint2f (CFloat , CFloat ) where fromPoint2f p = let V2 x y = fromPoint2f p in (x, y)
instance FromPoint2f (Float  , Float  ) where fromPoint2f p = let V2 x y = fromPoint2f p in (x, y)
instance FromPoint2d (CDouble, CDouble) where fromPoint2d p = let V2 x y = fromPoint2d p in (x, y)
instance FromPoint2d (Double , Double ) where fromPoint2d p = let V2 x y = fromPoint2d p in (x, y)
instance FromSize2i  (Int32  , Int32  ) where fromSize2i  s = let V2 x y = fromSize2i  s in (x, y)
instance FromSize2f  (CFloat , CFloat ) where fromSize2f  s = let V2 x y = fromSize2f  s in (x, y)
instance FromSize2f  (Float  , Float  ) where fromSize2f  s = let V2 x y = fromSize2f  s in (x, y)
instance FromPoint3i (Int32  , Int32  , Int32  ) where fromPoint3i p = let V3 x y z = fromPoint3i p in (x, y, z)
instance FromPoint3f (CFloat , CFloat , CFloat ) where fromPoint3f p = let V3 x y z = fromPoint3f p in (x, y, z)
instance FromPoint3f (Float  , Float  , Float  ) where fromPoint3f p = let V3 x y z = fromPoint3f p in (x, y, z)
instance FromPoint3d (CDouble, CDouble, CDouble) where fromPoint3d p = let V3 x y z = fromPoint3d p in (x, y, z)
instance FromPoint3d (Double , Double , Double ) where fromPoint3d p = let V3 x y z = fromPoint3d p in (x, y, z)
instance FromScalar  (CDouble, CDouble, CDouble, CDouble) where fromScalar s = let V4 x y z w = fromScalar s in (x, y, z, w)
instance FromScalar  (Double , Double , Double , Double ) where fromScalar s = let V4 x y z w = fromScalar s in (x, y, z, w)

point2iFromPtr :: IO (Ptr C'Point2i) -> IO Point2i
point2fFromPtr :: IO (Ptr C'Point2f) -> IO Point2f
point2dFromPtr :: IO (Ptr C'Point2d) -> IO Point2d
point3iFromPtr :: IO (Ptr C'Point3i) -> IO Point3i
point3fFromPtr :: IO (Ptr C'Point3f) -> IO Point3f
point3dFromPtr :: IO (Ptr C'Point3d) -> IO Point3d
size2iFromPtr  :: IO (Ptr C'Size2i ) -> IO Size2i
size2fFromPtr  :: IO (Ptr C'Size2f ) -> IO Size2f
scalarFromPtr  :: IO (Ptr C'Scalar ) -> IO Scalar
rectFromPtr    :: IO (Ptr C'Rect   ) -> IO Rect
rotatedRectFromPtr  :: IO (Ptr C'RotatedRect ) -> IO RotatedRect
termCriteriaFromPtr :: IO (Ptr C'TermCriteria) -> IO TermCriteria

point2iFromPtr = objFromPtr Point2i $ \ptr -> [CU.exp| void { delete $(Point2i * ptr) }|]
point2fFromPtr = objFromPtr Point2f $ \ptr -> [CU.exp| void { delete $(Point2f * ptr) }|]
point2dFromPtr = objFromPtr Point2d $ \ptr -> [CU.exp| void { delete $(Point2d * ptr) }|]
point3iFromPtr = objFromPtr Point3i $ \ptr -> [CU.exp| void { delete $(Point3i * ptr) }|]
point3fFromPtr = objFromPtr Point3f $ \ptr -> [CU.exp| void { delete $(Point3f * ptr) }|]
point3dFromPtr = objFromPtr Point3d $ \ptr -> [CU.exp| void { delete $(Point3d * ptr) }|]
size2iFromPtr  = objFromPtr Size2i  $ \ptr -> [CU.exp| void { delete $(Size2i  * ptr) }|]
size2fFromPtr  = objFromPtr Size2f  $ \ptr -> [CU.exp| void { delete $(Size2f  * ptr) }|]
scalarFromPtr  = objFromPtr Scalar  $ \ptr -> [CU.exp| void { delete $(Scalar  * ptr) }|]
rectFromPtr    = objFromPtr Rect    $ \ptr -> [CU.exp| void { delete $(Rect    * ptr) }|]
rotatedRectFromPtr  = objFromPtr RotatedRect  $ \ptr -> [CU.exp| void { delete $(RotatedRect  * ptr) }|]
termCriteriaFromPtr = objFromPtr TermCriteria $ \ptr -> [CU.exp| void { delete $(TermCriteria * ptr) }|]

withPoint2iPtr :: (ToPoint2i p) => p    -> (Ptr C'Point2i -> IO a) -> IO a
withPoint2fPtr :: (ToPoint2f p) => p    -> (Ptr C'Point2f -> IO a) -> IO a
withPoint2dPtr :: (ToPoint2d p) => p    -> (Ptr C'Point2d -> IO a) -> IO a
withPoint3iPtr :: (ToPoint3i p) => p    -> (Ptr C'Point3i -> IO a) -> IO a
withPoint3fPtr :: (ToPoint3f p) => p    -> (Ptr C'Point3f -> IO a) -> IO a
withPoint3dPtr :: (ToPoint3d p) => p    -> (Ptr C'Point3d -> IO a) -> IO a
withSize2iPtr  :: (ToSize2i s)  => s    -> (Ptr C'Size2i  -> IO a) -> IO a
withSize2fPtr  :: (ToSize2f s)  => s    -> (Ptr C'Size2f  -> IO a) -> IO a
withScalarPtr  :: (ToScalar s)  => s    -> (Ptr C'Scalar  -> IO a) -> IO a
withRectPtr    ::                  Rect -> (Ptr C'Rect    -> IO a) -> IO a
withRotatedRectPtr  :: RotatedRect  -> (Ptr C'RotatedRect  -> IO a) -> IO a
withTermCriteriaPtr :: TermCriteria -> (Ptr C'TermCriteria -> IO a) -> IO a

withPoint2iPtr = withForeignPtr . unPoint2i . toPoint2i
withPoint2fPtr = withForeignPtr . unPoint2f . toPoint2f
withPoint2dPtr = withForeignPtr . unPoint2d . toPoint2d
withPoint3iPtr = withForeignPtr . unPoint3i . toPoint3i
withPoint3fPtr = withForeignPtr . unPoint3f . toPoint3f
withPoint3dPtr = withForeignPtr . unPoint3d . toPoint3d
withSize2iPtr  = withForeignPtr . unSize2i  . toSize2i
withSize2fPtr  = withForeignPtr . unSize2f  . toSize2f
withScalarPtr  = withForeignPtr . unScalar  . toScalar
withRectPtr    = withForeignPtr . unRect
withRotatedRectPtr  = withForeignPtr . unRotatedRect
withTermCriteriaPtr = withForeignPtr . unTermCriteria

newPoint2i :: V2 Int32 -> IO Point2i
newPoint2i (V2 x y) = point2iFromPtr $
    [CU.exp|Point2i * { new cv::Point2i($(int32_t x), $(int32_t y)) }|]

newPoint2f :: V2 CFloat -> IO Point2f
newPoint2f (V2 x y) = point2fFromPtr $
    [CU.exp|Point2f * { new cv::Point2f($(float x), $(float y)) }|]

newPoint2d :: V2 CDouble -> IO Point2d
newPoint2d (V2 x y) = point2dFromPtr $
    [CU.exp|Point2d * { new cv::Point2d($(double x), $(double y)) }|]

newPoint3i :: V3 Int32 -> IO Point3i
newPoint3i (V3 x y z) = point3iFromPtr $
    [CU.exp|Point3i * { new cv::Point3i($(int32_t x), $(int32_t y), $(int32_t z)) }|]

newPoint3f :: V3 CFloat -> IO Point3f
newPoint3f (V3 x y z) = point3fFromPtr $
    [CU.exp|Point3f * { new cv::Point3f($(float x), $(float y), $(float z)) }|]

newPoint3d :: V3 CDouble -> IO Point3d
newPoint3d (V3 x y z) = point3dFromPtr $
    [CU.exp|Point3d * { new cv::Point3d($(double x), $(double y), $(double z)) }|]

newSize2i :: V2 Int32 -> IO Size2i
newSize2i (V2 x y) = size2iFromPtr $
    [CU.exp|Size2i * { new cv::Size2i($(int32_t x), $(int32_t y)) }|]

newSize2f :: V2 CFloat -> IO Size2f
newSize2f (V2 x y) = size2fFromPtr $
    [CU.exp|Size2f * { new cv::Size2f($(float x), $(float y)) }|]

newScalar :: V4 CDouble -> IO Scalar
newScalar (V4 x y z w) = scalarFromPtr $
    [CU.exp|Scalar * { new cv::Scalar( $(double x)
                                     , $(double y)
                                     , $(double z)
                                     , $(double w)
                                     )
                     }|]

newRect
    :: V2 Int32 -- ^ top left
    -> V2 Int32 -- ^ size
    -> IO Rect
newRect (V2 x y) (V2 width height) = rectFromPtr $
    [CU.exp|Rect * { new cv::Rect( $(int32_t x)
                                 , $(int32_t y)
                                 , $(int32_t width)
                                 , $(int32_t height)
                                 )
                   }|]

newRotatedRect
    :: (ToPoint2f point2f, ToSize2f size2f)
    => point2f -- ^ Rectangle mass center
    -> size2f -- ^ Width and height of the rectangle
    -> CFloat
       -- ^ The rotation angle (in degrees). When the angle is 0, 90,
       -- 180, 270 etc., the rectangle becomes an up-right rectangle.
    -> IO RotatedRect
newRotatedRect center size angle =
    rotatedRectFromPtr $
    withPoint2fPtr center $ \centerPtr ->
    withSize2fPtr size $ \sizePtr ->
      [CU.exp| RotatedRect * {
          new cv::RotatedRect( *$(Point2f * centerPtr)
                             , *$(Size2f * sizePtr)
                             , $(float angle)
                             )
      }|]


#include "termcriteria.hpp"
#num TERMCRITERIA_COUNT
#num TERMCRITERIA_EPS

newTermCriteria
    :: Maybe Int    -- ^ Optionally the maximum number of iterations/elements.
    -> Maybe Double -- ^ Optionally the desired accuracy.
    -> IO TermCriteria
newTermCriteria mbMaxCount mbEpsilon = termCriteriaFromPtr $
    [CU.exp|TermCriteria * {
      new cv::TermCriteria( $(int32_t c'type    )
                          , $(int32_t c'maxCount)
                          , $(double  c'epsilon )
                          )
    }|]
  where
    c'type =   maybe 0 (const c'TERMCRITERIA_COUNT) mbMaxCount
           .|. maybe 0 (const c'TERMCRITERIA_EPS  ) mbEpsilon
    c'maxCount = maybe 0 fromIntegral mbMaxCount
    c'epsilon  = maybe 0 realToFrac   mbEpsilon


--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

withPolygons
    :: forall a point2i
     . (ToPoint2i point2i)
    => V.Vector (V.Vector point2i)
    -> (Ptr (Ptr C'Point2i) -> IO a)
    -> IO a
withPolygons polygons act =
    allocaArray (V.length polygons) $ \polygonsPtr -> do
      let go :: Ptr (Ptr C'Point2i) -> Int -> IO a
          go !acc !ix
            | ix < V.length polygons =
                withPoint2is (V.unsafeIndex polygons ix) $ \ptsPtr -> do
                  poke acc ptsPtr
                  go (acc `plusPtr` sizeOf (undefined :: Ptr (Ptr C'Point2i))) (ix + 1)
            | otherwise = act polygonsPtr
      go polygonsPtr 0

withPoint2is
    :: forall a point2i
     . (ToPoint2i point2i)
    => V.Vector point2i
    -> (Ptr C'Point2i -> IO a)
    -> IO a
withPoint2is points act =
    allocaBytes (c'sizeof_Point2i * V.length points) $ \ptsPtr -> do
      V.foldM'_ copyNext ptsPtr points
      act ptsPtr
  where
    copyNext :: Ptr C'Point2i -> point2i -> IO (Ptr C'Point2i)
    copyNext !ptr point = copyPoint2i ptr point $> plusPtr ptr c'sizeof_Point2i

    copyPoint2i :: Ptr C'Point2i -> point2i -> IO ()
    copyPoint2i destPtr src =
      withPoint2iPtr (toPoint2i src) $ \srcPtr ->
        [C.exp| void { new($(Point2i * destPtr)) Point2i(*$(Point2i * srcPtr)) }|]

withPoint2fs
    :: forall a point2i
     . (ToPoint2f point2i)
    => V.Vector point2i
    -> (Ptr C'Point2f -> IO a)
    -> IO a
withPoint2fs points act =
    allocaBytes (c'sizeof_Point2f * V.length points) $ \ptsPtr -> do
      V.foldM'_ copyNext ptsPtr points
      act ptsPtr
  where
    copyNext :: Ptr C'Point2f -> point2i -> IO (Ptr C'Point2f)
    copyNext !ptr point = copyPoint2f ptr point $> plusPtr ptr c'sizeof_Point2f

    copyPoint2f :: Ptr C'Point2f -> point2i -> IO ()
    copyPoint2f destPtr src =
      withPoint2fPtr (toPoint2f src) $ \srcPtr ->
        [C.exp| void { new($(Point2f * destPtr)) Point2f(*$(Point2f * srcPtr)) }|]
