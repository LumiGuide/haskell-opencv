{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Internal
    ( -- * Point
      -- ** 2D types
      Point2i(..)
    , Point2f(..)
    , Point2d(..)
    , newPoint2i
    , newPoint2f
    , newPoint2d
      -- ** 3D types
    , Point3i(..)
    , Point3f(..)
    , Point3d(..)
    , newPoint3i
    , newPoint3f
    , newPoint3d
      -- * Size
    , Size2i(..)
    , Size2f(..)
    , newSize2i
    , newSize2f
      -- * Scalar
    , Scalar(..)
    , newScalar
      -- * Rect
    , Rect(..)
    , newRect
      -- * RotatedRect
    , RotatedRect(..)
    , newRotatedRect
      -- * TermCriteria
    , TermCriteria(..)
    , newTermCriteria
      -- * Range
    , Range(..)
    , newRange
    , newWholeRange
      -- * Polygons
    , withPolygons
    , withArrayPtr
      -- * Polymorphic stuff
    , PointT
    ) where

import "base" Control.Exception ( bracket_ )
import "base" Data.Bits ( (.|.) )
import "base" Data.Functor ( ($>) )
import "base" Data.Int ( Int32 )
import "base" Data.Proxy ( Proxy(..) )
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca, allocaBytes )
import "base" Foreign.Marshal.Array ( allocaArray )
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( sizeOf, peek, poke )
import "base" GHC.TypeLits ( Nat )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.PlacementNew
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Constants
import "this" OpenCV.Internal
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | 2D point with integer coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV Sphinx doc>
newtype Point2i = Point2i {unPoint2i :: ForeignPtr (C Point2i)}

-- | 2D point with 32 bit floating point coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV Sphinx doc>
newtype Point2f = Point2f {unPoint2f :: ForeignPtr (C Point2f)}

-- | 2D point with 64 bit floating point coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point OpenCV Sphinx doc>
newtype Point2d = Point2d {unPoint2d :: ForeignPtr (C Point2d)}

-- | 3D point with integer coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV Sphinx doc>
newtype Point3i = Point3i {unPoint3i :: ForeignPtr (C Point3i)}

-- | 3D point with 32 bit floating point coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV Sphinx doc>
newtype Point3f = Point3f {unPoint3f :: ForeignPtr (C Point3f)}

-- | 3D point with 64 bit floating point coördinates
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#point3 OpenCV Sphinx doc>
newtype Point3d = Point3d {unPoint3d :: ForeignPtr (C Point3d)}

-- | Size of an image or rectangle with integer values
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV Sphinx doc>
newtype Size2i = Size2i {unSize2i :: ForeignPtr (C Size2i)}

-- | Size of an image or rectangle with 32 bit floating point values
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#size OpenCV Sphinx doc>
newtype Size2f = Size2f {unSize2f :: ForeignPtr (C Size2f)}

-- | A 4-element vector with 64 bit floating point elements
--
-- The type 'Scalar' is widely used in OpenCV to pass pixel values.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#scalar OpenCV Sphinx doc>
newtype Scalar = Scalar {unScalar :: ForeignPtr (C Scalar)}

-- | 2D rectangles
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rect OpenCV Sphinx doc>
newtype Rect = Rect {unRect :: ForeignPtr (C Rect)}

-- | Rotated (i.e. not up-right) rectangles on a plane
--
-- Each rectangle is specified by the center point (mass center), length of each
-- side (represented by 'Size2f') and the rotation angle in degrees.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#rotatedrect OpenCV Sphinx doc>
newtype RotatedRect = RotatedRect {unRotatedRect :: ForeignPtr (C RotatedRect)}

-- | Termination criteria for iterative algorithms
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#termcriteria OpenCV Sphinx doc>
newtype TermCriteria = TermCriteria {unTermCriteria :: ForeignPtr (C TermCriteria)}

-- | A continuous subsequence (slice) of a sequence
--
-- The type is used to specify a row or a column span in a matrix (`Mat`) and
-- for many other purposes. @'mkRange' a b@ is basically the same as @a:b@ in
-- Matlab or @a..b@ in Python. As in Python, start is an inclusive left boundary
-- of the range and end is an exclusive right boundary of the range. Such a
-- half-opened interval is usually denoted as @[start, end)@.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#range OpenCV Sphinx doc>
newtype Range = Range {unRange :: ForeignPtr (C Range)}


--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

instance Convert (V2 Int32  ) Point2i where convert = unsafePerformIO . newPoint2i
instance Convert (V2 CFloat ) Point2f where convert = unsafePerformIO . newPoint2f
instance Convert (V2 CDouble) Point2d where convert = unsafePerformIO . newPoint2d
instance Convert (V3 Int32  ) Point3i where convert = unsafePerformIO . newPoint3i
instance Convert (V3 CFloat ) Point3f where convert = unsafePerformIO . newPoint3f
instance Convert (V3 CDouble) Point3d where convert = unsafePerformIO . newPoint3d
instance Convert (V2 Int32  ) Size2i  where convert = unsafePerformIO . newSize2i
instance Convert (V2 CFloat ) Size2f  where convert = unsafePerformIO . newSize2f
instance Convert (V4 CDouble) Scalar  where convert = unsafePerformIO . newScalar

instance Convert (V2 Float  ) Point2f where convert = convert . fmap (realToFrac :: Float  -> CFloat )
instance Convert (V2 Double ) Point2d where convert = convert . fmap (realToFrac :: Double -> CDouble)
instance Convert (V3 Float  ) Point3f where convert = convert . fmap (realToFrac :: Float  -> CFloat )
instance Convert (V3 Double ) Point3d where convert = convert . fmap (realToFrac :: Double -> CDouble)
instance Convert (V2 Float  ) Size2f  where convert = convert . fmap (realToFrac :: Float  -> CFloat )
instance Convert (V4 Double ) Scalar  where convert = convert . fmap (realToFrac :: Double -> CDouble)

instance Convert (Int32  , Int32  ) Point2i where convert (x, y) = convert $ V2 x y
instance Convert (CFloat , CFloat ) Point2f where convert (x, y) = convert $ V2 x y
instance Convert (Float  , Float  ) Point2f where convert (x, y) = convert $ V2 x y
instance Convert (CDouble, CDouble) Point2d where convert (x, y) = convert $ V2 x y
instance Convert (Double , Double ) Point2d where convert (x, y) = convert $ V2 x y
instance Convert (Int32  , Int32  ) Size2i  where convert (x, y) = convert $ V2 x y
instance Convert (CFloat , CFloat ) Size2f  where convert (x, y) = convert $ V2 x y
instance Convert (Float  , Float  ) Size2f  where convert (x, y) = convert $ V2 x y
instance Convert (Int32  , Int32  , Int32  ) Point3i where convert (x, y, z) = convert $ V3 x y z
instance Convert (CFloat , CFloat , CFloat ) Point3f where convert (x, y, z) = convert $ V3 x y z
instance Convert (Float  , Float  , Float  ) Point3f where convert (x, y, z) = convert $ V3 x y z
instance Convert (CDouble, CDouble, CDouble) Point3d where convert (x, y, z) = convert $ V3 x y z
instance Convert (Double , Double , Double ) Point3d where convert (x, y, z) = convert $ V3 x y z
instance Convert (CDouble, CDouble, CDouble, CDouble) Scalar where convert (x, y, z, w) = convert $ V4 x y z w
instance Convert (Double , Double , Double , Double ) Scalar where convert (x, y, z, w) = convert $ V4 x y z w

instance Convert Point2i (V2 Int32  ) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2i * p = $(Point2i * ptPtr);
          *$(int32_t * xPtr) = p->x;
          *$(int32_t * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance Convert Point2f (V2 CFloat ) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2f * p = $(Point2f * ptPtr);
          *$(float * xPtr) = p->x;
          *$(float * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance Convert Point2d (V2 CDouble) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2d * p = $(Point2d * ptPtr);
          *$(double * xPtr) = p->x;
          *$(double * yPtr) = p->y;
        }|]
        V2 <$> peek xPtr
           <*> peek yPtr

instance Convert Point3i (V3 Int32  ) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3i * p = $(Point3i * ptPtr);
          *$(int32_t * xPtr) = p->x;
          *$(int32_t * yPtr) = p->y;
          *$(int32_t * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance Convert Point3f (V3 CFloat ) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3f * p = $(Point3f * ptPtr);
          *$(float * xPtr) = p->x;
          *$(float * yPtr) = p->y;
          *$(float * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance Convert Point3d (V3 CDouble) where
    convert pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3d * p = $(Point3d * ptPtr);
          *$(double * xPtr) = p->x;
          *$(double * yPtr) = p->y;
          *$(double * zPtr) = p->z;
        }|]
        V3 <$> peek xPtr
           <*> peek yPtr
           <*> peek zPtr

instance Convert Size2i  (V2 Int32  ) where
    convert s = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withPtr s $ \sPtr -> do
        [CU.block| void {
          Size2i * s = $(Size2i * sPtr);
          *$(int32_t * wPtr) = s->width;
          *$(int32_t * hPtr) = s->height;
        }|]
        V2 <$> peek wPtr
           <*> peek hPtr

instance Convert Size2f  (V2 CFloat ) where
    convert s = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withPtr s $ \sPtr -> do
        [CU.block| void {
          Size2f * s = $(Size2f * sPtr);
          *$(float * wPtr) = s->width;
          *$(float * hPtr) = s->height;
        }|]
        V2 <$> peek wPtr
           <*> peek hPtr

instance Convert Scalar  (V4 CDouble) where
    convert s = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      alloca $ \wPtr ->
      withPtr s $ \sPtr -> do
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

instance Convert Point2f (V2 Float ) where convert = fmap (realToFrac :: CFloat  -> Float ) . convert
instance Convert Point2d (V2 Double) where convert = fmap (realToFrac :: CDouble -> Double) . convert
instance Convert Point3f (V3 Float ) where convert = fmap (realToFrac :: CFloat  -> Float ) . convert
instance Convert Point3d (V3 Double) where convert = fmap (realToFrac :: CDouble -> Double) . convert
instance Convert Size2f  (V2 Float ) where convert = fmap (realToFrac :: CFloat  -> Float ) . convert
instance Convert Scalar  (V4 Double) where convert = fmap (realToFrac :: CDouble -> Double) . convert

instance Convert Point2i (Int32  , Int32  ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Point2f (CFloat , CFloat ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Point2f (Float  , Float  ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Point2d (CDouble, CDouble) where convert a = let V2 x y = convert a in (x, y)
instance Convert Point2d (Double , Double ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Size2i  (Int32  , Int32  ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Size2f  (CFloat , CFloat ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Size2f  (Float  , Float  ) where convert a = let V2 x y = convert a in (x, y)
instance Convert Point3i (Int32  , Int32  , Int32  ) where convert a = let V3 x y z = convert a in (x, y, z)
instance Convert Point3f (CFloat , CFloat , CFloat ) where convert a = let V3 x y z = convert a in (x, y, z)
instance Convert Point3f (Float  , Float  , Float  ) where convert a = let V3 x y z = convert a in (x, y, z)
instance Convert Point3d (CDouble, CDouble, CDouble) where convert a = let V3 x y z = convert a in (x, y, z)
instance Convert Point3d (Double , Double , Double ) where convert a = let V3 x y z = convert a in (x, y, z)
instance Convert Scalar  (CDouble, CDouble, CDouble, CDouble) where convert a = let V4 x y z w = convert a in (x, y, z, w)
instance Convert Scalar  (Double , Double , Double , Double ) where convert a = let V4 x y z w = convert a in (x, y, z, w)

--------------------------------------------------------------------------------
-- Constructing new values
--------------------------------------------------------------------------------

newPoint2i :: V2 Int32 -> IO Point2i
newPoint2i (V2 x y) = fromPtr $
    [CU.exp|Point2i * { new cv::Point2i($(int32_t x), $(int32_t y)) }|]

newPoint2f :: V2 CFloat -> IO Point2f
newPoint2f (V2 x y) = fromPtr $
    [CU.exp|Point2f * { new cv::Point2f($(float x), $(float y)) }|]

newPoint2d :: V2 CDouble -> IO Point2d
newPoint2d (V2 x y) = fromPtr $
    [CU.exp|Point2d * { new cv::Point2d($(double x), $(double y)) }|]

newPoint3i :: V3 Int32 -> IO Point3i
newPoint3i (V3 x y z) = fromPtr $
    [CU.exp|Point3i * { new cv::Point3i($(int32_t x), $(int32_t y), $(int32_t z)) }|]

newPoint3f :: V3 CFloat -> IO Point3f
newPoint3f (V3 x y z) = fromPtr $
    [CU.exp|Point3f * { new cv::Point3f($(float x), $(float y), $(float z)) }|]

newPoint3d :: V3 CDouble -> IO Point3d
newPoint3d (V3 x y z) = fromPtr $
    [CU.exp|Point3d * { new cv::Point3d($(double x), $(double y), $(double z)) }|]

newSize2i :: V2 Int32 -> IO Size2i
newSize2i (V2 x y) = fromPtr $
    [CU.exp|Size2i * { new cv::Size2i($(int32_t x), $(int32_t y)) }|]

newSize2f :: V2 CFloat -> IO Size2f
newSize2f (V2 x y) = fromPtr $
    [CU.exp|Size2f * { new cv::Size2f($(float x), $(float y)) }|]

newScalar :: V4 CDouble -> IO Scalar
newScalar (V4 x y z w) = fromPtr $
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
newRect (V2 x y) (V2 width height) = fromPtr $
    [CU.exp|Rect * { new cv::Rect( $(int32_t x)
                                 , $(int32_t y)
                                 , $(int32_t width)
                                 , $(int32_t height)
                                 )
                   }|]

newRotatedRect
    :: ( Convert point2f Point2f
       , Convert size2f  Size2f
       )
    => point2f -- ^ Rectangle mass center
    -> size2f -- ^ Width and height of the rectangle
    -> CFloat
       -- ^ The rotation angle (in degrees). When the angle is 0, 90,
       -- 180, 270 etc., the rectangle becomes an up-right rectangle.
    -> IO RotatedRect
newRotatedRect center size angle = fromPtr $
    withPtr (convert center :: Point2f) $ \centerPtr ->
    withPtr (convert size   :: Size2f ) $ \sizePtr ->
      [CU.exp| RotatedRect * {
          new cv::RotatedRect( *$(Point2f * centerPtr)
                             , *$(Size2f * sizePtr)
                             , $(float angle)
                             )
      }|]


newTermCriteria
    :: Maybe Int    -- ^ Optionally the maximum number of iterations/elements.
    -> Maybe Double -- ^ Optionally the desired accuracy.
    -> IO TermCriteria
newTermCriteria mbMaxCount mbEpsilon = fromPtr $
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

newRange
    :: Int32 -- ^ Inclusive start
    -> Int32 -- ^ Exlusive end
    -> IO Range
newRange start end = fromPtr $
    [CU.exp|Range * { new cv::Range( $(int32_t start), $(int32_t end)) }|]

-- | Special 'Range' value which means "the whole sequence" or "the whole range"
newWholeRange :: IO Range
newWholeRange = fromPtr $
    [CU.block|Range * {
      cv::Range a = cv::Range::all();
      return new cv::Range(a.start, a.end);
    }|]


--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

withPolygons
    :: forall a point2i
     . (Convert point2i Point2i)
    => V.Vector (V.Vector point2i)
    -> (Ptr (Ptr (C Point2i)) -> IO a)
    -> IO a
withPolygons polygons act =
    allocaArray (V.length polygons) $ \polygonsPtr -> do
      let go :: Ptr (Ptr (C Point2i)) -> Int -> IO a
          go !acc !ix
            | ix < V.length polygons =
                let pts = V.map convert $ V.unsafeIndex polygons ix :: V.Vector Point2i
                in withArrayPtr pts $ \ptsPtr -> do
                     poke acc ptsPtr
                     go (acc `plusPtr` sizeOf (undefined :: Ptr (Ptr (C Point2i)))) (ix + 1)
            | otherwise = act polygonsPtr
      go polygonsPtr 0

-- | Perform an action with a temporary pointer to an array of values
--
-- The input values are placed consecutively in memory using the 'PlacementNew'
-- mechanism.
--
-- This function is intended for types which are not managed by the Haskell
-- runtime, but by a foreign system (such as C).
--
-- The pointer is not guaranteed to be usuable outside the scope of this
-- function. The same warnings apply as for 'withForeignPtr'.
withArrayPtr
    :: forall a b
     . (WithPtr a, CSizeOf (C a), PlacementNew (C a))
    => V.Vector a
    -> (Ptr (C a) -> IO b)
    -> IO b
withArrayPtr arr act =
    allocaBytes arraySize $ \arrPtr ->
      bracket_
        (V.foldM'_ copyNext arrPtr arr)
        (deconstructArray arrPtr )
        (act arrPtr)
  where
    elemSize = cSizeOf (Proxy :: Proxy (C a))
    arraySize = elemSize * V.length arr

    copyNext :: Ptr (C a) -> a -> IO (Ptr (C a))
    copyNext !ptr obj = copyObj ptr obj $> plusPtr ptr elemSize

    copyObj :: Ptr (C a) -> a -> IO ()
    copyObj dstPtr src =
        withPtr src $ \srcPtr ->
          placementNew srcPtr dstPtr

    deconstructArray :: Ptr (C a) -> IO ()
    deconstructArray !begin = deconstructNext begin
      where
        deconstructNext !ptr
            | ptr == end = pure ()
            | otherwise = do placementDelete ptr
                             deconstructNext $ ptr `plusPtr` elemSize

        end :: Ptr (C a)
        end = begin `plusPtr` arraySize

--------------------------------------------------------------------------------
-- Polymorphic stuff

-- | Calculates a point type given the dimension and element type
type family PointT (dim :: Nat) (depth :: *) :: * where
    PointT 2 Int32   = Point2i
    PointT 2 Float   = Point2f
    PointT 2 Double  = Point2d
    PointT 2 CFloat  = Point2f
    PointT 2 CDouble = Point2d

    PointT 3 Int32   = Point3i
    PointT 3 Float   = Point3f
    PointT 3 Double  = Point3d
    PointT 3 CFloat  = Point3f
    PointT 3 CDouble = Point3d

--------------------------------------------------------------------------------

type instance C Point2i      = C'Point2i
type instance C Point2f      = C'Point2f
type instance C Point2d      = C'Point2d
type instance C Point3i      = C'Point3i
type instance C Point3f      = C'Point3f
type instance C Point3d      = C'Point3d
type instance C Size2i       = C'Size2i
type instance C Size2f       = C'Size2f
type instance C Scalar       = C'Scalar
type instance C Rect         = C'Rect
type instance C RotatedRect  = C'RotatedRect
type instance C TermCriteria = C'TermCriteria
type instance C Range        = C'Range

--------------------------------------------------------------------------------

instance WithPtr Point2i      where withPtr = withForeignPtr . unPoint2i
instance WithPtr Point2f      where withPtr = withForeignPtr . unPoint2f
instance WithPtr Point2d      where withPtr = withForeignPtr . unPoint2d
instance WithPtr Point3i      where withPtr = withForeignPtr . unPoint3i
instance WithPtr Point3f      where withPtr = withForeignPtr . unPoint3f
instance WithPtr Point3d      where withPtr = withForeignPtr . unPoint3d
instance WithPtr Size2i       where withPtr = withForeignPtr . unSize2i
instance WithPtr Size2f       where withPtr = withForeignPtr . unSize2f
instance WithPtr Scalar       where withPtr = withForeignPtr . unScalar
instance WithPtr Rect         where withPtr = withForeignPtr . unRect
instance WithPtr RotatedRect  where withPtr = withForeignPtr . unRotatedRect
instance WithPtr TermCriteria where withPtr = withForeignPtr . unTermCriteria
instance WithPtr Range        where withPtr = withForeignPtr . unRange

--------------------------------------------------------------------------------

instance FromPtr Point2i where
    fromPtr = objFromPtr Point2i $ \ptr ->
                [CU.exp| void { delete $(Point2i * ptr) }|]

instance FromPtr Point2f where
    fromPtr = objFromPtr Point2f $ \ptr ->
                [CU.exp| void { delete $(Point2f * ptr) }|]

instance FromPtr Point2d where
    fromPtr = objFromPtr Point2d $ \ptr ->
                [CU.exp| void { delete $(Point2d * ptr) }|]

instance FromPtr Point3i where
    fromPtr = objFromPtr Point3i $ \ptr ->
                [CU.exp| void { delete $(Point3i * ptr) }|]

instance FromPtr Point3f where
    fromPtr = objFromPtr Point3f $ \ptr ->
                [CU.exp| void { delete $(Point3f * ptr) }|]

instance FromPtr Point3d where
    fromPtr = objFromPtr Point3d $ \ptr ->
                [CU.exp| void { delete $(Point3d * ptr) }|]

instance FromPtr Size2i where
    fromPtr = objFromPtr Size2i $ \ptr ->
                [CU.exp| void { delete $(Size2i * ptr) }|]

instance FromPtr Size2f where
    fromPtr = objFromPtr Size2f $ \ptr ->
                [CU.exp| void { delete $(Size2f * ptr) }|]

instance FromPtr Scalar where
    fromPtr = objFromPtr Scalar $ \ptr ->
                [CU.exp| void { delete $(Scalar * ptr) }|]

instance FromPtr Rect where
    fromPtr = objFromPtr Rect $ \ptr ->
                [CU.exp| void { delete $(Rect * ptr) }|]

instance FromPtr RotatedRect where
    fromPtr = objFromPtr RotatedRect $ \ptr ->
                [CU.exp| void { delete $(RotatedRect * ptr) }|]

instance FromPtr TermCriteria where
    fromPtr = objFromPtr TermCriteria $ \ptr ->
                [CU.exp| void { delete $(TermCriteria * ptr) }|]

instance FromPtr Range where
    fromPtr = objFromPtr Range $ \ptr ->
                [CU.exp| void { delete $(Range * ptr) }|]
