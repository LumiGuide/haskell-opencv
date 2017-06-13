{-# language CPP #-}
{-# language MultiParamTypeClasses #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types
    ( -- * Scalar
      Scalar(..)
    , newScalar
    , ToScalar(..), FromScalar(..)
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
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V4 ( V4(..) )
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Core.Types.Size
import "this" OpenCV.Internal
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types.Constants
import "this" OpenCV.Internal.C.PlacementNew
import "this" OpenCV.Internal.C.PlacementNew.TH
import "this" OpenCV.Internal.C.Types
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A 4-element vector with 64 bit floating point elements
--
-- The type 'Scalar' is widely used in OpenCV to pass pixel values.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#scalar OpenCV Sphinx doc>
newtype Scalar = Scalar {unScalar :: ForeignPtr (C Scalar)}

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

class ToScalar  a where toScalar  :: a -> Scalar

instance ToScalar  Scalar  where toScalar  = id

instance ToScalar  (V4 CDouble) where toScalar  = unsafePerformIO . newScalar

instance ToScalar  (V4 Double ) where toScalar  = toScalar  . fmap (realToFrac :: Double -> CDouble)

class FromScalar  a where fromScalar  :: Scalar  -> a

instance FromScalar  Scalar  where fromScalar  = id

instance FromScalar (V4 CDouble) where
    fromScalar s = unsafePerformIO $
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

instance FromScalar (V4 Double) where fromScalar = fmap (realToFrac :: CDouble -> Double) . fromScalar

--------------------------------------------------------------------------------
-- Constructing new values
--------------------------------------------------------------------------------

newScalar :: V4 CDouble -> IO Scalar
newScalar (V4 x y z w) = fromPtr $
    [CU.exp|Scalar * { new cv::Scalar( $(double x)
                                     , $(double y)
                                     , $(double z)
                                     , $(double w)
                                     )
                     }|]

newRotatedRect
    :: ( IsPoint2 point2 CFloat
       , IsSize   size   CFloat
       )
    => point2 CFloat -- ^ Rectangle mass center
    -> size   CFloat -- ^ Width and height of the rectangle
    -> CFloat
       -- ^ The rotation angle (in degrees). When the angle is 0, 90,
       -- 180, 270 etc., the rectangle becomes an up-right rectangle.
    -> IO RotatedRect
newRotatedRect center size angle = fromPtr $
    withPtr (toPoint center) $ \centerPtr ->
    withPtr (toSize  size)   $ \sizePtr   ->
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
    :: forall a point2
     . (IsPoint2 point2 Int32)
    => V.Vector (V.Vector (point2 Int32))
    -> (Ptr (Ptr (C Point2i)) -> IO a)
    -> IO a
withPolygons polygons act =
    allocaArray (V.length polygons) $ \polygonsPtr -> do
      let go :: Ptr (Ptr (C Point2i)) -> Int -> IO a
          go !acc !ix
            | ix < V.length polygons =
                let pts = V.map toPoint $ V.unsafeIndex polygons ix
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

type instance C Scalar       = C'Scalar
type instance C RotatedRect  = C'RotatedRect
type instance C TermCriteria = C'TermCriteria
type instance C Range        = C'Range

--------------------------------------------------------------------------------

instance WithPtr Scalar       where withPtr = withForeignPtr . unScalar
instance WithPtr RotatedRect  where withPtr = withForeignPtr . unRotatedRect
instance WithPtr TermCriteria where withPtr = withForeignPtr . unTermCriteria
instance WithPtr Range        where withPtr = withForeignPtr . unRange

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Scalar

--------------------------------------------------------------------------------

instance FromPtr Scalar where
    fromPtr = objFromPtr Scalar $ \ptr ->
                [CU.exp| void { delete $(Scalar * ptr) }|]

instance FromPtr RotatedRect where
    fromPtr = objFromPtr RotatedRect $ \ptr ->
                [CU.exp| void { delete $(RotatedRect * ptr) }|]

instance FromPtr TermCriteria where
    fromPtr = objFromPtr TermCriteria $ \ptr ->
                [CU.exp| void { delete $(TermCriteria * ptr) }|]

instance FromPtr Range where
    fromPtr = objFromPtr Range $ \ptr ->
                [CU.exp| void { delete $(Range * ptr) }|]
