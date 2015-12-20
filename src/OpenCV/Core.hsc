{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module OpenCV.Core
    ( -- * Point
      -- ** 2D types
      Point2i
    , Point2f
    , Point2d
      -- ** 3D types
    , Point3i
    , Point3f
    , Point3d
      -- ** 2D point conversion
    , isoPoint2iV2
    , isoPoint2fV2
    , isoPoint2dV2
      -- ** 3D point conversion
    , isoPoint3iV3
    , isoPoint3fV3
    , isoPoint3dV3
      -- * Size
    , Size2i
    , Size2f
      -- ** Size conversion
    , isoSize2iV2
    , isoSize2fV2
      -- * Rect
    , Rect
    , mkRect
    , rectTopLeft
    , rectBottomRight
    , rectSize
    , rectArea
    , rectContains
      -- * RotatedRect
    , RotatedRect
    , mkRotatedRect
    , rotatedRectCenter
    , rotatedRectSize
    , rotatedRectAngle
    , rotatedRectBoundingRect
    , rotatedRectPoints
      -- * TermCriteria
    , TermCriteria
    , mkTermCriteria
      -- * Scalar
    , Scalar
    , mkScalar
      -- * Matrix
    , Mat
    , newEmptyMat
    , cloneMat
    , matSubRect
      -- ** Repa
    , M
    , toRepa
    , fromRepa
      -- * Mutable Matrix
    , MutMat
    , IOMat
    , STMat
    , freeze
    , thaw
    , createMat
      -- * Exception
    , CvException
      -- * Operations on Arrays
    , addWeighted
    , minMaxLoc
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( peek, sizeOf )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lens" Control.Lens hiding (ix)
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import qualified "repa" Data.Array.Repa as Repa
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import "this" OpenCV.Unsafe

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"


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

{-# SPECIALIZE isoPoint2iV2 :: Iso' Point2i (V2 Int ) #-}
{-# SPECIALIZE isoPoint2iV2 :: Iso' Point2i (V2 CInt) #-}
isoPoint2iV2 :: forall a. (Integral a) => Iso' Point2i (V2 a)
isoPoint2iV2 = iso mkV2 (unsafePerformIO . newPoint2i)
  where
    mkV2 :: Point2i -> V2 a
    mkV2 pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2iPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2i * p = $(Point2i * ptPtr);
          *$(int * xPtr) = p->x;
          *$(int * yPtr) = p->y;
        }|]
        V2 <$> (fromIntegral <$> peek xPtr)
           <*> (fromIntegral <$> peek yPtr)

{-# SPECIALIZE isoPoint2fV2 :: Iso' Point2f (V2 Float ) #-}
{-# SPECIALIZE isoPoint2fV2 :: Iso' Point2f (V2 CFloat) #-}
isoPoint2fV2 :: forall a. (Real a, Fractional a) => Iso' Point2f (V2 a)
isoPoint2fV2 = iso mkV2 (unsafePerformIO . newPoint2f)
  where
    mkV2 :: Point2f -> V2 a
    mkV2 pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2fPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2f * p = $(Point2f * ptPtr);
          *$(float * xPtr) = p->x;
          *$(float * yPtr) = p->y;
        }|]
        V2 <$> (realToFrac <$> peek xPtr)
           <*> (realToFrac <$> peek yPtr)

{-# SPECIALIZE isoPoint2dV2 :: Iso' Point2d (V2 Double ) #-}
{-# SPECIALIZE isoPoint2dV2 :: Iso' Point2d (V2 CDouble) #-}
isoPoint2dV2 :: forall a. (Real a, Fractional a) => Iso' Point2d (V2 a)
isoPoint2dV2 = iso mkV2 (unsafePerformIO . newPoint2d)
  where
    mkV2 :: Point2d -> V2 a
    mkV2 pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      withPoint2dPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point2d * p = $(Point2d * ptPtr);
          *$(double * xPtr) = p->x;
          *$(double * yPtr) = p->y;
        }|]
        V2 <$> (realToFrac <$> peek xPtr)
           <*> (realToFrac <$> peek yPtr)

{-# SPECIALIZE isoPoint3iV3 :: Iso' Point3i (V3 Int ) #-}
{-# SPECIALIZE isoPoint3iV3 :: Iso' Point3i (V3 CInt) #-}
isoPoint3iV3 :: forall a. (Integral a) => Iso' Point3i (V3 a)
isoPoint3iV3 = iso mkV3 (unsafePerformIO . newPoint3i)
  where
    mkV3 :: Point3i -> V3 a
    mkV3 pt = unsafePerformIO $
      alloca $ \xPtr ->
      alloca $ \yPtr ->
      alloca $ \zPtr ->
      withPoint3iPtr pt $ \ptPtr -> do
        [CU.block| void {
          Point3i * p = $(Point3i * ptPtr);
          *$(int * xPtr) = p->x;
          *$(int * yPtr) = p->y;
          *$(int * zPtr) = p->z;
        }|]
        V3 <$> (fromIntegral <$> peek xPtr)
           <*> (fromIntegral <$> peek yPtr)
           <*> (fromIntegral <$> peek zPtr)

{-# SPECIALIZE isoPoint3fV3 :: Iso' Point3f (V3 Float ) #-}
{-# SPECIALIZE isoPoint3fV3 :: Iso' Point3f (V3 CFloat) #-}
isoPoint3fV3 :: forall a. (Real a, Fractional a) => Iso' Point3f (V3 a)
isoPoint3fV3 = iso mkV3 (unsafePerformIO . newPoint3f)
  where
    mkV3 :: Point3f -> V3 a
    mkV3 pt = unsafePerformIO $
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
        V3 <$> (realToFrac <$> peek xPtr)
           <*> (realToFrac <$> peek yPtr)
           <*> (realToFrac <$> peek zPtr)

{-# SPECIALIZE isoPoint3dV3 :: Iso' Point3d (V3 Double ) #-}
{-# SPECIALIZE isoPoint3dV3 :: Iso' Point3d (V3 CDouble) #-}
isoPoint3dV3 :: forall a. (Real a, Fractional a) => Iso' Point3d (V3 a)
isoPoint3dV3 = iso mkV3 (unsafePerformIO . newPoint3d)
  where
    mkV3 :: Point3d -> V3 a
    mkV3 pt = unsafePerformIO $
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
        V3 <$> (realToFrac <$> peek xPtr)
           <*> (realToFrac <$> peek yPtr)
           <*> (realToFrac <$> peek zPtr)

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

{-# SPECIALIZE isoSize2iV2 :: Iso' Size2i (V2 Int ) #-}
{-# SPECIALIZE isoSize2iV2 :: Iso' Size2i (V2 CInt) #-}
isoSize2iV2 :: forall a. (Integral a) => Iso' Size2i (V2 a)
isoSize2iV2 = iso mkV2 (unsafePerformIO . newSize2i)
  where
    mkV2 :: Size2i -> V2 a
    mkV2 pt = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withSize2iPtr pt $ \ptPtr -> do
        [CU.block| void {
          Size2i * p = $(Size2i * ptPtr);
          *$(int * wPtr) = p->width;
          *$(int * hPtr) = p->height;
        }|]
        V2 <$> (fromIntegral <$> peek wPtr)
           <*> (fromIntegral <$> peek hPtr)

{-# SPECIALIZE isoSize2fV2 :: Iso' Size2f (V2 Float ) #-}
{-# SPECIALIZE isoSize2fV2 :: Iso' Size2f (V2 CFloat) #-}
isoSize2fV2 :: forall a. (Real a, Fractional a) => Iso' Size2f (V2 a)
isoSize2fV2 = iso mkV2 (unsafePerformIO . newSize2f)
  where
    mkV2 :: Size2f -> V2 a
    mkV2 pt = unsafePerformIO $
      alloca $ \wPtr ->
      alloca $ \hPtr ->
      withSize2fPtr pt $ \ptPtr -> do
        [CU.block| void {
          Size2f * p = $(Size2f * ptPtr);
          *$(float * wPtr) = p->width;
          *$(float * hPtr) = p->height;
        }|]
        V2 <$> (realToFrac <$> peek wPtr)
           <*> (realToFrac <$> peek hPtr)


--------------------------------------------------------------------------------
--  Rect
--------------------------------------------------------------------------------

mkRect
    :: Int -- ^ x
    -> Int -- ^ y
    -> Int -- ^ width
    -> Int -- ^ height
    -> Rect
mkRect x y width height = unsafePerformIO $ rectFromPtr $
    [CU.exp|Rect * { new cv::Rect( $(int c'x)
                                 , $(int c'y)
                                 , $(int c'width)
                                 , $(int c'height)
                                 )
                   }|]
  where
    c'x      = fromIntegral x
    c'y      = fromIntegral y
    c'width  = fromIntegral width
    c'height = fromIntegral height

-- | The top-left corner
rectTopLeft :: Rect -> Point2i
rectTopLeft rect = unsafePerformIO $ point2iFromPtr $ withRectPtr rect $ \rectPtr ->
    [CU.exp| Point2i * { new Point2i($(Rect * rectPtr)->tl()) }|]

-- | The bottom-right corner
rectBottomRight :: Rect -> Point2i
rectBottomRight rect = unsafePerformIO $ point2iFromPtr $ withRectPtr rect $ \rectPtr ->
    [CU.exp| Point2i * { new Point2i($(Rect * rectPtr)->br()) }|]

-- | Size (width, height) of the rectangle
rectSize :: Rect -> Size2i
rectSize rect = unsafePerformIO $ size2iFromPtr $ withRectPtr rect $ \rectPtr ->
    [CU.exp| Size2i * { new Size2i($(Rect * rectPtr)->size()) }|]

-- | Area (width*height) of the rectangle
rectArea :: Rect -> Int
rectArea rect = fromIntegral $ unsafePerformIO $ withRectPtr rect $ \rectPtr ->
    [CU.exp| int { $(Rect * rectPtr)->area() }|]


-- | Checks whether the rectangle contains the point
rectContains :: Point2i -> Rect -> Bool
rectContains point rect =
    toBool $
      unsafePerformIO $
        withPoint2iPtr point $ \pointPtr ->
          withRectPtr rect $ \rectPtr ->
            [CU.exp| int { $(Rect * rectPtr)->contains(*$(Point2i * pointPtr)) }|]


--------------------------------------------------------------------------------
--  RotatedRect
--------------------------------------------------------------------------------

mkRotatedRect
    :: Point2f -- ^ Rectangle mass center
    -> Size2f -- ^ Width and height of the rectangle
    -> Float
       -- ^ The rotation angle (in degrees). When the angle is 0, 90,
       -- 180, 270 etc., the rectangle becomes an up-right rectangle.
    -> RotatedRect
mkRotatedRect center size angle =
    unsafePerformIO $
      rotatedRectFromPtr $
        withPoint2fPtr center $ \centerPtr ->
          withSize2fPtr size $ \sizePtr ->
            [CU.exp| RotatedRect * {
                new cv::RotatedRect( *$(Point2f * centerPtr)
                                   , *$(Size2f * sizePtr)
                                   , $(float c'angle)
                                   )
            }|]

  where
    c'angle = realToFrac angle

-- | Rectangle mass center
rotatedRectCenter :: RotatedRect -> Point2f
rotatedRectCenter rotRect = unsafePerformIO $
    point2fFromPtr $
      withRotatedRectPtr rotRect $ \rotRectPtr ->
        [CU.exp| Point2f * { new Point2f($(RotatedRect * rotRectPtr)->center) }|]

-- | Width and height of the rectangle
rotatedRectSize :: RotatedRect -> Size2f
rotatedRectSize rotRect = unsafePerformIO $
    size2fFromPtr $
      withRotatedRectPtr rotRect $ \rotRectPtr ->
        [CU.exp| Size2f * { new Size2f($(RotatedRect * rotRectPtr)->size) }|]

-- | The rotation angle (in degrees)
--
-- When the angle is 0, 90, 180, 270 etc., the rectangle becomes an
-- up-right rectangle.
rotatedRectAngle :: RotatedRect -> Float
rotatedRectAngle rotRect = realToFrac $ unsafePerformIO $
    withRotatedRectPtr rotRect $ \rotRectPtr ->
      [CU.exp| float { $(RotatedRect * rotRectPtr)->angle }|]

-- | The minimal up-right rectangle containing the rotated rectangle
rotatedRectBoundingRect :: RotatedRect -> Rect
rotatedRectBoundingRect rotRect =
    unsafePerformIO $ rectFromPtr $ withRotatedRectPtr rotRect $ \rotRectPtr ->
      [CU.exp| Rect * { new Rect($(RotatedRect * rotRectPtr)->boundingRect()) }|]

rotatedRectPoints :: RotatedRect -> (Point2f, Point2f, Point2f, Point2f)
rotatedRectPoints rotRect = unsafePerformIO $ do
    let v0 = V2 0 0 :: V2 CFloat
    p1 <- newPoint2f v0
    p2 <- newPoint2f v0
    p3 <- newPoint2f v0
    p4 <- newPoint2f v0
    withRotatedRectPtr rotRect $ \rotRectPtr ->
      withPoint2fPtr p1 $ \p1Ptr ->
      withPoint2fPtr p2 $ \p2Ptr ->
      withPoint2fPtr p3 $ \p3Ptr ->
      withPoint2fPtr p4 $ \p4Ptr ->
        [C.block| void {
          Point2f vertices[4];
          $(RotatedRect * rotRectPtr)->points(vertices);
          *$(Point2f * p1Ptr) = vertices[0];
          *$(Point2f * p2Ptr) = vertices[1];
          *$(Point2f * p3Ptr) = vertices[2];
          *$(Point2f * p4Ptr) = vertices[3];
        }|]
    pure (p1, p2, p3, p4)


--------------------------------------------------------------------------------
--  TermCriteria
--------------------------------------------------------------------------------

#include "termcriteria.hpp"
#num TERMCRITERIA_COUNT
#num TERMCRITERIA_EPS

mkTermCriteria :: Maybe Int    -- ^ Optionally the maximum number of iterations/elements.
               -> Maybe Double -- ^ Optionally the desired accuracy.
               -> TermCriteria
mkTermCriteria mbMaxCount mbEpsilon = unsafePerformIO $ termCriteriaFromPtr $
    [CU.exp|TermCriteria * {
      new cv::TermCriteria( $(int    c'type    )
                          , $(int    c'maxCount)
                          , $(double c'epsilon )
                          )
    }|]
  where
    c'type =   maybe 0 (const c'TERMCRITERIA_COUNT) mbMaxCount
           .|. maybe 0 (const c'TERMCRITERIA_EPS  ) mbEpsilon
    c'maxCount = maybe 0 fromIntegral mbMaxCount
    c'epsilon  = maybe 0 realToFrac   mbEpsilon


--------------------------------------------------------------------------------
--  Scalar
--------------------------------------------------------------------------------

mkScalar :: Double -> Double -> Double -> Double -> Scalar
mkScalar x y z w = unsafePerformIO $ scalarFromPtr $
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


--------------------------------------------------------------------------------
--  Matrix
--------------------------------------------------------------------------------

newEmptyMat :: IO Mat
newEmptyMat = matFromPtr [CU.exp|Mat * { new Mat() }|]

cloneMat :: Mat -> Mat
cloneMat = unsafePerformIO . cloneMatIO

cloneMatM :: (PrimMonad m) => Mat -> m Mat
cloneMatM = unsafePrimToPrim . cloneMatIO

cloneMatIO :: Mat -> IO Mat
cloneMatIO mat = matFromPtr $ withMatPtr mat $ \matPtr ->
    [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]

matSubRect :: Mat -> Rect -> Either CvException Mat
matSubRect matIn rect = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException (pure matOut) $
      withMatPtr matIn $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
      withRectPtr rect $ \rectPtr ->
        [cvExceptU|
          *$(Mat * matOutPtr) =
            Mat( *$(Mat * matInPtr)
               , *$(Rect * rectPtr)
               );
        |]


--------------------------------------------------------------------------------
--  Repa
--------------------------------------------------------------------------------

-- | Representation tag for Repa @'Repa.Array's@ for OpenCV @'Mat's@.
data M

-- | Converts an OpenCV @'Mat'rix@ into a Repa array. Returns 'Nothing' if the
-- desired 'Repa.Shape' @sh@ doesn't match the shape of the given matrix.
--
-- This is a zero-copy operation.
toRepa
    :: forall sh e
     . (Repa.Shape sh, Storable e)
    => Mat
    -> Maybe (Repa.Array M sh e)
toRepa mat = unsafePerformIO $ withMatPtr mat $ \matPtr ->
    alloca $ \(dimsPtr     :: Ptr CInt) ->
    alloca $ \(elemSizePtr :: Ptr CSize) ->
    alloca $ \(stepPtr     :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtrPtr  :: Ptr (Ptr CUChar)) -> do
      [CU.block| void {
        Mat * mat = $(Mat * matPtr);
        *$(int * dimsPtr)                = mat->dims;
        *$(size_t * elemSizePtr)         = mat->elemSize();
        *$(size_t * * stepPtr)           = mat->step.p;
        *$(unsigned char * * dataPtrPtr) = mat->data;
      }|]
      (dims :: Int) <- fromIntegral <$> peek dimsPtr
      if dims /= Repa.rank (Repa.zeroDim :: sh)
        then pure Nothing
        else do
          (elemSize :: CSize) <- peek elemSizePtr
          if fromIntegral elemSize /= sizeOf (undefined :: e)
            then pure Nothing
            else do
              (step :: Ptr CSize) <- peek stepPtr
              (sh :: sh) <- Repa.shapeOfList . map fromIntegral <$> peekArray dims step
              (dataPtr :: Ptr CUChar) <- peek dataPtrPtr
              pure $ Just $ Array mat dataPtr sh

-- | Converts a Repa array back into an OpenCV @'Mat'rix@.
--
-- This is a zero-copy operation.
fromRepa :: Repa.Array M sh e -> Mat
fromRepa (Array mat _ _) = mat

instance (Storable e) => Repa.Source M e where
    -- TODO (BvD): We might want to check for isContinuous() to optimize certain operations.

    data Array M sh e =
         Array !Mat -- The Mat is kept around so that the data doesn't get garbage collected.
               !(Ptr CUChar) -- Pointer to the data.
               !sh -- The shape of the data which is determined by mat->dims and mat->step.p.

    extent :: (Repa.Shape sh) => Repa.Array M sh e -> sh
    extent (Array _ _ sh) = sh

    index :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    index (Array mat dataPtr sh) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith mul (Repa.listOfShape sh)
                                   (Repa.listOfShape ix)

        mul size n | n < size  = size * n
                   | otherwise = error "Index out of range!"

    unsafeIndex :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    unsafeIndex (Array mat dataPtr sh) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith (*) (Repa.listOfShape sh)
                                   (Repa.listOfShape ix)

    linearIndex :: (Repa.Shape sh) => Repa.Array M sh e -> Int -> e
    linearIndex a ix = Repa.index a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    unsafeLinearIndex :: (Repa.Shape sh) => Repa.Array M sh e -> Int -> e
    unsafeLinearIndex a ix = Repa.unsafeIndex a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    deepSeqArray :: (Repa.Shape sh) => Repa.Array M sh e -> b -> b
    deepSeqArray (Array _mat _dataPtr sh) = Repa.deepSeq sh

-- TODO (BvD): Is it possible to define something like the following?
--
-- instance (Storable e) => Repa.Target M e where
--
--     newtype MVec M e = MVec IOMat
--
--     newMVec :: Int -> IO (MVec M e)
--     newMVec size = _todo_newMVec
--
--     unsafeWriteMVec :: MVec M e -> Int -> e -> IO ()
--     unsafeWriteMVec = _todo_unsafeWriteMVec
--
--     unsafeFreezeMVec :: sh  -> MVec M e -> IO (Array M sh e)
--     unsafeFreezeMVec = _todo_unsafeFreezeMVec
--
--     deepSeqMVec :: MVec M e -> a -> a
--     deepSeqMVec = _todo_deepSeqMVec
--
--     touchMVec :: MVec M e -> IO ()
--     touchMVec = _todo_touchMVec



--------------------------------------------------------------------------------
-- Mutable Matrix
--------------------------------------------------------------------------------

type IOMat   = MutMat RealWorld
type STMat s = MutMat s

freeze :: (PrimMonad m) => MutMat (PrimState m) -> m Mat
freeze = cloneMatM . unMutMat

thaw :: (PrimMonad m) => Mat -> m (MutMat (PrimState m))
thaw = fmap MutMat . cloneMatM

createMat :: (forall s. ST s (MutMat s)) -> Mat
createMat mk = runST $ unsafeFreeze =<< mk


--------------------------------------------------------------------------------
-- Operations on Arrays
--------------------------------------------------------------------------------

-- | Calculates the weighted sum of two arrays
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#addweighted OpenCV Sphinx doc>
addWeighted
    :: Mat    -- ^ src1
    -> Double -- ^ alpha
    -> Mat    -- ^ src2
    -> Double -- ^ beta
    -> Double -- ^ gamma
    -> Either CvException Mat
addWeighted src1 alpha src2 beta gamma = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src1 $ \src1Ptr ->
      withMatPtr src2 $ \src2Ptr ->
      withMatPtr dst $ \dstPtr ->
      [cvExcept|
        cv::addWeighted
          ( *$(Mat * src1Ptr)
          , $(double c'alpha)
          , *$(Mat * src2Ptr)
          , $(double c'beta)
          , $(double c'gamma)
          , *$(Mat * dstPtr)
          );
      |]
  where
    c'alpha = realToFrac alpha
    c'beta  = realToFrac beta
    c'gamma = realToFrac gamma

-- | Finds the global minimum and maximum in an array
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#minmaxloc OpenCV Sphinx doc>

-- TODO (RvD): implement mask
minMaxLoc :: Mat -> Either CvException (Double, Double, Point2i, Point2i)
minMaxLoc src = unsafePerformIO $ do
    let v0 = V2 0 0 :: V2 CInt
    minLoc <- newPoint2i v0
    maxLoc <- newPoint2i v0
    withMatPtr src $ \srcPtr ->
      withPoint2iPtr minLoc $ \minLocPtr ->
      withPoint2iPtr maxLoc $ \maxLocPtr ->
      alloca $ \minValPtr ->
      alloca $ \maxValPtr -> do
        handleCvException
          ( (,, minLoc, maxLoc)
            <$> (realToFrac <$> peek minValPtr)
            <*> (realToFrac <$> peek maxValPtr)
          )
          [cvExcept|
            cv::minMaxLoc( *$(Mat * srcPtr)
                         , $(double * minValPtr)
                         , $(double * maxValPtr)
                         , $(Point2i * minLocPtr)
                         , $(Point2i * maxLocPtr)
                         );
          |]
