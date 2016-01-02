{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For Show instances

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
    , isoScalarV4
      -- * Matrix
    , Mat
    , newEmptyMat
    , MatDepth(..)
    , newMat
    , eyeMat
    , cloneMat
    , matSubRect
    , matCopyTo
    , matCopyToM
    , matConvertTo
    , MatInfo(..)
    , matInfo
      -- ** Matrix conversions
      -- *** Simple matrix representation
    , HMat
    , hmShape
    , hmChannels
    , hmElems
    , HElems(..)
    , hElemsDepth
    , hElemsLength
    , hmat
      -- *** Vectors of Vectors
    , matToM23
    , matToM33
      -- *** Repa
    , MatElem
    , ToMatDepth(..)
    , NumChannels(..)
    , Elem, getChannels
    , (:::)(..)
    , (:*)
    , matElem
    , M
    , toRepa
    , fromRepa
    , repa
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
    , NormType(..)
    , NormAbsRel(..)
    , norm
    , normDiff
    , normalize
    , matSum
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray, allocaArray )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Ptr ( Ptr, plusPtr, castPtr )
import "base" Foreign.Storable ( Storable(..), peek, sizeOf, peekElemOff, pokeElemOff )
import "base" GHC.TypeLits
import "base" Data.Void ( Void )
import qualified "bytestring" Data.ByteString as B
import "deepseq" Control.DeepSeq (NFData, rnf)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lens" Control.Lens hiding ( ix )
import "linear" Linear.Matrix ( M23, M33 )
import "linear" Linear.Vector ( zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import qualified "repa" Data.Array.Repa as Repa
import "repa" Data.Array.Repa.Index ( Z(Z), (:.)((:.)) )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import "this" OpenCV.Unsafe
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Generic as VG
import qualified "vector" Data.Vector.Unboxed as VU
import qualified "vector" Data.Vector.Unboxed.Mutable as VUM

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

instance Show Point2i where
    showsPrec prec point = showParen (prec >= 10) $
                               shows (point ^. isoPoint2iV2 :: V2 Int)
                             . showString " ^. from isoPoint2iV2"

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

instance Show Rect where
    showsPrec prec rect = showParen (prec >= 10) $
                              showString "mkRect "
                            . shows x . showString " "
                            . shows y . showString " "
                            . shows w . showString " "
                            . shows h
      where
        x, y, w, h :: Int
        V2 x y = rectTopLeft rect ^. isoPoint2iV2
        V2 w h = rectSize    rect ^. isoSize2iV2

mkRect
    :: V2 Int -- ^ top left
    -> V2 Int -- ^ size
    -> Rect
mkRect (V2 x y) (V2 width height) = unsafePerformIO $ rectFromPtr $
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

{-# SPECIALIZE isoScalarV4 :: Iso' Scalar (V4 Float  ) #-}
{-# SPECIALIZE isoScalarV4 :: Iso' Scalar (V4 CFloat ) #-}
{-# SPECIALIZE isoScalarV4 :: Iso' Scalar (V4 Double ) #-}
{-# SPECIALIZE isoScalarV4 :: Iso' Scalar (V4 CDouble) #-}
isoScalarV4 :: forall a. (Real a, Fractional a) => Iso' Scalar (V4 a)
isoScalarV4 = iso mkV4 (unsafePerformIO . newScalar)
  where
    mkV4 :: Scalar -> V4 a
    mkV4 s = unsafePerformIO $
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
        V4 <$> (realToFrac <$> peek xPtr)
           <*> (realToFrac <$> peek yPtr)
           <*> (realToFrac <$> peek zPtr)
           <*> (realToFrac <$> peek wPtr)


--------------------------------------------------------------------------------
--  Matrix
--------------------------------------------------------------------------------

data MatDepth =
     MatDepth_8U
   | MatDepth_8S
   | MatDepth_16U
   | MatDepth_16S
   | MatDepth_32S
   | MatDepth_32F
   | MatDepth_64F
   | MatDepth_USRTYPE1
     deriving (Show, Eq)

#num CV_CN_MAX
#num CV_CN_SHIFT

#num CV_8U
#num CV_8S
#num CV_16U
#num CV_16S
#num CV_32S
#num CV_32F
#num CV_64F
#num CV_USRTYPE1

#num CV_MAT_DEPTH_MASK

marshalMatDepth :: MatDepth -> Int32
marshalMatDepth = \case
    MatDepth_8U       -> c'CV_8U
    MatDepth_8S       -> c'CV_8S
    MatDepth_16U      -> c'CV_16U
    MatDepth_16S      -> c'CV_16S
    MatDepth_32S      -> c'CV_32S
    MatDepth_32F      -> c'CV_32F
    MatDepth_64F      -> c'CV_64F
    MatDepth_USRTYPE1 -> c'CV_USRTYPE1

marshalFlags
    :: MatDepth
    -> Int -- ^ Number of channels
    -> Int32
marshalFlags depth cn =
    marshalMatDepth depth
      .|. ((fromIntegral cn - 1) `unsafeShiftL` c'CV_CN_SHIFT)

unmarshalDepth :: Int32 -> MatDepth
unmarshalDepth n
    | n == c'CV_8U       = MatDepth_8U
    | n == c'CV_8S       = MatDepth_8S
    | n == c'CV_16U      = MatDepth_16U
    | n == c'CV_16S      = MatDepth_16S
    | n == c'CV_32S      = MatDepth_32S
    | n == c'CV_32F      = MatDepth_32F
    | n == c'CV_64F      = MatDepth_64F
    | n == c'CV_USRTYPE1 = MatDepth_USRTYPE1
    | otherwise          = error $ "unknown depth " <> show n

unmarshalFlags :: Int32 -> (MatDepth, Int)
unmarshalFlags n =
    ( unmarshalDepth $ n .&. c'CV_MAT_DEPTH_MASK
    , 1 + (fromIntegral $ (n `unsafeShiftR` c'CV_CN_SHIFT) .&. (c'CV_CN_MAX - 1))
    )

newEmptyMat :: IO Mat
newEmptyMat = matFromPtr [CU.exp|Mat * { new Mat() }|]

newMat
    :: V.Vector Int -- ^ Vector of sizes
    -> MatDepth
    -> Int          -- ^ Number of channels
    -> Scalar       -- ^ Default element value
    -> IO Mat
newMat sizes matDepth cn defValue =
    withVector c'sizes $ \sizesPtr ->
    withScalarPtr defValue $ \scalarPtr ->
      matFromPtr [CU.exp|Mat * {
        new Mat( $(int     c'ndims)
               , $(int *   sizesPtr)
               , $(int32_t c'type)
               , *$(Scalar * scalarPtr)
               )
      }|]
  where
    c'ndims = fromIntegral $ VG.length sizes
    c'sizes = fmap fromIntegral sizes
    c'type  = marshalFlags matDepth cn

-- | Identity matrix
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#mat-eye OpenCV Sphinx doc>
eyeMat :: Int -> Int -> MatDepth -> Int -> Mat
eyeMat rows cols depth channels = unsafePerformIO $
    matFromPtr [CU.exp|Mat * {
      new Mat(Mat::eye( $(int     c'rows)
                      , $(int     c'cols)
                      , $(int32_t c'type)
                      ))
    }|]
  where
    c'rows = fromIntegral rows
    c'cols = fromIntegral cols
    c'type = marshalFlags depth channels

-- TODO (BvD): Move to some Utility module.
withVector
    :: (VG.Vector v a, Storable a)
    => v a
    -> (Ptr a -> IO b)
    -> IO b
withVector v f =
    allocaArray n $ \ptr ->
      let go !ix
              | ix < n = do
                  pokeElemOff ptr ix (VG.unsafeIndex v ix)
                  go (ix+1)
              | otherwise = f ptr
      in go 0
  where
    n = VG.length v

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

matCopyTo :: Mat -> V2 Int -> Mat -> Either CvException Mat
matCopyTo dst topLeft src = runST $ do
    dstMut <- thaw dst
    eResult <- matCopyToM dstMut topLeft src
    case eResult of
      Left err -> pure $ Left err
      Right () -> Right <$> unsafeFreeze dstMut

matCopyToM
    :: (PrimMonad m)
    => MutMat (PrimState m)
    -> V2 Int
    -> Mat
    -> m (Either CvException ())
matCopyToM dstMut (V2 x y) src =
    unsafePrimToPrim $ handleCvException (pure ()) $
    withMatPtr (unMutMat dstMut) $ \dstPtr ->
    withMatPtr src $ \srcPtr ->
      [cvExcept|
        const Mat * const srcPtr = $(const Mat * const srcPtr);
        const int x = $(int c'x);
        const int y = $(int c'y);
        srcPtr->copyTo( $(Mat * dstPtr)
                      ->rowRange(y, y + srcPtr->rows)
                       .colRange(x, x + srcPtr->cols)
                      );
      |]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y

          -- Mat * srcPtr = $(Mat * srcPtr);
          -- Mat dstRoi = Mat( *$(Mat * matOutPtr)
          --                 , Rect( *$(Point2i * topLeftPtr)
          --                       , srcPtr->size()
          --                       )
          --                 );
          -- srcPtr->copyTo(dstRoi);

-- | Converts an array to another data type with optional scaling
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html?highlight=convertto#mat-convertto OpenCV Sphinx doc>
matConvertTo
    :: Maybe MatDepth
    -> Maybe Double -- ^ Optional scale factor.
    -> Maybe Double -- ^ Optional delta added to the scaled values.
    -> Mat
    -> Either CvException Mat
matConvertTo rtype alpha beta src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          $(Mat * srcPtr)->
            convertTo( *$(Mat * dstPtr)
                     , $(int32_t c'rtype)
                     , $(double c'alpha)
                     , $(double c'beta)
                     );
        |]
  where
    c'rtype = maybe (-1) marshalMatDepth rtype
    c'alpha = maybe 1 realToFrac alpha
    c'beta  = maybe 0 realToFrac beta

withMatData :: Mat -> ([Int] -> Ptr Word8 -> IO a) -> IO a
withMatData mat f = withMatPtr mat $ \matPtr ->
    alloca $ \(dimsPtr     :: Ptr Int32      ) ->
    alloca $ \(stepPtr2    :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtr2    :: Ptr (Ptr Word8)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const dimsPtr ) = matPtr->dims;
        *$(size_t  * * const stepPtr2) = matPtr->step.p;
        *$(uint8_t * * const dataPtr2) = matPtr->data;
      }|]
      (dims  :: Int) <- fromIntegral <$> peek dimsPtr
      (stepPtr :: Ptr CSize) <- peek stepPtr2
      (dataPtr :: Ptr Word8) <- peek dataPtr2
      (step :: [Int]) <- map fromIntegral <$> peekArray dims stepPtr
      f step dataPtr

data MatInfo
   = MatInfo
     { miShape    :: ![Int]
     , miDepth    :: !MatDepth
     , miChannels :: !Int
     }
     deriving (Show, Eq)

matInfo :: Mat -> MatInfo
matInfo mat = unsafePerformIO $
    withMatPtr mat $ \matPtr ->
    alloca $ \(flagsPtr :: Ptr Int32) ->
    alloca $ \(dimsPtr  :: Ptr CInt) ->
    alloca $ \(sizePtr  :: Ptr (Ptr CInt)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t * const flagsPtr) = matPtr->flags;
        *$(int *     const dimsPtr ) = matPtr->dims;
        *$(int * *   const sizePtr ) = matPtr->size.p;
      }|]
      (depth, channels) <- unmarshalFlags <$> peek flagsPtr
      (dims :: Int) <- fromIntegral <$> peek dimsPtr
      (size :: Ptr CInt) <- peek sizePtr
      shape <- map fromIntegral <$> peekArray dims size
      pure MatInfo
           { miShape    = shape
           , miDepth    = depth
           , miChannels = channels
           }

data HMat
   = HMat
     { hmShape    :: ![Int]
     , hmChannels :: !Int
     , hmElems    :: !HElems
     } deriving (Show, Eq)

data HElems
   = HElems_8U       !(VU.Vector Word8)
   | HElems_8S       !(VU.Vector Int8)
   | HElems_16U      !(VU.Vector Word16)
   | HElems_16S      !(VU.Vector Int16)
   | HElems_32S      !(VU.Vector Int32)
   | HElems_32F      !(VU.Vector Float)
   | HElems_64F      !(VU.Vector Double)
   | HElems_USRTYPE1 !(V.Vector B.ByteString)
     deriving (Show, Eq)

hElemsDepth :: HElems -> MatDepth
hElemsDepth = \case
    HElems_8U       _v -> MatDepth_8U
    HElems_8S       _v -> MatDepth_8S
    HElems_16U      _v -> MatDepth_16U
    HElems_16S      _v -> MatDepth_16S
    HElems_32S      _v -> MatDepth_32S
    HElems_32F      _v -> MatDepth_32F
    HElems_64F      _v -> MatDepth_64F
    HElems_USRTYPE1 _v -> MatDepth_USRTYPE1

hElemsLength :: HElems -> Int
hElemsLength = \case
    HElems_8U       v -> VG.length v
    HElems_8S       v -> VG.length v
    HElems_16U      v -> VG.length v
    HElems_16S      v -> VG.length v
    HElems_32S      v -> VG.length v
    HElems_32F      v -> VG.length v
    HElems_64F      v -> VG.length v
    HElems_USRTYPE1 v -> VG.length v

hmat :: Iso' Mat HMat
hmat = iso matToHMat hMatToMat

matToHMat :: Mat -> HMat
matToHMat mat = unsafePerformIO $ withMatData mat $ \step dataPtr -> do
    elems <- copyElems info step dataPtr
    pure HMat
         { hmShape    = miShape    info
         , hmChannels = miChannels info
         , hmElems    = elems
         }
  where
    info = matInfo mat

    copyElems
        :: MatInfo
        -> [Int]     -- ^ step
        -> Ptr Word8 -- ^ data
        -> IO HElems
    copyElems (MatInfo shape depth channels) step dataPtr =
        case depth of
          MatDepth_8U  -> HElems_8U  <$> copyToVec
          MatDepth_8S  -> HElems_8S  <$> copyToVec
          MatDepth_16U -> HElems_16U <$> copyToVec
          MatDepth_16S -> HElems_16S <$> copyToVec
          MatDepth_32S -> HElems_32S <$> copyToVec
          MatDepth_32F -> HElems_32F <$> copyToVec
          MatDepth_64F -> HElems_64F <$> copyToVec
          MatDepth_USRTYPE1 -> HElems_USRTYPE1 <$> error "todo"
      where
        copyToVec :: (Storable a, VU.Unbox a) => IO (VU.Vector a)
        copyToVec = do
            v <- VUM.unsafeNew $ product0 shape * channels
            forM_ (zip [0,channels..] $ dimPositions shape) $ \(posIx, pos) -> do
                let elemPtr = matElemAddress dataPtr step pos
                forM_ [0 .. channels - 1] $ \channelIx -> do
                  e <- peekElemOff elemPtr channelIx
                  VUM.unsafeWrite v (posIx + channelIx) e
            VU.unsafeFreeze v

hMatToMat :: HMat -> Mat
hMatToMat (HMat shape channels elems) = unsafePerformIO $ do
    mat <- newMat sizes depth channels scalar
    withMatData mat copyElems
    pure mat
  where
    sizes = V.fromList shape
    depth = hElemsDepth elems
    scalar = (zero :: V4 Double) ^. from isoScalarV4

    copyElems :: [Int] -> Ptr Word8 -> IO ()
    copyElems step dataPtr = case elems of
        HElems_8U       v -> copyFromVec v
        HElems_8S       v -> copyFromVec v
        HElems_16U      v -> copyFromVec v
        HElems_16S      v -> copyFromVec v
        HElems_32S      v -> copyFromVec v
        HElems_32F      v -> copyFromVec v
        HElems_64F      v -> copyFromVec v
        HElems_USRTYPE1 _v -> error "todo"
      where
        copyFromVec :: (Storable a, VU.Unbox a) => VU.Vector a -> IO ()
        copyFromVec v =
            forM_ (zip [0,channels..] $ dimPositions shape) $ \(posIx, pos) -> do
              let elemPtr = matElemAddress dataPtr step pos
              forM_ [0 .. channels - 1] $ \channelIx ->
                pokeElemOff elemPtr channelIx $ VU.unsafeIndex v (posIx + channelIx)

-- | All possible positions (indexes) for a given shape (list of
-- sizes per dimension).
dimPositions :: [Int] -> [[Int]]
dimPositions shape = sequence $ map (enumFromTo 0) $ map pred shape

matElemAddress :: Ptr Word8 -> [Int] -> [Int] -> Ptr a
matElemAddress dataPtr step pos = dataPtr `plusPtr` offset
    where
      offset = sum $ zipWith (*) step pos

--------------------------------------------------------------------------------

matToM23 :: (MatElem e) => Mat -> Either String (M23 e)
matToM23 = fmap repaToM23 . toRepa

matToM33 :: (MatElem e) => Mat -> Either String (M33 e)
matToM33 = fmap repaToM33 . toRepa

repaToM23 :: (Storable e) => Repa.Array M Repa.DIM2 e -> M23 e
repaToM23 a =
    V2 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
  where
    i row col = Repa.unsafeIndex a $ Z :. row :. col

repaToM33 :: (Storable e) => Repa.Array M Repa.DIM2 e -> M33 e
repaToM33 a =
    V3 (V3 (i 0 0) (i 1 0) (i 2 0))
       (V3 (i 0 1) (i 1 1) (i 2 1))
       (V3 (i 0 2) (i 1 2) (i 2 2))
  where
    i row col = Repa.unsafeIndex a $ Z :. row :. col


--------------------------------------------------------------------------------
--  Repa
--------------------------------------------------------------------------------

-- | A handy constraint for elements of a Repa @'Repa.Array' 'M' sh e@.
type MatElem e = (Storable e, ToMatDepth e, NumChannels e)

-- | Class of types that have a corresponding statically known 'MatDepth'.
class ToMatDepth a where
    -- | Determine the statically known depth of @a@.
    toMatDepth :: Proxy a -> MatDepth

instance ToMatDepth Word8  where toMatDepth = const MatDepth_8U
instance ToMatDepth Int8   where toMatDepth = const MatDepth_8S
instance ToMatDepth Word16 where toMatDepth = const MatDepth_16U
instance ToMatDepth Int16  where toMatDepth = const MatDepth_16S
instance ToMatDepth Int32  where toMatDepth = const MatDepth_32S
instance ToMatDepth Float  where toMatDepth = const MatDepth_32F
instance ToMatDepth Double where toMatDepth = const MatDepth_64F
-- TODO (BvD): instance ToMatDepth ? where toMatDepth = const MatDepth_USRTYPE1

instance (ToMatDepth a) => ToMatDepth (a ::: _as) where
    toMatDepth = const $ toMatDepth (Proxy :: Proxy a)

instance (ToMatDepth a) => ToMatDepth (Elem _n a) where
    toMatDepth = const $ toMatDepth (Proxy :: Proxy a)

-- | Class of types that have a statically known number of channels.
class NumChannels a where
    -- | Determine the statically known number of elements in @a@.
    numChannels :: Proxy a -> Int

instance NumChannels Word8  where numChannels = const 1
instance NumChannels Int8   where numChannels = const 1
instance NumChannels Word16 where numChannels = const 1
instance NumChannels Int16  where numChannels = const 1
instance NumChannels Int32  where numChannels = const 1
instance NumChannels Float  where numChannels = const 1
instance NumChannels Double where numChannels = const 1

instance (NumChannels as) => NumChannels (a ::: as) where
    numChannels = const $ 1 + numChannels (Proxy :: Proxy as)

instance (KnownNat n) => NumChannels (Elem n a) where
    numChannels = const $ fromInteger $ natVal (Proxy :: Proxy n)

-- | A 'MatElem' that contains @n@ times an @a@.
--
-- Implementation detail: the @n@ times an @a@ is stored in an unboxed 'VU.Vector'.
newtype Elem (n::Nat) a = Elem
    { getChannels :: VU.Vector a -- ^ Retrieve the channel elements.
    } deriving (Eq, Show)

-- | Handy way of constructing a nested type of ':::'.
--
-- For example: @3 :* Word8 ~ Word8 ::: Word8 ::: Word8@
type family (:*) (n :: Nat) a where
    0 :* a = Void
    1 :* a = a
    n :* a = a ::: ((n-1) :* a)

-- | A 'MatElem` used for small number of channels.
--
-- Use ':*' to construct a ':::'.
data a ::: as = !a ::: !as deriving (Eq, Show)

infixr 5 :::

-- | Internal class used in 'matElem'.
class VU.Unbox (El e) => FromElem n e where
    type El e :: *
    fromElem :: Elem n (El e) -> e
    writeElems :: Proxy n -> VUM.MVector s (El e) -> Int -> e -> ST s ()

instance FromElem 1 Word8  where type El Word8   = Word8 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int8   where type El Int8    = Int8  ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Word16 where type El Word16  = Word16; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int16  where type El Int16   = Int16 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int32  where type El Int32   = Int32 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Float  where type El Float   = Float ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Double where type El Double  = Double; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite

instance (FromElem (n-1) as, El as ~ a) => FromElem n (a ::: as) where
    type El (a ::: as) = a

    fromElem e = VU.unsafeHead vu ::: fromElem eTail
        where
          vu = getChannels e

          eTail :: Elem (n-1) a
          eTail = Elem $ VU.unsafeTail vu :: Elem (n-1) a

    writeElems _proxy vum !i (x ::: xs) = do
        VUM.unsafeWrite vum i x
        writeElems (Proxy :: Proxy (n-1)) vum (i+1) xs

-- | An isomorphism between 'Elem' and ':*'.
matElem
    :: forall (n :: Nat) e a
     . ( KnownNat n
       , 1 <= n
       , e ~ (n :* a)
       , El e ~ a
       , FromElem n e
       , VU.Unbox a
       )
    => Iso' (Elem n a) e
matElem = iso fromElem toElem
    where
      toElem :: n :* a -> Elem n a
      toElem e = Elem $ VU.create $ do
        let proxyN :: Proxy n
            proxyN = Proxy
            n = fromInteger $ natVal proxyN
        vum <- VUM.new n
        writeElems proxyN vum 0 e
        return vum

instance (Storable a, Storable as) => Storable (a ::: as) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: as)
    alignment _ = alignment (undefined :: a) -- TODO (BvD): is this correct?
    peek ptr = (:::) <$> peek (castPtr ptr)
                     <*> peek (ptr `plusPtr` sizeOf (undefined :: a))
    poke ptr (x ::: xs) = do poke (castPtr ptr) x
                             poke (ptr `plusPtr` sizeOf (undefined :: a)) xs

instance (Storable a, KnownNat n, VUM.Unbox a) => Storable (Elem n a) where
    sizeOf _ = sizeOf (undefined :: a) * fromInteger (natVal (Proxy :: Proxy n))
    alignment _ = alignment (undefined :: a) -- TODO (BvD): is this correct?
    peek ptr = do
      let n = fromInteger (natVal (Proxy :: Proxy n))
      vum <- VUM.new n
      let go !ix
            | ix < n = do x <- peekElemOff (castPtr ptr) ix
                          VUM.unsafeWrite vum ix x
                          go (ix+1)
            | otherwise = VU.unsafeFreeze vum
      Elem <$> go 0
    poke ptr = VU.imapM_ (pokeElemOff (castPtr ptr)) . getChannels

-- | Representation tag for Repa @'Repa.Array's@ for OpenCV @'Mat's@.
data M

-- | Converts an OpenCV @'Mat'rix@ into a Repa array. Returns 'Nothing' if the
-- desired 'Repa.Shape' @sh@ doesn't match the shape of the given matrix.
--
-- This is a zero-copy operation.
toRepa
    :: forall sh e
     . (Repa.Shape sh, MatElem e)
    => Mat
    -> Either String (Repa.Array M sh e)
toRepa mat = unsafePerformIO $ withMatPtr mat $ \matPtr ->
    alloca $ \(flagsPtr    :: Ptr Int32) ->
    alloca $ \(dimsPtr     :: Ptr Int32) ->
    alloca $ \(sizePtr     :: Ptr (Ptr CInt)) ->
    alloca $ \(stepPtr     :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtrPtr  :: Ptr (Ptr CUChar)) -> do
      [CU.block| void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t  * const flagsPtr)          = matPtr->flags;
        *$(int32_t  * const dimsPtr)           = matPtr->dims;
        *$(int    * * const sizePtr)           = matPtr->size.p;
        *$(size_t * * const stepPtr)           = matPtr->step.p;
        *$(unsigned char * * const dataPtrPtr) = matPtr->data;
      }|]
      (depth, channels) <- unmarshalFlags <$> peek flagsPtr
      let expectedDepth    = toMatDepth  (Proxy :: Proxy e)
          expectedChannels = numChannels (Proxy :: Proxy e)
      if depth /= expectedDepth
        then pure $ Left $
               "The expected depth of " <> show expectedDepth <>
               " doesn't equal the actual depth of " <> show depth <> "!"
        else do
          if channels /= expectedChannels
            then pure $ Left $
                   "Expected " <> show expectedChannels <> " channels" <>
                   " but got " <> show channels <> "!"
            else do
              (dims :: Int) <- fromIntegral <$> peek dimsPtr
              let expectedRank :: Int
                  expectedRank = Repa.rank (Repa.zeroDim :: sh)
              if dims /= expectedRank
                then pure $ Left $
                       "The expected rank of " <> show expectedRank <>
                       " doesn't equal the actual number of dimensions " <> show dims <> "!"
                else do
                  (size :: Ptr CInt) <- peek sizePtr
                  (sizeShape :: sh) <- Repa.shapeOfList . map fromIntegral <$> peekArray dims size

                  (step :: Ptr CSize) <- peek stepPtr
                  (stepShape :: sh) <- Repa.shapeOfList . map fromIntegral <$> peekArray dims step

                  (dataPtr :: Ptr CUChar) <- peek dataPtrPtr
                  pure $ Right $ Array mat dataPtr sizeShape stepShape

-- | Converts a Repa array back into an OpenCV @'Mat'rix@.
--
-- This is a zero-copy operation.
fromRepa :: Repa.Array M sh e -> Mat
fromRepa (Array mat _ _ _) = mat

repa :: (Repa.Shape sh, MatElem e) => Prism' Mat (Repa.Array M sh e)
repa = prism' fromRepa (either (const Nothing) Just . toRepa)

instance (Repa.Shape sh, Storable e) => NFData (Repa.Array M sh e) where
    rnf a = Repa.deepSeqArray a ()

instance (Storable e) => Repa.Source M e where
    -- TODO (BvD): We might want to check for isContinuous() to optimize certain operations.

    data Array M sh e =
         Array !Mat -- The Mat is kept around so that the data doesn't get garbage collected.
               !(Ptr CUChar) -- Pointer to the data.
               !sh -- The shape of the extent which is determined by mat->dims and mat->size.p.
               !sh -- The shape of the data which is determined by mat->dims and mat->step.p.

    extent :: (Repa.Shape sh) => Repa.Array M sh e -> sh
    extent (Array _ _ sizeShape _) = sizeShape

    index :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    index (Array mat dataPtr sizeShape stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith3 mul (Repa.listOfShape sizeShape)
                                    (Repa.listOfShape stepShape)
                                    (Repa.listOfShape ix)

        mul size step i
            | i < size  = step * i
            | otherwise = error $
                "Index " <> show i <> " >= size: " <> show size

    unsafeIndex :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    unsafeIndex (Array mat dataPtr _ stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith (*) (Repa.listOfShape stepShape)
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
    deepSeqArray (Array _mat _dataPtr sizeShape stepShape) =
        Repa.deepSeq stepShape . Repa.deepSeq sizeShape

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

-- | Normalization type
data NormType
   = Norm_Inf
   | Norm_L1
   | Norm_L2
   | Norm_L2SQR
   | Norm_Hamming
   | Norm_Hamming2
   | Norm_MinMax
     deriving (Show, Eq)

data NormAbsRel
   = NormRelative
   | NormAbsolute
     deriving (Show, Eq)

#num NORM_INF
#num NORM_L1
#num NORM_L2
#num NORM_L2SQR
#num NORM_HAMMING
#num NORM_HAMMING2
#num NORM_MINMAX

#num NORM_RELATIVE

marshalNormType :: NormAbsRel -> NormType -> Int32
marshalNormType absRel normType =
    case absRel of
      NormRelative -> c'normType .|. c'NORM_RELATIVE
      NormAbsolute -> c'normType
  where
    c'normType = case normType of
        Norm_Inf      -> c'NORM_INF
        Norm_L1       -> c'NORM_L1
        Norm_L2       -> c'NORM_L2
        Norm_L2SQR    -> c'NORM_L2SQR
        Norm_Hamming  -> c'NORM_HAMMING
        Norm_Hamming2 -> c'NORM_HAMMING2
        Norm_MinMax   -> c'NORM_MINMAX

-- | Calculates an absolute array norm
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
norm
    :: NormType
    -> Maybe Mat
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat -- ^ Input array.
    -> Either CvException Double  -- ^ Calculated norm.
norm normType mbMask src = unsafePerformIO $
    withMatPtr   src    $ \srcPtr  ->
    withMbMatPtr mbMask $ \mskPtr  ->
    alloca              $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * srcPtr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                  );
      |]
  where
    c'normType = marshalNormType NormAbsolute normType

-- | Calculates an absolute difference norm, or a relative difference norm
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
normDiff
    :: NormAbsRel -- ^ Absolute or relative norm.
    -> NormType
    -> Maybe Mat
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat -- ^ First input array.
    -> Mat -- ^ Second input array of the same size and type as the first.
    -> Either CvException Double -- ^ Calculated norm.
normDiff absRel normType mbMask src1 src2 = unsafePerformIO $
    withMatPtr   src1   $ \src1Ptr ->
    withMatPtr   src2   $ \src2Ptr ->
    withMbMatPtr mbMask $ \mskPtr  ->
    alloca              $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * src1Ptr)
                  , *$(Mat * src2Ptr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                  );
      |]
  where
    c'normType = marshalNormType absRel normType

-- | Normalizes the norm or value range of an array
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#normalize OpenCV Sphinx doc>
normalize
    :: Double
       -- ^ Norm value to normalize to or the lower range boundary in case of
       -- the range normalization.
    -> Double
       -- ^ Upper range boundary in case of the range normalization; it is not
       -- used for the norm normalization.
    -> NormType
    -> Maybe MatDepth
    -> Maybe Mat -- ^ Optional operation mask.
    -> Mat -- ^ Input array.
    -> Either CvException Mat
normalize alpha beta normType dtype mbMask src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src      $ \srcPtr ->
      withMatPtr dst      $ \dstPtr ->
      withMbMatPtr mbMask $ \mskPtr ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::normalize( *$(Mat * srcPtr)
                       , *$(Mat * dstPtr)
                       , $(double c'alpha)
                       , $(double c'beta)
                       , $(int32_t c'normType)
                       , $(int32_t c'dtype)
                       , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                       );
        |]
  where
    c'alpha    = realToFrac alpha
    c'beta     = realToFrac beta
    c'normType = marshalNormType NormAbsolute normType
    c'dtype    = maybe (-1) marshalMatDepth dtype

-- | Calculates the sum of array elements
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#sum OpenCV Sphinx doc>
matSum
    :: Mat -- ^ Input array that must have from 1 to 4 channels.
    -> Either CvException Scalar
matSum src = unsafePerformIO $ do
    s <- newScalar $ pure (0 :: Double)
    handleCvException (pure s) $
      withMatPtr src $ \srcPtr ->
      withScalarPtr s $ \sPtr ->
        [cvExcept|
          *$(Scalar * sPtr) = cv::sum(*$(Mat * srcPtr));
        |]

--------------------------------------------------------------------------------

product0 :: (Num a) => [a] -> a
product0 [] = 0
product0 xs = product xs
