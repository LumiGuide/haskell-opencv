{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For Show instances

module OpenCV.Core.Types
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
    , module OpenCV.Core.Types.Mat
      -- * Exception
    , CvException
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Storable ( Storable(..), peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lens" Control.Lens hiding ( ix )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat

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
