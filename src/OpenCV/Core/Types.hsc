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
      -- * Size
    , Size2i
    , Size2f
      -- * Scalar
    , Scalar
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
      -- * Range
    , Range
    , mkRange
    , wholeRange
      -- * Matrix
    , module OpenCV.Core.Types.Mat
      -- * Exception
    , CvException
      -- * Polymorphic stuff
    , PointT
    , C
    , WithPtr
    , FromPtr
    , CSizeOf
    , PlacementNew
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.Vector ( zero )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Exception
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.TypeLevel

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
                               showString "fromPoint2i "
                             . shows (convert point :: V2 Int32)

instance Show Size2i where
    showsPrec prec size = showParen (prec >= 10) $
                              showString "fromSize2i "
                            . shows (convert size :: V2 Int32)


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
        x, y, w, h :: Int32
        V2 x y = convert $ rectTopLeft rect
        V2 w h = convert $ rectSize    rect

mkRect
    :: V2 Int32 -- ^ top left
    -> V2 Int32 -- ^ size
    -> Rect
mkRect pos size = unsafePerformIO $ newRect pos size

-- | The top-left corner
rectTopLeft :: Rect -> Point2i
rectTopLeft rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr ->
    [CU.exp| Point2i * { new Point2i($(Rect * rectPtr)->tl()) }|]

-- | The bottom-right corner
rectBottomRight :: Rect -> Point2i
rectBottomRight rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr ->
    [CU.exp| Point2i * { new Point2i($(Rect * rectPtr)->br()) }|]

-- | Size (width, height) of the rectangle
rectSize :: Rect -> Size2i
rectSize rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr ->
    [CU.exp| Size2i * { new Size2i($(Rect * rectPtr)->size()) }|]

-- | Area (width*height) of the rectangle
rectArea :: Rect -> Int32
rectArea rect = unsafePerformIO $ withPtr rect $ \rectPtr ->
    [CU.exp| int32_t { $(Rect * rectPtr)->area() }|]


-- | Checks whether the rectangle contains the point
rectContains :: (Convert point2i Point2i) => point2i -> Rect -> Bool
rectContains point rect =
    toBool $
      unsafePerformIO $
        withPtr (convert point :: Point2i) $ \pointPtr ->
          withPtr rect $ \rectPtr ->
            [CU.exp| int { $(Rect * rectPtr)->contains(*$(Point2i * pointPtr)) }|]


--------------------------------------------------------------------------------
--  RotatedRect
--------------------------------------------------------------------------------

mkRotatedRect
    :: ( Convert point2f Point2f
       , Convert size2f  Size2f
       )
    => point2f -- ^ Rectangle mass center
    -> size2f -- ^ Width and height of the rectangle
    -> Float
       -- ^ The rotation angle (in degrees). When the angle is 0, 90,
       -- 180, 270 etc., the rectangle becomes an up-right rectangle.
    -> RotatedRect
mkRotatedRect center size angle =
    unsafePerformIO $ newRotatedRect center size (realToFrac angle)

-- | Rectangle mass center
rotatedRectCenter :: RotatedRect -> Point2f
rotatedRectCenter rotRect = unsafePerformIO $ fromPtr $
      withPtr rotRect $ \rotRectPtr ->
        [CU.exp| Point2f * { new Point2f($(RotatedRect * rotRectPtr)->center) }|]

-- | Width and height of the rectangle
rotatedRectSize :: RotatedRect -> Size2f
rotatedRectSize rotRect = unsafePerformIO $ fromPtr $
      withPtr rotRect $ \rotRectPtr ->
        [CU.exp| Size2f * { new Size2f($(RotatedRect * rotRectPtr)->size) }|]

-- | The rotation angle (in degrees)
--
-- When the angle is 0, 90, 180, 270 etc., the rectangle becomes an
-- up-right rectangle.
rotatedRectAngle :: RotatedRect -> Float
rotatedRectAngle rotRect = realToFrac $ unsafePerformIO $
    withPtr rotRect $ \rotRectPtr ->
      [CU.exp| float { $(RotatedRect * rotRectPtr)->angle }|]

-- | The minimal up-right rectangle containing the rotated rectangle
rotatedRectBoundingRect :: RotatedRect -> Rect
rotatedRectBoundingRect rotRect =
    unsafePerformIO $ fromPtr $ withPtr rotRect $ \rotRectPtr ->
      [CU.exp| Rect * { new Rect($(RotatedRect * rotRectPtr)->boundingRect()) }|]

rotatedRectPoints :: RotatedRect -> (Point2f, Point2f, Point2f, Point2f)
rotatedRectPoints rotRect = unsafePerformIO $ do
    p1 <- newPoint2f zero
    p2 <- newPoint2f zero
    p3 <- newPoint2f zero
    p4 <- newPoint2f zero
    withPtr rotRect $ \rotRectPtr ->
      withPtr p1 $ \p1Ptr ->
      withPtr p2 $ \p2Ptr ->
      withPtr p3 $ \p3Ptr ->
      withPtr p4 $ \p4Ptr ->
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

mkTermCriteria
    :: Maybe Int    -- ^ Optionally the maximum number of iterations/elements.
    -> Maybe Double -- ^ Optionally the desired accuracy.
    -> TermCriteria
mkTermCriteria mbMaxCount mbEpsilon =
    unsafePerformIO $ newTermCriteria mbMaxCount mbEpsilon

--------------------------------------------------------------------------------
-- Range
--------------------------------------------------------------------------------

mkRange :: Int32 -> Int32 -> Range
mkRange start end = unsafePerformIO $ newRange start end

wholeRange :: Range
wholeRange = unsafePerformIO newWholeRange
