{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For Show instances

module OpenCV.Core.Types
    ( -- * Point
      -- ** 2D types
      Point2i, ToPoint2i(toPoint2i), FromPoint2i(fromPoint2i)
    , Point2f, ToPoint2f(toPoint2f), FromPoint2f(fromPoint2f)
    , Point2d, ToPoint2d(toPoint2d), FromPoint2d(fromPoint2d)
      -- ** 3D types
    , Point3i, ToPoint3i(toPoint3i), FromPoint3i(fromPoint3i)
    , Point3f, ToPoint3f(toPoint3f), FromPoint3f(fromPoint3f)
    , Point3d, ToPoint3d(toPoint3d), FromPoint3d(fromPoint3d)
      -- * Size
    , Size2i, ToSize2i(toSize2i), FromSize2i(fromSize2i)
    , Size2f, ToSize2f(toSize2f), FromSize2f(fromSize2f)
      -- * Scalar
    , Scalar, ToScalar(toScalar), FromScalar(fromScalar)
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
      -- * Matrix
    , module OpenCV.Core.Types.Mat
      -- * Exception
    , CvException
    ) where

import "base" Foreign.Marshal.Utils ( toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.Vector ( zero )
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
                               showString "fromPoint2i "
                             . shows (fromPoint2i point :: V2 Int32)

instance Show Size2i where
    showsPrec prec size = showParen (prec >= 10) $
                              showString "fromSize2i "
                            . shows (fromSize2i size :: V2 Int32)

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
        V2 x y = fromPoint2i $ rectTopLeft rect
        V2 w h = fromSize2i  $ rectSize    rect

mkRect
    :: V2 Int32 -- ^ top left
    -> V2 Int32 -- ^ size
    -> Rect
mkRect pos size = unsafePerformIO $ newRect pos size

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
rectArea :: Rect -> Int32
rectArea rect = unsafePerformIO $ withRectPtr rect $ \rectPtr ->
    [CU.exp| int32_t { $(Rect * rectPtr)->area() }|]


-- | Checks whether the rectangle contains the point
rectContains :: (ToPoint2i point2i) => point2i -> Rect -> Bool
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
    :: (ToPoint2f point2f, ToSize2f size2f)
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
    p1 <- newPoint2f zero
    p2 <- newPoint2f zero
    p3 <- newPoint2f zero
    p4 <- newPoint2f zero
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

mkTermCriteria
    :: Maybe Int    -- ^ Optionally the maximum number of iterations/elements.
    -> Maybe Double -- ^ Optionally the desired accuracy.
    -> TermCriteria
mkTermCriteria mbMaxCount mbEpsilon =
    unsafePerformIO $ newTermCriteria mbMaxCount mbEpsilon
