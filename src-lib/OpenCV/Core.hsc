{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core
    ( -- * Point
      -- ** 2D types
      Point2i
    , Point
    , Point2f
    , Point2d
      -- ** 3D types
    , Point3i
    , Point3f
    , Point3d
      -- ** 2D point construction
    , mkPoint2i
    , mkPoint
    , mkPoint2f
    , mkPoint2d
      -- ** 3D point construction
    , mkPoint3i
    , mkPoint3f
    , mkPoint3d
      -- * Size
    , Size2i
    , Size
    , Size2f
    , mkSize2i
    , mkSize
    , mkSize2f
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
      -- * Mutable Matrix
    , MutMat
    , IOMat
    , STMat
    , freeze
    , thaw
    , createMat
      -- * Exception
    , CvException
    ) where

import "base" Foreign.Marshal.Utils ( toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
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

mkPoint2i :: Int -> Int -> Point2i
mkPoint2i x y = unsafePerformIO $ point2iFromPtr $
    [CU.exp|Point2i * { new cv::Point2i($(int c'x), $(int c'y)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y

mkPoint :: Int -> Int -> Point
mkPoint = mkPoint2i

mkPoint2f :: Float -> Float -> Point2f
mkPoint2f x y = unsafePerformIO $ newPoint2f x y

newPoint2f :: Float -> Float -> IO Point2f
newPoint2f x y = point2fFromPtr $
    [CU.exp|Point2f * { new cv::Point2f($(float c'x), $(float c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y

mkPoint2d :: Double -> Double -> Point2d
mkPoint2d x y = unsafePerformIO $ point2dFromPtr $
    [CU.exp|Point2d * { new cv::Point2d($(double c'x), $(double c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y

mkPoint3i :: Int -> Int -> Int -> Point3i
mkPoint3i x y z = unsafePerformIO $ point3iFromPtr $
    [CU.exp|Point3i * { new cv::Point3i($(int c'x), $(int c'y), $(int c'z)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y
    c'z = fromIntegral z

mkPoint3f :: Float -> Float -> Float -> Point3f
mkPoint3f x y z = unsafePerformIO $ point3fFromPtr $
    [CU.exp|Point3f * { new cv::Point3f($(float c'x), $(float c'y), $(float c'z)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y
    c'z = realToFrac z

mkPoint3d :: Double -> Double -> Double -> Point3d
mkPoint3d x y z = unsafePerformIO $ point3dFromPtr $
    [CU.exp|Point3d * { new cv::Point3d($(double c'x), $(double c'y), $(double c'z)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y
    c'z = realToFrac z

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

mkSize2i :: Int -> Int -> Size2i
mkSize2i x y = unsafePerformIO $ size2iFromPtr $
    [CU.exp|Size2i * { new cv::Size2i($(int c'x), $(int c'y)) }|]
  where
    c'x = fromIntegral x
    c'y = fromIntegral y

mkSize :: Int -> Int -> Size
mkSize = mkSize2i

mkSize2f :: Float -> Float -> Size2f
mkSize2f x y = unsafePerformIO $ size2fFromPtr $
    [CU.exp|Size2f * { new cv::Size2f($(float c'x), $(float c'y)) }|]
  where
    c'x = realToFrac x
    c'y = realToFrac y


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
            [CU.exp| int { $(Rect * rectPtr)->contains(*$(Point * pointPtr)) }|]


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
    p1 <- newPoint2f 0 0
    p2 <- newPoint2f 0 0
    p3 <- newPoint2f 0 0
    p4 <- newPoint2f 0 0
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
cloneMat mat = unsafePerformIO $ matFromPtr $ withMatPtr mat $ \matPtr ->
    [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]

cloneMatM :: (PrimMonad m) => Mat -> m Mat
cloneMatM mat = unsafePrimToPrim $ matFromPtr $ withMatPtr mat $ \matPtr ->
    [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]

matSubRect :: Mat -> Rect -> Either CvException Mat
matSubRect matIn rect = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException matOut $
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
