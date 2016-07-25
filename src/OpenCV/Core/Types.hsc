{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- For Show instances

module OpenCV.Core.Types
    ( -- * Mutable values
      Mut
    , Mutable
    , FreezeThaw(..)
      -- * Point
    , module OpenCV.Core.Types.Point
      -- * Size
    , Size2i
    , Size2f
    , ToSize2i(..), FromSize2i(..)
    , ToSize2f(..), FromSize2f(..)
      -- * Scalar
    , Scalar
    , ToScalar(..), FromScalar(..)
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
      -- * KeyPoint
    , KeyPoint
    , KeyPointRec(..)
    , mkKeyPoint
    , keyPointAsRec
      -- * DMatch
    , DMatch
    , DMatchRec(..)
    , mkDMatch
    , dmatchAsRec
      -- * Matrix
    , module OpenCV.Core.Types.Mat
    , module OpenCV.Core.Types.Matx
      -- * Exception
    , module OpenCV.Exception
     -- * Algorithm
    , Algorithm(..)
      -- * Polymorphic stuff
    , WithPtr
    , FromPtr
    , CSizeOf
    , PlacementNew
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.Vector ( zero )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.PlacementNew
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Constants
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Matx
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Exception
import "this" OpenCV.Internal
import "this" OpenCV.Mutable

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
        V2 x y = fromPoint  $ rectTopLeft rect
        V2 w h = fromSize2i $ rectSize    rect

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
rectContains :: (ToPoint2i point2i) => point2i -> Rect -> Bool
rectContains point rect =
    toBool $
      unsafePerformIO $
        withPtr (toPoint point) $ \pointPtr ->
          withPtr rect $ \rectPtr ->
            [CU.exp| int { $(Rect * rectPtr)->contains(*$(Point2i * pointPtr)) }|]


--------------------------------------------------------------------------------
--  RotatedRect
--------------------------------------------------------------------------------

mkRotatedRect
    :: ( ToPoint2f point2f
       , ToSize2f  size2f
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


--------------------------------------------------------------------------------
-- KeyPoint
--------------------------------------------------------------------------------

{- | Data structure for salient point detectors

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#keypoint OpenCV Sphinx doc>
-}
newtype KeyPoint = KeyPoint {unKeyPoint :: ForeignPtr C'KeyPoint}

type instance C KeyPoint = C'KeyPoint

instance WithPtr KeyPoint where
    withPtr = withForeignPtr . unKeyPoint

instance FromPtr KeyPoint where
    fromPtr = objFromPtr KeyPoint $ \ptr ->
                [CU.exp| void { delete $(KeyPoint * ptr) }|]

instance CSizeOf C'KeyPoint where
    cSizeOf _proxy = c'sizeof_KeyPoint

data KeyPointRec
   = KeyPointRec
     { kptPoint :: !(V2 Float)
       -- ^ Coordinates of the keypoints.
     , kptSize :: !Float
       -- ^ Diameter of the meaningful keypoint neighborhood.
     , kptAngle :: !Float
       -- ^ Computed orientation of the keypoint (-1 if not applicable); it's in
       -- [0,360) degrees and measured relative to image coordinate system, ie
       -- in clockwise.
     , kptResponse :: !Float
       -- ^ The response by which the most strong keypoints have been
       -- selected. Can be used for the further sorting or subsampling.
     , kptOctave :: !Int32
       -- ^ Octave (pyramid layer) from which the keypoint has been extracted.
     , kptClassId :: !Int32
       -- ^ Object class (if the keypoints need to be clustered by an object
       -- they belong to).
     } deriving (Eq, Show)

newKeyPoint :: KeyPointRec -> IO KeyPoint
newKeyPoint KeyPointRec{..} = fromPtr $
    [CU.exp|KeyPoint * {
      new cv::KeyPoint
          ( cv::Point2f($(float c'x), $(float c'y))
          , $(float c'kptSize)
          , $(float c'kptAngle)
          , $(float c'kptResponse)
          , $(int32_t kptOctave)
          , $(int32_t kptClassId)
          )
    }|]
  where
    V2 c'x c'y = realToFrac <$> kptPoint
    c'kptSize     = realToFrac kptSize
    c'kptAngle    = realToFrac kptAngle
    c'kptResponse = realToFrac kptResponse

mkKeyPoint :: KeyPointRec -> KeyPoint
mkKeyPoint = unsafePerformIO . newKeyPoint

keyPointAsRec :: KeyPoint -> KeyPointRec
keyPointAsRec kpt = unsafePerformIO $
    withPtr kpt $ \kptPtr ->
    alloca $ \xPtr        ->
    alloca $ \yPtr        ->
    alloca $ \sizePtr     ->
    alloca $ \anglePtr    ->
    alloca $ \responsePtr ->
    alloca $ \octavePtr   ->
    alloca $ \classIdPtr  -> do
      [CU.block|void {
        KeyPoint * kpt = $(KeyPoint * kptPtr);
        *$(float   * xPtr       ) = kpt->pt.x    ;
        *$(float   * yPtr       ) = kpt->pt.y    ;
        *$(float   * sizePtr    ) = kpt->size    ;
        *$(float   * anglePtr   ) = kpt->angle   ;
        *$(float   * responsePtr) = kpt->response;
        *$(int32_t * octavePtr  ) = kpt->octave  ;
        *$(int32_t * classIdPtr ) = kpt->class_id;
      }|]
      KeyPointRec
        <$> ( V2 <$> (realToFrac <$> peek xPtr)
                 <*> (realToFrac <$> peek yPtr)
            )
        <*> (realToFrac <$> peek sizePtr    )
        <*> (realToFrac <$> peek anglePtr   )
        <*> (realToFrac <$> peek responsePtr)
        <*> peek octavePtr
        <*> peek classIdPtr

--------------------------------------------------------------------------------
-- DMatch
--------------------------------------------------------------------------------

{- | Class for matching keypoint descriptors: query descriptor index, train
descriptor index, train image index, and distance between descriptors

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#dmatch OpenCV Sphinx Doc>
-}
newtype DMatch = DMatch {unDMatch :: ForeignPtr C'DMatch}

type instance C DMatch = C'DMatch

instance WithPtr DMatch where
    withPtr = withForeignPtr . unDMatch

instance FromPtr DMatch where
    fromPtr = objFromPtr DMatch $ \ptr ->
                [CU.exp| void { delete $(DMatch * ptr) }|]

instance CSizeOf C'DMatch where
    cSizeOf _proxy = c'sizeof_DMatch

data DMatchRec
   = DMatchRec
     { dmatchQueryIdx :: !Int32
       -- ^ Query descriptor index.
     , dmatchTrainIdx :: !Int32
       -- ^ Train descriptor index.
     , dmatchImgIdx   :: !Int32
       -- ^ Train image index.
     , dmatchDistance :: !Float
     } deriving (Eq, Show)

newDMatch :: DMatchRec -> IO DMatch
newDMatch DMatchRec{..} = fromPtr $
    [CU.exp|DMatch * {
      new cv::DMatch
          ( $(int32_t dmatchQueryIdx)
          , $(int32_t dmatchTrainIdx)
          , $(int32_t dmatchImgIdx)
          , $(float c'distance)
          )
    }|]
  where
    c'distance = realToFrac dmatchDistance

mkDMatch :: DMatchRec -> DMatch
mkDMatch = unsafePerformIO . newDMatch

dmatchAsRec :: DMatch -> DMatchRec
dmatchAsRec dmatch = unsafePerformIO $
    withPtr dmatch $ \dmatchPtr ->
    alloca $ \queryIdxPtr ->
    alloca $ \trainIdxPtr ->
    alloca $ \imgIdxPtr ->
    alloca $ \distancePtr -> do
      [CU.block|void {
        DMatch * dmatch = $(DMatch * dmatchPtr);
        *$(int32_t * queryIdxPtr) = dmatch->queryIdx;
        *$(int32_t * trainIdxPtr) = dmatch->trainIdx;
        *$(int32_t * imgIdxPtr  ) = dmatch->imgIdx  ;
        *$(float   * distancePtr) = dmatch->distance;
      }|]
      DMatchRec
        <$> peek queryIdxPtr
        <*> peek trainIdxPtr
        <*> peek imgIdxPtr
        <*> (realToFrac <$> peek distancePtr)

--------------------------------------------------------------------------------
-- Algorithm
--------------------------------------------------------------------------------

class Algorithm a m where
    algorithmClearState :: a -> m ()
    algorithmIsEmpty :: a -> m Bool
