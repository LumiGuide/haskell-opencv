{-# language CPP #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language NoImplicitPrelude #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.ImgProc.FeatureDetection
    ( canny
    , goodFeaturesToTrack
    , houghCircles
    , houghLinesP
    , GoodFeaturesToTrackDetectionMethod(..)
    , CannyNorm(..)
    , Circle(..)
    , LineSegment(..)
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Maybe
import qualified "vector" Data.Vector as V
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import "base" Prelude hiding ( lines )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear ( V2(..), V3(..), V4(..) )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel
#if MIN_VERSION_base(4,9,0)
import "base" Data.Foldable ( Foldable )
import "base" Data.Traversable ( Traversable )
#endif

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Feature Detection
--------------------------------------------------------------------------------

{- |

Finds edges in an image using the
<http://docs.opencv.org/2.4/modules/imgproc/doc/feature_detection.html#canny86 Canny86>
algorithm.

Example:

@
cannyImg
    :: forall shape channels depth
     . (Mat shape channels depth ~ Lambda)
    => Mat shape ('S 1) depth
cannyImg = exceptError $
  canny 30 200 Nothing CannyNormL1 lambda
@

<<doc/generated/examples/cannyImg.png cannyImg>>

-}
canny
    :: Double
       -- ^ First threshold for the hysteresis procedure.
    -> Double
       -- ^ Second threshold for the hysteresis procedure.
    -> Maybe Int32
       -- ^ Aperture size for the @Sobel()@ operator. If not specified defaults
       -- to @3@. Must be 3, 5 or 7.
    -> CannyNorm
       -- ^ A flag, indicating whether to use the more accurate L2 norm or the default L1 norm.
    -> Mat ('S [h, w]) channels ('S Word8)
       -- ^ 8-bit input image.
    -> CvExcept (Mat ('S [h, w]) ('S 1) ('S Word8))
canny threshold1 threshold2 apertureSize norm src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          cv::Canny
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , $(double c'threshold1)
          , $(double c'threshold2)
          , $(int32_t c'apertureSize)
          , $(bool c'l2Gradient)
          );
        |]
  where
    c'threshold1 = realToFrac threshold1
    c'threshold2 = realToFrac threshold2
    c'apertureSize = fromMaybe 3 apertureSize
    c'l2Gradient =
      fromBool $
        case norm of
          CannyNormL1 -> False
          CannyNormL2 -> True

-- | A flag, indicating whether to use the more accurate L2 norm or the default L1 norm.
data CannyNorm
   = CannyNormL1
   | CannyNormL2
   deriving (Show, Eq)

data Circle
   = Circle
     { circleCenter :: V2 Float
     , circleRadius :: Float
     } deriving (Show)

{- |

Determines strong corners on an image.

The function finds the most prominent corners in the image or in the specified image region.

* Function calculates the corner quality measure at every source image pixel using the cornerMinEigenVal or cornerHarris.
* Function performs a non-maximum suppression (the local maximums in 3 x 3 neighborhood are retained).
* The corners with the minimal eigenvalue less than @𝚚𝚞𝚊𝚕𝚒𝚝𝚢𝙻𝚎𝚟𝚎𝚕 * max(x,y) qualityMeasureMap(x,y)@ are rejected.
* The remaining corners are sorted by the quality measure in the descending order.
* Function throws away each corner for which there is a stronger corner at a distance less than maxDistance.

Example:

@
goodFeaturesToTrackTraces
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Frog)
    => Mat (ShapeT [height, width]) ('S channels) ('S depth)
goodFeaturesToTrackTraces = exceptError $ do
  imgG <- cvtColor bgr gray frog
  let features = goodFeaturesToTrack imgG 20 0.01 0.5 Nothing Nothing CornerMinEigenVal
  withMatM (Proxy :: Proxy [height, width])
           (Proxy :: Proxy channels)
           (Proxy :: Proxy depth)
           white $ \\imgM -> do
    void $ matCopyToM imgM (V2 0 0) frog Nothing
    forM_ features $ \\f -> do
      circle imgM (round \<$> f :: V2 Int32) 2 blue 5 LineType_AA 0
@

<<doc/generated/examples/goodFeaturesToTrackTraces.png goodFeaturesToTrackTraces>>
-}
goodFeaturesToTrack
  :: (depth `In` ['S Word8, 'S Float, 'D])
  => Mat ('S [h, w]) ('S 1) depth
  -- ^ Input 8-bit or floating-point 32-bit, single-channel image.
  -> Int32
  -- ^ Maximum number of corners to return. If there are more corners than are
  -- found, the strongest of them is returned.
  -> Double
  -- ^ Parameter characterizing the minimal accepted quality of image corners.
  -- The parameter value is multiplied by the best corner quality measure,
  -- which is the minimal eigenvalue (see cornerMinEigenVal ) or the Harris
  -- function response (see cornerHarris ). The corners with the quality measure
  -- less than the product are rejected. For example, if the best corner has the
  -- quality measure = 1500, and the qualityLevel=0.01 , then all the corners with
  -- the quality measure less than 15 are rejected.
  -> Double
  -- ^ Minimum possible Euclidean distance between the returned corners.
  -> Maybe (Mat ('S [h, w]) ('S 1) ('S Word8))
  -- ^ Optional region of interest. If the image is not empty (it needs to have
  -- the type CV_8UC1 and the same size as image ), it specifies the region in which
  -- the corners are detected.
  -> Maybe Int32
  -- ^ Size of an average block for computing a derivative covariation matrix
  -- over each pixel neighborhood. See cornerEigenValsAndVecs.
  -> GoodFeaturesToTrackDetectionMethod
  -- ^ Parameter indicating whether to use a Harris detector (see cornerHarris)
  -- or cornerMinEigenVal.
  -> V.Vector (V2 Float)
goodFeaturesToTrack src maxCorners qualityLevel minDistance mbMask blockSize detector = unsafePerformIO $ do
  withPtr src  $ \srcPtr ->
    withPtr mbMask $ \mskPtr ->
    alloca $ \(cornersLengthsPtr :: Ptr Int32) ->
    alloca $ \(cornersPtrPtr :: Ptr (Ptr (Ptr C'Point2f))) -> mask_ $ do
    [C.block| void {
      std::vector<cv::Point2f> corners;
      Mat * mskPtr = $(Mat * mskPtr);
      cv::goodFeaturesToTrack
      ( *$(Mat * srcPtr)
      , corners
      , $(int32_t maxCorners)
      , $(double c'qualityLevel)
      , $(double c'minDistance)
      , mskPtr ? _InputArray(*mskPtr) : _InputArray(cv::noArray())
      , $(int32_t c'blockSize)
      , $(bool c'useHarrisDetector)
      , $(double c'harrisK)
      );

      cv::Point2f * * * cornersPtrPtr = $(Point2f * * * cornersPtrPtr);
      cv::Point2f * * cornersPtr = new cv::Point2f * [corners.size()];
      *cornersPtrPtr = cornersPtr;

      *$(int32_t * cornersLengthsPtr) = corners.size();

      for (std::vector<cv::Point2f>::size_type i = 0; i != corners.size(); i++) {
        cornersPtr[i] = new cv::Point2f( corners[i] );
      }
    }|]
    numCorners <- fromIntegral <$> peek cornersLengthsPtr
    cornersPtr <- peek cornersPtrPtr
    (corners :: [V2 Float]) <-
        peekArray numCorners cornersPtr >>=
        mapM (fmap (fmap fromCFloat . fromPoint) . fromPtr . pure)
    [CU.block| void {
      delete [] *$(Point2f * * * cornersPtrPtr);
    }|]
    pure (V.fromList  corners)
  where
    c'qualityLevel = realToFrac qualityLevel
    c'minDistance  = realToFrac minDistance
    c'blockSize    = fromMaybe 3 blockSize
    c'useHarrisDetector =
      fromBool $
        case detector of
          HarrisDetector _kValue -> True
          CornerMinEigenVal -> False
    c'harrisK =
      realToFrac $
        case detector of
          HarrisDetector kValue -> kValue
          CornerMinEigenVal -> 0.04

data GoodFeaturesToTrackDetectionMethod
   = HarrisDetector Double -- ^ Harris detector and it free k parameter
   | CornerMinEigenVal
   deriving (Show, Eq)

{- |

Finds circles in a grayscale image using a modification of the Hough
transformation.

Example:

@
houghCircleTraces
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Circles_1000x625)
    => Mat (ShapeT [height, width]) ('S channels) ('S depth)
houghCircleTraces = exceptError $ do
  imgG <- cvtColor bgr gray circles_1000x625
  let circles = houghCircles 1 10 Nothing Nothing Nothing Nothing imgG
  withMatM (Proxy :: Proxy [height, width])
           (Proxy :: Proxy channels)
           (Proxy :: Proxy depth)
           white $ \\imgM -> do
    void $ matCopyToM imgM (V2 0 0) circles_1000x625 Nothing
    forM_ circles $ \\c -> do
      circle imgM (round \<$> circleCenter c :: V2 Int32) (round (circleRadius c)) blue 1 LineType_AA 0
@

<<doc/generated/examples/houghCircleTraces.png houghCircleTraces>>
-}
houghCircles
  :: Double
     -- ^ Inverse ratio of the accumulator resolution to the image resolution.
     -- For example, if @dp=1@, the accumulator has the same resolution as the
     -- input image. If @dp=2@, the accumulator has half as big width and height.
  -> Double
     -- ^ Minimum distance between the centers of the detected circles. If the
     -- parameter is too small, multiple neighbor circles may be falsely
     -- detected in addition to a true one. If it is too large, some circles may
     -- be missed.
  -> Maybe Double
     -- ^ The higher threshold of the two passed to the 'canny' edge detector
     -- (the lower one is twice smaller). Default is 100.
  -> Maybe Double
     -- ^ The accumulator threshold for the circle centers at the detection
     -- stage. The smaller it is, the more false circles may be detected.
     -- Circles, corresponding to the larger accumulator values, will be returned
     -- first. Default is 100.
  -> Maybe Int32
     -- ^ Minimum circle radius.
  -> Maybe Int32
     -- ^ Maximum circle radius.
  -> Mat ('S [h, w]) ('S 1) ('S Word8)
  -> V.Vector Circle
houghCircles dp minDist param1 param2 minRadius maxRadius src = unsafePerformIO $
  withPtr src $ \srcPtr ->
  alloca $ \(circleLengthsPtr :: Ptr Int32) ->
  alloca $ \(circlesPtrPtr :: Ptr (Ptr (Ptr C'Vec3f))) -> mask_ $ do
    _ <- [cvExcept|
      std::vector<cv::Vec3f> circles;
      cv::HoughCircles(
        *$(Mat * srcPtr),
        circles,
        CV_HOUGH_GRADIENT,
        $(double c'dp),
        $(double c'minDist),
        $(double c'param1),
        $(double c'param2),
        $(int32_t c'minRadius),
        $(int32_t c'maxRadius)
      );

      cv::Vec3f * * * circlesPtrPtr = $(Vec3f * * * circlesPtrPtr);
      cv::Vec3f * * circlesPtr = new cv::Vec3f * [circles.size()];
      *circlesPtrPtr = circlesPtr;

      *$(int32_t * circleLengthsPtr) = circles.size();

      for (std::vector<cv::Vec3f>::size_type i = 0; i != circles.size(); i++) {
        circlesPtr[i] = new cv::Vec3f( circles[i] );
      }
    |]
    numCircles <- fromIntegral <$> peek circleLengthsPtr
    circlesPtr <- peek circlesPtrPtr
    (circles :: [V3 Float]) <-
        peekArray numCircles circlesPtr >>=
        mapM (fmap (fmap fromCFloat . fromVec) . fromPtr . pure)
    [CU.block| void { delete [] *$(Vec3f * * * circlesPtrPtr); }|]
    pure (V.fromList (map (\(V3 x y r) -> Circle (V2 x y) r) circles))
  where c'dp = realToFrac dp
        c'minDist = realToFrac minDist
        c'param1 = realToFrac (fromMaybe 100 param1)
        c'param2 = realToFrac (fromMaybe 100 param2)
        c'minRadius = fromIntegral (fromMaybe 0 minRadius)
        c'maxRadius = fromIntegral (fromMaybe 0 maxRadius)

data LineSegment depth
   = LineSegment
     { lineSegmentStart :: !(V2 depth)
     , lineSegmentStop  :: !(V2 depth)
     } deriving (Foldable, Functor, Traversable, Show)

type instance VecDim LineSegment = 4

instance (IsVec V4 depth) => IsVec LineSegment depth where
    toVec (LineSegment (V2 x1 y1) (V2 x2 y2)) =
        toVec (V4 x1 y1 x2 y2)

    fromVec vec =
        LineSegment
        { lineSegmentStart = V2 x1 y1
        , lineSegmentStop  = V2 x2 y2
        }
      where
        V4 x1 y1 x2 y2 = fromVec vec

{- |
Example:

@
houghLinesPTraces
  :: forall (width    :: Nat)
            (height   :: Nat)
            (channels :: Nat)
            (depth    :: *  )
   . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Building_868x600)
  => Mat (ShapeT [height, width]) ('S channels) ('S depth)
houghLinesPTraces = exceptError $ do
    edgeImg <- canny 50 200 Nothing CannyNormL1 building_868x600
    edgeImgBgr <- cvtColor gray bgr edgeImg
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \\imgM -> do
      edgeImgM <- thaw edgeImg
      lineSegments <- houghLinesP 1 (pi / 180) 80 (Just 30) (Just 10) edgeImgM
      void $ matCopyToM imgM (V2 0 0) edgeImgBgr Nothing
      forM_ lineSegments $ \\lineSegment -> do
        line imgM
             (lineSegmentStart lineSegment)
             (lineSegmentStop  lineSegment)
             red 2 LineType_8 0
@

<<doc/generated/examples/houghLinesPTraces.png houghLinesPTraces>>
-}
houghLinesP
  :: (PrimMonad m)
  => Double
     -- ^ Distance resolution of the accumulator in pixels.
  -> Double
     -- ^ Angle resolution of the accumulator in radians.
  -> Int32
     -- ^ Accumulator threshold parameter. Only those lines are returned that
     -- get enough votes (> threshold).
  -> Maybe Double
     -- ^ Minimum line length. Line segments shorter than that are rejected.
  -> Maybe Double
     -- ^ Maximum allowed gap between points on the same line to link them.
  -> Mut (Mat ('S [h, w]) ('S 1) ('S Word8)) (PrimState m)
     -- ^ Source image. May be modified by the function.
  -> m (V.Vector (LineSegment Int32))
houghLinesP rho theta threshold minLineLength maxLineGap src = unsafePrimToPrim $
    withPtr src $ \srcPtr ->
    -- Pointer to number of lines.
    alloca $ \(numLinesPtr :: Ptr Int32) ->
    -- Pointer to array of Vec4i pointers. The array is allocated in
    -- C++. Each element of the array points to a Vec4i that is also
    -- allocated in C++.
    alloca $ \(linesPtrPtr :: Ptr (Ptr (Ptr C'Vec4i))) -> mask_ $ do
      [C.block| void {
        std::vector<cv::Vec4i> lines = std::vector<cv::Vec4i>();
        cv::HoughLinesP
          ( *$(Mat * srcPtr)
          , lines
          , $(double  c'rho)
          , $(double  c'theta)
          , $(int32_t threshold)
          , $(double  c'minLineLength)
          , $(double  c'maxLineGap)
          );

        *$(int32_t * numLinesPtr) = lines.size();

        cv::Vec4i * * * linesPtrPtr = $(Vec4i * * * linesPtrPtr);
        cv::Vec4i * * linesPtr = new cv::Vec4i * [lines.size()];
        *linesPtrPtr = linesPtr;

        for (std::vector<cv::Vec4i>::size_type ix = 0; ix != lines.size(); ix++)
        {
          cv::Vec4i & org = lines[ix];
          cv::Vec4i * newLine = new cv::Vec4i(org[0], org[1], org[2], org[3]);
          linesPtr[ix] = newLine;
        }
      }|]

      numLines <- fromIntegral <$> peek numLinesPtr
      linesPtr <- peek linesPtrPtr
      lineSegments  <- mapM (fmap fromVec . fromPtr . pure) =<< peekArray numLines linesPtr

      -- Free the array of Vec4i pointers. This does not free the
      -- Vec4i's pointed to by the elements of the array. That is the
      -- responsibility of Haskell's Vec4i finalizer.
      [CU.block| void {
        delete [] *$(Vec4i * * * linesPtrPtr);
      }|]

      pure $ V.fromList lineSegments
  where
    c'rho           = realToFrac rho
    c'theta         = realToFrac theta
    c'minLineLength = maybe 0 realToFrac minLineLength
    c'maxLineGap    = maybe 0 realToFrac maxLineGap
