{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module OpenCV.Features2d
    ( -- * ORB
      Orb
    , OrbScoreType(..)
    , WTA_K(..)
    , OrbParams(..)
    , defaultOrbParams
    , mkOrb
    , orbDetectAndCompute

      -- * BLOB
    , SimpleBlobDetector
    , SimpleBlobDetectorParams(..)
    , BlobFilterByArea(..)
    , BlobFilterByCircularity(..)
    , BlobFilterByColor(..)
    , BlobFilterByConvexity(..)
    , BlobFilterByInertia(..)
    , defaultSimpleBlobDetectorParams
    , mkSimpleBlobDetector
    , blobDetect

      -- * DescriptorMatcher
    , DescriptorMatcher(..)
      -- ** BFMatcher
    , BFMatcher
    , newBFMatcher
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Word
import "base" Data.Maybe
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr, nullPtr )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.ArrayOps
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception ( cvExcept, unsafeWrapException )
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/features2d.hpp"
C.include "orb.hpp"
C.include "simple_blob_detector.hpp"

C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/features2d.hpp"

#include "namespace.hpp"
#include "orb.hpp"
#include "simple_blob_detector.hpp"

infinity :: Float
infinity = 1 / 0

--------------------------------------------------------------------------------
-- ORB - Oriented BRIEF
--------------------------------------------------------------------------------

-- Internally, an Orb is a pointer to a @cv::Ptr<cv::ORB>@, which in turn points
-- to an actual @cv::ORB@ object.
newtype Orb = Orb {unOrb :: ForeignPtr C'Ptr_ORB}

type instance C Orb = C'Ptr_ORB

instance WithPtr Orb where
    withPtr = withForeignPtr . unOrb

instance FromPtr Orb where
    fromPtr = objFromPtr Orb $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::ORB> * orb_ptr_ptr = $(Ptr_ORB * ptr);
                  orb_ptr_ptr->release();
                  delete orb_ptr_ptr;
                }|]

--------------------------------------------------------------------------------

data WTA_K
   = WTA_K_2
   | WTA_K_3
   | WTA_K_4

marshalWTA_K :: WTA_K -> Int32
marshalWTA_K = \case
    WTA_K_2 -> 2
    WTA_K_3 -> 3
    WTA_K_4 -> 4

data OrbScoreType
   = HarrisScore
   | FastScore

#num HARRIS_SCORE
#num FAST_SCORE

marshalOrbScoreType :: OrbScoreType -> Int32
marshalOrbScoreType = \case
    HarrisScore -> c'HARRIS_SCORE
    FastScore   -> c'FAST_SCORE

data OrbParams
   = OrbParams
     { orb_nfeatures :: !Int32
       -- ^ The maximum number of features to retain.
     , orb_scaleFactor :: !Float
       -- ^ Pyramid decimation ratio, greater than 1. 'orb_scaleFactor' == 2
       -- means the classical pyramid, where each next level has 4x less pixels
       -- than the previous, but such a big scale factor will degrade feature
       -- matching scores dramatically. On the other hand, too close to 1 scale
       -- factor will mean that to cover certain scale range you will need more
       -- pyramid levels and so the speed will suffer.
     , orb_nlevels :: !Int32
       -- ^ The number of pyramid levels. The smallest level will have linear
       -- size equal to input_image_linear_size / 'orb_scaleFactor' **
       -- 'orb_nlevels'.
     , orb_edgeThreshold :: !Int32
       -- ^ This is size of the border where the features are not detected. It
       -- should roughly match the patchSize parameter.
     , orb_firstLevel :: !Int32
       -- ^ It should be 0 in the current implementation.
     , orb_WTA_K :: !WTA_K
       -- ^ The number of points that produce each element of the oriented BRIEF
       -- descriptor. The default value 'WTA_K_2' means the BRIEF where we take
       -- a random point pair and compare their brightnesses, so we get 0/1
       -- response. Other possible values are 'WTA_K_3' and 'WTA_K_4'. For
       -- example, 'WTA_K_3' means that we take 3 random points (of course,
       -- those point coordinates are random, but they are generated from the
       -- pre-defined seed, so each element of BRIEF descriptor is computed
       -- deterministically from the pixel rectangle), find point of maximum
       -- brightness and output index of the winner (0, 1 or 2). Such output
       -- will occupy 2 bits, and therefore it will need a special variant of
       -- Hamming distance, denoted as 'Norm_Hamming2' (2 bits per bin). When
       -- 'WTA_K_4', we take 4 random points to compute each bin (that will also
       -- occupy 2 bits with possible values 0, 1, 2 or 3).
     , orb_scoreType :: !OrbScoreType
       -- ^ The default 'HarrisScore' means that Harris algorithm is used to
       -- rank features (the score is written to KeyPoint::score and is used to
       -- retain best nfeatures features); 'FastScore' is alternative value of
       -- the parameter that produces slightly less stable keypoints, but it is
       -- a little faster to compute.
     , orb_patchSize :: !Int32
       -- ^ Size of the patch used by the oriented BRIEF descriptor. Of course,
       -- on smaller pyramid layers the perceived image area covered by a
       -- feature will be larger.
     , orb_fastThreshold :: !Int32
     }

defaultOrbParams :: OrbParams
defaultOrbParams =
    OrbParams
     { orb_nfeatures     = 500
     , orb_scaleFactor   = 1.2
     , orb_nlevels       = 8
     , orb_edgeThreshold = 31
     , orb_firstLevel    = 0
     , orb_WTA_K         = WTA_K_2
     , orb_scoreType     = HarrisScore
     , orb_patchSize     = 31
     , orb_fastThreshold = 20
     }

--------------------------------------------------------------------------------

newOrb :: OrbParams -> IO Orb
newOrb OrbParams{..} = fromPtr
    [CU.block|Ptr_ORB * {
      cv::Ptr<cv::ORB> orbPtr =
        cv::ORB::create
        ( $(int32_t orb_nfeatures)
        , $(float   c'scaleFactor)
        , $(int32_t orb_nlevels)
        , $(int32_t orb_edgeThreshold)
        , $(int32_t orb_firstLevel)
        , $(int32_t c'WTA_K)
        , $(int32_t c'scoreType)
        , $(int32_t orb_patchSize)
        , $(int32_t orb_fastThreshold)
        );
      return new cv::Ptr<cv::ORB>(orbPtr);
    }|]
  where
    c'scaleFactor = realToFrac orb_scaleFactor
    c'WTA_K       = marshalWTA_K        orb_WTA_K
    c'scoreType   = marshalOrbScoreType orb_scoreType

mkOrb :: OrbParams -> Orb
mkOrb = unsafePerformIO . newOrb

--------------------------------------------------------------------------------

{- | Detect keypoints and compute descriptors

Example:

@
orbDetectAndComputeImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . (Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Frog)
    => Mat (ShapeT [height, width]) ('S channels) ('S depth)
orbDetectAndComputeImg = exceptError $ do
    (kpts, _descs) <- orbDetectAndCompute orb frog Nothing
    withMatM (Proxy :: Proxy [height, width])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      void $ matCopyToM imgM (V2 0 0) frog Nothing
      forM_ kpts $ \kpt -> do
        let kptRec = keyPointAsRec kpt
        circle imgM (round \<$> kptPoint kptRec :: V2 Int32) 5 blue 1 LineType_AA 0
  where
    orb = mkOrb defaultOrbParams
@

<<doc/generated/examples/orbDetectAndComputeImg.png orbDetectAndComputeImg>>
-}
orbDetectAndCompute
    :: Orb
    -> Mat ('S [height, width]) channels depth -- ^ Image.
    -> Maybe (Mat ('S [height, width]) ('S 1) ('S Word8)) -- ^ Mask.
    -> CvExcept ( V.Vector KeyPoint
                , Mat 'D 'D 'D
                )
orbDetectAndCompute orb img mbMask = unsafeWrapException $ do
    descriptors <- newEmptyMat
    withPtr orb $ \orbPtr ->
      withPtr img $ \imgPtr ->
      withPtr mbMask $ \maskPtr ->
      withPtr descriptors $ \descPtr ->
      alloca $ \(numPtsPtr :: Ptr Int32) ->
      alloca $ \(arrayPtrPtr :: Ptr (Ptr (Ptr C'KeyPoint))) -> mask_ $ do
        ptrException <- [cvExcept|
          cv::ORB * orb = *$(Ptr_ORB * orbPtr);
          cv::Mat * maskPtr = $(Mat * maskPtr);

          std::vector<cv::KeyPoint> keypoints = std::vector<cv::KeyPoint>();
          orb->
            detectAndCompute
            ( *$(Mat * imgPtr)
            , maskPtr ? cv::_InputArray(*maskPtr) : cv::_InputArray(noArray())
            , keypoints
            , *$(Mat * descPtr)
            , false
            );

          *$(int32_t * numPtsPtr) = keypoints.size();

          cv::KeyPoint * * * arrayPtrPtr = $(KeyPoint * * * arrayPtrPtr);
          cv::KeyPoint * * arrayPtr = new cv::KeyPoint * [keypoints.size()];
          *arrayPtrPtr = arrayPtr;

          for (std::vector<cv::KeyPoint>::size_type ix = 0; ix != keypoints.size(); ix++)
          {
            cv::KeyPoint & org = keypoints[ix];
            cv::KeyPoint * newPt =
              new cv::KeyPoint( org.pt
                              , org.size
                              , org.angle
                              , org.response
                              , org.octave
                              , org.class_id
                              );
            arrayPtr[ix] = newPt;
          }
        |]
        if ptrException /= nullPtr
        then Left . BindingException <$> fromPtr (pure ptrException)
        else do
          numPts <- fromIntegral <$> peek numPtsPtr
          arrayPtr <- peek arrayPtrPtr
          keypoints <- mapM (fromPtr . pure) =<< peekArray numPts arrayPtr

          [CU.block| void {
            delete [] *$(KeyPoint * * * arrayPtrPtr);
          }|]

          pure $ Right (V.fromList keypoints, relaxMat descriptors)

--------------------------------------------------------------------------------
-- BLOB - Binary Large OBject
--------------------------------------------------------------------------------

-- Internally, a SimpleBlobDetector is a pointer to a @cv::Ptr<cv::SimpleBlobDetector>@, which in turn points
-- to an actual @cv::SimpleBlobDetector@ object.
newtype SimpleBlobDetector = SimpleBlobDetector {unSimpleBlobDetector :: ForeignPtr C'Ptr_SimpleBlobDetector}

type instance C SimpleBlobDetector = C'Ptr_SimpleBlobDetector

instance WithPtr SimpleBlobDetector where
    withPtr = withForeignPtr . unSimpleBlobDetector

instance FromPtr SimpleBlobDetector where
    fromPtr = objFromPtr SimpleBlobDetector $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::SimpleBlobDetector> * simpleBlobDetector_ptr_ptr = $(Ptr_SimpleBlobDetector * ptr);
                  simpleBlobDetector_ptr_ptr->release();
                  delete simpleBlobDetector_ptr_ptr;
                }|]

data BlobFilterByArea
     = BlobFilterByArea
     { blob_minArea :: !Float
     , blob_maxArea :: !Float
     } deriving Eq

data BlobFilterByCircularity
     = BlobFilterByCircularity
     { blob_minCircularity :: !Float
     , blob_maxCircularity :: !Float
     } deriving Eq

data BlobFilterByColor
     = BlobFilterByColor
     { blob_blobColor :: !Word8
     } deriving Eq

data BlobFilterByConvexity
     = BlobFilterByConvexity
     { blob_minConvexity :: !Float
     , blob_maxConvexity :: !Float
     } deriving Eq

data BlobFilterByInertia
     = BlobFilterByInertia
     { blob_minInertiaRatio :: !Float
     , blob_maxInertiaRatio :: !Float
     } deriving Eq

data SimpleBlobDetectorParams
   = SimpleBlobDetectorParams
     { blob_minThreshold :: !Float
     , blob_maxThreshold :: !Float
     , blob_thresholdStep :: !Float
     , blob_minRepeatability :: !Int32
     , blob_minDistBetweenBlobs :: !Float
     , blob_filterByArea :: !(Maybe BlobFilterByArea)
       -- ^ Extracted blobs have an area between 'minArea' (inclusive) and
       --   'maxArea' (exclusive).
     , blob_filterByCircularity :: !(Maybe BlobFilterByCircularity)
       -- ^ Extracted blobs have circularity
       --   @(4 * pi * Area)/(perimeter * perimeter)@ between 'minCircularity'
       --   (inclusive) and 'maxCircularity' (exclusive).
     , blob_filterByColor :: !(Maybe BlobFilterByColor)
       -- ^ This filter compares the intensity of a binary image at the center of
       --   a blob to 'blobColor'. If they differ, the blob is filtered out. Use
       --   @blobColor = 0@ to extract dark blobs and @blobColor = 255@ to extract
       --   light blobs.
     , blob_filterByConvexity :: !(Maybe BlobFilterByConvexity)
       -- ^ Extracted blobs have convexity (area / area of blob convex hull) between
       --   'minConvexity' (inclusive) and 'maxConvexity' (exclusive).
     , blob_filterByInertia :: !(Maybe BlobFilterByInertia)
       -- ^ Extracted blobs have this ratio between 'minInertiaRatio' (inclusive)
       --   and 'maxInertiaRatio' (exclusive).
     }

defaultSimpleBlobDetectorParams :: SimpleBlobDetectorParams
defaultSimpleBlobDetectorParams =
    SimpleBlobDetectorParams
    { blob_minThreshold = 50
    , blob_maxThreshold = 220
    , blob_thresholdStep = 10
    , blob_minRepeatability = 2
    , blob_minDistBetweenBlobs = 10
    , blob_filterByArea = Just (BlobFilterByArea 25 5000)
    , blob_filterByCircularity = Nothing
    , blob_filterByColor = Just (BlobFilterByColor 0)
    , blob_filterByConvexity = Just (BlobFilterByConvexity 0.95 infinity)
    , blob_filterByInertia = Just (BlobFilterByInertia 0.1 infinity)
    }

--------------------------------------------------------------------------------

newSimpleBlobDetector :: SimpleBlobDetectorParams -> IO SimpleBlobDetector
newSimpleBlobDetector SimpleBlobDetectorParams{..} = fromPtr
    [CU.block|Ptr_SimpleBlobDetector * {
      cv::SimpleBlobDetector::Params params;
      params.blobColor           = $(unsigned char c'blobColor);
      params.filterByArea        = $(bool c'filterByArea);
      params.filterByCircularity = $(bool c'filterByCircularity);
      params.filterByColor       = $(bool c'filterByColor);
      params.filterByConvexity   = $(bool c'filterByConvexity);
      params.filterByInertia     = $(bool c'filterByInertia);
      params.maxArea             = $(float c'maxArea);
      params.maxCircularity      = $(float c'maxCircularity);
      params.maxConvexity        = $(float c'maxConvexity);
      params.maxInertiaRatio     = $(float c'maxInertiaRatio);
      params.maxThreshold        = $(float c'maxThreshold);
      params.minArea             = $(float c'minArea);
      params.minCircularity      = $(float c'minCircularity);
      params.minConvexity        = $(float c'minConvexity);
      params.minDistBetweenBlobs = $(float c'minDistBetweenBlobs);
      params.minInertiaRatio     = $(float c'minInertiaRatio);
      params.minRepeatability    = $(float c'minRepeatability);
      params.minThreshold        = $(float c'minThreshold);
      params.thresholdStep       = $(float c'thresholdStep);
      cv::Ptr<cv::SimpleBlobDetector> detectorPtr =
        cv::SimpleBlobDetector::create(params);
      return new cv::Ptr<cv::SimpleBlobDetector>(detectorPtr);
    }|]
  where
    c'minThreshold        = realToFrac blob_minThreshold
    c'maxThreshold        = realToFrac blob_maxThreshold
    c'thresholdStep       = realToFrac blob_thresholdStep
    c'minRepeatability    = realToFrac blob_minRepeatability
    c'minDistBetweenBlobs = realToFrac blob_minDistBetweenBlobs
    c'filterByArea        = fromBool (isJust blob_filterByArea)
    c'filterByCircularity = fromBool (isJust blob_filterByCircularity)
    c'filterByColor       = fromBool (isJust blob_filterByColor)
    c'filterByConvexity   = fromBool (isJust blob_filterByConvexity)
    c'filterByInertia     = fromBool (isJust blob_filterByInertia)
    c'minArea             = realToFrac (fromMaybe 25 (fmap blob_minArea blob_filterByArea))
    c'maxArea             = realToFrac (fromMaybe 5000 (fmap blob_maxArea blob_filterByArea))
    c'minCircularity      = realToFrac (fromMaybe 0.8 (fmap blob_minCircularity blob_filterByCircularity))
    c'maxCircularity      = realToFrac (fromMaybe infinity (fmap blob_maxCircularity blob_filterByCircularity))
    c'blobColor           = fromIntegral (fromMaybe 0 (fmap blob_blobColor blob_filterByColor))
    c'minConvexity        = realToFrac (fromMaybe 0.95 (fmap blob_minConvexity blob_filterByConvexity))
    c'maxConvexity        = realToFrac (fromMaybe infinity (fmap blob_maxConvexity blob_filterByConvexity))
    c'minInertiaRatio     = realToFrac (fromMaybe 0.1 (fmap blob_minInertiaRatio blob_filterByInertia))
    c'maxInertiaRatio     = realToFrac (fromMaybe infinity (fmap blob_maxInertiaRatio blob_filterByInertia))

mkSimpleBlobDetector :: SimpleBlobDetectorParams -> SimpleBlobDetector
mkSimpleBlobDetector = unsafePerformIO . newSimpleBlobDetector

--------------------------------------------------------------------------------

{- | Detect keypoints and compute descriptors
-}
blobDetect
    :: SimpleBlobDetector
    -> Mat ('S [height, width]) channels depth -- ^ Image.
    -> Maybe (Mat ('S [height, width]) ('S 1) ('S Word8)) -- ^ Mask.
    -> CvExcept (V.Vector KeyPoint)
blobDetect detector img mbMask = unsafeWrapException $ do
    withPtr detector $ \detectorPtr ->
      withPtr img $ \imgPtr ->
      withPtr mbMask $ \maskPtr ->
      alloca $ \(numPtsPtr :: Ptr Int32) ->
      alloca $ \(arrayPtrPtr :: Ptr (Ptr (Ptr C'KeyPoint))) -> mask_ $ do
        ptrException <- [cvExcept|
          cv::SimpleBlobDetector * detector = *$(Ptr_SimpleBlobDetector * detectorPtr);
          cv::Mat * maskPtr = $(Mat * maskPtr);

          std::vector<cv::KeyPoint> keypoints = std::vector<cv::KeyPoint>();
          detector->
            detect
            ( *$(Mat * imgPtr)
            , keypoints
            , maskPtr ? cv::_InputArray(*maskPtr) : cv::_InputArray(noArray())
            );

          *$(int32_t * numPtsPtr) = keypoints.size();

          cv::KeyPoint * * * arrayPtrPtr = $(KeyPoint * * * arrayPtrPtr);
          cv::KeyPoint * * arrayPtr = new cv::KeyPoint * [keypoints.size()];
          *arrayPtrPtr = arrayPtr;

          for (std::vector<cv::KeyPoint>::size_type ix = 0; ix != keypoints.size(); ix++)
          {
            arrayPtr[ix] = new cv::KeyPoint(keypoints[ix]);
          }
        |]
        if ptrException /= nullPtr
        then Left . BindingException <$> fromPtr (pure ptrException)
        else do
          numPts <- fromIntegral <$> peek numPtsPtr
          arrayPtr <- peek arrayPtrPtr
          keypoints <- mapM (fromPtr . pure) =<< peekArray numPts arrayPtr

          [CU.block| void {
            delete [] *$(KeyPoint * * * arrayPtrPtr);
          }|]

          pure $ Right (V.fromList keypoints)

--------------------------------------------------------------------------------
-- DescriptorMatcher
--------------------------------------------------------------------------------

class DescriptorMatcher a where
    match
        :: a
        -> Mat 'D 'D 'D -- ^ Query set of descriptors.
        -> Mat 'D 'D 'D -- ^ Train set of descriptors.
        -> Maybe (Mat ('S [height, width]) ('S 1) ('S Word8))
           -- ^ Mask specifying permissible matches between an input query and
           -- train matrices of descriptors..
        -> IO (V.Vector DMatch)

--------------------------------------------------------------------------------
-- BFMatcher
--------------------------------------------------------------------------------

{- | Brute-force descriptor matcher

For each descriptor in the first set, this matcher finds the closest descriptor
in the second set by trying each one. This descriptor matcher supports masking
permissible matches of descriptor sets.

Example:

@
bfMatcherImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Frog
       , width2 ~ (*) width 2
       )
    => IO (Mat (ShapeT [height, width2]) ('S channels) ('S depth))
bfMatcherImg = do
    let (kpts1, descs1) = exceptError $ orbDetectAndCompute orb frog        Nothing
        (kpts2, descs2) = exceptError $ orbDetectAndCompute orb rotatedFrog Nothing

    bfmatcher <- newBFMatcher Norm_Hamming True
    matches <- match bfmatcher
                     descs1 -- Query descriptors
                     descs2 -- Train descriptors
                     Nothing
    exceptErrorIO $ pureExcept $
      withMatM (Proxy :: Proxy [height, width2])
               (Proxy :: Proxy channels)
               (Proxy :: Proxy depth)
               white $ \imgM -> do
        matCopyToM imgM (V2 0     0) frog        Nothing
        matCopyToM imgM (V2 width 0) rotatedFrog Nothing

        -- Draw the matches as lines from the query image to the train image.
        forM_ matches $ \dmatch -> do
          let matchRec = dmatchAsRec dmatch
              queryPt = kpts1 V.! fromIntegral (dmatchQueryIdx matchRec)
              trainPt = kpts2 V.! fromIntegral (dmatchTrainIdx matchRec)
              queryPtRec = keyPointAsRec queryPt
              trainPtRec = keyPointAsRec trainPt

          -- We translate the train point one width to the right in order to
          -- match the position of rotatedFrog in imgM.
          line imgM
               (round \<$> kptPoint queryPtRec :: V2 Int32)
               ((round \<$> kptPoint trainPtRec :: V2 Int32) ^+^ V2 width 0)
               blue 1 LineType_AA 0
  where
    orb = mkOrb defaultOrbParams {orb_nfeatures = 50}

    width = fromInteger $ natVal (Proxy :: Proxy width)

    rotatedFrog = exceptError $
                  warpAffine frog rotMat InterArea False False (BorderConstant black)
    rotMat = getRotationMatrix2D (V2 250 195 :: V2 CFloat) 45 0.8
@

<<doc/generated/examples/bfMatcherImg.png bfMatcherImg>>

<http://docs.opencv.org/3.0-last-rst/modules/features2d/doc/common_interfaces_of_descriptor_matchers.html#bfmatcher OpenCV Sphinx doc>
-}
newtype BFMatcher = BFMatcher {unBFMatcher :: ForeignPtr C'BFMatcher}

type instance C BFMatcher = C'BFMatcher

instance WithPtr BFMatcher where
    withPtr = withForeignPtr . unBFMatcher

instance FromPtr BFMatcher where
    fromPtr = objFromPtr BFMatcher $ \ptr ->
                [CU.exp| void { delete $(BFMatcher * ptr) }|]

--------------------------------------------------------------------------------

newBFMatcher
    :: NormType
       -- ^ 'Norm_L1' and 'Norm_L2' norms are preferable choices for SIFT and
       -- SURF descriptors, 'Norm_Hamming' should be used with 'Orb', BRISK and
       -- BRIEF, 'Norm_Hamming2' should be used with 'Orb' when 'WTA_K_3' or
       -- 'WTA_K_4' (see 'orb_WTA_K').
    -> Bool
       -- ^ If it is false, this is will be default 'BFMatcher' behaviour when
       -- it finds the k nearest neighbors for each query descriptor. If
       -- crossCheck == True, then the @knnMatch()@ method with @k=1@ will only
       -- return pairs @(i,j)@ such that for i-th query descriptor the j-th
       -- descriptor in the matcher's collection is the nearest and vice versa,
       -- i.e. the 'BFMatcher' will only return consistent pairs. Such technique
       -- usually produces best results with minimal number of outliers when
       -- there are enough matches. This is alternative to the ratio test, used
       -- by D. Lowe in SIFT paper.
    -> IO BFMatcher
newBFMatcher normType crossCheck = fromPtr
    [CU.exp|BFMatcher * {
      new cv::BFMatcher
          ( $(int32_t c'normType)
          , $(bool c'crossCheck)
          )
    }|]
  where
    c'normType = marshalNormType NormAbsolute normType
    c'crossCheck = fromBool crossCheck

--------------------------------------------------------------------------------

instance DescriptorMatcher BFMatcher where
  match bf queryDescriptors trainDescriptors mbMask =
      withPtr bf               $ \bfPtr    ->
      withPtr queryDescriptors $ \queryPtr ->
      withPtr trainDescriptors $ \trainPtr ->
      withPtr mbMask           $ \maskPtr  ->
      alloca $ \(numMatchesPtr :: Ptr Int32) ->
      alloca $ \(arrayPtrPtr :: Ptr (Ptr (Ptr C'DMatch))) -> mask_ $ do
        [C.block| void {
          cv::Mat * maskPtr = $(Mat * maskPtr);
          std::vector<cv::DMatch> matches = std::vector<cv::DMatch>();

          $(BFMatcher * bfPtr)->match
            ( *$(Mat * queryPtr)
            , *$(Mat * trainPtr)
            , matches
            , maskPtr ? cv::_InputArray(*maskPtr) : cv::_InputArray(noArray())
            );

          *$(int32_t * numMatchesPtr) = matches.size();

          cv::DMatch * * * arrayPtrPtr = $(DMatch * * * arrayPtrPtr);
          cv::DMatch * * arrayPtr = new cv::DMatch * [matches.size()];
          *arrayPtrPtr = arrayPtr;

          for (std::vector<cv::DMatch>::size_type ix = 0; ix != matches.size(); ix++)
          {
            cv::DMatch & org = matches[ix];
            cv::DMatch * newMatch =
              new cv::DMatch( org.queryIdx
                            , org.trainIdx
                            , org.imgIdx
                            , org.distance
                            );
            arrayPtr[ix] = newMatch;
          }
        }|]

        (numMatches :: Int) <- fromIntegral <$> peek numMatchesPtr
        arrayPtr <- peek arrayPtrPtr
        matches <- mapM (fromPtr . pure) =<< peekArray numMatches arrayPtr

        [CU.block| void {
          delete [] *$(DMatch * * * arrayPtrPtr);
        }|]

        pure $ V.fromList matches

