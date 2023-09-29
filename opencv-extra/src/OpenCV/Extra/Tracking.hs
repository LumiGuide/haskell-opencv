{-# language MultiParamTypeClasses #-}
{-# language PackageImports #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}

{- |
Tracking extra opencv module
-}

module OpenCV.Extra.Tracking
  ( Tracker(..)
  , MultiTracker (..)
  , MultiTrackerAlt (..)
  , TrackerType (..)
  , TrackerFeature (..)
  , TrackerFeatureType (..)
  , newTracker
  , initTracker
  , updateTracker
  , newMultiTracker
--   , newMultiTrackerAlt
  , newTrackerFeature
  ) where

import "base" Data.Word
import "base" Data.Int (Int32)
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.C.String ( withCString )
import "base" Foreign.Marshal.Utils ( toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Internal
import "opencv" OpenCV.Internal.C.FinalizerTH
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.TypeLevel
import "primitive" Control.Monad.Primitive
import "this" OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )
import "this" OpenCV.Extra.Internal.C.Types

--------------------------------------------------------------------------------

C.context openCvExtraCtx

C.include "opencv2/core.hpp"
C.include "opencv2/tracking.hpp"
C.include "opencv2/tracking/tracking_legacy.hpp"

C.include "tracking.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------

data TrackerType
   = MIL         -- ^
   | KCF         -- ^
   | GOTURN      -- ^
     deriving (Eq, Show, Enum, Bounded)

type FEATURE2D_DetectorType = String
type FEATURE2D_DescriptorType = String

data TrackerFeatureType
   = HAAR      -- ^ Haar Feature-based
   | HOG       -- ^ Histogram of Oriented Gradients features
   | LBP       -- ^ Local Binary Pattern features
   | FEATURE2D -- ^ All types of Feature2D
       FEATURE2D_DetectorType
       FEATURE2D_DescriptorType
     deriving (Eq, Show)

--------------------------------------------------------------------------------


newtype Tracker s = Tracker { unTracker :: ForeignPtr C'Ptr_Tracker }

type instance C (Tracker s) = C'Ptr_Tracker

instance WithPtr (Tracker s) where withPtr = withForeignPtr . unTracker

mkFinalizer ReleaseDeletePtr "deleteTracker" "cv::Ptr<cv::Tracker>" ''C'Ptr_Tracker

instance FromPtr (Tracker s) where fromPtr = objFromPtr Tracker deleteTracker

newTracker
    :: (PrimMonad m)
    => TrackerType
       -- ^ Name
    -> m (Tracker (PrimState m))
newTracker tType = unsafePrimToPrim $ fromPtr $ case tType of
    MIL        -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerMIL::create());}|]
    KCF        -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerKCF::create());}|]
    GOTURN     -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerGOTURN::create());}|]

initTracker
    :: (PrimMonad m, IsRect rect Int32)
    => Tracker (PrimState m)
    -> Mat ('S '[ 'D, 'D]) ('D) ('D)
    -> rect Int32
    -> m ()
initTracker trk srcImg boundingBox = unsafePrimToPrim $
    withPtr trk $ \trkPtr ->
    withPtr srcImg $ \srcPtr ->
    withPtr (toRect boundingBox) $ \rectPtr ->
         [C.block| void {
           (*$(Ptr_Tracker * trkPtr))->init
              ( *$(Mat * srcPtr)
              , *$(const Rect2i * rectPtr)
              );
         }
         |]

updateTracker
     :: (PrimMonad m)
     => Tracker (PrimState m)
     -> Mat ('D) ('D) ('S Word8)
     -> m (Maybe (Rect Int32))
updateTracker trk srcImg = unsafePrimToPrim $
    withPtr trk $ \trkPtr ->
      withPtr srcImg $ \srcPtr ->
      withPtr rect $ \rectPtr -> do
        ok <- toBool <$> [C.block| bool {
          Rect2i boundingBox = *$(Rect2i * rectPtr);
          return (*$(Ptr_Tracker * trkPtr))->update
              ( *$(Mat * srcPtr)
              // , *$(Rect2i * rectPtr)
              , boundingBox
              );
         }
         |]
        pure $ if ok then Just rect else Nothing
  where
    rect :: Rect2i
    rect = toRect HRect{hRectTopLeft = 0, hRectSize = 0}

--------------------------------------------------------------------------------

newtype TrackerFeature  s = TrackerFeature  { unTrackerFeature  :: ForeignPtr C'Ptr_TrackerFeature }
newtype MultiTracker    s = MultiTracker    { unMultiTracker    :: ForeignPtr C'Ptr_MultiTracker }
newtype MultiTrackerAlt s = MultiTrackerAlt { unMultiTrackerAlt :: ForeignPtr C'Ptr_MultiTrackerAlt }

type instance C (TrackerFeature  s) = C'Ptr_TrackerFeature
type instance C (MultiTracker    s) = C'Ptr_MultiTracker
type instance C (MultiTrackerAlt s) = C'Ptr_MultiTrackerAlt

mkFinalizer ReleaseDeletePtr "deleteTrackerFeature"  "cv::Ptr<cv::detail::tracking::TrackerFeature>"   ''C'Ptr_TrackerFeature
mkFinalizer ReleaseDeletePtr "deleteMultiTracker"    "cv::Ptr<cv::legacy::tracking::MultiTracker>"     ''C'Ptr_MultiTracker
mkFinalizer ReleaseDeletePtr "deleteMultiTrackerAlt" "cv::Ptr<cv::legacy::tracking::MultiTracker_Alt>" ''C'Ptr_MultiTrackerAlt

instance WithPtr (TrackerFeature s) where
    withPtr = withForeignPtr . unTrackerFeature

instance WithPtr (MultiTracker s) where
    withPtr = withForeignPtr . unMultiTracker

instance WithPtr (MultiTrackerAlt s) where
    withPtr = withForeignPtr . unMultiTrackerAlt

instance FromPtr (TrackerFeature s) where
    fromPtr = objFromPtr TrackerFeature deleteTrackerFeature

instance FromPtr (MultiTracker s) where
    fromPtr = objFromPtr MultiTracker deleteMultiTracker

instance FromPtr (MultiTrackerAlt s) where
    fromPtr = objFromPtr MultiTrackerAlt deleteMultiTrackerAlt

--------------------------------------------------------------------------------

newMultiTracker
    :: (PrimMonad m)
    => m (MultiTracker (PrimState m))
newMultiTracker =
  unsafePrimToPrim $ fromPtr $
    [CU.block|Ptr_MultiTracker * {
      return new cv::Ptr<cv::legacy::tracking::MultiTracker>(new cv::legacy::tracking::MultiTracker());
    }|]

--------------------------------------------------------------------------------

newTrackerFeature
    :: (PrimMonad m)
    => TrackerFeatureType -- ^ Name
    -> m (TrackerFeature (PrimState m))
newTrackerFeature trackerFeatureType =
  unsafePrimToPrim $ fromPtr $ case trackerFeatureType of
    HAAR ->
      [CU.block|Ptr_TrackerFeature * {
        return new cv::Ptr<cv::detail::tracking::TrackerFeature>(new cv::detail::tracking::TrackerContribFeatureHAAR());
      }|]
    HOG ->
      [CU.block|Ptr_TrackerFeature * {
        return new cv::Ptr<cv::detail::tracking::TrackerFeature>(new cv::detail::tracking::TrackerFeatureHOG());
      }|]
    LBP ->
      [CU.block|Ptr_TrackerFeature * {
        return new cv::Ptr<cv::detail::tracking::TrackerFeature>(new cv::detail::tracking::TrackerFeatureLBP());
      }|]
    FEATURE2D detectorType descriptorType ->
      withCString (show detectorType) $ \c'detectorType ->
      withCString (show descriptorType) $ \c'descriptorType ->

        [CU.block|Ptr_TrackerFeature * {
          return new cv::Ptr<cv::detail::tracking::TrackerFeature>(
            new cv::detail::tracking::TrackerFeatureFeature2d(
              cv::String($(const char * c'detectorType)),
              cv::String($(const char * c'descriptorType))
            )
          );
        }|]
