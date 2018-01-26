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

C.include "tracking.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------

data TrackerType
   = BOOSTING    -- ^
   | MIL         -- ^
   | KCF         -- ^
   | MEDIANFLOW  -- ^
   | TLD         -- ^
   | GOTURN      -- ^
     deriving (Eq, Show, Enum, Bounded)

data TrackerFeatureType
   = HAAR      -- ^ Haar Feature-based
   | HOG       -- ^ soon Histogram of Oriented Gradients features
   | LBP       -- ^ soon Local Binary Pattern features
   | FEATURE2D -- ^ soon All types of Feature2D
     deriving (Eq, Show, Enum, Bounded)

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
    BOOSTING   -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerBoosting::create());}|]
    MIL        -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerMIL::create());}|]
    KCF        -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerKCF::create());}|]
    MEDIANFLOW -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerMedianFlow::create());}|]
    TLD        -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerTLD::create());}|]
    GOTURN     -> [CU.block|Ptr_Tracker * {return new cv::Ptr<cv::Tracker>(cv::TrackerGOTURN::create());}|]

initTracker
    :: (PrimMonad m, IsRect rect C.CDouble)
    => Tracker (PrimState m)
    -> Mat ('S '[ 'D, 'D]) ('D) ('D)
    -> rect C.CDouble
    -> m Bool
initTracker trk srcImg boundingBox = unsafePrimToPrim $
    withPtr trk $ \trkPtr ->
    withPtr srcImg $ \srcPtr ->
    withPtr (toRect boundingBox) $ \rectPtr -> toBool <$>
           [C.block| bool {
           return (*$(Ptr_Tracker * trkPtr))->init
              ( *$(Mat * srcPtr)
              , *$(Rect2d * rectPtr)
              );
         }
         |]

updateTracker
     :: (PrimMonad m)
     => Tracker (PrimState m)
     -> Mat ('D) ('D) ('S Word8)
     -> m (Maybe (Rect C.CDouble))
--      -> m (Maybe (Rect Int32))
updateTracker trk srcImg = unsafePrimToPrim $
    withPtr trk $ \trkPtr ->
      withPtr srcImg $ \srcPtr ->
      withPtr rect $ \rectPtr -> do
        ok <- toBool <$> [C.block| bool {
           return (*$(Ptr_Tracker * trkPtr))->update
              ( *$(Mat * srcPtr)
              , *$(Rect2d * rectPtr)
              );
         }
         |]
        pure $ if ok then Just rect else Nothing
  where
    rect :: Rect2d
    rect = toRect HRect{hRectTopLeft = 0, hRectSize = 0}

--------------------------------------------------------------------------------

newtype TrackerFeature  s = TrackerFeature  { unTrackerFeature  :: ForeignPtr C'Ptr_TrackerFeature }
newtype MultiTracker    s = MultiTracker    { unMultiTracker    :: ForeignPtr C'Ptr_MultiTracker }
newtype MultiTrackerAlt s = MultiTrackerAlt { unMultiTrackerAlt :: ForeignPtr C'Ptr_MultiTrackerAlt }

type instance C (TrackerFeature  s) = C'Ptr_TrackerFeature
type instance C (MultiTracker    s) = C'Ptr_MultiTracker
type instance C (MultiTrackerAlt s) = C'Ptr_MultiTrackerAlt

mkFinalizer ReleaseDeletePtr "deleteTrackerFeature"  "cv::Ptr<cv::TrackerFeature>"   ''C'Ptr_TrackerFeature
mkFinalizer ReleaseDeletePtr "deleteMultiTracker"    "cv::Ptr<cv::MultiTracker>"     ''C'Ptr_MultiTracker
mkFinalizer ReleaseDeletePtr "deleteMultiTrackerAlt" "cv::Ptr<cv::MultiTracker_Alt>" ''C'Ptr_MultiTrackerAlt

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
      return new cv::Ptr<cv::MultiTracker>(new cv::MultiTracker());
    }|]

--------------------------------------------------------------------------------

newTrackerFeature
    :: (PrimMonad m)
    => TrackerFeatureType -- ^ Name
    -> m (TrackerFeature (PrimState m))
newTrackerFeature trackerFeatureType =
  unsafePrimToPrim $ fromPtr $
    withCString (show trackerFeatureType) $ \c'trackerFeatureType ->
    [CU.block|Ptr_TrackerFeature * {
        cv::Ptr<cv::TrackerFeature> ftracker =
          cv::TrackerFeature::create(cv::String($(const char * c'trackerFeatureType)));
          return new cv::Ptr<cv::TrackerFeature>(ftracker);
    }|]
