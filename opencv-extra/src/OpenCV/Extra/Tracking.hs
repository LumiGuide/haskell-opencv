{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language MultiParamTypeClasses #-}

{-# language PackageImports #-}
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
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.Internal
import "opencv" OpenCV.TypeLevel
import "opencv" OpenCV.Internal.C.Types
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

instance WithPtr (Tracker s) where
    withPtr = withForeignPtr . unTracker


instance FromPtr (Tracker s) where
    fromPtr = objFromPtr Tracker $ \ptr ->
                [CU.block| void {
                  delete $(Ptr_Tracker * ptr);
                }|]

newTracker
  :: (PrimMonad m)
  => TrackerType
     -- ^ Name
  -> m (Tracker (PrimState m))
newTracker trackerType =
  unsafePrimToPrim $ fromPtr $
    withCString (show trackerType) $ \c'trackerType ->
    [CU.block|Ptr_Tracker * {
      cv::Ptr<cv::Tracker> tacker =
        cv::Tracker::create ( cv::String($(const char * c'trackerType)));
      return new cv::Ptr<cv::Tracker>(tacker);
    }|]

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
        return $ if ok
           then
              Just rect
           else
              Nothing
  where
    rect :: Rect2d
    rect = toRect HRect{ hRectTopLeft = pure 0
                       , hRectSize    = pure 0
                       }

--------------------------------------------------------------------------------

newtype TrackerFeature s = TrackerFeature { unTrackerFeature :: ForeignPtr C'Ptr_TrackerFeature }
type instance C (TrackerFeature s) = C'Ptr_TrackerFeature

instance WithPtr (TrackerFeature s) where
    withPtr = withForeignPtr . unTrackerFeature


instance FromPtr (TrackerFeature s) where
    fromPtr = objFromPtr TrackerFeature $ \ptr ->
                [CU.block| void {
                  delete $(Ptr_TrackerFeature * ptr);
                }|]

newtype MultiTracker s = MultiTracker { unMultiTracker :: ForeignPtr C'Ptr_MultiTracker }
type instance C (MultiTracker s) = C'Ptr_MultiTracker

instance WithPtr (MultiTracker s) where
    withPtr = withForeignPtr . unMultiTracker

instance FromPtr (MultiTracker s) where
    fromPtr = objFromPtr MultiTracker $ \ptr ->
                [CU.block| void {
                  delete $(Ptr_MultiTracker * ptr);
                }|]

newtype MultiTrackerAlt s = MultiTrackerAlt { unMultiTrackerAlt :: ForeignPtr C'Ptr_MultiTrackerAlt }
type instance C (MultiTrackerAlt s) = C'Ptr_MultiTrackerAlt

instance WithPtr (MultiTrackerAlt s) where
    withPtr = withForeignPtr . unMultiTrackerAlt

instance FromPtr (MultiTrackerAlt s) where
    fromPtr = objFromPtr MultiTrackerAlt $ \ptr ->
                [CU.block| void {
                  delete $(Ptr_MultiTrackerAlt * ptr);
                }|]

--------------------------------------------------------------------------------

newMultiTracker
    :: (PrimMonad m)
    => TrackerType
       -- ^ Name
    -> m (MultiTracker (PrimState m))
newMultiTracker trackerType =
  unsafePrimToPrim $ fromPtr $
    withCString (show trackerType) $ \c'trackerType ->
    [CU.block|Ptr_MultiTracker * {
    cv::Ptr<cv::MultiTracker> mtracker =
      new cv::MultiTracker ( cv::String($(const char * c'trackerType)));
      return new cv::Ptr<cv::MultiTracker>(mtracker);
    }|]

--------------------------------------------------------------------------------

newTrackerFeature
  :: (PrimMonad m)
  => TrackerFeatureType
     -- ^ Name
  -> m (TrackerFeature (PrimState m))
newTrackerFeature trackerFeatureType =
  unsafePrimToPrim $ fromPtr $
    withCString (show trackerFeatureType) $ \c'trackerFeatureType ->
    [CU.block|Ptr_TrackerFeature * {
        cv::Ptr<cv::TrackerFeature> ftracker =
          cv::TrackerFeature::create ( cv::String($(const char * c'trackerFeatureType)));
          return new cv::Ptr<cv::TrackerFeature>(ftracker);

    }|]
