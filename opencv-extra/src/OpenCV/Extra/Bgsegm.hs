{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language MultiParamTypeClasses #-}

{- |
Two additional background subtraction algorithms. These algorithms do
not support `getBackgroundImage` (and probably never will).
-}
module OpenCV.Extra.Bgsegm
    ( -- * Background subtractors
      BackgroundSubtractorGMG
    , BackgroundSubtractorMOG

    , newBackgroundSubtractorGMG
    , newBackgroundSubtractorMOG
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Maybe
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Core.Types
import "opencv" OpenCV.Internal
import "opencv" OpenCV.Internal.Core.Types.Mat
import "opencv" OpenCV.Internal.C.Types
import "opencv" OpenCV.Video.MotionAnalysis ( BackgroundSubtractor(..) )
import "primitive" Control.Monad.Primitive
import "this" OpenCV.Internal.Extra.C.Inline ( openCvExtraCtx )

--------------------------------------------------------------------------------

C.context openCvExtraCtx

C.include "opencv2/core.hpp"
C.include "opencv2/video.hpp"
C.include "opencv2/bgsegm.hpp"
C.include "video_motion_analysis_bgsegm.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------

newtype BackgroundSubtractorGMG s
      = BackgroundSubtractorGMG
        { unBackgroundSubtractorGMG :: ForeignPtr C'Ptr_BackgroundSubtractorGMG }

newtype BackgroundSubtractorMOG s
      = BackgroundSubtractorMOG
        { unBackgroundSubtractorMOG :: ForeignPtr C'Ptr_BackgroundSubtractorMOG }

type instance C (BackgroundSubtractorGMG  s) = C'Ptr_BackgroundSubtractorGMG
type instance C (BackgroundSubtractorMOG s) = C'Ptr_BackgroundSubtractorMOG

instance WithPtr (BackgroundSubtractorGMG s) where
    withPtr = withForeignPtr . unBackgroundSubtractorGMG

instance WithPtr (BackgroundSubtractorMOG s) where
    withPtr = withForeignPtr . unBackgroundSubtractorMOG

instance FromPtr (BackgroundSubtractorGMG s) where
    fromPtr = objFromPtr BackgroundSubtractorGMG $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::bgsegm::BackgroundSubtractorGMG> * knn_ptr_ptr =
                    $(Ptr_BackgroundSubtractorGMG * ptr);
                  knn_ptr_ptr->release();
                  delete knn_ptr_ptr;
                }|]

instance FromPtr (BackgroundSubtractorMOG s) where
    fromPtr = objFromPtr BackgroundSubtractorMOG $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::bgsegm::BackgroundSubtractorMOG> * mog2_ptr_ptr =
                    $(Ptr_BackgroundSubtractorMOG * ptr);
                  mog2_ptr_ptr->release();
                  delete mog2_ptr_ptr;
                }|]

--------------------------------------------------------------------------------

newBackgroundSubtractorGMG
    :: (PrimMonad m)
    => Maybe Int32
       -- ^ Number of frames used to initialize the background models.
    -> Maybe Double
       -- ^ Threshold value, above which it is marked foreground, else background.
    -> m (BackgroundSubtractorGMG (PrimState m))
newBackgroundSubtractorGMG mbInitializationFrames mbDecisionThreshold =
  unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_BackgroundSubtractorGMG * {
      cv::Ptr<cv::bgsegm::BackgroundSubtractorGMG> gmgPtr =
        cv::bgsegm::createBackgroundSubtractorGMG
        ( $(int32_t c'initializationFrames)
        , $(double  c'decisionThreshold   )
        );
      return new cv::Ptr<cv::bgsegm::BackgroundSubtractorGMG>(gmgPtr);
    }|]
  where
    c'initializationFrames = fromMaybe 120 mbInitializationFrames
    c'decisionThreshold    = maybe 0.8 realToFrac mbDecisionThreshold

newBackgroundSubtractorMOG
    :: (PrimMonad m)
    => Maybe Int32
       -- ^ Length of the history.
    -> Maybe Int32
       -- ^ Number of Gaussian mixtures.
    -> Maybe Double
       -- ^ Background ratio.
    -> Maybe Double
       -- ^ Noise strength (standard deviation of the brightness or each
       --  color channel). 0 means some automatic value.
    -> m (BackgroundSubtractorMOG (PrimState m))
newBackgroundSubtractorMOG mbHistory mbNumGausianMix mbBackgroundRatio mbNoise
  = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_BackgroundSubtractorMOG * {
      cv::Ptr<cv::bgsegm::BackgroundSubtractorMOG> mog2Ptr =
        cv::bgsegm::createBackgroundSubtractorMOG
        ( $(int32_t c'history       )
        , $(int32_t c'numGausianMix )
        , $(double  c'backgroundRatio )
        , $(double  c'noise )
        );
      return new cv::Ptr<cv::bgsegm::BackgroundSubtractorMOG>(mog2Ptr);
    }|]
  where
    c'history          = fromMaybe 200 mbHistory
    c'numGausianMix    = fromMaybe 5   mbNumGausianMix
    c'backgroundRatio  = maybe 0.7 realToFrac mbBackgroundRatio
    c'noise            = maybe 0   realToFrac mbNoise

--------------------------------------------------------------------------------

instance (PrimMonad m, s ~ PrimState m) => Algorithm (BackgroundSubtractorGMG s) m where
    algorithmClearState knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
          [C.block|void {
              cv::bgsegm::BackgroundSubtractorGMG * knn = *$(Ptr_BackgroundSubtractorGMG * knnPtr);
              knn->clear();
          }|]

    algorithmIsEmpty gmg = unsafePrimToPrim $
        withPtr gmg $ \gmgPtr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::bgsegm::BackgroundSubtractorGMG * gmg = *$(Ptr_BackgroundSubtractorGMG * gmgPtr);
              *$(bool * emptyPtr) = gmg->empty();
          }|]
          toBool <$> peek emptyPtr

instance (PrimMonad m, s ~ PrimState m) => Algorithm (BackgroundSubtractorMOG s) m where
    algorithmClearState mog2 = unsafePrimToPrim $
        withPtr mog2 $ \mog2Ptr ->
          [C.block|void {
              cv::bgsegm::BackgroundSubtractorMOG * mog2 = *$(Ptr_BackgroundSubtractorMOG * mog2Ptr);
              mog2->clear();
          }|]

    algorithmIsEmpty mog2 = unsafePrimToPrim $
        withPtr mog2 $ \mog2Ptr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::bgsegm::BackgroundSubtractorMOG * mog2 = *$(Ptr_BackgroundSubtractorMOG * mog2Ptr);
              *$(bool * emptyPtr) = mog2->empty();
          }|]
          toBool <$> peek emptyPtr

instance (PrimMonad m, s ~ PrimState m) => BackgroundSubtractor (BackgroundSubtractorGMG s) m where
    bgSubApply gmg learningRate img = unsafePrimToPrim $ do
        fgMask <- newEmptyMat
        withPtr gmg $ \gmgPtr ->
          withPtr img $ \imgPtr ->
          withPtr fgMask $ \fgMaskPtr -> mask_ $ do
            [C.block| void {
              cv::bgsegm::BackgroundSubtractorGMG * gmg = *$(Ptr_BackgroundSubtractorGMG * gmgPtr);
              gmg->apply
              ( *$(Mat * imgPtr)
              , *$(Mat * fgMaskPtr)
              , $(double c'learningRate)
              );
            }|]
        pure $ unsafeCoerceMat fgMask
      where
        c'learningRate = realToFrac learningRate

    -- not supported by the backend
    getBackgroundImage gmg = unsafePrimToPrim $ do
        img <- newEmptyMat
        withPtr gmg $ \gmgPtr ->
          withPtr img $ \imgPtr -> mask_ $ do
            [C.block| void {
              cv::bgsegm::BackgroundSubtractorGMG * gmg = *$(Ptr_BackgroundSubtractorGMG * gmgPtr);
              gmg->getBackgroundImage(*$(Mat * imgPtr));
            }|]
            pure $ unsafeCoerceMat img

instance (PrimMonad m, s ~ PrimState m) => BackgroundSubtractor (BackgroundSubtractorMOG s) m where
    bgSubApply mog learningRate img = unsafePrimToPrim $ do
        fgMask <- newEmptyMat
        withPtr mog $ \mogPtr ->
          withPtr img $ \imgPtr ->
          withPtr fgMask $ \fgMaskPtr -> mask_ $ do
            [C.block| void {
              cv::bgsegm::BackgroundSubtractorMOG * mog = *$(Ptr_BackgroundSubtractorMOG * mogPtr);
              mog->apply
              ( *$(Mat * imgPtr)
              , *$(Mat * fgMaskPtr)
              , $(double c'learningRate)
              );
            }|]
        pure $ unsafeCoerceMat fgMask
      where
        c'learningRate = realToFrac learningRate

    -- not supported by the backend
    getBackgroundImage mog = unsafePrimToPrim $ do
        img <- newEmptyMat
        withPtr mog $ \mogPtr ->
          withPtr img $ \imgPtr -> mask_ $ do
            [C.block| void {
              cv::bgsegm::BackgroundSubtractorMOG * mog = *$(Ptr_BackgroundSubtractorMOG * mogPtr);
              mog->getBackgroundImage(*$(Mat * imgPtr));
            }|]
            pure $ unsafeCoerceMat img
