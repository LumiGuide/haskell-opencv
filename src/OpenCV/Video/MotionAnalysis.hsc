{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module OpenCV.Video.MotionAnalysis
    ( -- * BackgroundSubtractor
      BackgroundSubtractor(..)
      -- * Background subtractors
    , BackgroundSubtractorMOG2
    , BackgroundSubtractorKNN
    , newBackgroundSubtractorKNN
    , newBackgroundSubtractorMOG2
    ) where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Maybe
import "base" Data.Word
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool, toBool )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/video.hpp"
C.include "video_motion_analysis.hpp"

C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/video.hpp"

#include "namespace.hpp"
#include "video_motion_analysis.hpp"

--------------------------------------------------------------------------------
-- BackgroundSubtractor
--------------------------------------------------------------------------------

class BackgroundSubtractor a where
    bgSubApply
        :: (PrimMonad m)
        => a (PrimState m)
        -> Double
           -- ^ The value between 0 and 1 that indicates how fast the background
           -- model is learnt. Negative parameter value makes the algorithm to
           -- use some automatically chosen learning rate. 0 means that the
           -- background model is not updated at all, 1 means that the
           -- background model is completely reinitialized from the last frame.
        -> Mat ('S [h, w]) channels depth
           -- ^ Next video frame.
        -> m (Mat ('S [h, w]) ('S 1) ('S Word8))
           -- ^ The output foreground mask as an 8-bit binary image.

    getBackgroundImage
        :: (PrimMonad m)
        => a (PrimState m)
        -> m (Mat ('S [h, w]) channels depth)
           -- ^ The output background image.

{- |

Example:

@
carAnim :: Animation (ShapeT [240, 320]) ('S 3) ('S Word8)
carAnim = carOverhead

mog2Anim :: IO (Animation (ShapeT [240, 320]) ('S 3) ('S Word8))
mog2Anim = do
    mog2 <- newBackgroundSubtractorMOG2 Nothing Nothing Nothing
    forM carOverhead $ \(delay, img) -> do
      fg <- bgSubApply mog2 0.1 img
      fgBgr <- exceptErrorIO $ pureExcept $ cvtColor gray bgr fg
      pure (delay, fgBgr)
@

Original:
<<doc/generated/examples/car.gif carAnim>>

Foreground:
<<doc/generated/examples/mog2.gif mog2Anim>>
-}

--------------------------------------------------------------------------------
-- Background subtractors
--------------------------------------------------------------------------------

newtype BackgroundSubtractorKNN s
      = BackgroundSubtractorKNN
        { unBackgroundSubtractorKNN :: ForeignPtr C'Ptr_BackgroundSubtractorKNN }

newtype BackgroundSubtractorMOG2 s
      = BackgroundSubtractorMOG2
        { unBackgroundSubtractorMOG2 :: ForeignPtr C'Ptr_BackgroundSubtractorMOG2 }

type instance C (BackgroundSubtractorKNN  s) = C'Ptr_BackgroundSubtractorKNN
type instance C (BackgroundSubtractorMOG2 s) = C'Ptr_BackgroundSubtractorMOG2

instance WithPtr (BackgroundSubtractorKNN s) where
    withPtr = withForeignPtr . unBackgroundSubtractorKNN

instance WithPtr (BackgroundSubtractorMOG2 s) where
    withPtr = withForeignPtr . unBackgroundSubtractorMOG2

instance FromPtr (BackgroundSubtractorKNN s) where
    fromPtr = objFromPtr BackgroundSubtractorKNN $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::BackgroundSubtractorKNN> * knn_ptr_ptr =
                    $(Ptr_BackgroundSubtractorKNN * ptr);
                  knn_ptr_ptr->release();
                  delete knn_ptr_ptr;
                }|]

instance FromPtr (BackgroundSubtractorMOG2 s) where
    fromPtr = objFromPtr BackgroundSubtractorMOG2 $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::BackgroundSubtractorMOG2> * mog2_ptr_ptr =
                    $(Ptr_BackgroundSubtractorMOG2 * ptr);
                  mog2_ptr_ptr->release();
                  delete mog2_ptr_ptr;
                }|]

--------------------------------------------------------------------------------

newBackgroundSubtractorKNN
    :: (PrimMonad m)
    => Maybe Int32
       -- ^ Length of the history.
    -> Maybe Double
       -- ^ Threshold on the squared distance between the pixel and the sample
       -- to decide whether a pixel is close to that sample. This parameter does
       -- not affect the background update.
    -> Maybe Bool
       -- ^ If 'True', the algorithm will detect shadows and mark them. It
       -- decreases the speed a bit, so if you do not need this feature, set the
       -- parameter to 'False'.
    -> m (BackgroundSubtractorKNN (PrimState m))
newBackgroundSubtractorKNN mbHistory mbDist2Threshold mbDetectShadows = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_BackgroundSubtractorKNN * {
      cv::Ptr<cv::BackgroundSubtractorKNN> knnPtr =
        cv::createBackgroundSubtractorKNN
        ( $(int32_t c'history       )
        , $(double  c'dist2Threshold)
        , $(bool    c'detectShadows )
        );
      return new cv::Ptr<cv::BackgroundSubtractorKNN>(knnPtr);
    }|]
  where
    c'history        = fromMaybe 500 mbHistory
    c'dist2Threshold = maybe 400 realToFrac mbDist2Threshold
    c'detectShadows  = fromBool $ fromMaybe True mbDetectShadows

newBackgroundSubtractorMOG2
    :: (PrimMonad m)
    => Maybe Int32
       -- ^ Length of the history.
    -> Maybe Double
       -- ^ Threshold on the squared Mahalanobis distance between the pixel and
       -- the model to decide whether a pixel is well described by the
       -- background model. This parameter does not affect the background
       -- update.
    -> Maybe Bool
       -- ^ If 'True', the algorithm will detect shadows and mark them. It
       -- decreases the speed a bit, so if you do not need this feature, set the
       -- parameter to 'False'.
    -> m (BackgroundSubtractorMOG2 (PrimState m))
newBackgroundSubtractorMOG2 mbHistory mbVarThreshold mbDetectShadows = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_BackgroundSubtractorMOG2 * {
      cv::Ptr<cv::BackgroundSubtractorMOG2> mog2Ptr =
        cv::createBackgroundSubtractorMOG2
        ( $(int32_t c'history      )
        , $(double  c'varThreshold )
        , $(bool    c'detectShadows)
        );
      return new cv::Ptr<cv::BackgroundSubtractorMOG2>(mog2Ptr);
    }|]
  where
    c'history       = fromMaybe 500 mbHistory
    c'varThreshold  = maybe 16 realToFrac mbVarThreshold
    c'detectShadows = fromBool $ fromMaybe True mbDetectShadows

--------------------------------------------------------------------------------

instance Algorithm BackgroundSubtractorKNN where
    algorithmClearState knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
          [C.block|void {
              cv::BackgroundSubtractorKNN * knn = *$(Ptr_BackgroundSubtractorKNN * knnPtr);
              knn->clear();
          }|]

    algorithmIsEmpty knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::BackgroundSubtractorKNN * knn = *$(Ptr_BackgroundSubtractorKNN * knnPtr);
              *$(bool * emptyPtr) = knn->empty();
          }|]
          toBool <$> peek emptyPtr

instance Algorithm BackgroundSubtractorMOG2 where
    algorithmClearState mog2 = unsafePrimToPrim $
        withPtr mog2 $ \mog2Ptr ->
          [C.block|void {
              cv::BackgroundSubtractorMOG2 * mog2 = *$(Ptr_BackgroundSubtractorMOG2 * mog2Ptr);
              mog2->clear();
          }|]

    algorithmIsEmpty mog2 = unsafePrimToPrim $
        withPtr mog2 $ \mog2Ptr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::BackgroundSubtractorMOG2 * mog2 = *$(Ptr_BackgroundSubtractorMOG2 * mog2Ptr);
              *$(bool * emptyPtr) = mog2->empty();
          }|]
          toBool <$> peek emptyPtr

instance BackgroundSubtractor BackgroundSubtractorKNN where
    bgSubApply knn learningRate img = unsafePrimToPrim $ do
        fgMask <- newEmptyMat
        withPtr knn $ \knnPtr ->
          withPtr img $ \imgPtr ->
          withPtr fgMask $ \fgMaskPtr -> mask_ $ do
            [C.block| void {
              cv::BackgroundSubtractorKNN * knn = *$(Ptr_BackgroundSubtractorKNN * knnPtr);
              knn->apply
              ( *$(Mat * imgPtr)
              , *$(Mat * fgMaskPtr)
              , $(double c'learningRate)
              );
            }|]
        pure $ unsafeCoerceMat fgMask
      where
        c'learningRate = realToFrac learningRate

    getBackgroundImage knn = unsafePrimToPrim $ do
        img <- newEmptyMat
        withPtr knn $ \knnPtr ->
          withPtr img $ \imgPtr -> mask_ $ do
            [C.block| void {
              cv::BackgroundSubtractorKNN * knn = *$(Ptr_BackgroundSubtractorKNN * knnPtr);
              knn->getBackgroundImage(*$(Mat * imgPtr));
            }|]
            pure $ unsafeCoerceMat img

instance BackgroundSubtractor BackgroundSubtractorMOG2 where
    bgSubApply mog2 learningRate img = unsafePrimToPrim $ do
        fgMask <- newEmptyMat
        withPtr mog2 $ \mog2Ptr ->
          withPtr img $ \imgPtr ->
          withPtr fgMask $ \fgMaskPtr -> mask_ $ do
            [C.block| void {
              cv::BackgroundSubtractorMOG2 * mog2 = *$(Ptr_BackgroundSubtractorMOG2 * mog2Ptr);
              mog2->apply
              ( *$(Mat * imgPtr)
              , *$(Mat * fgMaskPtr)
              , $(double c'learningRate)
              );
            }|]
        pure $ unsafeCoerceMat fgMask
      where
        c'learningRate = realToFrac learningRate

    getBackgroundImage mog2 = unsafePrimToPrim $ do
        img <- newEmptyMat
        withPtr mog2 $ \mog2Ptr ->
          withPtr img $ \imgPtr -> mask_ $ do
            [C.block| void {
              cv::BackgroundSubtractorMOG2 * mog2 = *$(Ptr_BackgroundSubtractorMOG2 * mog2Ptr);
              mog2->getBackgroundImage(*$(Mat * imgPtr));
            }|]
            pure $ unsafeCoerceMat img
