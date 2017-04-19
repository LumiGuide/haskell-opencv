{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language MultiParamTypeClasses #-}

module OpenCV.Extra.XPhoto.WhiteBalancer where

import "base" Control.Exception ( mask_ )
import "base" Data.Int
import "base" Data.Word
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
-- import "opencv" OpenCV.Video.MotionAnalysis ( BackgroundSubtractor(..) )
import "primitive" Control.Monad.Primitive
import "this" OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx )
import "this" OpenCV.Extra.Internal.C.Types
import "opencv" OpenCV.TypeLevel

C.context openCvExtraCtx

C.include "opencv2/core.hpp"
C.include "opencv2/xphoto.hpp"
C.include "white-ballance.hpp"

C.using "namespace cv"

--------------------------------------------------------------------------------
-- WhiteBalancer
--------------------------------------------------------------------------------

class WhiteBalancer a m where
    balanceWhite
        :: a
        -> Mat ('S [h, w]) channels depth
           -- ^ The input Image.
        -> m (Mat ('S [h, w]) ('S 1) ('S Word8))
           -- ^ The output image.

--------------------------------------------------------------------------------
-- Background subtractors
--------------------------------------------------------------------------------

newtype GrayworldWB s
      = GrayworldWB
        { unGrayworldWB :: ForeignPtr C'Ptr_GrayworldWB }

type instance C (GrayworldWB  s) = C'Ptr_GrayworldWB

instance WithPtr (GrayworldWB s) where
    withPtr = withForeignPtr . unGrayworldWB

instance FromPtr (GrayworldWB s) where
    fromPtr = objFromPtr GrayworldWB $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::xphoto::GrayworldWB> * knn_ptr_ptr =
                    $(Ptr_GrayworldWB * ptr);
                  knn_ptr_ptr->release();
                  delete knn_ptr_ptr;
                }|]

newtype LearningBasedWB s
      = LearningBasedWB
        { unLearningBasedWB :: ForeignPtr C'Ptr_LearningBasedWB }

type instance C (LearningBasedWB  s) = C'Ptr_LearningBasedWB

instance WithPtr (LearningBasedWB s) where
    withPtr = withForeignPtr . unLearningBasedWB

instance FromPtr (LearningBasedWB s) where
    fromPtr = objFromPtr LearningBasedWB $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::xphoto::LearningBasedWB> * knn_ptr_ptr =
                    $(Ptr_LearningBasedWB * ptr);
                  knn_ptr_ptr->release();
                  delete knn_ptr_ptr;
                }|]

newtype SimpleWB s
      = SimpleWB
        { unSimpleWB :: ForeignPtr C'Ptr_SimpleWB }

type instance C (SimpleWB  s) = C'Ptr_SimpleWB

instance WithPtr (SimpleWB s) where
    withPtr = withForeignPtr . unSimpleWB

instance FromPtr (SimpleWB s) where
    fromPtr = objFromPtr SimpleWB $ \ptr ->
                [CU.block| void {
                  cv::Ptr<cv::xphoto::SimpleWB> * knn_ptr_ptr =
                    $(Ptr_SimpleWB * ptr);
                  knn_ptr_ptr->release();
                  delete knn_ptr_ptr;
                }|]

---
instance (PrimMonad m, s ~ PrimState m) => Algorithm (GrayworldWB s) m where
    algorithmClearState knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
          [C.block|void {
              cv::xphoto::GrayworldWB * knn = *$(Ptr_GrayworldWB * knnPtr);
              knn->clear();
          }|]

    algorithmIsEmpty knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::xphoto::GrayworldWB * knn = *$(Ptr_GrayworldWB * knnPtr);
              *$(bool * emptyPtr) = knn->empty();
          }|]
          toBool <$> peek emptyPtr

instance (PrimMonad m, s ~ PrimState m) => Algorithm (LearningBasedWB s) m where
    algorithmClearState knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
          [C.block|void {
              cv::xphoto::LearningBasedWB * knn = *$(Ptr_LearningBasedWB * knnPtr);
              knn->clear();
          }|]

    algorithmIsEmpty knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::xphoto::LearningBasedWB * knn = *$(Ptr_LearningBasedWB * knnPtr);
              *$(bool * emptyPtr) = knn->empty();
          }|]
          toBool <$> peek emptyPtr

instance (PrimMonad m, s ~ PrimState m) => Algorithm (SimpleWB s) m where
    algorithmClearState knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
          [C.block|void {
              cv::xphoto::SimpleWB * knn = *$(Ptr_SimpleWB * knnPtr);
              knn->clear();
          }|]

    algorithmIsEmpty knn = unsafePrimToPrim $
        withPtr knn $ \knnPtr ->
        alloca $ \emptyPtr -> do
          [C.block|void {
              cv::xphoto::SimpleWB * knn = *$(Ptr_SimpleWB * knnPtr);
              *$(bool * emptyPtr) = knn->empty();
          }|]
          toBool <$> peek emptyPtr
---

instance (PrimMonad m, s ~ PrimState m) => WhiteBalancer (GrayworldWB s) m where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> mask_ $ do
            [C.block| void {
              cv::xphoto::GrayworldWB * wb = *$(Ptr_GrayworldWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut

instance (PrimMonad m, s ~ PrimState m) => WhiteBalancer (LearningBasedWB s) m where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> mask_ $ do
            [C.block| void {
              cv::xphoto::LearningBasedWB * wb = *$(Ptr_LearningBasedWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut

instance (PrimMonad m, s ~ PrimState m) => WhiteBalancer (SimpleWB s) m where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> mask_ $ do
            [C.block| void {
              cv::xphoto::SimpleWB * wb = *$(Ptr_SimpleWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut
