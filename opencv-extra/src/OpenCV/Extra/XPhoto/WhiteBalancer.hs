{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module OpenCV.Extra.XPhoto.WhiteBalancer
 ( WhiteBalancer (..)
 , GrayworldWB
 , LearningBasedWB
 , SimpleWB
 , newGrayworldWB
 , newLearningBasedWB
 , newSimpleWB
 ) where

import "base" Data.Int
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

class WhiteBalancer a where
    balanceWhite
        :: (PrimMonad m)
        => a (PrimState m)
        ->    Mat ('S [h, w]) channels depth  -- ^ The input Image.
        -> m (Mat ('S [h, w]) channels depth) -- ^ The output image.

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
instance Algorithm GrayworldWB where
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

instance Algorithm LearningBasedWB where
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

instance Algorithm SimpleWB where
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

instance WhiteBalancer GrayworldWB where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> do
            [C.block| void {
              cv::xphoto::GrayworldWB * wb = *$(Ptr_GrayworldWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut

instance WhiteBalancer LearningBasedWB where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> do
            [C.block| void {
              cv::xphoto::LearningBasedWB * wb = *$(Ptr_LearningBasedWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut

instance WhiteBalancer SimpleWB where
    balanceWhite wbAlg imgIn = unsafePrimToPrim $ do
        imgOut <- newEmptyMat
        withPtr wbAlg $ \wbAlgPtr ->
          withPtr imgIn $ \imgInPtr ->
          withPtr imgOut $ \imgOutPtr -> do
            [C.block| void {
              cv::xphoto::SimpleWB * wb = *$(Ptr_SimpleWB * wbAlgPtr);
              wb->balanceWhite
              ( *$(Mat * imgInPtr)
              , *$(Mat * imgOutPtr)
              );
            }|]
            pure $ unsafeCoerceMat imgOut
---

{-| Perform GrayworldWB a simple grayworld white balance algorithm.

Example:

@
grayworldWBImg
    :: forall h w h2 w2 c d
     . ( Mat (ShapeT [h, w]) ('S c) ('S d) ~ Sailboat_768x512
       , w2 ~ ((*) w 2)
       , h2 ~ ((*) h 2)
       )
    => IO (Mat ('S ['S h2, 'S w2]) ('S c) ('S d))
grayworldWBImg = do
    let
      bw :: (WhiteBalancer a) => a (PrimState IO) -> IO (Mat (ShapeT [h, w]) ('S c) ('S d))
      bw = flip balanceWhite sailboat_768x512
    balancedGrayworldWB <- bw =<< newGrayworldWB Nothing
    balancedLearningBasedWB <- bw =<< newLearningBasedWB Nothing Nothing Nothing
    balancedSimpleWB <- bw =<< newSimpleWB Nothing Nothing Nothing Nothing Nothing
    pure $ exceptError $
        withMatM
          (Proxy :: Proxy [h2, w2])
          (Proxy :: Proxy c)
          (Proxy :: Proxy d)
          black $ \\imgM -> do
            matCopyToM imgM (V2 0 0) sailboat_768x512 Nothing
            matCopyToM imgM (V2 w 0) balancedGrayworldWB Nothing
            matCopyToM imgM (V2 0 h) balancedLearningBasedWB Nothing
            matCopyToM imgM (V2 w h) balancedSimpleWB Nothing
  where
    w = fromInteger $ natVal (Proxy :: Proxy w)
    h = fromInteger $ natVal (Proxy :: Proxy h)
@

<<doc/generated/examples/grayworldWBImg.png grayworldWBImg>>

-}

newGrayworldWB
    :: (PrimMonad m)
    => Maybe Double
       -- ^ A threshold of 1 means that all pixels are used to white-balance,
       -- while a threshold of 0 means no pixels are used. Lower thresholds
       -- are useful in white-balancing saturated images. Default: 0.9.
    -> m (GrayworldWB (PrimState m))
newGrayworldWB mbVarThreshold = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_GrayworldWB * {
      cv::Ptr<cv::xphoto::GrayworldWB> wbAlg = cv::xphoto::createGrayworldWB ();
      wbAlg->setSaturationThreshold($(double  c'varThreshold ));
      return new cv::Ptr<cv::xphoto::GrayworldWB>(wbAlg);
    }|]
  where
    c'varThreshold = maybe 0.9 realToFrac mbVarThreshold

newLearningBasedWB
    :: (PrimMonad m)
    => Maybe Int32
      -- ^ default 64, Defines the size of one dimension of a
      -- three-dimensional RGB histogram that is used internally by the algorithm.
      -- It often makes sense to increase the number of bins for images with
      -- higher bit depth (e.g. 256 bins for a 12 bit image).
    -> Maybe Int32
      -- ^ default 255, Maximum possible value of the input image (e.g. 255 for 8 bit images, 4095 for 12 bit images)
    -> Maybe Double
      -- ^ default 0.98, Threshold that is used to determine saturated pixels,
      -- i.e. pixels where at least one of the channels exceeds
    -> m (LearningBasedWB (PrimState m))
newLearningBasedWB mbVarHistBinNum mbRangeMaxVal mbVarSaturationThreshold
  = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_LearningBasedWB * {
      cv::Ptr<cv::xphoto::LearningBasedWB> wbAlg = cv::xphoto::createLearningBasedWB ();
      wbAlg->setHistBinNum($(int  c'varHistBinNum ));
      wbAlg->setRangeMaxVal($(int  c'varRangeMaxVal ));
      wbAlg->setSaturationThreshold($(double  c'varSaturationThreshold ));
      return new cv::Ptr<cv::xphoto::LearningBasedWB>(wbAlg);
    }|]
  where
    c'varHistBinNum          = maybe 64   fromIntegral mbVarHistBinNum
    c'varRangeMaxVal         = maybe 255  fromIntegral mbRangeMaxVal
    c'varSaturationThreshold = maybe 0.98 realToFrac   mbVarSaturationThreshold

newSimpleWB
    :: (PrimMonad m)
    => Maybe Double -- ^ Input Min (default: 0)
    -> Maybe Double -- ^ Input Max (default: 255)
    -> Maybe Double -- ^ Output Min (default: 0)
    -> Maybe Double -- ^ Output Max (default: 255)
    -> Maybe Double -- ^ Percent of top/bottom values to ignore (default: 2)
    -> m (SimpleWB (PrimState m))
newSimpleWB mbIMin mbIMax mbOMin mbOMax mbP = unsafePrimToPrim $ fromPtr
    [CU.block|Ptr_SimpleWB * {
      cv::Ptr<cv::xphoto::SimpleWB> wbAlg = cv::xphoto::createSimpleWB ();
      wbAlg->setInputMin( $(double  c'varIMin  ));
      wbAlg->setInputMax( $(double  c'varIMax  ));
      wbAlg->setOutputMin($(double  c'varOMin  ));
      wbAlg->setOutputMax($(double  c'varOMax  ));
      wbAlg->setP($(double  c'varP     ));
      return new cv::Ptr<cv::xphoto::SimpleWB>(wbAlg);
    }|]
  where
    c'varIMin  = maybe 0   realToFrac mbIMin
    c'varIMax  = maybe 255 realToFrac mbIMax
    c'varOMin  = maybe 0   realToFrac mbOMin
    c'varOMax  = maybe 255 realToFrac mbOMax
    c'varP     = maybe 2   realToFrac mbP
