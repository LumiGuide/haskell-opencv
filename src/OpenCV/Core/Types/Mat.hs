{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Mat
    ( -- * Matrix
      coerceMat
    , relaxMat
    , typeCheckMat
    , DepthT

    , Mat
    , Depth(..)
    , ToDepth(toDepth)
    , ToDepthDS(toDepthDS)
    , ToChannels, toChannels
    , ToChannelsDS, toChannelsDS
    , ToShape(toShape)
    , ToShapeDS(toShapeDS)
    , emptyMat
    , mkMat
    , mkMatM
    , eyeMat
    , cloneMat
    , matSubRect
    , matCopyTo
    , matCopyToM
    , matConvertTo
    , MatInfo(..)
    , matInfo
      -- * Mutable Matrix
    , MutMat
    , IOMat
    , STMat
    , freeze
    , thaw
    , createMat
    , withMatM
    ) where

import "base" Control.Monad.ST ( RealWorld, ST, runST )
import "base" Data.Int ( Int32 )
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word ( Word8 )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Exception.Internal
import "this" OpenCV.TypeLevel
import "this" OpenCV.Unsafe
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "transformers" Control.Monad.Trans.Except

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

emptyMat :: Mat ('S '[]) ('S 1) ('S Word8)
emptyMat = unsafePerformIO newEmptyMat

-- TODO (RvD): check for negative sizes
-- This crashes OpenCV
mkMat
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> CvExcept (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
mkMat shape channels depth defValue =
    unsafeCvExcept $ newMat shape channels depth defValue

-- TODO (RvD): check for negative sizes
-- This crashes OpenCV
mkMatM
    :: ( PrimMonad m
       , ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> CvExceptT m (MutMat (ShapeT shape) (ChannelsT channels) (DepthT depth) (PrimState m))
mkMatM shape channels depth defValue = do
    mat <- mapExceptT unsafePrimToPrim $ newMat shape channels depth defValue
    unsafeThaw mat

-- | Identity matrix
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#mat-eye OpenCV Sphinx doc>
eyeMat
    :: ( ToInt32    height
       , ToInt32    width
       , ToChannels channels
       , ToDepth    depth
       )
    => height   -- ^
    -> width    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> Mat (ShapeT (height ::: width ::: Z)) (ChannelsT channels) (DepthT depth)
eyeMat height width channels depth = unsafeCoerceMat $ unsafePerformIO $
    fromPtr [CU.exp|Mat * {
      new Mat(Mat::eye( $(int32_t c'height)
                      , $(int32_t c'width)
                      , $(int32_t c'type)
                      ))
    }|]
  where
    c'type = marshalFlags depth' channels'

    c'height  = toInt32    height
    c'width   = toInt32    width
    channels' = toChannels channels
    depth'    = toDepth    depth

cloneMat :: Mat shape channels depth
         -> Mat shape channels depth
cloneMat = unsafePerformIO . cloneMatIO

cloneMatM :: (PrimMonad m)
          => Mat shape channels depth
          -> m (Mat shape channels depth)
cloneMatM = unsafePrimToPrim . cloneMatIO

cloneMatIO :: Mat shape channels depth
           -> IO (Mat shape channels depth)
cloneMatIO mat =
    fmap unsafeCoerceMat $ fromPtr $ withPtr mat $ \matPtr ->
      [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]


{- | Extract a sub region from a 2D-matrix (image)

Example:

@
matSubRectImg :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
matSubRectImg = exceptError $
    withMatM (h ::: 2 * w ::: Z)
             (Proxy :: Proxy 3)
             (Proxy :: Proxy Word8)
             white $ \imgM -> do
      matCopyToM imgM (V2 0 0) birds_512x341 Nothing
      matCopyToM imgM (V2 w 0) subImg        Nothing
      lift $ rectangle imgM subRect blue 1 LineType_4 0
      lift $ rectangle imgM (mkRect (V2 w 0) (V2 w h)) blue 1 LineType_4 0
  where
    subRect = mkRect (V2 96 131) (V2 90 60)
    subImg = exceptError $
               resize (ResizeAbs $ toSize2i (w, h)) InterCubic =<<
               matSubRect birds_512x341 subRect
    [h, w] = miShape $ matInfo birds_512x341
@

<<doc/generated/examples/matSubRectImg.png matSubRectImg>>
-}
matSubRect
    :: Mat ('S [height, width]) channels depth
    -> Rect
    -> CvExcept (Mat ('S ['D, 'D]) channels depth)
matSubRect matIn rect = unsafeWrapException $ do
    matOut <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat matOut) $
      withPtr matIn  $ \matInPtr  ->
      withPtr matOut $ \matOutPtr ->
      withPtr rect   $ \rectPtr   ->
        [cvExceptU|
          *$(Mat * matOutPtr) =
            Mat( *$(Mat * matInPtr)
               , *$(Rect * rectPtr)
               );
        |]

matCopyTo
    :: Mat ('S [dstHeight, dstWidth]) channels depth -- ^
    -> V2 Int32 -- ^
    -> Mat ('S [srcHeight, srcWidth]) channels depth -- ^
    -> Maybe (Mat ('S [srcHeight, srcWidth]) ('S 1) ('S Word8))
    -> CvExcept (Mat ('S [dstHeight, dstWidth]) channels depth)
matCopyTo dst topLeft src mbSrcMask = runST $ do
    dstMut <- thaw dst
    eResult <- runExceptT $ matCopyToM dstMut topLeft src mbSrcMask
    case eResult of
      Left err -> pure $ throwE err
      Right () -> pure <$> unsafeFreeze dstMut

matCopyToM
    :: (PrimMonad m)
    => MutMat ('S [dstHeight, dstWidth]) channels depth (PrimState m) -- ^
    -> V2 Int32 -- ^
    -> Mat ('S [srcHeight, srcWidth]) channels depth -- ^
    -> Maybe (Mat ('S [srcHeight, srcWidth]) ('S 1) ('S Word8))
    -> CvExceptT m ()
matCopyToM dstMut (V2 x y) src mbSrcMask = ExceptT $
    unsafePrimToPrim $ handleCvException (pure ()) $
    withPtr (unMutMat dstMut) $ \dstPtr ->
    withPtr src $ \srcPtr ->
    withPtr mbSrcMask $ \srcMaskPtr ->
      [cvExcept|
        const cv::Mat * const srcPtr = $(const Mat * const srcPtr);
        const int32_t x = $(int32_t x);
        const int32_t y = $(int32_t y);
        cv::Mat * srcMaskPtr = $(Mat * srcMaskPtr);
        srcPtr->copyTo( $(Mat * dstPtr)
                      ->rowRange(y, y + srcPtr->rows)
                       .colRange(x, x + srcPtr->cols)
                      , srcMaskPtr
                        ? cv::_InputArray(*srcMaskPtr)
                        : cv::_InputArray(cv::noArray())
                      );
      |]

          -- Mat * srcPtr = $(Mat * srcPtr);
          -- Mat dstRoi = Mat( *$(Mat * matOutPtr)
          --                 , Rect( *$(Point2i * topLeftPtr)
          --                       , srcPtr->size()
          --                       )
          --                 );
          -- srcPtr->copyTo(dstRoi);

{- | Converts an array to another data type with optional scaling

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html?highlight=convertto#mat-convertto OpenCV Sphinx doc>
-}
matConvertTo
    :: forall shape channels srcDepth dstDepth
     . (ToDepthDS (Proxy dstDepth))
    => Maybe Double -- ^ Optional scale factor.
    -> Maybe Double -- ^ Optional delta added to the scaled values.
    -> Mat shape channels srcDepth
    -> CvExcept (Mat shape channels dstDepth)
matConvertTo alpha beta src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          $(Mat * srcPtr)->
            convertTo( *$(Mat * dstPtr)
                     , $(int32_t c'rtype)
                     , $(double c'alpha)
                     , $(double c'beta)
                     );
        |]
  where
    rtype :: Maybe Depth
    rtype = dsToMaybe $ toDepthDS (Proxy :: Proxy dstDepth)

    c'rtype = maybe (-1) marshalDepth rtype
    c'alpha = maybe 1 realToFrac alpha
    c'beta  = maybe 0 realToFrac beta


--------------------------------------------------------------------------------
-- Mutable Matrix
--------------------------------------------------------------------------------

type IOMat shape channels depth   = MutMat shape channels depth RealWorld
type STMat shape channels depth s = MutMat shape channels depth s

freeze
    :: (PrimMonad m)
    => MutMat shape channels depth (PrimState m) -- ^
    -> m (Mat shape channels depth)
freeze = cloneMatM . unMutMat

thaw
    :: (PrimMonad m)
    => Mat shape channels depth -- ^
    -> m (MutMat shape channels depth (PrimState m))
thaw = fmap MutMat . cloneMatM

createMat
    :: (forall s. CvExceptT (ST s) (MutMat shape channels depth s)) -- ^
    -> CvExcept (Mat shape channels depth)
createMat mk = runCvExceptST $ unsafeFreeze =<< mk

withMatM
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> (  forall s
       .  MutMat (ShapeT shape) (ChannelsT channels) (DepthT depth) (PrimState (ST s))
       -> CvExceptT (ST s) ()
       )
    -> CvExcept (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
withMatM shape channels depth defValue f = createMat $ do
    mutMat <- mkMatM shape channels depth defValue
    f mutMat
    pure mutMat
