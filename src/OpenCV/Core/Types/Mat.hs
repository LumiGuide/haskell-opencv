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
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Exception
import "this" OpenCV.TypeLevel
import "this" OpenCV.Unsafe
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

emptyMat :: Mat ('S '[]) ('S 1) ('S Word8)
emptyMat = unsafePerformIO newEmptyMat

mkMat
    :: ( Convert shape    (V.Vector Int32)
       , Convert channels Int32
       , Convert depth    Depth
       , Convert scalar   Scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> Mat (ShapeT shape) (ChannelsT channels) (DepthT depth)
mkMat shape channels depth defValue =
    unsafePerformIO $ newMat shape channels depth defValue

mkMatM
    :: ( PrimMonad m
       , Convert shape    (V.Vector Int32)
       , Convert channels Int32
       , Convert depth    Depth
       , Convert scalar   Scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> m (MutMat (ShapeT shape) (ChannelsT channels) (DepthT depth) (PrimState m))
mkMatM shape channels depth defValue = do
    mat <- unsafePrimToPrim $ newMat shape channels depth defValue
    unsafeThaw mat

-- | Identity matrix
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#mat-eye OpenCV Sphinx doc>
eyeMat
    :: ( Convert height   Int32
       , Convert width    Int32
       , Convert channels Int32
       , Convert depth    Depth
       )
    => height   -- ^
    -> width    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> Mat (ShapeT (height ::: width ::: Z)) (ChannelsT channels) (DepthT depth)
eyeMat height width channels depth = unsafeCoerceMat $ unsafePerformIO $
    matFromPtr [CU.exp|Mat * {
      new Mat(Mat::eye( $(int32_t c'height)
                      , $(int32_t c'width)
                      , $(int32_t c'type)
                      ))
    }|]
  where
    c'type = marshalFlags depth' channels'

    c'height  = convert height
    c'width   = convert width
    channels' = convert channels
    depth'    = convert depth

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
    fmap unsafeCoerceMat $ matFromPtr $ withMatPtr mat $ \matPtr ->
      [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]


{- | Extract a sub region from a 2D-matrix (image)

Example:

@
matSubRectImg :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
matSubRectImg = createMat $ do
    imgM <- mkMatM (h ::: 2 * w ::: Z) (Proxy :: Proxy 3) (Proxy :: Proxy Word8) white
    void $ matCopyToM imgM (V2 0 0) birds_512x341
    void $ matCopyToM imgM (V2 w 0) subImg
    rectangle imgM subRect blue 1 LineType_4 0
    rectangle imgM (mkRect (V2 w 0) (V2 w h)) blue 1 LineType_4 0
    pure imgM
  where
    subRect = mkRect (V2 96 131) (V2 90 60)
    subImg = either throw id $
               resize (ResizeAbs $ convert (w, h)) InterCubic =<<
               matSubRect birds_512x341 subRect
    [h, w] = miShape $ matInfo birds_512x341
@

<<doc/generated/examples/matSubRectImg.png matSubRectImg>>
-}
matSubRect
    :: Mat ('S [height, width]) channels depth
    -> Rect
    -> Either CvException (Mat ('S ['D, 'D]) channels depth)
matSubRect matIn rect = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat matOut) $
      withMatPtr matIn $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
      withPtr rect $ \rectPtr ->
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
    -> Either CvException (Mat ('S [dstHeight, dstWidth]) channels depth)
matCopyTo dst topLeft src = runST $ do
    dstMut <- thaw dst
    eResult <- matCopyToM dstMut topLeft src
    case eResult of
      Left err -> pure $ Left err
      Right () -> Right <$> unsafeFreeze dstMut

matCopyToM
    :: (PrimMonad m)
    => MutMat ('S [dstHeight, dstWidth]) channels depth (PrimState m) -- ^
    -> V2 Int32 -- ^
    -> Mat ('S [srcHeight, srcWidth]) channels depth -- ^
    -> m (Either CvException ())
matCopyToM dstMut (V2 x y) src =
    unsafePrimToPrim $ handleCvException (pure ()) $
    withMatPtr (unMutMat dstMut) $ \dstPtr ->
    withMatPtr src $ \srcPtr ->
      [cvExcept|
        const Mat * const srcPtr = $(const Mat * const srcPtr);
        const int32_t x = $(int32_t x);
        const int32_t y = $(int32_t y);
        srcPtr->copyTo( $(Mat * dstPtr)
                      ->rowRange(y, y + srcPtr->rows)
                       .colRange(x, x + srcPtr->cols)
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
     . (Convert (Proxy dstDepth) (DS Depth))
    => Maybe Double -- ^ Optional scale factor.
    -> Maybe Double -- ^ Optional delta added to the scaled values.
    -> Mat shape channels srcDepth
    -> Either CvException (Mat shape channels dstDepth)
matConvertTo alpha beta src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
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
    rtype = dsToMaybe $ convert (Proxy :: Proxy dstDepth)

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
    :: (forall s. ST s (MutMat shape channels depth s)) -- ^
    -> Mat shape channels depth
createMat mk = runST $ unsafeFreeze =<< mk
