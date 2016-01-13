{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.Types.Mat
    ( -- * Matrix
      Mat
    , MatDepth(..)
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

import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
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

emptyMat :: Mat
emptyMat = unsafePerformIO newEmptyMat

mkMat
    :: (ToScalar scalar)
    => V.Vector Int32 -- ^ Vector of sizes
    -> MatDepth
    -> Int32        -- ^ Number of channels
    -> scalar       -- ^ Default element value
    -> Mat
mkMat sizes matDepth cn defValue = unsafePerformIO $ newMat sizes matDepth cn defValue

mkMatM
    :: (PrimMonad m, ToScalar scalar)
    => V.Vector Int32 -- ^ Vector of sizes
    -> MatDepth
    -> Int32        -- ^ Number of channels
    -> scalar       -- ^ Default element value
    -> m (MutMat (PrimState m))
mkMatM sizes matDepth cn defValue = unsafeThaw $ mkMat sizes matDepth cn defValue

-- | Identity matrix
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html#mat-eye OpenCV Sphinx doc>
eyeMat :: Int32 -> Int32 -> MatDepth -> Int32 -> Mat
eyeMat rows cols depth channels = unsafePerformIO $
    matFromPtr [CU.exp|Mat * {
      new Mat(Mat::eye( $(int32_t rows)
                      , $(int32_t cols)
                      , $(int32_t c'type)
                      ))
    }|]
  where
    c'type = marshalFlags depth channels

cloneMat :: Mat -> Mat
cloneMat = unsafePerformIO . cloneMatIO

cloneMatM :: (PrimMonad m) => Mat -> m Mat
cloneMatM = unsafePrimToPrim . cloneMatIO

cloneMatIO :: Mat -> IO Mat
cloneMatIO mat = matFromPtr $ withMatPtr mat $ \matPtr ->
    [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]


{- | Extract a sub region from a 2D-matrix (image)

Example:

@
matSubRectImg :: 'Mat'
matSubRectImg = 'createMat' $ do
    imgM <- 'mkMatM' ('V.fromList' [h, 2 * w]) 'MatDepth_8U' 3 white
    'void' $ 'matCopyToM' imgM (V2 0 0) birds_512x341
    'void' $ 'matCopyToM' imgM (V2 w 0) subImg
    rectangle imgM subRect blue 1 LineType_4 0
    rectangle imgM (mkRect (V2 w 0) (V2 w h)) blue 1 LineType_4 0
    pure imgM
  where
    subRect = mkRect (V2 96 131) (V2 90 60)
    subImg = 'either' 'throw' 'id' $
               resize (ResizeAbs $ 'toSize2i' (w, h)) InterCubic =<<
               'matSubRect' birds_512x341 subRect
    [h, w] = 'miShape' $ 'matInfo' birds_512x341
@

<<doc/generated/matSubRectImg.png matSubRectImg>>
-}
matSubRect :: Mat -> Rect -> Either CvException Mat
matSubRect matIn rect = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException (pure matOut) $
      withMatPtr matIn $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
      withRectPtr rect $ \rectPtr ->
        [cvExceptU|
          *$(Mat * matOutPtr) =
            Mat( *$(Mat * matInPtr)
               , *$(Rect * rectPtr)
               );
        |]

matCopyTo :: Mat -> V2 Int32 -> Mat -> Either CvException Mat
matCopyTo dst topLeft src = runST $ do
    dstMut <- thaw dst
    eResult <- matCopyToM dstMut topLeft src
    case eResult of
      Left err -> pure $ Left err
      Right () -> Right <$> unsafeFreeze dstMut

matCopyToM
    :: (PrimMonad m)
    => MutMat (PrimState m)
    -> V2 Int32
    -> Mat
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

-- | Converts an array to another data type with optional scaling
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/basic_structures.html?highlight=convertto#mat-convertto OpenCV Sphinx doc>
matConvertTo
    :: Maybe MatDepth
    -> Maybe Double -- ^ Optional scale factor.
    -> Maybe Double -- ^ Optional delta added to the scaled values.
    -> Mat
    -> Either CvException Mat
matConvertTo rtype alpha beta src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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
    c'rtype = maybe (-1) marshalMatDepth rtype
    c'alpha = maybe 1 realToFrac alpha
    c'beta  = maybe 0 realToFrac beta

data MatInfo
   = MatInfo
     { miShape    :: ![Int32]
     , miDepth    :: !MatDepth
     , miChannels :: !Int32
     }
     deriving (Show, Eq)

matInfo :: Mat -> MatInfo
matInfo mat = unsafePerformIO $
    withMatPtr mat $ \matPtr ->
    alloca $ \(flagsPtr :: Ptr Int32) ->
    alloca $ \(dimsPtr  :: Ptr Int32) ->
    alloca $ \(sizePtr  :: Ptr (Ptr Int32)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const flagsPtr) = matPtr->flags;
        *$(int32_t *   const dimsPtr ) = matPtr->dims;
        *$(int32_t * * const sizePtr ) = matPtr->size.p;
      }|]
      (depth, channels) <- unmarshalFlags <$> peek flagsPtr
      dims <- peek dimsPtr
      size <- peek sizePtr
      shape <- peekArray (fromIntegral dims) size
      pure MatInfo
           { miShape    = shape
           , miDepth    = depth
           , miChannels = channels
           }

--------------------------------------------------------------------------------
-- Mutable Matrix
--------------------------------------------------------------------------------

type IOMat   = MutMat RealWorld
type STMat s = MutMat s

freeze :: (PrimMonad m) => MutMat (PrimState m) -> m Mat
freeze = cloneMatM . unMutMat

thaw :: (PrimMonad m) => Mat -> m (MutMat (PrimState m))
thaw = fmap MutMat . cloneMatM

createMat :: (forall s. ST s (MutMat s)) -> Mat
createMat mk = runST $ unsafeFreeze =<< mk
