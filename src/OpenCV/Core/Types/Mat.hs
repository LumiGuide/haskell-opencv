{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenCV.Core.Types.Mat
    ( -- * Matrix
      Mat
    , MatShape
    , MatChannels
    , MatDepth
    , ToMat(..), FromMat(..)

    , typeCheckMat
    , relaxMat
    , coerceMat

    , emptyMat
    , mkMat
    , eyeMat
    , cloneMat
    , matSubRect
    , matCopyTo
    , matConvertTo

      -- * Mutable Matrix
    , typeCheckMatM
    , relaxMatM
    , coerceMatM

    , freeze
    , thaw
    , mkMatM
    , createMat
    , withMatM
    , cloneMatM
    , matCopyToM

    , zipMatWith

      -- * Meta information
    , MatInfo(..)
    , matInfo

    , Depth(..)

    , ShapeT
    , ChannelsT
    , DepthT

    , ToShape(toShape)
    , ToShapeDS(toShapeDS)
    , ToChannels, toChannels
    , ToChannelsDS, toChannelsDS
    , ToDepth(toDepth)
    , ToDepthDS(toDepthDS)
    ) where

import "base" Control.Monad ( forM, forM_ )
import "base" Control.Monad.ST ( runST )
import "base" Data.Int ( Int32 )
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word ( Word8 )
import "base" Foreign.C.Types ( CDouble )
import "base" Foreign.Marshal.Array ( peekArray, pokeArray )
import "base" Foreign.Ptr ( Ptr, castPtr, plusPtr )
import "base" Foreign.ForeignPtr.Safe ( ForeignPtr, newForeignPtr_ )
import "base" Foreign.Storable ( Storable )
import "base" GHC.Exts ( Constraint(..) )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.Vector ( zero )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import "vector" Data.Vector.Storable ( Vector, toList, unsafeFromForeignPtr )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types.Rect ( Rect2i )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Core.Types.Mat.ToFrom
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Class ( lift )
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
      lift $ rectangle imgM (toRect $ HRect (V2 w 0) (V2 w h) :: Rect2i) blue 1 LineType_4 0
  where
    subRect = toRect $ HRect (V2 96 131) (V2 90 60)
    subImg = exceptError $
               resize (ResizeAbs $ toSize $ V2 w h) InterCubic =<<
               matSubRect birds_512x341 subRect
    [h, w] = miShape $ matInfo birds_512x341
@

<<doc/generated/examples/matSubRectImg.png matSubRectImg>>
-}
matSubRect
    :: Mat ('S [height, width]) channels depth
    -> Rect2i
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
               , *$(Rect2i * rectPtr)
               );
        |]

matCopyTo
    :: Mat ('S [dstHeight, dstWidth]) channels depth -- ^
    -> V2 Int32 -- ^
    -> Mat ('S [srcHeight, srcWidth]) channels depth -- ^
    -> Maybe (Mat ('S [srcHeight, srcWidth]) ('S 1) ('S Word8))
    -> CvExcept (Mat ('S [dstHeight, dstWidth]) channels depth)
matCopyTo dst topLeft src mbSrcMask = runST $ do
    dstM <- thaw dst
    eResult <- runExceptT $ matCopyToM dstM topLeft src mbSrcMask
    case eResult of
      Left err -> pure $ throwE err
      Right () -> pure <$> unsafeFreeze dstM


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

matCopyToM
    :: (PrimMonad m)
    => Mut (Mat ('S [dstHeight, dstWidth]) channels depth) (PrimState m) -- ^
    -> V2 Int32 -- ^
    -> Mat ('S [srcHeight, srcWidth]) channels depth -- ^
    -> Maybe (Mat ('S [srcHeight, srcWidth]) ('S 1) ('S Word8))
    -> CvExceptT m ()
matCopyToM dstM (V2 x y) src mbSrcMask = ExceptT $
    unsafePrimToPrim $ handleCvException (pure ()) $
    withPtr dstM $ \dstPtr ->
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


-- |Zips a given list of matrices of equal shape, channels, and depth,
-- by applying the given function to the corresponding matrix elements
-- at each position.
zipMatWith :: forall (shape :: [DS Nat]) (channels :: Nat) (depth :: *)
         . ( Storable depth, All IsStatic shape )
        => [Mat ('S shape) ('S channels) ('S depth)]
        -> ([Vector depth] -> Vector depth)
        -> Maybe (Mat ('S shape) ('S channels) ('S depth))
zipMatWith []   _ = Nothing
zipMatWith mats f = unsafePerformIO . exceptErrorIO . (Just <$>) $ do
    resultMat <- newMat shape numChannels depth (toScalar (zero :: V4 CDouble))
    lift $ withMatData resultMat $ \newStep newPtr ->
        go (fromIntegral <$> newStep) (castPtr newPtr)
    return $ unsafeCoerceMat resultMat
  where
    MatInfo shape !depth !numChannels = matInfo (head mats)

    positions :: [[Int]]
    positions = mapM (enumFromTo 0 . pred) (fromIntegral <$> shape)

    stepsAndPtrs :: [([Int], ForeignPtr depth)]
    stepsAndPtrs = unsafePerformIO $ forM mats $ \mat ->
        withMatData mat $ \step ptr -> do
            fptr <- newForeignPtr_ . castPtr $ ptr
            return (fromIntegral <$> step, fptr)

    go :: [Int] -> Ptr depth -> IO ()
    go newStep newPtr =
        forM_ positions $ \pos -> do
            pixels <- forM stepsAndPtrs $ \(step, dataPtr) -> do
                let offs = sum $ zipWith (*) step pos
                return $ unsafeFromForeignPtr dataPtr offs (fromIntegral numChannels)
            pokeArray (dest pos) (toList $ f pixels)
        where
          dest :: [Int] -> Ptr depth
          dest pos = matElemAddress (castPtr newPtr) newStep pos


class All (p :: k -> Constraint) (xs :: [k])
instance All p '[]
instance (p x, All p xs) => All p (x ': xs)

class IsStatic (ds :: DS a)
instance IsStatic ('S a)
