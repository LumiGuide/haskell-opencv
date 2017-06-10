{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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

    , matFromFunc

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

    , All
    , IsStatic
    , foldMat

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
import "base" Data.List ( foldl' )
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word ( Word8 )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Ptr ( Ptr, castPtr, plusPtr )
import "base" Foreign.Storable ( Storable )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V4 ( V4(..) )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types.Rect ( Rect2i )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Core.Types.Mat.ToFrom
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel
import "this" OpenCV.Unsafe ( unsafeWrite )
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as DV

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
             white $ \\imgM -> do
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

{- | Create a matrix whose elements are defined by a function.

Example:

@
matFromFuncImg
  :: forall size. (size ~ 300)
  => Mat (ShapeT [size, size]) ('S 4) ('S Word8)
matFromFuncImg = exceptError $
    matFromFunc
      (Proxy :: Proxy [size, size])
      (Proxy :: Proxy 4)
      (Proxy :: Proxy Word8)
      example
  where
    example [y, x] 0 = 255 - normDist (V2 x y ^-^ bluePt )
    example [y, x] 1 = 255 - normDist (V2 x y ^-^ greenPt)
    example [y, x] 2 = 255 - normDist (V2 x y ^-^ redPt  )
    example [y, x] 3 =       normDist (V2 x y ^-^ alphaPt)
    example _pos _channel = error "impossible"

    normDist :: V2 Int -> Word8
    normDist v = floor $ min 255 $ 255 * Linear.norm (fromIntegral \<$> v) / s'

    bluePt  = V2 0 0
    greenPt = V2 s s
    redPt   = V2 s 0
    alphaPt = V2 0 s

    s = fromInteger $ natVal (Proxy :: Proxy size) :: Int
    s' = fromIntegral s :: Double
@

<<doc/generated/examples/matFromFuncImg.png matFromFuncImg>>
-}
matFromFunc
    :: forall shape channels depth
     . ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , Storable   (StaticDepthT depth)
       )
    => shape
    -> channels
    -> depth
    -> ([Int] -> Int -> StaticDepthT depth) -- ^
    -> CvExcept (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
matFromFunc shape channels depth func =
    withMatM shape channels depth (0 :: V4 Double) $ \matM ->
      forM_ positions $ \pos ->
        forM_ [0 .. fromIntegral channels' - 1] $ \channel ->
           unsafeWrite matM pos channel $ func pos channel
  where
    positions :: [[Int]]
    positions = dimPositions $ V.toList $ V.map fromIntegral shapeVec

    shapeVec :: V.Vector Int32
    shapeVec = toShape shape

    channels' :: Int32
    channels' = toChannels channels

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


-- |Transforms a given list of matrices of equal shape, channels, and depth,
-- by folding the given function over all matrix elements at each position.
foldMat :: forall (shape :: [DS Nat]) (channels :: Nat) (depth :: *) a
         . ( Storable depth
           , Storable a
           , All IsStatic shape
           )
        => (a -> DV.Vector depth -> a) -- ^
        -> a
        -> [Mat ('S shape) ('S channels) ('S depth)]
        -> Maybe (DV.Vector a)
foldMat _ _ []   = Nothing
foldMat f z mats = Just . DV.fromList . unsafePerformIO $ mapM go (dimPositions shape)
  where
    go :: [Int32] -> IO a
    go pos = pixelsAt pos >>= return . foldl' f z

    MatInfo !shape _ !channels = matInfo (head mats)

    stepsAndPtrs :: IO [([Int32], Ptr depth)]
    stepsAndPtrs = forM mats $ \mat ->
        withMatData mat $ \step ptr ->
            return (fromIntegral <$> step, castPtr ptr)

    pixelsAt :: [Int32] -> IO [DV.Vector depth]
    pixelsAt pos = mapM go' =<< stepsAndPtrs
      where
        go' :: ([Int32], Ptr depth) -> IO (DV.Vector depth)
        go' (step, dataPtr) = do
            let !offset = fromIntegral . sum $ zipWith (*) step pos
            vals <- peekArray (fromIntegral channels) (dataPtr `plusPtr` offset)
            return $ DV.fromList vals
