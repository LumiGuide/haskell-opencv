{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

{- | Operations on arrays
-}
module OpenCV.Core.ArrayOps
    ( -- * Per element operations
      -- $per_element_intro
      matScalarAdd
    , matScalarMult
    , matAbs
    , matAbsDiff
    , matAdd
    , matSubtract
    , matAddWeighted
    , matScaleAdd
    , matMax
      -- ** Bitwise operations
      -- $bitwise_intro
    , bitwiseNot
    , bitwiseAnd
    , bitwiseOr
    , bitwiseXor
      -- * Channel operations
    , matMerge
    , matSplit
    , matChannelMapM
      -- * Other
    , minMaxLoc
    , NormType(..)
    , NormAbsRel(..)
    , norm
    , normDiff
    , normalize
    , matSum
    , matSumM
    , meanStdDev
    , perspectiveTransform
    ) where

import "base" Data.Int ( Int32 )
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( allocaArray, peekArray )
import "base" Foreign.Ptr ( Ptr, castPtr )
import "base" Foreign.Storable ( Storable(..), peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.Vector ( zero )
import "linear" Linear.V2 ( V2(..) )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.ArrayOps
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Per element operations
--------------------------------------------------------------------------------

{- $per_element_intro

The following functions work on the individual elements of matrices.

Examples are based on the following two images:

<<doc/generated/flower_512x341.png Flower>>
<<doc/generated/sailboat_512x341.png Sailboat>>
-}

matScalarAdd
    :: (ToScalar scalar)
    => Mat shape channels depth -- ^
    -> scalar
    -> Mat shape channels depth
matScalarAdd src x = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr (toScalar x) $ \xPtr ->
      withPtr dst $ \dstPtr ->
        withPtr src $ \srcPtr ->
          [C.block| void {
            *$(Mat * dstPtr) = *$(Mat * srcPtr) + *$(Scalar * xPtr);
          }|]
    pure $ unsafeCoerceMat dst

matScalarMult
    :: Mat shape channels depth -- ^
    -> Double
    -> Mat shape channels depth
matScalarMult src x = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr dst $ \dstPtr ->
      withPtr src $ \srcPtr ->
        [C.block| void {
          *$(Mat * dstPtr) = *$(Mat * srcPtr) * $(double c'x);
        }|]
    pure $ unsafeCoerceMat dst
  where
    c'x = realToFrac x

{- | Calculates an absolute value of each matrix element.

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#abs OpenCV Sphinx doc>
-}
matAbs
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
matAbs src = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr dst $ \dstPtr ->
      withPtr src $ \srcPtr ->
        [C.block| void {
          *$(Mat * dstPtr) = cv::abs(*$(Mat * srcPtr));
        }|]
    pure $ unsafeCoerceMat dst

{- | Calculates the per-element absolute difference between two arrays.

Example:

@
matAbsDiffImg :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
matAbsDiffImg = matAbsDiff flower_512x341 sailboat_512x341
@

<<doc/generated/examples/matAbsDiffImg.png matAbsDiffImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#absdiff OpenCV Sphinx doc>
-}
matAbsDiff
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Mat shape channels depth
matAbsDiff src1 src2 = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr dst $ \dstPtr ->
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
        [C.block| void {
          cv::absdiff( *$(Mat * src1Ptr)
                     , *$(Mat * src2Ptr)
                     , *$(Mat * dstPtr )
                     );
        }|]
    pure $ unsafeCoerceMat dst

{- | Calculates the per-element sum of two arrays.

Example:

@
matAddImg :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
matAddImg = matAdd flower_512x341 sailboat_512x341
@

<<doc/generated/examples/matAddImg.png matAddImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#add OpenCV Sphinx doc>
-}
-- TODO (RvD): handle different depths
matAdd
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Mat shape channels depth
matAdd src1 src2 = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr dst $ \dstPtr ->
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
        [C.block| void {
          cv::add
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        }|]
    pure $ unsafeCoerceMat dst

{- | Calculates the per-element difference between two arrays

Example:

@
matSubtractImg :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
matSubtractImg = matSubtract flower_512x341 sailboat_512x341
@

<<doc/generated/examples/matSubtractImg.png matSubtractImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#subtract OpenCV Sphinx doc>
-}
-- TODO (RvD): handle different depths
matSubtract
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Mat shape channels depth
matSubtract src1 src2 = unsafePerformIO $ do
    dst <- newEmptyMat
    withPtr dst $ \dstPtr ->
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
        [C.block| void {
          cv::subtract
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        }|]
    pure $ unsafeCoerceMat dst

{- | Calculates the weighted sum of two arrays

Example:

@
matAddWeightedImg :: Mat (ShapeT [341, 512]) ('S 3) ('S Word8)
matAddWeightedImg = exceptError $
    matAddWeighted flower_512x341 0.5 sailboat_512x341 0.5 0.0
@

<<doc/generated/examples/matAddWeightedImg.png matAddWeightedImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#addweighted OpenCV Sphinx doc>
-}

-- TODO (RvD): handle different depths
matAddWeighted
    :: forall shape channels srcDepth dstDepth
     . (ToDepthDS (Proxy dstDepth))
    => Mat shape channels srcDepth -- ^ src1
    -> Double -- ^ alpha
    -> Mat shape channels srcDepth -- ^ src2
    -> Double -- ^ beta
    -> Double -- ^ gamma
    -> CvExcept (Mat shape channels dstDepth)
matAddWeighted src1 alpha src2 beta gamma = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
      withPtr dst $ \dstPtr ->
      [cvExcept|
        cv::addWeighted
          ( *$(Mat * src1Ptr)
          , $(double c'alpha)
          , *$(Mat * src2Ptr)
          , $(double c'beta)
          , $(double c'gamma)
          , *$(Mat * dstPtr)
          , $(int32_t c'dtype)
          );
      |]
  where
    c'alpha = realToFrac alpha
    c'beta  = realToFrac beta
    c'gamma = realToFrac gamma
    c'dtype = maybe (-1) marshalDepth $ dsToMaybe $ toDepthDS (Proxy :: Proxy dstDepth)

{- | Calculates the sum of a scaled array and another array.

The function scaleAdd is one of the classical primitive linear algebra
operations, known as DAXPY or SAXPY in BLAS. It calculates the sum of a scaled
array and another array.

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#scaleadd OpenCV Sphinx doc>
-}
matScaleAdd
    :: Mat shape channels depth
       -- ^ First input array.
    -> Double
       -- ^ Scale factor for the first array.
    -> Mat shape channels depth
       -- ^ Second input array.
    -> CvExcept (Mat shape channels depth)
matScaleAdd src1 scale src2 = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
      withPtr dst  $ \dstPtr  ->
      [cvExcept|
        cv::scaleAdd
          ( *$(Mat * src1Ptr)
          , $(double c'scale)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          );
      |]
  where
    c'scale = realToFrac scale

matMax
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> CvExcept (Mat shape channels depth)
matMax src1 src2 = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr dst $ \dstPtr ->
      withPtr src1 $ \src1Ptr ->
      withPtr src2 $ \src2Ptr ->
        [cvExcept|
          cv::max
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          );
        |]

--------------------------------------------------------------------------------
-- Per element bitwise operations
--------------------------------------------------------------------------------

{- $bitwise_intro

The examples for the bitwise operations make use of the following images:

Example:

@
type VennShape = [200, 320]

vennCircleAImg :: Mat (ShapeT VennShape) ('S 1) ('S Word8)
vennCircleAImg = exceptError $
    withMatM
      (Proxy :: Proxy VennShape)
      (Proxy :: Proxy 1)
      (Proxy :: Proxy Word8)
      black $ \imgM -> lift $ vennCircleA imgM white (-1)

vennCircleBImg :: Mat (ShapeT VennShape) ('S 1) ('S Word8)
vennCircleBImg = exceptError $
    withMatM
      (Proxy :: Proxy VennShape)
      (Proxy :: Proxy 1)
      (Proxy :: Proxy Word8)
      black $ \imgM -> lift $ vennCircleB imgM white (-1)
@

<<doc/generated/examples/vennCircleAImg.png vennCircleAImg>>
<<doc/generated/examples/vennCircleBImg.png vennCircleBImg>>
-}

{- |

Example:

@
bitwiseNotImg :: Mat (ShapeT VennShape) ('S 3) ('S Word8)
bitwiseNotImg = exceptError $ do
    img <- bitwiseNot vennCircleAImg
    imgBgr <- cvtColor gray bgr img
    createMat $ do
      imgM <- lift $ thaw imgBgr
      lift $ vennCircleA imgM blue 2
      pure imgM
@

<<doc/generated/examples/bitwiseNotImg.png bitwiseNotImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#bitwise-not OpenCV Sphinx doc>
-}
bitwiseNot
    :: Mat shape channels depth -- ^
    -> CvExcept (Mat shape channels depth)
bitwiseNot src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src    $ \srcPtr ->
      withPtr dst    $ \dstPtr ->
        [cvExcept|
          cv::bitwise_not
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        |]

{- |

Example:

@
bitwiseAndImg :: Mat (ShapeT VennShape) ('S 3) ('S Word8)
bitwiseAndImg = exceptError $ do
    img <- bitwiseAnd vennCircleAImg vennCircleBImg
    imgBgr <- cvtColor gray bgr img
    createMat $ do
      imgM <- lift $ thaw imgBgr
      lift $ vennCircleA imgM blue 2
      lift $ vennCircleB imgM red  2
      pure imgM
@

<<doc/generated/examples/bitwiseAndImg.png bitwiseAndImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#bitwise-and OpenCV Sphinx doc>
-}
bitwiseAnd
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> CvExcept (Mat shape channels depth)
bitwiseAnd src1 src2 = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src1   $ \src1Ptr ->
      withPtr src2   $ \src2Ptr ->
      withPtr dst    $ \dstPtr  ->
        [cvExcept|
          cv::bitwise_and
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        |]

{- |

Example:

@
bitwiseOrImg :: Mat (ShapeT VennShape) ('S 3) ('S Word8)
bitwiseOrImg = exceptError $ do
    img <- bitwiseOr vennCircleAImg vennCircleBImg
    imgBgr <- cvtColor gray bgr img
    createMat $ do
      imgM <- lift $ thaw imgBgr
      lift $ vennCircleA imgM blue 2
      lift $ vennCircleB imgM red  2
      pure imgM
@

<<doc/generated/examples/bitwiseOrImg.png bitwiseOrImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#bitwise-or OpenCV Sphinx doc>
-}
bitwiseOr
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> CvExcept (Mat shape channels depth)
bitwiseOr src1 src2 = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src1   $ \src1Ptr ->
      withPtr src2   $ \src2Ptr ->
      withPtr dst    $ \dstPtr  ->
        [cvExcept|
          cv::bitwise_or
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        |]

{- |

Example:

@
bitwiseXorImg :: Mat (ShapeT VennShape) ('S 3) ('S Word8)
bitwiseXorImg = exceptError $ do
    img <- bitwiseXor vennCircleAImg vennCircleBImg
    imgBgr <- cvtColor gray bgr img
    createMat $ do
      imgM <- lift $ thaw imgBgr
      lift $ vennCircleA imgM blue 2
      lift $ vennCircleB imgM red  2
      pure imgM
@

<<doc/generated/examples/bitwiseXorImg.png bitwiseXorImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#bitwise-xor OpenCV Sphinx doc>
-}
bitwiseXor
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> CvExcept (Mat shape channels depth)
bitwiseXor src1 src2 = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src1   $ \src1Ptr ->
      withPtr src2   $ \src2Ptr ->
      withPtr dst    $ \dstPtr  ->
        [cvExcept|
          cv::bitwise_xor
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , cv::noArray()
          );
        |]

{- | Creates one multichannel array out of several single-channel ones.

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#merge OpenCV Sphinx doc>
-}
matMerge
    :: V.Vector (Mat shape ('S 1) depth) -- ^
    -> Mat shape 'D depth
matMerge srcVec = unsafePerformIO $ do
    dst <- newEmptyMat
    withArrayPtr srcVec $ \srcVecPtr ->
      withPtr dst $ \dstPtr ->
        [C.block| void {
          cv::merge
          ( $(Mat * srcVecPtr)
          , $(size_t c'srcVecLength)
          , *$(Mat * dstPtr)
          );
        }|]
    pure $ unsafeCoerceMat dst
  where
    c'srcVecLength = fromIntegral $ V.length srcVec

{- | Divides a multi-channel array into several single-channel arrays.

Example:

@
matSplitImg
    :: forall (width    :: Nat)
              (width3   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width3 ~ ((*) width 3)
       )
    => Mat (ShapeT [height, width3]) ('S channels) ('S depth)
matSplitImg = exceptError $ do
    zeroImg <- mkMat (Proxy :: Proxy [height, width])
                     (Proxy :: Proxy 1)
                     (Proxy :: Proxy depth)
                     black
    let blueImg  = matMerge $ V.fromList [channelImgs V.! 0, zeroImg, zeroImg]
        greenImg = matMerge $ V.fromList [zeroImg, channelImgs V.! 1, zeroImg]
        redImg   = matMerge $ V.fromList [zeroImg, zeroImg, channelImgs V.! 2]

    withMatM (Proxy :: Proxy [height, width3])
             (Proxy :: Proxy channels)
             (Proxy :: Proxy depth)
             white $ \imgM -> do
      matCopyToM imgM (V2 (w*0) 0) (unsafeCoerceMat blueImg)  Nothing
      matCopyToM imgM (V2 (w*1) 0) (unsafeCoerceMat greenImg) Nothing
      matCopyToM imgM (V2 (w*2) 0) (unsafeCoerceMat redImg)   Nothing
  where
    channelImgs = matSplit birds_512x341

    w :: Int32
    w = fromInteger $ natVal (Proxy :: Proxy width)
@

<<doc/generated/examples/matSplitImg.png matSplitImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#split OpenCV Sphinx doc>
-}
matSplit
    :: Mat shape channels depth -- ^
    -> V.Vector (Mat shape ('S 1) depth)
matSplit src = unsafePerformIO $
    withPtr src $ \srcPtr ->
    allocaArray numChans $ \(splitsArray :: Ptr (Ptr C'Mat)) -> do
      [C.block| void {
        cv::Mat * srcPtr = $(Mat * srcPtr);
        int32_t numChans = $(int32_t c'numChans);
        cv::Mat *splits = new cv::Mat[numChans];
        cv::split(*srcPtr, splits);
        for(int i = 0; i < numChans; i++){
          $(Mat * * splitsArray)[i] = new cv::Mat(splits[i]);
        }
        delete [] splits;
      }|]
      fmap V.fromList . mapM (fromPtr . pure) =<< peekArray numChans splitsArray
  where
    numChans = fromIntegral $ miChannels $ matInfo src

    c'numChans :: Int32
    c'numChans = fromIntegral numChans

{- | Apply the same 1 dimensional action to every channel
-}
matChannelMapM
   :: Monad m
   => (Mat shape ('S 1) depth -> m (Mat shape ('S 1) depth))
   -> Mat shape channelsOut depth
   -> m (Mat shape channelsOut depth)
matChannelMapM f img = unsafeCoerceMat . matMerge <$> V.mapM f (matSplit img)

{- | Finds the global minimum and maximum in an array

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#minmaxloc OpenCV Sphinx doc>
-}
-- TODO (RvD): implement mask
minMaxLoc
    :: Mat ('S [height, width]) channels depth -- ^
    -> CvExcept (Double, Double, Point2i, Point2i)
minMaxLoc src = unsafeWrapException $ do
    minLoc <- toPointIO $ V2 0 0
    maxLoc <- toPointIO $ V2 0 0
    withPtr src $ \srcPtr ->
      withPtr minLoc $ \minLocPtr ->
      withPtr maxLoc $ \maxLocPtr ->
      alloca $ \minValPtr ->
      alloca $ \maxValPtr -> do
        handleCvException
          ( (,, minLoc, maxLoc)
            <$> (realToFrac <$> peek minValPtr)
            <*> (realToFrac <$> peek maxValPtr)
          )
          [cvExcept|
            cv::minMaxLoc( *$(Mat * srcPtr)
                         , $(double * minValPtr)
                         , $(double * maxValPtr)
                         , $(Point2i * minLocPtr)
                         , $(Point2i * maxLocPtr)
                         );
          |]

{- | Calculates an absolute array norm

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
-}
norm
    :: NormType
    -> Maybe (Mat shape ('S 1) ('S Word8))
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'Depth_8U' and 1 channel.
    -> Mat shape channels depth -- ^ Input array.
    -> CvExcept Double  -- ^ Calculated norm.
norm normType mbMask src = unsafeWrapException $
    withPtr src    $ \srcPtr  ->
    withPtr mbMask $ \mskPtr  ->
    alloca         $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * srcPtr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(cv::noArray())
                  );
      |]
  where
    c'normType = marshalNormType NormAbsolute normType

{- | Calculates an absolute difference norm, or a relative difference norm

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
-}
normDiff
    :: NormAbsRel -- ^ Absolute or relative norm.
    -> NormType
    -> Maybe (Mat shape ('S 1) ('S Word8))
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'Depth_8U' and 1 channel.
    -> Mat shape channels depth -- ^ First input array.
    -> Mat shape channels depth -- ^ Second input array of the same size and type as the first.
    -> CvExcept Double -- ^ Calculated norm.
normDiff absRel normType mbMask src1 src2 = unsafeWrapException $
    withPtr src1   $ \src1Ptr ->
    withPtr src2   $ \src2Ptr ->
    withPtr mbMask $ \mskPtr  ->
    alloca         $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * src1Ptr)
                  , *$(Mat * src2Ptr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(cv::noArray())
                  );
      |]
  where
    c'normType = marshalNormType absRel normType

{- | Normalizes the norm or value range of an array

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#normalize OpenCV Sphinx doc>
-}
normalize
    :: forall shape channels srcDepth dstDepth
     . (ToDepthDS (Proxy dstDepth))
    => Double
       -- ^ Norm value to normalize to or the lower range boundary in case of
       -- the range normalization.
    -> Double
       -- ^ Upper range boundary in case of the range normalization; it is not
       -- used for the norm normalization.
    -> NormType
    -> Maybe (Mat shape ('S 1) ('S Word8)) -- ^ Optional operation mask.
    -> Mat shape channels srcDepth -- ^ Input array.
    -> CvExcept (Mat shape channels dstDepth)
normalize alpha beta normType mbMask src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src    $ \srcPtr ->
      withPtr dst    $ \dstPtr ->
      withPtr mbMask $ \mskPtr ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::normalize( *$(Mat * srcPtr)
                       , *$(Mat * dstPtr)
                       , $(double c'alpha)
                       , $(double c'beta)
                       , $(int32_t c'normType)
                       , $(int32_t c'dtype)
                       , mskPtr ? _InputArray(*mskPtr) : _InputArray(cv::noArray())
                       );
        |]
  where
    c'alpha    = realToFrac alpha
    c'beta     = realToFrac beta
    c'normType = marshalNormType NormAbsolute normType
    c'dtype    = maybe (-1) marshalDepth $ dsToMaybe $ toDepthDS (Proxy :: Proxy dstDepth)

{- | Calculates the sum of array elements

Example:

@
matSumImg :: Mat (ShapeT [201, 201]) ('S 3) ('S Word8)
matSumImg = exceptError $
    withMatM
      (Proxy :: Proxy [201, 201])
      (Proxy :: Proxy 3)
      (Proxy :: Proxy Word8)
      black $ \imgM -> do
        -- Draw a filled circle. Each pixel has a value of (255,255,255)
        lift $ circle imgM (pure radius :: V2 Int32) radius white (-1) LineType_8 0
        -- Calculate the sum of all pixels.
        scalar <- matSumM imgM
        let V4 area _y _z _w = fromScalar scalar :: V4 Double
        -- Circle area = pi * radius * radius
        let approxPi = area \/ 255 \/ (radius * radius)
        lift $ putText imgM
                       (T.pack $ show approxPi)
                       (V2 40 110 :: V2 Int32)
                       (Font FontHersheyDuplex NotSlanted 1)
                       blue
                       1
                       LineType_AA
                       False
  where
    radius :: forall a. Num a => a
    radius = 100
@

<<doc/generated/examples/matSumImg.png matSumImg>>

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#sum OpenCV Sphinx doc>
-}
matSum
    :: Mat shape channels depth
       -- ^ Input array that must have from 1 to 4 channels.
    -> CvExcept Scalar
matSum src = runCvExceptST $ matSumM =<< unsafeThaw src

matSumM
    :: (PrimMonad m)
    => Mut (Mat shape channels depth) (PrimState m)
       -- ^ Input array that must have from 1 to 4 channels.
    -> CvExceptT m Scalar
matSumM srcM = ExceptT $ unsafePrimToPrim $ do
    s <- newScalar zero
    handleCvException (pure s) $
      withPtr srcM $ \srcPtr ->
      withPtr s    $ \sPtr   ->
        [cvExcept|
          *$(Scalar * sPtr) = cv::sum(*$(Mat * srcPtr));
        |]

{- | Calculates a mean and standard deviation of array elements

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#meanstddev OpenCV Sphinx doc>
-}
meanStdDev
    :: (1 <= channels, channels <= 4)
    => Mat shape ('S channels) depth
    -> Maybe (Mat shape ('S 1) ('S Word8))
       -- ^ Optional operation mask.
    -> CvExcept (Scalar, Scalar)
meanStdDev src mask = unsafeWrapException $ do
    mean   <- newScalar $ pure 0
    stddev <- newScalar $ pure 0
    handleCvException (pure (mean, stddev)) $
      withPtr src    $ \srcPtr    ->
      withPtr mask   $ \maskPtr   ->
      withPtr mean   $ \meanPtr   ->
      withPtr stddev $ \stddevPtr ->
        [cvExcept|
          cv::Mat * maskPtr = $(Mat * maskPtr);
          cv::meanStdDev
          ( *$(Mat * srcPtr)
          , *$(Scalar * meanPtr)
          , *$(Scalar * stddevPtr)
          , maskPtr ? cv::_InputArray(*maskPtr) : cv::_InputArray(cv::noArray())
          );
        |]


{-| Performs the perspective matrix transformation of vectors. 

    TODO: Modify this function for accept 3D points
    TODO: Generalize return type to
          V.Vector (point2 CDouble)

<http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#perspectivetransform OpenCV Sphinx doc>
-}
perspectiveTransform
    :: (IsPoint2 point2 CDouble)
    => V.Vector (point2 CDouble)
    -> Mat ('S '[ 'S 3, 'S 3 ]) ('S 1) ('S Double)
    -> V.Vector (Point2d)
perspectiveTransform srcPoints transformationMat = unsafePerformIO $
    withArrayPtr (V.map toPoint srcPoints) $ \srcPtr ->
    withPtr transformationMat $ \tmPtr ->
    allocaArray numPts $ \(dstPtr :: Ptr (V2 CDouble)) -> do
        let dstPtr' = castPtr dstPtr
        [C.block| void {
            cv::_InputArray srcPts  = cv::_InputArray( $(Point2d * srcPtr),  $(int32_t c'numPts));
            cv::_OutputArray dstPts = cv::_OutputArray($(Point2d * dstPtr'), $(int32_t c'numPts));
            cv::perspectiveTransform
                ( srcPts
                , dstPts
                , *$(Mat * tmPtr)
                );
            }|]
        peekArray numPts dstPtr >>= return . V.fromList . map toPoint
  where
    numPts   = fromIntegral $ V.length srcPoints
    c'numPts = fromIntegral $ V.length srcPoints
