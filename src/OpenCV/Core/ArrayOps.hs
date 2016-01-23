{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Core.ArrayOps
    ( addWeighted
    , minMaxLoc
    , NormType(..)
    , NormAbsRel(..)
    , norm
    , normDiff
    , normalize
    , matSum
    , bitwise_not
    , bitwise_and
    , bitwise_or
    , bitwise_xor
    ) where

import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( Storable(..), peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.Vector ( zero )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Exception
import "this" OpenCV.TypeLevel
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Core.ArrayOps.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Operations on Arrays
--------------------------------------------------------------------------------

-- | Calculates the weighted sum of two arrays
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#addweighted OpenCV Sphinx doc>
addWeighted
    :: forall shape channels srcDepth dstDepth
     . (Convert (Proxy dstDepth) (DS MatDepth))
    => Mat shape channels srcDepth -- ^ src1
    -> Double -- ^ alpha
    -> Mat shape channels srcDepth -- ^ src2
    -> Double -- ^ beta
    -> Double -- ^ gamma
    -> Either CvException (Mat shape channels dstDepth)
addWeighted src1 alpha src2 beta gamma = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr src1 $ \src1Ptr ->
      withMatPtr src2 $ \src2Ptr ->
      withMatPtr dst $ \dstPtr ->
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
    c'dtype = maybe (-1) marshalMatDepth $ dsToMaybe $ convert (Proxy :: Proxy dstDepth)

-- | Finds the global minimum and maximum in an array
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#minmaxloc OpenCV Sphinx doc>

-- TODO (RvD): implement mask
minMaxLoc
    :: Mat ('S [height, width]) channels depth -- ^
    -> Either CvException (Double, Double, Point2i, Point2i)
minMaxLoc src = unsafePerformIO $ do
    minLoc <- newPoint2i zero
    maxLoc <- newPoint2i zero
    withMatPtr src $ \srcPtr ->
      withPoint2iPtr minLoc $ \minLocPtr ->
      withPoint2iPtr maxLoc $ \maxLocPtr ->
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

-- | Calculates an absolute array norm
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
norm
    :: NormType
    -> Maybe (Mat shape ('S 1) ('S Word8))
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat shape channels depth -- ^ Input array.
    -> Either CvException Double  -- ^ Calculated norm.
norm normType mbMask src = unsafePerformIO $
    withMatPtr   src    $ \srcPtr  ->
    withMbMatPtr mbMask $ \mskPtr  ->
    alloca              $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * srcPtr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                  );
      |]
  where
    c'normType = marshalNormType NormAbsolute normType

-- | Calculates an absolute difference norm, or a relative difference norm
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
normDiff
    :: NormAbsRel -- ^ Absolute or relative norm.
    -> NormType
    -> Maybe (Mat shape ('S 1) ('S Word8))
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat shape channels depth -- ^ First input array.
    -> Mat shape channels depth -- ^ Second input array of the same size and type as the first.
    -> Either CvException Double -- ^ Calculated norm.
normDiff absRel normType mbMask src1 src2 = unsafePerformIO $
    withMatPtr   src1   $ \src1Ptr ->
    withMatPtr   src2   $ \src2Ptr ->
    withMbMatPtr mbMask $ \mskPtr  ->
    alloca              $ \normPtr ->
    handleCvException (realToFrac <$> peek normPtr) $
      [cvExcept|
        Mat * mskPtr = $(Mat * mskPtr);
        *$(double * normPtr) =
          cv::norm( *$(Mat * src1Ptr)
                  , *$(Mat * src2Ptr)
                  , $(int32_t c'normType)
                  , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                  );
      |]
  where
    c'normType = marshalNormType absRel normType

-- | Normalizes the norm or value range of an array
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#normalize OpenCV Sphinx doc>
normalize
    :: forall shape channels srcDepth dstDepth
     . (Convert (Proxy dstDepth) (Maybe MatDepth))
    => Double
       -- ^ Norm value to normalize to or the lower range boundary in case of
       -- the range normalization.
    -> Double
       -- ^ Upper range boundary in case of the range normalization; it is not
       -- used for the norm normalization.
    -> NormType
    -> Maybe (Mat shape ('S 1) ('S Word8)) -- ^ Optional operation mask.
    -> Mat shape channels srcDepth -- ^ Input array.
    -> Either CvException (Mat shape channels dstDepth)
normalize alpha beta normType mbMask src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr src      $ \srcPtr ->
      withMatPtr dst      $ \dstPtr ->
      withMbMatPtr mbMask $ \mskPtr ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::normalize( *$(Mat * srcPtr)
                       , *$(Mat * dstPtr)
                       , $(double c'alpha)
                       , $(double c'beta)
                       , $(int32_t c'normType)
                       , $(int32_t c'dtype)
                       , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
                       );
        |]
  where
    c'alpha    = realToFrac alpha
    c'beta     = realToFrac beta
    c'normType = marshalNormType NormAbsolute normType
    c'dtype    = maybe (-1) marshalMatDepth $ convert (Proxy :: Proxy dstDepth)

-- | Calculates the sum of array elements
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#sum OpenCV Sphinx doc>
matSum
    :: -- (1 <= channels, channels <= 4)
    -- =>
    Mat shape channels depth -- ^ Input array that must have from 1 to 4 channels.
    -> Either CvException Scalar
matSum src = unsafePerformIO $ do
    s <- newScalar zero
    handleCvException (pure s) $
      withMatPtr src $ \srcPtr ->
      withScalarPtr s $ \sPtr ->
        [cvExcept|
          *$(Scalar * sPtr) = cv::sum(*$(Mat * srcPtr));
        |]

bitwise_not
    :: Mat shape channels depth -- ^
    -> Maybe (Mat shape ('S 1) ('S Word8))
    -> Either CvException (Mat shape channels depth)
bitwise_not src mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr   src    $ \srcPtr ->
      withMatPtr   dst    $ \dstPtr ->
      withMbMatPtr mbMask $ \mskPtr ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::bitwise_not
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
          );
        |]

bitwise_and
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Maybe (Mat shape ('S 1) ('S Word8))
    -> Either CvException (Mat shape channels depth)
bitwise_and src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr   src1   $ \src1Ptr ->
      withMatPtr   src2   $ \src2Ptr ->
      withMatPtr   dst    $ \dstPtr  ->
      withMbMatPtr mbMask $ \mskPtr  ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::bitwise_and
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
          );
        |]

bitwise_or
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Maybe (Mat shape ('S 1) ('S Word8))
    -> Either CvException (Mat shape channels depth)
bitwise_or src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr   src1   $ \src1Ptr ->
      withMatPtr   src2   $ \src2Ptr ->
      withMatPtr   dst    $ \dstPtr  ->
      withMbMatPtr mbMask $ \mskPtr  ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::bitwise_or
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
          );
        |]

bitwise_xor
    :: Mat shape channels depth -- ^
    -> Mat shape channels depth
    -> Maybe (Mat shape ('S 1) ('S Word8))
    -> Either CvException (Mat shape channels depth)
bitwise_xor src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr   src1   $ \src1Ptr ->
      withMatPtr   src2   $ \src2Ptr ->
      withMatPtr   dst    $ \dstPtr  ->
      withMbMatPtr mbMask $ \mskPtr  ->
        [cvExcept|
          Mat * mskPtr = $(Mat * mskPtr);
          cv::bitwise_xor
          ( *$(Mat * src1Ptr)
          , *$(Mat * src2Ptr)
          , *$(Mat * dstPtr)
          , mskPtr ? _InputArray(*mskPtr) : _InputArray(noArray())
          );
        |]
