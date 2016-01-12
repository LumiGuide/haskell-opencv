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

import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( Storable(..), peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.Vector ( zero )
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------
-- Operations on Arrays
--------------------------------------------------------------------------------

-- | Calculates the weighted sum of two arrays
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#addweighted OpenCV Sphinx doc>
addWeighted
    :: Mat    -- ^ src1
    -> Double -- ^ alpha
    -> Mat    -- ^ src2
    -> Double -- ^ beta
    -> Double -- ^ gamma
    -> Either CvException Mat
addWeighted src1 alpha src2 beta gamma = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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
          );
      |]
  where
    c'alpha = realToFrac alpha
    c'beta  = realToFrac beta
    c'gamma = realToFrac gamma

-- | Finds the global minimum and maximum in an array
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#minmaxloc OpenCV Sphinx doc>

-- TODO (RvD): implement mask
minMaxLoc :: Mat -> Either CvException (Double, Double, Point2i, Point2i)
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

-- | Normalization type
data NormType
   = Norm_Inf
   | Norm_L1
   | Norm_L2
   | Norm_L2SQR
   | Norm_Hamming
   | Norm_Hamming2
   | Norm_MinMax
     deriving (Show, Eq)

data NormAbsRel
   = NormRelative
   | NormAbsolute
     deriving (Show, Eq)

#num NORM_INF
#num NORM_L1
#num NORM_L2
#num NORM_L2SQR
#num NORM_HAMMING
#num NORM_HAMMING2
#num NORM_MINMAX

#num NORM_RELATIVE

marshalNormType :: NormAbsRel -> NormType -> Int32
marshalNormType absRel normType =
    case absRel of
      NormRelative -> c'normType .|. c'NORM_RELATIVE
      NormAbsolute -> c'normType
  where
    c'normType = case normType of
        Norm_Inf      -> c'NORM_INF
        Norm_L1       -> c'NORM_L1
        Norm_L2       -> c'NORM_L2
        Norm_L2SQR    -> c'NORM_L2SQR
        Norm_Hamming  -> c'NORM_HAMMING
        Norm_Hamming2 -> c'NORM_HAMMING2
        Norm_MinMax   -> c'NORM_MINMAX

-- | Calculates an absolute array norm
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#norm OpenCV Sphinx doc>
norm
    :: NormType
    -> Maybe Mat
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat -- ^ Input array.
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
    -> Maybe Mat
       -- ^ Optional operation mask; it must have the same size as the input
       -- array, depth 'MatDepth_8U' and 1 channel.
    -> Mat -- ^ First input array.
    -> Mat -- ^ Second input array of the same size and type as the first.
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
    :: Double
       -- ^ Norm value to normalize to or the lower range boundary in case of
       -- the range normalization.
    -> Double
       -- ^ Upper range boundary in case of the range normalization; it is not
       -- used for the norm normalization.
    -> NormType
    -> Maybe MatDepth
    -> Maybe Mat -- ^ Optional operation mask.
    -> Mat -- ^ Input array.
    -> Either CvException Mat
normalize alpha beta normType dtype mbMask src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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
    c'dtype    = maybe (-1) marshalMatDepth dtype

-- | Calculates the sum of array elements
--
-- <http://docs.opencv.org/3.0-last-rst/modules/core/doc/operations_on_arrays.html#sum OpenCV Sphinx doc>
matSum
    :: Mat -- ^ Input array that must have from 1 to 4 channels.
    -> Either CvException Scalar
matSum src = unsafePerformIO $ do
    s <- newScalar zero
    handleCvException (pure s) $
      withMatPtr src $ \srcPtr ->
      withScalarPtr s $ \sPtr ->
        [cvExcept|
          *$(Scalar * sPtr) = cv::sum(*$(Mat * srcPtr));
        |]

bitwise_not :: Mat -> Maybe Mat -> Either CvException Mat
bitwise_not src mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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

bitwise_and :: Mat -> Mat -> Maybe Mat -> Either CvException Mat
bitwise_and src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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

bitwise_or :: Mat -> Mat -> Maybe Mat -> Either CvException Mat
bitwise_or src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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

bitwise_xor :: Mat -> Mat -> Maybe Mat -> Either CvException Mat
bitwise_xor src1 src2 mbMask = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
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
