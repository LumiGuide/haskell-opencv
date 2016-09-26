{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgProc.Types where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types ( CDouble )
import "linear" Linear.V4 ( V4(..) )
import "linear" Linear.Vector ( zero )
import "this" OpenCV.Core.Types ( Scalar, toScalar )
import "this" OpenCV.ImgProc.Types

--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"

#num INTER_NEAREST
#num INTER_LINEAR
#num INTER_CUBIC
#num INTER_AREA
#num INTER_LANCZOS4

marshalInterpolationMethod :: InterpolationMethod -> Int32
marshalInterpolationMethod = \case
   InterNearest  -> c'INTER_NEAREST
   InterLinear   -> c'INTER_LINEAR
   InterCubic    -> c'INTER_CUBIC
   InterArea     -> c'INTER_AREA
   InterLanczos4 -> c'INTER_LANCZOS4

#num BORDER_CONSTANT
#num BORDER_REPLICATE
#num BORDER_REFLECT
#num BORDER_WRAP
#num BORDER_REFLECT_101
#num BORDER_TRANSPARENT
#num BORDER_ISOLATED

marshalBorderMode :: BorderMode -> (Int32, Scalar)
marshalBorderMode = \case
    BorderConstant s  -> (c'BORDER_CONSTANT    , s         )
    BorderReplicate   -> (c'BORDER_REPLICATE   , zeroScalar)
    BorderReflect     -> (c'BORDER_REFLECT     , zeroScalar)
    BorderWrap        -> (c'BORDER_WRAP        , zeroScalar)
    BorderReflect101  -> (c'BORDER_REFLECT_101 , zeroScalar)
    BorderTransparent -> (c'BORDER_TRANSPARENT , zeroScalar)
    BorderIsolated    -> (c'BORDER_ISOLATED    , zeroScalar)
  where
    zeroScalar :: Scalar
    zeroScalar = toScalar (zero :: V4 CDouble)
