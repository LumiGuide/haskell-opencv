{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Mat.Marshal
    ( marshalDepth
    , unmarshalDepth
    , marshalFlags
    , unmarshalFlags
    ) where

import "base" Data.Bits
import "base" Data.Int
import "base" Data.Monoid ( (<>) )
import "this" OpenCV.Internal.Core.Types.Mat.Depth

--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

#num CV_8U
#num CV_8S
#num CV_16U
#num CV_16S
#num CV_32S
#num CV_32F
#num CV_64F
#num CV_USRTYPE1

marshalDepth :: Depth -> Int32
marshalDepth = \case
    Depth_8U       -> c'CV_8U
    Depth_8S       -> c'CV_8S
    Depth_16U      -> c'CV_16U
    Depth_16S      -> c'CV_16S
    Depth_32S      -> c'CV_32S
    Depth_32F      -> c'CV_32F
    Depth_64F      -> c'CV_64F
    Depth_USRTYPE1 -> c'CV_USRTYPE1

unmarshalDepth :: Int32 -> Depth
unmarshalDepth n
    | n == c'CV_8U       = Depth_8U
    | n == c'CV_8S       = Depth_8S
    | n == c'CV_16U      = Depth_16U
    | n == c'CV_16S      = Depth_16S
    | n == c'CV_32S      = Depth_32S
    | n == c'CV_32F      = Depth_32F
    | n == c'CV_64F      = Depth_64F
    | n == c'CV_USRTYPE1 = Depth_USRTYPE1
    | otherwise          = error $ "unknown depth " <> show n

#num CV_CN_SHIFT

marshalFlags
    :: Depth
    -> Int32 -- ^ Number of channels
    -> Int32
marshalFlags depth cn =
    marshalDepth depth
      .|. ((cn - 1) `unsafeShiftL` c'CV_CN_SHIFT)

#num CV_CN_MAX
#num CV_MAT_DEPTH_MASK

unmarshalFlags :: Int32 -> (Depth, Int32)
unmarshalFlags n =
    ( unmarshalDepth $ n .&. c'CV_MAT_DEPTH_MASK
    , 1 + ((n `unsafeShiftR` c'CV_CN_SHIFT) .&. (c'CV_CN_MAX - 1))
    )
