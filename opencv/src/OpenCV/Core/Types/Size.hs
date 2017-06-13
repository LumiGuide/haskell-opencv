{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Size
    ( Size
    , IsSize(..)

    , Size2i, Size2f, Size2d
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import qualified "inline-c"     Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Size
import "this" OpenCV.Internal.Core.Types.Size.TH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.using "namespace cv"

mkSizeType "Size2i" ''Int32   "int32_t"
mkSizeType "Size2f" ''CFloat  "float"
mkSizeType "Size2d" ''CDouble "double"
