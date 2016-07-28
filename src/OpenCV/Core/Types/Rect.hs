{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Rect
  ( Rect
  , HRect(..)

  , RectPoint
  , RectSize
  , IsRect(..)

  , Rect2i
  , Rect2f
  , Rect2d
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import qualified "inline-c"     Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Rect.Internal
import "this" OpenCV.Core.Types.Rect.TH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.using "namespace cv"

mkRectType "Rect2i" ''Int32   "int32_t" "Point2i" "Size2i"
mkRectType "Rect2f" ''CFloat  "float"   "Point2f" "Size2f"
mkRectType "Rect2d" ''CDouble "double"  "Point2d" "Size2d"
