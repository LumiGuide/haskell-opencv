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

  , fmapRect
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import qualified "inline-c"     Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Rect
import "this" OpenCV.Internal.Core.Types.Rect.TH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.using "namespace cv"

mkRectType "Rect2i" ''Int32   "int32_t" "Point2i" "Size2i"
mkRectType "Rect2f" ''CFloat  "float"   "Point2f" "Size2f"
mkRectType "Rect2d" ''CDouble "double"  "Point2d" "Size2d"

fmapRect :: forall a b. (IsRect Rect a, IsRect HRect a, IsRect Rect b, IsRect HRect b)
   => (a -> b) -> Rect a -> Rect b
fmapRect f r = toRect hrB
  where
    hrA :: HRect a
    hrA = fromRect r

    hrB :: HRect b
    hrB = fmap f hrA
