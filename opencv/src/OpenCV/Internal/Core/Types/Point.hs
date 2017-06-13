{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Point
  ( Point(..)
  , PointDim
  , IsPoint(..)
  , IsPoint2
  , IsPoint3
  ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" GHC.TypeLits
import "linear" Linear ( V2, V3 )
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

newtype Point (dim :: Nat) (depth :: *)
      = Point {unPoint :: ForeignPtr (C'Point dim depth)}

type instance C (Point dim depth) = C'Point dim depth

instance WithPtr (Point dim depth) where
    withPtr = withForeignPtr . unPoint

type family PointDim (v :: * -> *) :: Nat

type instance PointDim (Point dim) = dim

type instance PointDim V2 = 2
type instance PointDim V3 = 3

class IsPoint (p :: * -> *) (depth :: *)  where
    toPoint   :: p depth -> Point (PointDim p) depth
    fromPoint :: Point (PointDim p) depth -> p depth

    toPointIO :: p depth -> IO (Point (PointDim p) depth)
    toPointIO = pure . toPoint

type IsPoint2 p depth = (IsPoint p depth, PointDim p ~ 2)
type IsPoint3 p depth = (IsPoint p depth, PointDim p ~ 3)

--------------------------------------------------------------------------------

instance (IsPoint V2 a, Show a)
      => Show (Point 2 a) where
    showsPrec prec point =
        showParen (prec >= 10)
        $ showString "toPoint "
        . showParen True (shows v2)
      where
        v2 :: V2 a
        v2 = fromPoint point

instance (IsPoint V3 a, Show a)
      => Show (Point 3 a) where
    showsPrec prec point =
        showParen (prec >= 10)
        $ showString "toPoint "
        . showParen True (shows v3)
      where
        v3 :: V3 a
        v3 = fromPoint point
