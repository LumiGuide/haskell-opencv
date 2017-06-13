{-# language CPP #-}
{-# language MultiParamTypeClasses #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Vec
    ( Vec(..)
    , VecDim
    , IsVec(..)
    ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" GHC.TypeLits
import "linear" Linear ( V2, V3, V4 )
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

newtype Vec (dim :: Nat) (depth :: *)
      = Vec {unVec :: ForeignPtr (C'Vec dim depth)}

type instance C (Vec dim depth) = C'Vec dim depth

instance WithPtr (Vec dim depth) where
    withPtr = withForeignPtr . unVec

type family VecDim (v :: * -> *) :: Nat

type instance VecDim (Vec dim) = dim

type instance VecDim V2 = 2
type instance VecDim V3 = 3
type instance VecDim V4 = 4

class IsVec (v :: * -> *) (depth :: *)  where
    toVec   :: v depth -> Vec (VecDim v) depth
    fromVec :: Vec (VecDim v) depth -> v depth

    toVecIO :: v depth -> IO (Vec (VecDim v) depth)
    toVecIO = pure . toVec

--------------------------------------------------------------------------------

instance (IsVec V2 a, Show a)
      => Show (Vec 2 a) where
    showsPrec prec vec =
        showParen (prec >= 10)
        $ showString "toVec "
        . showParen True (shows v2)
      where
        v2 :: V2 a
        v2 = fromVec vec

instance (IsVec V3 a, Show a)
      => Show (Vec 3 a) where
    showsPrec prec vec =
        showParen (prec >= 10)
        $ showString "toVec "
        . showParen True (shows v3)
      where
        v3 :: V3 a
        v3 = fromVec vec

instance (IsVec V4 a, Show a)
      => Show (Vec 4 a) where
    showsPrec prec vec =
        showParen (prec >= 10)
        $ showString "toVec "
        . showParen True (shows v4)
      where
        v4 :: V4 a
        v4 = fromVec vec
