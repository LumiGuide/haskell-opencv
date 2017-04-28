{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Mutable
  ( Mut(..)
  , Mutable
  , FreezeThaw(..)
  ) where

import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )

-- | Wrapper for mutable values
newtype Mut a s = Mut { unMut :: a }

type family Mutable (a :: *) :: * -> *

class FreezeThaw a where
    freeze :: (PrimMonad m) => Mutable a (PrimState m) -> m a
    thaw   :: (PrimMonad m) => a -> m (Mutable a (PrimState m))

    unsafeFreeze :: (PrimMonad m) => Mutable a (PrimState m) -> m a
    unsafeThaw   :: (PrimMonad m) => a -> m (Mutable a (PrimState m))
