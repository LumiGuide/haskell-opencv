module OpenCV.Unsafe
    ( -- * Mutable Matrix
      unsafeFreeze
    , unsafeThaw
    ) where

import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState )
import "this" OpenCV.Internal


unsafeFreeze :: (PrimMonad m) => MutMat (PrimState m) -> m Mat
unsafeFreeze = pure . unMutMat

unsafeThaw :: (PrimMonad m) => Mat -> m (MutMat (PrimState m))
unsafeThaw = pure . MutMat
