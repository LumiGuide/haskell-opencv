module OpenCV.Unsafe
    ( unsafeCoerceMat
    , unsafeCoerceMatM
      -- * Mutable Matrix
    , unsafeFreeze
    , unsafeThaw
    , unsafeRead
    , unsafeWrite
    ) where

import "base" Foreign.Storable ( peek, poke )
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive
    ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types.Mat.Internal

unsafeFreeze
    :: (PrimMonad m)
    => MutMat shape channels depth (PrimState m)
    -> m (Mat shape channels depth)
unsafeFreeze = pure . unMutMat

unsafeThaw
    :: (PrimMonad m)
    => Mat shape channels depth
    -> m (MutMat shape channels depth (PrimState m))
unsafeThaw = pure . MutMat

unsafeRead
    :: (PrimMonad m, Storable value)
    => MutMat shape channels depth (PrimState m)
    -> [Int]
    -> m value
unsafeRead matM pos =
    unsafePrimToPrim $ withMatData (unMutMat matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in peek elemPtr

unsafeWrite
    :: (PrimMonad m, Storable value)
    => MutMat shape channels depth (PrimState m)
    -> [Int] -> value -> m ()
unsafeWrite matM pos value =
    unsafePrimToPrim $ withMatData (unMutMat matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in poke elemPtr value
