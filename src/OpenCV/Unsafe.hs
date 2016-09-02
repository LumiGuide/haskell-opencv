module OpenCV.Unsafe
    ( unsafeCoerceMat
    , unsafeCoerceMatM
      -- * Mutable Matrix
    , unsafeFreeze
    , unsafeThaw
    , unsafeRead
    , unsafeWrite
    ) where

import "base" Foreign.Storable ( Storable, peek, poke )
import "primitive" Control.Monad.Primitive
    ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Mutable

unsafeRead
    :: (PrimMonad m, Storable value)
    => Mut (Mat shape channels depth) (PrimState m)
    -> [Int]
    -> m value
unsafeRead matM pos =
    unsafePrimToPrim $ withMatData (unMut matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in peek elemPtr

unsafeWrite
    :: (PrimMonad m, Storable value)
    => Mut (Mat shape channels depth) (PrimState m)
    -> [Int] -> value -> m ()
unsafeWrite matM pos value =
    unsafePrimToPrim $ withMatData (unMut matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in poke elemPtr value
