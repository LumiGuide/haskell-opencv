module OpenCV.Unsafe
    ( unsafeCoerceMat
    , unsafeCoerceMatM
      -- * Mutable Matrix
    , unsafeFreeze
    , unsafeThaw
    , unsafeRead
    , unsafeWrite
    ) where

import "base" Foreign.Ptr ( plusPtr )
import "base" Foreign.Storable ( Storable, peek, poke, sizeOf )
import "primitive" Control.Monad.Primitive
    ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Mutable

unsafeRead
    :: forall m shape channels depth value
     . (PrimMonad m, Storable value)
    => Mut (Mat shape channels depth) (PrimState m)
    -> [Int] -- ^ position
    -> Int -- ^ channel
    -> m value
unsafeRead matM pos channel =
    unsafePrimToPrim $ withMatData (unMut matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in peek (elemPtr `plusPtr` (channel * sizeOf dummyValue))
  where
    dummyValue :: value
    dummyValue = error "dummy"

unsafeWrite
    :: (PrimMonad m, Storable value)
    => Mut (Mat shape channels depth) (PrimState m)
    -> [Int] -- ^ position
    -> Int -- ^ channel
    -> value
    -> m ()
unsafeWrite matM pos channel value =
    unsafePrimToPrim $ withMatData (unMut matM) $ \step dataPtr ->
      let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
      in poke (elemPtr `plusPtr` (channel * sizeOf value)) value
