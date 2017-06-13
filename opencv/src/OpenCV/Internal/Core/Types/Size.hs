{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Size
  ( Size(..)
  , IsSize(..)
  ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "linear" Linear ( V2 )
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

newtype Size (depth :: *)
      = Size {unSize :: ForeignPtr (C'Size depth)}

type instance C (Size depth) = C'Size depth

instance WithPtr (Size depth) where
    withPtr = withForeignPtr . unSize

class IsSize (p :: * -> *) (depth :: *)  where
    toSize   :: p depth -> Size depth
    fromSize :: Size depth -> p depth

    toSizeIO :: p depth -> IO (Size depth)
    toSizeIO = pure . toSize

--------------------------------------------------------------------------------

instance (IsSize V2 a, Show a)
      => Show (Size a) where
    showsPrec prec size =
        showParen (prec >= 10)
        $ showString "toSize "
        . showParen True (shows v2)
      where
        v2 :: V2 a
        v2 = fromSize size
