{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal
  ( objFromPtr
  ) where

import "base" Control.Exception ( mask_ )
import "base" Foreign.ForeignPtr ( ForeignPtr, newForeignPtr  )
import "base" Foreign.Ptr ( Ptr, FunPtr )

objFromPtr :: (ForeignPtr c -> hask) -> FunPtr (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr finalizer objPtr
