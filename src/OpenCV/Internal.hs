{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal
  ( objFromPtr
  ) where

import "base" Control.Exception ( mask_ )
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.ForeignPtr ( ForeignPtr  )
import "base" Foreign.Ptr ( Ptr )

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr objPtr (finalizer objPtr)
