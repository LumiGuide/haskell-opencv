{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal
  ( objFromPtr
  , objFromPtr2
  ) where

import "base" Control.Exception ( mask_ )
import qualified "base" Foreign.Concurrent as Conc ( newForeignPtr  )
import "base" Foreign.ForeignPtr ( ForeignPtr, newForeignPtr  )
import "base" Foreign.Ptr ( Ptr, FunPtr )

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> Conc.newForeignPtr objPtr (finalizer objPtr)

objFromPtr2 :: (ForeignPtr c -> hask) -> FunPtr (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr2 haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr finalizer objPtr
