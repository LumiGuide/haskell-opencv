{-# language CPP #-}
{-# language MultiParamTypeClasses #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Matx
    ( Matx(..)
    , MatxDimR
    , MatxDimC
    , IsMatx(..)
    ) where

import "base" Data.Kind ( Type )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" GHC.TypeLits
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

newtype Matx (dimR :: Nat) (dimC :: Nat) (depth :: Type)
      = Matx {unMatx :: ForeignPtr (C'Matx dimR dimC depth)}

type instance C (Matx dimR dimC depth) = C'Matx dimR dimC depth

instance WithPtr (Matx dimR dimC depth) where
    withPtr = withForeignPtr . unMatx

type family MatxDimR (m :: Type -> Type) :: Nat
type family MatxDimC (m :: Type -> Type) :: Nat

type instance MatxDimR (Matx dimR dimC) = dimR
type instance MatxDimC (Matx dimR dimC) = dimC

class IsMatx (m :: Type -> Type) depth where
    toMatx   :: m depth -> Matx (MatxDimR m) (MatxDimC m) depth
    fromMatx :: Matx (MatxDimR m) (MatxDimC m) depth -> m depth

    toMatxIO :: m depth -> IO (Matx (MatxDimR m) (MatxDimC m) depth)
    toMatxIO = pure . toMatx
