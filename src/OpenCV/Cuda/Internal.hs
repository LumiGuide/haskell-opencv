{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Cuda.Internal
  ( CudaMat(..)
  ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" GHC.TypeLits
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/cuda.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Cuda Matrix
--------------------------------------------------------------------------------

newtype CudaMat (height   :: DS Nat)
                (width    :: DS Nat)
                (channels :: DS Nat)
                (depth    :: DS *)
      = CudaMat {unCudaMat :: ForeignPtr (C (CudaMat height width channels depth))}

type instance C (CudaMat height width channels depth) = C'CudaMat

type instance Mutable (CudaMat height width channels depth) =
    Mut (CudaMat height width channels depth)

instance WithPtr (CudaMat height width channels depth) where
    withPtr = withForeignPtr . unCudaMat

instance FromPtr (CudaMat height width channels depth) where
    fromPtr = objFromPtr CudaMat $ \ptr ->
                [CU.exp| void { delete $(CudaMat * ptr) }|]
