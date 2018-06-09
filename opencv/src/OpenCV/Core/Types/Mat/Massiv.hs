{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language InstanceSigs #-}
{-# language ConstraintKinds #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ExistentialQuantification #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

module OpenCV.Core.Types.Mat.Massiv ( M ) where

import "base" Data.Int
import "base" Data.Proxy
import "base" Data.Typeable
import "base" Data.Word
import "base" Data.Maybe ( fromJust )
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( Storable(..), peek, sizeOf )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import "deepseq" Control.DeepSeq (NFData, rnf)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V4 (V4(V4))
import qualified "massiv" Data.Massiv.Array as Massiv
import qualified "massiv" Data.Massiv.Core as Massiv
import qualified "massiv" Data.Massiv.Array.Unsafe as Massiv
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel
import "this" OpenCV.Core.Types
import qualified "vector" Data.Vector as V

#if !MIN_VERSION_base(4,11,0)
import "base" Data.Monoid
#endif

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
--  Repa
--------------------------------------------------------------------------------

-- | Representation tag for Massiv @'Massiv.Array's@ for OpenCV @'Mat's@.
data M (shape :: [DS Nat]) (channels :: Nat)

data instance Massiv.Array (M shape channels) ix depth =
    Array
    { mMat :: !(Mat ('S shape) ('S channels) ('S depth))
      -- ^ The Mat is kept around so that the data doesn't get garbage collected.
    , mComp :: !Massiv.Comp -- ^ Massiv computation strategy.
    , mData :: !(Ptr Word8) -- ^ Pointer to the data.
    , mSize :: !(Ptr Int32) -- ^ The shape of the extent which is determined by mat->size.p.
    , mStep :: !(Ptr CSize) -- ^ The shape of the data which is determined by mat->step.p.
    }

instance NFData (Massiv.Array (M shape channels) ix depth) where
    rnf Array{} = ()

instance ( KnownNat channels
         , KnownNat dims
         , Typeable shape
         , dims ~ Length shape
         , ix ~ Massiv.Ix (dims + 1)
         , Massiv.Index ix
         , Storable depth
         , ToDepth (Proxy depth)
         ) => Massiv.Construct (M shape channels) ix depth where
  getComp = mComp

  setComp c arr = arr { mComp = c }

  unsafeMakeArray
      :: Massiv.Comp
      -> ix
      -> (ix -> depth)
      -> Massiv.Array (M shape channels) ix depth
  unsafeMakeArray comp !sz f = toMassiv mat comp
    where
      mat :: Mat ('S shape) ('S channels) ('S depth)
      mat = unsafeCoerceMat $ exceptError $ withMatM
              shapeV
              (Proxy :: Proxy channels)
              (Proxy :: Proxy depth)
              black $ \imgM -> do
        -- TODO: write to imgM using f according to the comp strategy
        pure ()

      shapeV :: V.Vector Int32
      shapeV = V.generate (fromIntegral $ Massiv.rank sz) $ \ix ->
                 fromIntegral $ fromJust $ Massiv.getIndex sz $ fromIntegral ix

      black = toScalar (V4 0 0 0 0 :: V4 Double)

-- | Converts an OpenCV @'Mat'rix@ into a Massiv array.
--
-- This is a zero-copy operation.
toMassiv
    :: forall (shape    :: [DS Nat])
              (channels :: Nat)
              (depth    :: *)
              (dims     :: Nat)
              (ix       :: *)
     . ( Storable depth
       , KnownNat channels
       , KnownNat dims
       , dims ~ Length shape
       , ix ~ Massiv.Ix (dims + 1)
       )
    => Mat ('S shape) ('S channels) ('S depth) -- ^
    -> Massiv.Comp -- ^ Massiv computation strategy.
    -> Massiv.Array (M shape channels) ix depth
toMassiv mat comp = unsafePerformIO $ withPtr mat $ \matPtr ->
    alloca $ \(sizePtrPtr :: Ptr (Ptr Int32)) ->
    alloca $ \(stepPtrPtr :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtrPtr :: Ptr (Ptr Word8)) -> do
      [CU.block| void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t * * const sizePtrPtr) = matPtr->size.p;
        *$(size_t  * * const stepPtrPtr) = matPtr->step.p;
        *$(uint8_t * * const dataPtrPtr) = matPtr->data;
      }|]
      (sizePtr :: Ptr Int32) <- peek sizePtrPtr
      (stepPtr :: Ptr CSize) <- peek stepPtrPtr
      (dataPtr :: Ptr Word8) <- peek dataPtrPtr
      pure Array
           { mMat  = mat
           , mComp = comp
           , mData = dataPtr
           , mSize = sizePtr
           , mStep = stepPtr
           }
