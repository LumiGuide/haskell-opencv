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

module OpenCV.Core.Types.Mat.Repa
    ( M
    , DIM
    , toRepa
    ) where

import "base" Data.Int
import "base" Data.Monoid
import "base" Data.Proxy
import "base" Data.Word
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
import qualified "repa" Data.Array.Repa as Repa
import           "repa" Data.Array.Repa.Index ( (:.) )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
--  Repa
--------------------------------------------------------------------------------

-- | Representation tag for Repa @'Repa.Array's@ for OpenCV @'Mat's@.
data M (shape :: [DS Nat]) (channels :: Nat)

type family DIM (n :: Nat) :: * where
    DIM 0 = Repa.Z
    DIM n = DIM (n-1) :. Int

-- | Converts an OpenCV @'Mat'rix@ into a Repa array.
--
-- This is a zero-copy operation.
toRepa
    :: forall (shape    :: [DS Nat])
              (channels :: Nat)
              (depth    :: *)
              (dims     :: Nat)
              (sh       :: *)
     . ( Storable depth
       , KnownNat channels
       , KnownNat dims
       , dims ~ Length shape
       , sh ~ DIM ((dims + 1))
       )
    => Mat ('S shape) ('S channels) ('S depth) -- ^
    -> Repa.Array (M shape channels) sh depth
toRepa mat = unsafePerformIO $ withPtr mat $ \matPtr ->
    alloca $ \(sizePtr    :: Ptr (Ptr Int32)) ->
    alloca $ \(stepPtr    :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtrPtr :: Ptr (Ptr Word8)) -> do
      [CU.block| void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t * * const sizePtr   ) = matPtr->size.p;
        *$(size_t  * * const stepPtr   ) = matPtr->step.p;
        *$(uint8_t * * const dataPtrPtr) = matPtr->data;
      }|]
      let dims = fromInteger $ natVal (Proxy :: Proxy dims)

      (size :: Ptr Int32) <- peek sizePtr
      sizeShape <- map fromIntegral <$> peekArray dims size
      let sizes = sizeShape <> [fromInteger $ natVal (Proxy :: Proxy channels)]

      (step :: Ptr CSize) <- peek stepPtr
      stepShape <- map fromIntegral <$> peekArray dims step
      let steps = stepShape <> [sizeOf (undefined :: depth)]

      (dataPtr :: Ptr Word8) <- peek dataPtrPtr
      pure $ Array mat dataPtr sizes steps

instance (Repa.Shape sh, Storable depth) => NFData (Repa.Array (M shape channels) sh depth) where
    rnf a = Repa.deepSeqArray a ()

instance (Storable depth) => Repa.Source (M shape channels) depth where
    -- TODO (BvD): We might want to check for isContinuous() to optimize certain operations.

    data Array (M shape channels) sh depth =
          Array !(Mat ('S shape) ('S channels) ('S depth))
                 -- The Mat is kept around so that the data doesn't get garbage collected.
                !(Ptr Word8) -- Pointer to the data.
                ![Int] -- The shape of the extent which is determined by mat->dims and mat->size.p.
                ![Int] -- The shape of the data which is determined by mat->dims and mat->step.p.

    extent :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> sh
    extent (Array _ _ sizeShape _) = Repa.shapeOfList sizeShape

    index :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> sh -> depth
    index (Array mat dataPtr sizeShape stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr depth
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith3 mul sizeShape stepShape (Repa.listOfShape ix)

        mul size step i
            | i < size  = step * i
            | otherwise = error $
                "Index " <> show i <> " >= size: " <> show size

    unsafeIndex :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> sh -> depth
    unsafeIndex (Array mat dataPtr _ stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr depth
        elemPtr = matElemAddress dataPtr stepShape (Repa.listOfShape ix)

    linearIndex :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> Int -> depth
    linearIndex a ix = Repa.index a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    unsafeLinearIndex :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> Int -> depth
    unsafeLinearIndex a ix = Repa.unsafeIndex a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    deepSeqArray :: (Repa.Shape sh) => Repa.Array (M shape channels) sh depth -> b -> b
    deepSeqArray = seq

-- TODO (BvD): Is it possible to define something like the following?
--
-- instance (Storable depth) => Repa.Target (M shape channels) depth where
--
--     newtype MVec (M shape channels) depth = MVec IOMat
--
--     newMVec :: Int -> IO (MVec (M shape channels) depth)
--     newMVec size = _todo_newMVec
--
--     unsafeWriteMVec :: MVec (M shape channels) depth -> Int -> depth -> IO ()
--     unsafeWriteMVec = _todo_unsafeWriteMVec
--
--     unsafeFreezeMVec :: sh  -> MVec (M shape channels) depth -> IO (Array (M shape channels) sh depth)
--     unsafeFreezeMVec = _todo_unsafeFreezeMVec
--
--     deepSeqMVec :: MVec (M shape channels) depth -> a -> a
--     deepSeqMVec = _todo_deepSeqMVec
--
--     touchMVec :: MVec (M shape channels) depth -> IO ()
--     touchMVec = _todo_touchMVec
