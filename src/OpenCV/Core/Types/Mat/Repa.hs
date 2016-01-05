{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenCV.Core.Types.Mat.Repa
    ( MatElem
    , ToMatDepth(..)
    , NumChannels(..)
    , Elem, getChannels
    , (:::)(..)
    , (:*)
    , matElem
    , M
    , toRepa
    , fromRepa
    , repa
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Ptr ( Ptr, plusPtr, castPtr )
import "base" Foreign.Storable ( Storable(..), peek, sizeOf, peekElemOff, pokeElemOff )
import "base" GHC.TypeLits
import "base" Data.Void ( Void )
import "deepseq" Control.DeepSeq (NFData, rnf)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lens" Control.Lens hiding ( ix )
import "lumi-hackage-extended" Lumi.Prelude
import qualified "repa" Data.Array.Repa as Repa
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import qualified "vector" Data.Vector.Unboxed as VU
import qualified "vector" Data.Vector.Unboxed.Mutable as VUM

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
--  Repa
--------------------------------------------------------------------------------

-- | A handy constraint for elements of a Repa @'Repa.Array' 'M' sh e@.
type MatElem e = (Storable e, ToMatDepth e, NumChannels e)

-- | Class of types that have a corresponding statically known 'MatDepth'.
class ToMatDepth a where
    -- | Determine the statically known depth of @a@.
    toMatDepth :: Proxy a -> MatDepth

instance ToMatDepth Word8  where toMatDepth = const MatDepth_8U
instance ToMatDepth Int8   where toMatDepth = const MatDepth_8S
instance ToMatDepth Word16 where toMatDepth = const MatDepth_16U
instance ToMatDepth Int16  where toMatDepth = const MatDepth_16S
instance ToMatDepth Int32  where toMatDepth = const MatDepth_32S
instance ToMatDepth Float  where toMatDepth = const MatDepth_32F
instance ToMatDepth Double where toMatDepth = const MatDepth_64F
-- TODO (BvD): instance ToMatDepth ? where toMatDepth = const MatDepth_USRTYPE1

instance (ToMatDepth a) => ToMatDepth (a ::: _as) where
    toMatDepth = const $ toMatDepth (Proxy :: Proxy a)

instance (ToMatDepth a) => ToMatDepth (Elem _n a) where
    toMatDepth = const $ toMatDepth (Proxy :: Proxy a)

-- | Class of types that have a statically known number of channels.
class NumChannels a where
    -- | Determine the statically known number of elements in @a@.
    numChannels :: Proxy a -> Int

instance NumChannels Word8  where numChannels = const 1
instance NumChannels Int8   where numChannels = const 1
instance NumChannels Word16 where numChannels = const 1
instance NumChannels Int16  where numChannels = const 1
instance NumChannels Int32  where numChannels = const 1
instance NumChannels Float  where numChannels = const 1
instance NumChannels Double where numChannels = const 1

instance (NumChannels as) => NumChannels (a ::: as) where
    numChannels = const $ 1 + numChannels (Proxy :: Proxy as)

instance (KnownNat n) => NumChannels (Elem n a) where
    numChannels = const $ fromInteger $ natVal (Proxy :: Proxy n)

-- | A 'MatElem' that contains @n@ times an @a@.
--
-- Implementation detail: the @n@ times an @a@ is stored in an unboxed 'VU.Vector'.
newtype Elem (n::Nat) a = Elem
    { getChannels :: VU.Vector a -- ^ Retrieve the channel elements.
    } deriving (Eq, Show)

-- | Handy way of constructing a nested type of ':::'.
--
-- For example: @3 :* Word8 ~ Word8 ::: Word8 ::: Word8@
type family (:*) (n :: Nat) a where
    0 :* a = Void
    1 :* a = a
    n :* a = a ::: ((n-1) :* a)

-- | A 'MatElem` used for small number of channels.
--
-- Use ':*' to construct a ':::'.
data a ::: as = !a ::: !as deriving (Eq, Show)

infixr 5 :::

-- | Internal class used in 'matElem'.
class VU.Unbox (El e) => FromElem n e where
    type El e :: *
    fromElem :: Elem n (El e) -> e
    writeElems :: Proxy n -> VUM.MVector s (El e) -> Int -> e -> ST s ()

instance FromElem 1 Word8  where type El Word8   = Word8 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int8   where type El Int8    = Int8  ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Word16 where type El Word16  = Word16; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int16  where type El Int16   = Int16 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Int32  where type El Int32   = Int32 ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Float  where type El Float   = Float ; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite
instance FromElem 1 Double where type El Double  = Double; fromElem = VU.unsafeHead . getChannels; writeElems = const VUM.unsafeWrite

instance (FromElem (n-1) as, El as ~ a) => FromElem n (a ::: as) where
    type El (a ::: as) = a

    fromElem e = VU.unsafeHead vu ::: fromElem eTail
        where
          vu = getChannels e

          eTail :: Elem (n-1) a
          eTail = Elem $ VU.unsafeTail vu :: Elem (n-1) a

    writeElems _proxy vum !i (x ::: xs) = do
        VUM.unsafeWrite vum i x
        writeElems (Proxy :: Proxy (n-1)) vum (i+1) xs

-- | An isomorphism between 'Elem' and ':*'.
matElem
    :: forall (n :: Nat) e a
     . ( KnownNat n
       , 1 <= n
       , e ~ (n :* a)
       , El e ~ a
       , FromElem n e
       , VU.Unbox a
       )
    => Iso' (Elem n a) e
matElem = iso fromElem toElem
    where
      toElem :: n :* a -> Elem n a
      toElem e = Elem $ VU.create $ do
        let proxyN :: Proxy n
            proxyN = Proxy
            n = fromInteger $ natVal proxyN
        vum <- VUM.new n
        writeElems proxyN vum 0 e
        return vum

instance (Storable a, Storable as) => Storable (a ::: as) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: as)
    alignment _ = alignment (undefined :: a) -- TODO (BvD): is this correct?
    peek ptr = (:::) <$> peek (castPtr ptr)
                     <*> peek (ptr `plusPtr` sizeOf (undefined :: a))
    poke ptr (x ::: xs) = do poke (castPtr ptr) x
                             poke (ptr `plusPtr` sizeOf (undefined :: a)) xs

instance (Storable a, KnownNat n, VUM.Unbox a) => Storable (Elem n a) where
    sizeOf _ = sizeOf (undefined :: a) * fromInteger (natVal (Proxy :: Proxy n))
    alignment _ = alignment (undefined :: a) -- TODO (BvD): is this correct?
    peek ptr = do
      let n = fromInteger (natVal (Proxy :: Proxy n))
      vum <- VUM.new n
      let go !ix
            | ix < n = do x <- peekElemOff (castPtr ptr) ix
                          VUM.unsafeWrite vum ix x
                          go (ix+1)
            | otherwise = VU.unsafeFreeze vum
      Elem <$> go 0
    poke ptr = VU.imapM_ (pokeElemOff (castPtr ptr)) . getChannels

-- | Representation tag for Repa @'Repa.Array's@ for OpenCV @'Mat's@.
data M

-- | Converts an OpenCV @'Mat'rix@ into a Repa array. Returns 'Nothing' if the
-- desired 'Repa.Shape' @sh@ doesn't match the shape of the given matrix.
--
-- This is a zero-copy operation.
toRepa
    :: forall sh e
     . (Repa.Shape sh, MatElem e)
    => Mat
    -> Either String (Repa.Array M sh e)
toRepa mat = unsafePerformIO $ withMatPtr mat $ \matPtr ->
    alloca $ \(flagsPtr    :: Ptr Int32) ->
    alloca $ \(dimsPtr     :: Ptr Int32) ->
    alloca $ \(sizePtr     :: Ptr (Ptr CInt)) ->
    alloca $ \(stepPtr     :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtrPtr  :: Ptr (Ptr CUChar)) -> do
      [CU.block| void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t  * const flagsPtr)          = matPtr->flags;
        *$(int32_t  * const dimsPtr)           = matPtr->dims;
        *$(int    * * const sizePtr)           = matPtr->size.p;
        *$(size_t * * const stepPtr)           = matPtr->step.p;
        *$(unsigned char * * const dataPtrPtr) = matPtr->data;
      }|]
      (depth, channels) <- unmarshalFlags <$> peek flagsPtr
      let expectedDepth    = toMatDepth  (Proxy :: Proxy e)
          expectedChannels = numChannels (Proxy :: Proxy e)
      if depth /= expectedDepth
        then pure $ Left $
               "The expected depth of " <> show expectedDepth <>
               " doesn't equal the actual depth of " <> show depth <> "!"
        else do
          if channels /= expectedChannels
            then pure $ Left $
                   "Expected " <> show expectedChannels <> " channels" <>
                   " but got " <> show channels <> "!"
            else do
              (dims :: Int) <- fromIntegral <$> peek dimsPtr
              let expectedRank :: Int
                  expectedRank = Repa.rank (Repa.zeroDim :: sh)
              if dims /= expectedRank
                then pure $ Left $
                       "The expected rank of " <> show expectedRank <>
                       " doesn't equal the actual number of dimensions " <> show dims <> "!"
                else do
                  (size :: Ptr CInt) <- peek sizePtr
                  sizeShape <- map fromIntegral <$> peekArray dims size

                  (step :: Ptr CSize) <- peek stepPtr
                  stepShape <- map fromIntegral <$> peekArray dims step

                  (dataPtr :: Ptr CUChar) <- peek dataPtrPtr
                  pure $ Right $ Array mat dataPtr sizeShape stepShape

-- | Converts a Repa array back into an OpenCV @'Mat'rix@.
--
-- This is a zero-copy operation.
fromRepa :: Repa.Array M sh e -> Mat
fromRepa (Array mat _ _ _) = mat

repa :: (Repa.Shape sh, MatElem e) => Prism' Mat (Repa.Array M sh e)
repa = prism' fromRepa (either (const Nothing) Just . toRepa)

instance (Repa.Shape sh, Storable e) => NFData (Repa.Array M sh e) where
    rnf a = Repa.deepSeqArray a ()

instance (Storable e) => Repa.Source M e where
    -- TODO (BvD): We might want to check for isContinuous() to optimize certain operations.

    data Array M sh e =
         Array !Mat -- The Mat is kept around so that the data doesn't get garbage collected.
               !(Ptr CUChar) -- Pointer to the data.
               ![Int] -- The shape of the extent which is determined by mat->dims and mat->size.p.
               ![Int] -- The shape of the data which is determined by mat->dims and mat->step.p.

    extent :: (Repa.Shape sh) => Repa.Array M sh e -> sh
    extent (Array _ _ sizeShape _) = Repa.shapeOfList sizeShape

    index :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    index (Array mat dataPtr sizeShape stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith3 mul sizeShape stepShape (Repa.listOfShape ix)

        mul size step i
            | i < size  = step * i
            | otherwise = error $
                "Index " <> show i <> " >= size: " <> show size

    unsafeIndex :: (Repa.Shape sh) => Repa.Array M sh e -> sh -> e
    unsafeIndex (Array mat dataPtr _ stepShape) ix =
        unsafePerformIO $ keepMatAliveDuring mat $ peek elemPtr
      where
        elemPtr :: Ptr e
        elemPtr = dataPtr `plusPtr` offset

        offset :: Int
        offset = sum $ zipWith (*) stepShape (Repa.listOfShape ix)

    linearIndex :: (Repa.Shape sh) => Repa.Array M sh e -> Int -> e
    linearIndex a ix = Repa.index a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    unsafeLinearIndex :: (Repa.Shape sh) => Repa.Array M sh e -> Int -> e
    unsafeLinearIndex a ix = Repa.unsafeIndex a sh
        where
          sh = Repa.fromIndex (Repa.extent a) ix

    deepSeqArray :: (Repa.Shape sh) => Repa.Array M sh e -> b -> b
    deepSeqArray = seq

-- TODO (BvD): Is it possible to define something like the following?
--
-- instance (Storable e) => Repa.Target M e where
--
--     newtype MVec M e = MVec IOMat
--
--     newMVec :: Int -> IO (MVec M e)
--     newMVec size = _todo_newMVec
--
--     unsafeWriteMVec :: MVec M e -> Int -> e -> IO ()
--     unsafeWriteMVec = _todo_unsafeWriteMVec
--
--     unsafeFreezeMVec :: sh  -> MVec M e -> IO (Array M sh e)
--     unsafeFreezeMVec = _todo_unsafeFreezeMVec
--
--     deepSeqMVec :: MVec M e -> a -> a
--     deepSeqMVec = _todo_deepSeqMVec
--
--     touchMVec :: MVec M e -> IO ()
--     touchMVec = _todo_touchMVec
