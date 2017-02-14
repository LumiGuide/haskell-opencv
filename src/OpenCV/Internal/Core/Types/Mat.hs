{-# language CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# language QuasiQuotes #-}
{-# language ConstraintKinds #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 800
{-# options_ghc -Wno-redundant-constraints #-}
#endif

{-# options_ghc -fno-warn-orphans #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Mat
    ( -- * Matrix
      Mat(..)

    , typeCheckMat
    , relaxMat
    , coerceMat
    , unsafeCoerceMat

    , keepMatAliveDuring
    , newEmptyMat
    , newMat
    , withMatData
    , matElemAddress
    , mkMat
    , cloneMat

      -- * Mutable matrix
    , typeCheckMatM
    , relaxMatM
    , coerceMatM
    , unsafeCoerceMatM

    , mkMatM
    , createMat
    , withMatM
    , cloneMatM

      -- * Meta information
    , MatInfo(..)
    , matInfo

    , dimPositions

    , Depth(..)
    , marshalDepth
    , unmarshalDepth
    , marshalFlags
    , unmarshalFlags

    , ShapeT
    , ChannelsT
    , DepthT
    , StaticDepthT

    , ToShape(toShape)
    , ToShapeDS(toShapeDS)
    , ToChannels, toChannels
    , ToChannelsDS, toChannelsDS
    , ToDepth(toDepth)
    , ToDepthDS(toDepthDS)
    ) where

import "base" Control.Monad.ST ( ST )
import "base" Data.Int
import "base" Data.Maybe
import "base" Data.Monoid ( (<>) )
import "base" Data.Proxy
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( allocaArray, peekArray )
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( Storable(..), peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import "base" Unsafe.Coerce ( unsafeCoerce )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.C.PlacementNew.TH
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat.Depth
import "this" OpenCV.Internal.Core.Types.Mat.Marshal
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Generic as VG

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

newtype Mat (shape    :: DS [DS Nat])
            (channels :: DS Nat)
            (depth    :: DS *)
      = Mat {unMat :: ForeignPtr (C (Mat shape channels depth))}

type instance C (Mat shape channels depth) = C'Mat

type instance Mutable (Mat shape channels depth) = Mut (Mat shape channels depth)

instance WithPtr (Mat shape channels depth) where
    withPtr = withForeignPtr . unMat

instance FromPtr (Mat shape channels depth) where
    fromPtr = objFromPtr Mat $ \ptr ->
                [CU.exp| void { delete $(Mat * ptr) }|]

instance FreezeThaw (Mat shape channels depth) where
    freeze = cloneMatM . unMut
    thaw = fmap Mut . cloneMatM

    unsafeFreeze = pure . unMut
    unsafeThaw = pure . Mut

{- | Tests whether a 'Mat' is deserving of its type level attributes

Checks if the properties encoded in the type of a 'Mat' correspond to
the value level representation. For each property that does not hold
this function will produce an error message. If everything checks out
it will produce an empty list.

The following properties are checked:

 * Dimensionality
 * Size of each dimension
 * Number of channels
 * Depth (data type of elements)

If a property is explicitly encoded as statically unknown ('D'ynamic)
it will not be checked.
-}
typeCheckMat
    :: forall shape channels depth
     . ( ToShapeDS    (Proxy shape)
       , ToChannelsDS (Proxy channels)
       , ToDepthDS    (Proxy depth)
       )
    => Mat shape channels depth -- ^ The matrix to be checked.
    -> [CoerceMatError] -- ^ Error messages.
typeCheckMat mat =
       fromMaybe [] (checkShape <$> dsToMaybe dsExpectedShape)
    <> maybeToList (dsToMaybe dsExpectedNumChannels >>= checkNumChannels)
    <> maybeToList (dsToMaybe dsExpectedDepth >>= checkDepth)
  where
    mi = matInfo mat

    dsExpectedShape :: DS [DS Int32]
    dsExpectedShape = toShapeDS (Proxy :: Proxy shape)

    dsExpectedNumChannels :: DS Int32
    dsExpectedNumChannels = toChannelsDS (Proxy :: Proxy channels)

    dsExpectedDepth :: DS Depth
    dsExpectedDepth = toDepthDS (Proxy :: Proxy depth)

    checkShape :: [DS Int32] -> [CoerceMatError]
    checkShape expectedShape = maybe checkSizes (:[]) dimCheck
      where
        dimCheck :: Maybe CoerceMatError
        dimCheck | expectedDim == actualDim = Nothing
                 | otherwise = Just $ ShapeError $ ExpectationError expectedDim actualDim
          where
            expectedDim = length expectedShape
            actualDim = length (miShape mi)

        checkSizes :: [CoerceMatError]
        checkSizes = catMaybes $ zipWith3 checkSize [1..] expectedShape (miShape mi)
          where
            checkSize :: Int -> DS Int32 -> Int32 -> Maybe CoerceMatError
            checkSize dimIx dsExpected actual = dsToMaybe dsExpected >>= \expected ->
                if expected == actual
                then Nothing
                else Just $ SizeError dimIx
                          $ fromIntegral
                            <$> ExpectationError expected actual

    checkNumChannels :: Int32 -> Maybe CoerceMatError
    checkNumChannels expectedNumChannels
        | miChannels mi == expectedNumChannels = Nothing
        | otherwise = Just $ ChannelError
                           $ fromIntegral
                             <$> ExpectationError expectedNumChannels (miChannels mi)

    checkDepth :: Depth -> Maybe CoerceMatError
    checkDepth expectedDepth
        | miDepth mi == expectedDepth = Nothing
        | otherwise = Just $ DepthError
                           $ ExpectationError expectedDepth (miDepth mi)

-- | Relaxes the type level constraints
--
-- Only identical or looser constraints are allowed. For tighter
-- constraints use 'coerceMat'.
--
-- This allows you to \'forget\' type level guarantees for zero
-- cost. Similar to 'unsafeCoerceMat', but totally safe.
--
-- [Identical] @a@ to @b@ with @a ~ b@
-- [Looser]  @(\''S' a)@ to @\''D'@ or @(\''S' a)@ to @(\''S' b)@ with @'MayRelax' a b@
-- [Tighter] @\''D'@ to @(\''S' a)@
relaxMat
    :: ( MayRelax shapeIn    shapeOut
       , MayRelax channelsIn channelsOut
       , MayRelax depthIn    depthOut
       )
    => Mat shapeIn  channelsIn  depthIn  -- ^ Original 'Mat'.
    -> Mat shapeOut channelsOut depthOut -- ^ 'Mat' with relaxed constraints.
relaxMat = unsafeCoerce

coerceMat
    :: ( ToShapeDS    (Proxy shapeOut)
       , ToChannelsDS (Proxy channelsOut)
       , ToDepthDS    (Proxy depthOut)
       )
    => Mat shapeIn channelsIn depthIn -- ^
    -> CvExcept (Mat shapeOut channelsOut depthOut)
coerceMat matIn | null errors = pure matOut
                | otherwise   = throwE $ CoerceMatError errors
  where
    matOut = unsafeCoerceMat matIn
    errors = typeCheckMat matOut

unsafeCoerceMat
    :: Mat shapeIn  channelsIn  depthIn
    -> Mat shapeOut channelsOut depthOut
unsafeCoerceMat = unsafeCoerce

-- | Similar to 'withPtr' in that it keeps the 'ForeignPtr' alive
-- during the execution of the given action but it doesn't extract the 'Ptr'
-- from the 'ForeignPtr'.
keepMatAliveDuring :: Mat shape channels depth -> IO a -> IO a
keepMatAliveDuring mat m = do
    x <- m
    touchForeignPtr $ unMat mat
    pure x

newEmptyMat :: IO (Mat ('S '[]) ('S 1) ('S Word8))
newEmptyMat = unsafeCoerceMat <$> fromPtr [CU.exp|Mat * { new Mat() }|]

-- TODO (RvD): what happens if we construct a mat with more than 4 channels?
-- A scalar is just 4 values. What would be the default value of the 5th channel?
newMat
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       -- , MinLengthDS 2 shape
       -- , 1 .<=? channels
       -- , channels .<=? 512
       -- , 2 <= Length shape
       -- , 1 <= channels
       -- , channels <= 512
       )
    => shape -- ^
    -> channels
    -> depth
    -> scalar
    -> CvExceptT IO (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
newMat shape channels depth defValue = ExceptT $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withVector shape' $ \shapePtr ->
      withPtr (toScalar defValue) $ \scalarPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          *$(Mat * dstPtr) =
            Mat( $(int32_t c'ndims)
               , $(int32_t * shapePtr)
               , $(int32_t c'type)
               , *$(Scalar * scalarPtr)
               );
        |]
  where
    c'ndims = fromIntegral $ VG.length shape'
    c'type  = marshalFlags depth' channels'

    shape'    = toShape shape
    channels' = toChannels channels
    depth'    = toDepth depth

-- TODO (BvD): Move to some Utility module.
withVector
    :: (VG.Vector v a, Storable a)
    => v a -- ^
    -> (Ptr a -> IO b)
    -> IO b
withVector v f =
    allocaArray n $ \ptr ->
      let go !ix
              | ix < n = do
                  pokeElemOff ptr ix (VG.unsafeIndex v ix)
                  go (ix+1)
              | otherwise = f ptr
      in go 0
  where
    n = VG.length v

withMatData
    :: Mat shape channels depth -- ^
    -> ([CSize] -> Ptr Word8 -> IO a)
    -> IO a
withMatData mat f = withPtr mat $ \matPtr ->
    alloca $ \(dimsPtr  :: Ptr Int32      ) ->
    alloca $ \(stepPtr2 :: Ptr (Ptr CSize)) ->
    alloca $ \(dataPtr2 :: Ptr (Ptr Word8)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const dimsPtr ) = matPtr->dims;
        *$(size_t  * * const stepPtr2) = matPtr->step.p;
        *$(uint8_t * * const dataPtr2) = matPtr->data;
      }|]
      dims    <- peek dimsPtr
      stepPtr <- peek stepPtr2
      dataPtr <- peek dataPtr2
      step    <- peekArray (fromIntegral dims) stepPtr
      f step dataPtr

matElemAddress :: Ptr Word8 -> [Int] -> [Int] -> Ptr a
matElemAddress dataPtr step pos = dataPtr `plusPtr` offset
    where
      offset = sum $ zipWith (*) step pos

-- TODO (RvD): check for negative sizes
-- This crashes OpenCV
mkMat
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> CvExcept (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
mkMat shape channels depth defValue =
    unsafeCvExcept $ newMat shape channels depth defValue

cloneMat :: Mat shape channels depth
         -> Mat shape channels depth
cloneMat = unsafePerformIO . cloneMatIO

cloneMatIO :: Mat shape channels depth
           -> IO (Mat shape channels depth)
cloneMatIO mat =
    fmap unsafeCoerceMat $ fromPtr $ withPtr mat $ \matPtr ->
      [C.exp|Mat * { new Mat($(Mat * matPtr)->clone()) }|]

--------------------------------------------------------------------------------
-- Mutable matrix
--------------------------------------------------------------------------------

typeCheckMatM
    :: forall shape channels depth s
     . ( ToShapeDS    (Proxy shape)
       , ToChannelsDS (Proxy channels)
       , ToDepthDS    (Proxy depth)
       )
    => Mut (Mat shape channels depth) s -- ^ The matrix to be checked.
    -> [CoerceMatError] -- ^ Error messages.
typeCheckMatM = typeCheckMat . unMut

relaxMatM
    :: ( MayRelax shapeIn    shapeOut
       , MayRelax channelsIn channelsOut
       , MayRelax depthIn    depthOut
       )
    => Mut (Mat shapeIn  channelsIn  depthIn ) s -- ^ Original 'Mat'.
    -> Mut (Mat shapeOut channelsOut depthOut) s -- ^ 'Mat' with relaxed constraints.
relaxMatM = unsafeCoerce

coerceMatM
    :: ( ToShapeDS    (Proxy shapeOut)
       , ToChannelsDS (Proxy channelsOut)
       , ToDepthDS    (Proxy depthOut)
       )
    => Mut (Mat shapeIn channelsIn depthIn) s -- ^
    -> CvExcept (Mut (Mat shapeOut channelsOut depthOut) s)
coerceMatM = fmap Mut . coerceMat . unMut

unsafeCoerceMatM
    :: Mut (Mat shapeIn  channelsIn  depthIn ) s
    -> Mut (Mat shapeOut channelsOut depthOut) s
unsafeCoerceMatM = unsafeCoerce

-- TODO (RvD): check for negative sizes
-- This crashes OpenCV
mkMatM
    :: ( PrimMonad m
       , ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> CvExceptT m (Mut (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth)) (PrimState m))
mkMatM shape channels depth defValue = do
    mat <- mapExceptT unsafePrimToPrim $ newMat shape channels depth defValue
    unsafeThaw mat

createMat
    :: (forall s. CvExceptT (ST s) (Mut (Mat shape channels depth) s)) -- ^
    -> CvExcept (Mat shape channels depth)
createMat mk = runCvExceptST $ unsafeFreeze =<< mk

withMatM
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> (  forall s
       .  Mut (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth)) (PrimState (ST s))
       -> CvExceptT (ST s) ()
       )
    -> CvExcept (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
withMatM shape channels depth defValue f = createMat $ do
    matM <- mkMatM shape channels depth defValue
    f matM
    pure matM

cloneMatM :: (PrimMonad m)
          => Mat shape channels depth
          -> m (Mat shape channels depth)
cloneMatM = unsafePrimToPrim . cloneMatIO


--------------------------------------------------------------------------------
-- Meta information
--------------------------------------------------------------------------------

data MatInfo
   = MatInfo
     { miShape    :: ![Int32]
     , miDepth    :: !Depth
     , miChannels :: !Int32
     }
     deriving (Show, Eq)

matInfo :: Mat shape channels depth -> MatInfo
matInfo mat = unsafePerformIO $
    withPtr mat $ \matPtr ->
    alloca $ \(flagsPtr :: Ptr Int32) ->
    alloca $ \(dimsPtr  :: Ptr Int32) ->
    alloca $ \(sizePtr  :: Ptr (Ptr Int32)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const flagsPtr) = matPtr->flags;
        *$(int32_t *   const dimsPtr ) = matPtr->dims;
        *$(int32_t * * const sizePtr ) = matPtr->size.p;
      }|]
      (depth, channels) <- unmarshalFlags <$> peek flagsPtr
      dims <- peek dimsPtr
      size <- peek sizePtr
      shape <- peekArray (fromIntegral dims) size
      pure MatInfo
           { miShape    = shape
           , miDepth    = depth
           , miChannels = channels
           }

-- | All possible positions (indexes) for a given shape (list of
-- sizes per dimension).
--
-- @
-- dimPositions [3, 4]
-- [ [0, 0], [0, 1], [0, 2], [0, 3]
-- , [1, 0], [1, 1], [1, 2], [1, 3]
-- , [2, 0], [2, 1], [2, 2], [2, 3]
-- ]
-- @
dimPositions :: (Num a, Enum a) => [a] -> [[a]]
dimPositions = traverse (enumFromTo 0 . pred)

--------------------------------------------------------------------------------

type family ShapeT (a :: ka) :: DS [DS Nat] where
    ShapeT [Int32]          = 'D
    ShapeT (V.Vector Int32) = 'D
    ShapeT (x ::: xs)       = 'S (DSNats (x ::: xs))
    ShapeT (xs :: [Nat])    = 'S (DSNats xs)
    ShapeT (Proxy a)        = ShapeT a

type ChannelsT a = DSNat a

--------------------------------------------------------------------------------

class ToShape a where
    toShape :: a -> V.Vector Int32

-- | identity
instance ToShape (V.Vector Int32) where
    toShape = id

-- | direct conversion to 'V.Vector'
instance ToShape [Int32] where
    toShape = V.fromList

-- | empty 'V.Vector'
instance ToShape (Proxy '[]) where
    toShape _proxy = V.empty

-- | fold over the type level list
instance (ToInt32 (Proxy a), ToShape (Proxy as))
      => ToShape (Proxy (a ': as)) where
    toShape _proxy =
        V.cons
          (toInt32 (Proxy :: Proxy a))
          (toShape (Proxy :: Proxy as))

-- | empty 'V.Vector'
instance ToShape Z where
    toShape Z = V.empty

-- | fold over ':::'
instance (ToInt32 a, ToShape as) => ToShape (a ::: as) where
    toShape (a ::: as) = V.cons (toInt32 a) (toShape as)

--------------------------------------------------------------------------------

class ToShapeDS a where
    toShapeDS :: a -> DS [DS Int32]

instance ToShapeDS (proxy 'D) where
    toShapeDS _proxy = D

instance (ToNatListDS (Proxy as)) => ToShapeDS (Proxy ('S as)) where
    toShapeDS _proxy = S $ toNatListDS (Proxy :: Proxy as)

--------------------------------------------------------------------------------

type ToChannels a = ToInt32 a

toChannels :: (ToInt32 a) => a -> Int32
toChannels = toInt32

type ToChannelsDS a = ToNatDS a

toChannelsDS :: (ToChannelsDS a) => a -> DS Int32
toChannelsDS = toNatDS

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Mat
