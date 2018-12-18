{-# language ConstraintKinds #-}
{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language QuasiQuotes #-}
{-# language RankNTypes #-}
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
    , checkMatShape
    , checkMatChannels
    , checkMatDefValue
    , relaxMat
    , coerceMat
    , unsafeCoerceMat

    , keepMatAliveDuring
    , newEmptyMat
    , newMat
    , withMatData
    , matToVec
    , vecToMat
    , unsafeWithMatAsVec
    , unsafeWithVecAsMat
    , matElemAddress
    , mkMat
    , cloneMat
    , isContinuous

      -- * Mutable matrix
    , typeCheckMatM
    , relaxMatM
    , coerceMatM
    , unsafeCoerceMatM

    , mkMatM
    , createMat
    , withMatM
    , cloneMatM
    , deallocateMatM

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
    , ToChannels(toChannels)
    , ToChannelsDS(toChannelsDS)
    , ToDepth(toDepth)
    , ToDepthDS(toDepthDS)

    , ValidDimensions
    , ValidDimensions'
    , ValidChannels
    , ValidChannels'
    ) where

import "base" Control.Exception ( throwIO )
import "base" Control.Monad ( when )
import "base" Control.Monad.IO.Class
import "base" Control.Monad.ST ( ST )
import "base" Data.Foldable ( traverse_ )
import "base" Data.Int
import "base" Data.Kind ( Constraint )
import qualified "base" Data.List.NonEmpty as NE
import "base" Data.Maybe
import "base" Data.Monoid ( (<>) )
import "base" Data.Proxy
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr
    ( ForeignPtr, withForeignPtr, touchForeignPtr, newForeignPtr_ )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( allocaArray, peekArray )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Ptr ( Ptr, plusPtr, castPtr )
import "base" Foreign.Storable ( Storable(..), peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import "base" Unsafe.Coerce ( unsafeCoerce )
import "deepseq" Control.DeepSeq ( NFData(..) )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear ( V4 )
import "mtl" Control.Monad.Error.Class ( MonadError, throwError )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.C.FinalizerTH
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
import qualified "vector" Data.Vector.Storable as VS
import qualified "vector" Data.Vector.Generic as VG

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

{- | n-dimensional dense array

The class Mat represents an n-dimensional dense numerical
single-channel or multi-channel array. It can be used to store real or
complex-valued vectors and matrices, grayscale or color images, voxel
volumes, vector fields, point clouds, tensors or histograms.

The Mat type has 3 type parameters that describe properties of the
matrix: __shape__, __channels__ and __depth__.

  [@shape@] The number of dimensions and the size of each
  dimension. Images are 2-dimensional.

  [@channels@] The number of channels. An RGB color image has 3
  channels. A grayscale image has 1 channel.

  [@depth@] The type of the elements of the matrix. In an RGB image a
  single pixel would be 3 individual elements of type 'Word8' (a
  single byte). But if you calculate a derivative of an image the
  @depth@ of the result would probably be a 'Float' or a 'Double'.

Looking at the kinds of these parameters you will notice that
everything is wrapped in the 'DS' kind. This kind allows you to choose
how much information is fixed at compile time. A type wrapped in ''S'
is __s__tatically known and can be reasoned about. Like a depth of
''S' 'Word8'. The opposite is the ''D' type, which means some
information is __d__ynamically known. A depth of ''D' can not be
reasoned about at compile time. Another way to think about 'DS' is
that it is basically a 'Maybe'. See also the 'dsToMaybe' function.

Example matrix:

@
  Mat ('S '[ 'S 1080, 'S 1920 ]) ('S 3) ('S Word8)
@

A 1920x1080 3 channel image where each element is a single byte.
-}
newtype Mat (shape    :: DS [DS Nat])
            (channels :: DS Nat)
            (depth    :: DS *)
      = Mat {unMat :: ForeignPtr (C (Mat shape channels depth))}

type instance C (Mat shape channels depth) = C'Mat

type instance Mutable (Mat shape channels depth) = Mut (Mat shape channels depth)

instance WithPtr (Mat shape channels depth) where
    withPtr = withForeignPtr . unMat

mkFinalizer DeletePtr "deleteMat" "cv::Mat" ''C'Mat

instance FromPtr (Mat shape channels depth) where
    fromPtr = objFromPtr Mat deleteMat

instance FreezeThaw (Mat shape channels depth) where
    freeze = cloneMatM . unMut
    thaw = fmap Mut . cloneMatM

    unsafeFreeze = pure . unMut
    unsafeThaw = pure . Mut

instance NFData (Mat shape channels depth) where
    rnf (Mat !_fptr) = ()

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

checkMatShape
    :: forall m
     . (Applicative m, MonadError CvException m) => V.Vector Int32 -> m ()
checkMatShape shape = do
    traverse_ checkDim shape
    when (V.length shape < 0 || V.length shape > 32) $
      throwError $ CvException $
        "d == 0 || (d >= 2 && d <= 32), but number of length of shape is " ++ show (V.length shape)
    when (V.length shape == 1) $
      throwError $ CvException $
        "1 dimensional mats are not allowed, please add another dimension of size 1"
  where
    checkDim :: Int32 -> m ()
    checkDim dim =
        when (dim < 1) $
          throwError $ CvException $ "invalid dimension size: " ++ show dim

checkMatChannels :: (Applicative m, MonadError CvException m) => Int32 -> m ()
checkMatChannels channels =
    when (channels < 1 || channels > 512) $
      throwError $ CvException $ "invalid number of channels: " ++ show channels

checkMatDefValue :: (Applicative m, MonadError CvException m) => Int32 -> Scalar -> m ()
checkMatDefValue channels defValue =
    when (channels > 4 && fromScalar defValue /= (0 :: V4 Double)) $
      throwError $ CvException $ "with more than 4 channels the default value must be 0"

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
       , MonadError CvException m
       )
    => Mat shapeIn channelsIn depthIn -- ^
    -> m (Mat shapeOut channelsOut depthOut)
coerceMat matIn =
    case NE.nonEmpty errors of
      Nothing -> pure matOut
      Just neErrors -> throwError $ CoerceMatError neErrors
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

newEmptyMat :: MonadIO m => m (Mat ('S '[]) ('S 1) ('S Word8))
newEmptyMat = liftIO ( unsafeCoerceMat <$> fromPtr [CU.exp|Mat * { new Mat() }|] )

newMat
    :: forall shape channels depth scalar m
     . ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       , MonadError CvException m
       , MonadIO m
       )
    => shape -- ^
    -> channels
    -> depth
    -> scalar
    -> m (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
newMat shape channels depth defValue = do
    checkMatShape shape'
    checkMatChannels channels'
    checkMatDefValue channels' defValue'
    wrapException $ liftIO $ do
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
    defValue' = toScalar defValue

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

-- | Convert a Mat to a Storable Vector.
matToVec
    :: (Storable depth)
    => Mat shape channels ('S depth)
    -> VS.Vector depth
matToVec mat =
    unsafePerformIO $
    unsafeWithMatAsVec mat $ \tmpVec ->
      VS.thaw tmpVec >>= VS.unsafeFreeze

-- | Convert a Storable Vector to a Mat.
--
-- The @shape@ and @channels@ of the Mat must exactly match the
-- length of the vector.
vecToMat
    :: ( Storable depth
       , ToShape shape
       , ToChannels channels
       , ToDepth (Proxy depth)
       , MonadError CvException m
       )
    => shape
    -> channels
    -> VS.Vector depth
    -> m (Mat (ShapeT shape) (ChannelsT channels) ('S depth))
vecToMat shape channels vec =
    unsafeCvExcept $
    unsafeWithVecAsMat shape channels vec $ \tmpMat ->
      liftIO $ cloneMatIO tmpMat

-- | Access a Mat's data via a temporary Storable Vector.
--
-- Data is only copied when the Mat is not continuous.
--
-- The storable vector and its data may no longer be used after the
-- supplied computation terminates.
unsafeWithMatAsVec
    :: forall a shape channels depth. (Storable depth)
    => Mat shape channels ('S depth)
    -> (VS.Vector depth -> IO a)
       -- ^ A computation to perform on the vector.
    -> IO a
unsafeWithMatAsVec mat f =
    withMatData continuousMat $ \_step dataPtr -> do
      foreignDataPtr :: ForeignPtr depth <- newForeignPtr_ $ castPtr dataPtr
      f $ VS.unsafeFromForeignPtr0 foreignDataPtr numElems
  where
    numElems = fromIntegral $ product $ miChannels i : miShape i
      where
        i = matInfo continuousMat

    continuousMat
        | isContinuous mat = mat
        | otherwise = cloneMat mat

-- | Create a temporary Mat from a Storable Vector's data.
--
-- The @shape@ and @channels@ of the Mat must exactly match the
-- length of the vector.
--
-- No data is copied.
--
-- The Mat and its data may no longer be used after the supplied
-- computation terminates.
unsafeWithVecAsMat
    :: forall a shape channels depth m
     . ( Storable depth
       , ToShape shape
       , ToChannels channels
       , ToDepth (Proxy depth)
       , MonadError CvException m
       , MonadIO m
       )
    => shape
    -> channels
    -> VS.Vector depth
    -> (Mat (ShapeT shape) (ChannelsT channels) ('S depth) -> ExceptT CvException IO a)
    -> m a
unsafeWithVecAsMat shape channels vec f = do
    checkMatShape shape'
    checkMatChannels channels'
    checkVec
    unsafeWrapException $ VS.unsafeWith vec $ \vecPtr ->
      withVector shape' $ \shapePtr -> do
        let dataPtr :: Ptr Word8
            dataPtr = castPtr vecPtr
        dst <- unsafeCoerceMat <$> newEmptyMat
        result <- handleCvException (pure ()) $
          withPtr dst $ \dstPtr ->
            [cvExcept|
              *$(Mat * dstPtr) =
                Mat( $(int32_t c'ndims)
                   , $(int32_t * shapePtr)
                   , $(int32_t c'type)
                   , (void *)$(uint8_t * dataPtr)
                   );
            |]
        case result of
          Left err -> pure $ Left err
          Right () -> runExceptT $ f dst
  where
    checkVec =
        when (VS.length vec /= requiredVecLength) $
          throwError $ CvException $ concat
            [ "Mat requires a vector of length "
            , show requiredVecLength
            , ", but supplied vector has length "
            , show $ VS.length vec
            ]

    requiredVecLength :: Int
    requiredVecLength = fromIntegral $ channels' * product shape'

    c'ndims = fromIntegral $ VG.length shape'
    c'type  = marshalFlags depth' channels'

    shape'    = toShape shape
    channels' = toChannels channels
    depth'    = toDepth (Proxy :: Proxy depth)

matElemAddress :: Ptr Word8 -> [Int] -> [Int] -> Ptr a
matElemAddress dataPtr step pos = dataPtr `plusPtr` offset
    where
      offset = sum $ zipWith (*) step pos

mkMat
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       , MonadError CvException m
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> m (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
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

isContinuous :: Mat shape channels depth -> Bool
isContinuous mat = toBool $ unsafePerformIO $
    withPtr mat $ \matPtr ->
      [CU.exp| bool { $(Mat * matPtr)->isContinuous() } |]

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
       , MonadError CvException m
       )
    => Mut (Mat shapeIn channelsIn depthIn) s -- ^
    -> m (Mut (Mat shapeOut channelsOut depthOut) s)
coerceMatM = fmap Mut . coerceMat . unMut

unsafeCoerceMatM
    :: Mut (Mat shapeIn  channelsIn  depthIn ) s
    -> Mut (Mat shapeOut channelsOut depthOut) s
unsafeCoerceMatM = unsafeCoerce

-- TODO (RvD): check for negative sizes
mkMatM
    :: ( PrimMonad m
       , ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       , MonadError CvException m
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> m (Mut (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth)) (PrimState m))
mkMatM shape channels depth defValue = do
    mat <-
      runExceptT (exceptTFromIO (newMat shape channels depth defValue))
        >>= either throwError return
    unsafeThaw mat
  where
    exceptTFromIO :: PrimMonad m => ExceptT e IO a -> ExceptT e m a
    exceptTFromIO = mapExceptT unsafePrimToPrim

createMat
    :: MonadError CvException m
    => (forall s. ExceptT CvException (ST s) (Mut (Mat shape channels depth) s)) -- ^
    -> m (Mat shape channels depth)
createMat mk = runCvExceptST ( unsafeFreeze =<< mk )

withMatM
    :: ( ToShape    shape
       , ToChannels channels
       , ToDepth    depth
       , ToScalar   scalar
       , MonadError CvException m
       )
    => shape    -- ^
    -> channels -- ^
    -> depth    -- ^
    -> scalar   -- ^
    -> (  forall s
       .  Mut (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth)) (PrimState (ST s))
       -> ExceptT CvException (ST s) ()
       )
    -> m (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
withMatM shape channels depth defValue f = createMat $ do
    matM <- mkMatM shape channels depth defValue
    f matM
    pure matM

cloneMatM :: (PrimMonad m)
          => Mat shape channels depth
          -> m (Mat shape channels depth)
cloneMatM = unsafePrimToPrim . cloneMatIO

-- | Deallocates the matrix data.
--
-- Highly unsafe. Subsequent operations that need the data will
-- generate exceptions (or segfaults).
deallocateMatM
    :: (PrimMonad m)
    => Mut (Mat shape channels depth) (PrimState m)
    -> m ()
deallocateMatM mutMat = unsafePrimToPrim $ do
    e <- handleCvException (pure ()) $
           withPtr mutMat $ \mutMatPtr ->
             [cvExcept| $(Mat * mutMatPtr)->deallocate(); |]
    either throwIO pure e

--------------------------------------------------------------------------------
-- Meta information
--------------------------------------------------------------------------------

data MatInfo
   = MatInfo
     { miShape    :: ![Int32]
     , miDepth    :: !Depth
     , miChannels :: !Int32
     } deriving (Show, Eq)

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
    ShapeT (xs :: [DS Nat]) = 'S (DSNats xs)
    ShapeT ('S a)           = ShapeT a
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
instance (ValidDimensions 0) => ToShape (Proxy '[]) where
    toShape _proxy = V.empty

-- | fold over the type level list
instance ( ValidDimensions (Length (a ': as))
         , ToInt32 (Proxy a)
         , ToUncheckedShape (Proxy (a ': as))
         )
      => ToShape (Proxy (a ': as)) where
    toShape proxy = toUncheckedShape proxy

-- | empty 'V.Vector'
instance (ValidDimensions 0) => ToShape Z where
    toShape Z = V.empty

-- | fold over ':::'
instance ( dims ~ Length (a ::: as)
         , ValidDimensions dims
         , ToInt32 a
         , ToUncheckedShape as
         ) => ToShape (a ::: as) where
    toShape (a ::: as) = toUncheckedShape (a ::: as)

-- | strip away 'S'
instance (ToShape (Proxy a)) => ToShape (Proxy ('S a)) where
    toShape _proxy = toShape (Proxy :: Proxy a)

--------------------------------------------------------------------------------

-- | Helper for 'ToShape'.
class ToUncheckedShape a where
    toUncheckedShape :: a -> V.Vector Int32

-- | empty 'V.Vector'
instance ToUncheckedShape (Proxy '[]) where
    toUncheckedShape _proxy = V.empty

-- | fold over the type level list
instance (ToInt32 (Proxy a), ToUncheckedShape (Proxy as))
      => ToUncheckedShape (Proxy (a ': as)) where
    toUncheckedShape _proxy =
        V.cons
          (toInt32          (Proxy :: Proxy a ))
          (toUncheckedShape (Proxy :: Proxy as))

-- | empty 'V.Vector'
instance ToUncheckedShape Z where
    toUncheckedShape Z = V.empty

-- | fold over ':::'
instance (ToInt32 a, ToUncheckedShape as) => ToUncheckedShape (a ::: as) where
    toUncheckedShape (a ::: as) = V.cons (toInt32 a) (toUncheckedShape as)

--------------------------------------------------------------------------------

-- | Constraint which expresses a valid number of dimensions for a
-- 'Mat'.
--
-- Number of dimensions must be 0 or [2..32].
type ValidDimensions dims = ValidDimensions' dims (CmpNat dims 33)

-- | 'ValidDimensions' helper which produces custom type errors.
type family ValidDimensions' (dims :: Nat) (cmpMax :: Ordering) :: Constraint where
    ValidDimensions' 0 _ = ()
    ValidDimensions' 1 _ =
        TypeError ('Text "Mat may not be 1-dimensional. Please add another dimension of size 1")
    ValidDimensions' _ 'LT = ()
    ValidDimensions' n _ =
        TypeError ('Text "Mat has too many dimensions: " ':<>: 'ShowType n ':<>: 'Text " > 32")

--------------------------------------------------------------------------------

class ToShapeDS a where
    toShapeDS :: a -> DS [DS Int32]

instance ToShapeDS (proxy 'D) where
    toShapeDS _proxy = D

instance (ToNatListDS (Proxy as)) => ToShapeDS (Proxy ('S as)) where
    toShapeDS _proxy = S $ toNatListDS (Proxy :: Proxy as)

--------------------------------------------------------------------------------

-- | Conversions to number of channels.
class ToChannels a where
    toChannels :: a -> Int32

-- | value level: identity
instance ToChannels Int32 where
    toChannels = id

-- | type level: reify the known natural number @n@
instance (KnownNat n, ValidChannels n) => ToChannels (proxy n) where
    toChannels = fromInteger . natVal

-- | strip away 'S'
instance (ToChannels (Proxy n)) => ToChannels (proxy ('S n)) where
    toChannels _proxy = toChannels (Proxy :: Proxy n)

--------------------------------------------------------------------------------

-- | Constraint which expresses a valid number of channels for a
-- 'Mat'.
--
-- Number of channels must be [1..512].
type ValidChannels channels = ValidChannels' channels (CmpNat channels 513)

-- | 'ValidChannels' helper which produces custom type errors.
type family ValidChannels' (channels :: Nat) (cmpMax :: Ordering) :: Constraint where
    ValidChannels' 0 _ = TypeError ('Text "Mat must have at least 1 channel")
    ValidChannels' _ 'LT = ()
    ValidChannels' n _ =
        TypeError ('Text "Mat has too many channels: " ':<>: 'ShowType n ':<>: 'Text " > 512")

--------------------------------------------------------------------------------

-- | Type level to value level conversion of numbers of channels that
-- are either 'D'ynamically or 'S'tatically known.
--
-- > toChannelsDS (Proxy ('S 42)) == S 42
-- > toChannelsDS (Proxy 'D) == D
class ToChannelsDS a where
    toChannelsDS :: a -> DS Int32

-- | value level numbers are dynamically known
instance ToChannelsDS (proxy 'D) where
    toChannelsDS _proxy = D

-- | type level numbers are statically known
instance (ToChannels (Proxy n)) => ToChannelsDS (Proxy ('S n)) where
    toChannelsDS _proxy = S $ toChannels (Proxy :: Proxy n)

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Mat
