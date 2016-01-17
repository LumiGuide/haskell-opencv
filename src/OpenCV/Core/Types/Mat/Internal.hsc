{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenCV.Core.Types.Mat.Internal
    where
    -- ( typeCheckMat
    -- , coerceMat
    -- , unsafeCoerceMat
    -- , unsafeCoerceMatM

    -- , Mat(..)
    -- , MutMat(..)
    -- , MatDepth(..)
    -- , matFromPtr
    -- , withMatPtr
    -- , withMbMatPtr
    -- , keepMatAliveDuring
    -- , newEmptyMat
    -- , newMat
    -- , newMatDyn
    -- , marshalMatDepth
    -- , marshalFlags
    -- , unmarshalDepth
    -- , unmarshalFlags
    -- , withMatData
    -- , matElemAddress

    -- , MatInfo(..)
    -- , matInfo
    -- ) where

import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( allocaArray, peekArray )
import "base" Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import "base" Foreign.Storable ( Storable(..), peek )
import "base" GHC.TypeLits
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Internal
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Generic as VG


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

data MatDepth =
     MatDepth_8U
   | MatDepth_8S
   | MatDepth_16U
   | MatDepth_16S
   | MatDepth_32S
   | MatDepth_32F
   | MatDepth_64F
   | MatDepth_USRTYPE1
     deriving (Show, Eq)

instance Convert (Proxy Word8 ) MatDepth where convert _p = MatDepth_8U
instance Convert (Proxy Int8  ) MatDepth where convert _p = MatDepth_8S
instance Convert (Proxy Word16) MatDepth where convert _p = MatDepth_16U
instance Convert (Proxy Int16 ) MatDepth where convert _p = MatDepth_16S
instance Convert (Proxy Int32 ) MatDepth where convert _p = MatDepth_32S
instance Convert (Proxy Float ) MatDepth where convert _p = MatDepth_32F
instance Convert (Proxy Double) MatDepth where convert _p = MatDepth_64F
-- TODO (BvD): instance ToMatDepth ? where toMatDepth = const MatDepth_USRTYPE1
-- RvD: perhaps ByteString? Or a fixed size (statically) vector of bytes

type family DepthT a :: DS * where
    DepthT MatDepth = 'D
    DepthT (Proxy d) = 'S d

--------------------------------------------------------------------------------

#num CV_CN_MAX
#num CV_CN_SHIFT

#num CV_8U
#num CV_8S
#num CV_16U
#num CV_16S
#num CV_32S
#num CV_32F
#num CV_64F
#num CV_USRTYPE1

#num CV_MAT_DEPTH_MASK

marshalMatDepth :: MatDepth -> Int32
marshalMatDepth = \case
    MatDepth_8U       -> c'CV_8U
    MatDepth_8S       -> c'CV_8S
    MatDepth_16U      -> c'CV_16U
    MatDepth_16S      -> c'CV_16S
    MatDepth_32S      -> c'CV_32S
    MatDepth_32F      -> c'CV_32F
    MatDepth_64F      -> c'CV_64F
    MatDepth_USRTYPE1 -> c'CV_USRTYPE1

marshalFlags
    :: MatDepth
    -> Int32 -- ^ Number of channels
    -> Int32
marshalFlags depth cn =
    marshalMatDepth depth
      .|. ((cn - 1) `unsafeShiftL` c'CV_CN_SHIFT)

unmarshalDepth :: Int32 -> MatDepth
unmarshalDepth n
    | n == c'CV_8U       = MatDepth_8U
    | n == c'CV_8S       = MatDepth_8S
    | n == c'CV_16U      = MatDepth_16U
    | n == c'CV_16S      = MatDepth_16S
    | n == c'CV_32S      = MatDepth_32S
    | n == c'CV_32F      = MatDepth_32F
    | n == c'CV_64F      = MatDepth_64F
    | n == c'CV_USRTYPE1 = MatDepth_USRTYPE1
    | otherwise          = error $ "unknown depth " <> show n

unmarshalFlags :: Int32 -> (MatDepth, Int32)
unmarshalFlags n =
    ( unmarshalDepth $ n .&. c'CV_MAT_DEPTH_MASK
    , 1 + ((n `unsafeShiftR` c'CV_CN_SHIFT) .&. (c'CV_CN_MAX - 1))
    )

--------------------------------------------------------------------------------

newtype Mat (shape    :: DS [DS Nat])
            (channels :: DS Nat)
            (depth    :: DS *)
      = Mat {unMat :: ForeignPtr C'Mat}

newtype MutMat (shape    :: DS [DS Nat])
               (channels :: DS Nat)
               (depth    :: DS *)
               (s        :: *)
      = MutMat {unMutMat :: Mat shape channels depth}

--------------------------------------------------------------------------------

-- | Tests whether a Mat is deserving of its type level attributes
typeCheckMat
    :: forall (shape    :: DS [DS Nat])
              (channels :: DS Nat)
              (depth    :: DS *)
     . ( Convert (Proxy shape)    (DS [DS Int32])
       , Convert (Proxy channels) (DS Int32)
       , Convert (Proxy depth)    (DS MatDepth)
       )
    => Mat shape channels depth -> [String]
typeCheckMat mat =
       fromMaybe [] (checkShape <$> dsToMaybe dsExpectedShape)
    <> maybeToList (dsToMaybe dsExpectedNumChannels >>= checkNumChannels)
    <> maybeToList (dsToMaybe dsExpectedDepth >>= checkDepth)
  where
    mi = matInfo mat

    dsExpectedShape :: DS [DS Int32]
    dsExpectedShape = convert (Proxy :: Proxy shape)

    dsExpectedNumChannels :: DS Int32
    dsExpectedNumChannels = convert (Proxy :: Proxy channels)

    dsExpectedDepth :: DS MatDepth
    dsExpectedDepth = convert (Proxy :: Proxy depth)

    checkShape :: [DS Int32] -> [String]
    checkShape expectedShape = maybe checkSizes (:[]) dimCheck
      where
        dimCheck :: Maybe String
        dimCheck | expectedDim == actualDim = Nothing
                 | otherwise =
                     Just $ "Expected dimension "
                            <> show expectedDim
                            <> " doesn't equal actual dimension "
                            <> show actualDim
          where
            expectedDim = length expectedShape
            actualDim = length (miShape mi)

        checkSizes :: [String]
        checkSizes = catMaybes $ zipWith3 checkSize [1..] expectedShape (miShape mi)
          where
            checkSize :: Int -> DS Int32 -> Int32 -> Maybe String
            checkSize dimIx dsExpected actual = dsToMaybe dsExpected >>= \expected ->
                if expected == actual
                then Nothing
                else Just $ "Expected size "
                            <> show expected
                            <> " doesn't equal actual size "
                            <> show actual
                            <> " for the "
                            <> show dimIx
                            <> (if dimIx == 2 then "nd" else "th")
                            <> " dimension"

    checkNumChannels :: Int32 -> Maybe String
    checkNumChannels expectedNumChannels
        | miChannels mi == expectedNumChannels = Nothing
        | otherwise = Just $ "Expected number of channels "
                             <> show expectedNumChannels
                             <> " doesn't equal actual number of channels "
                             <> show (miChannels mi)

    checkDepth :: MatDepth -> Maybe String
    checkDepth expectedDepth
        | miDepth mi == expectedDepth = Nothing
        | otherwise = Just $ "Expected depth "
                             <> show expectedDepth
                             <> " doesn't equal actual depth "
                             <> show (miDepth mi)


coerceMat
    :: ( Convert (Proxy shapeOut)    (DS [DS Int32])
       , Convert (Proxy channelsOut) (DS Int32)
       , Convert (Proxy depthOut)    (DS MatDepth)
       )
    => Mat shapeIn channelsIn depthIn -- ^
    -> Either [String] (Mat shapeOut channelsOut depthOut)
coerceMat matIn | null errors = Right matOut
                | otherwise   = Left errors
  where
    matOut = unsafeCoerceMat matIn
    errors = typeCheckMat matOut

unsafeCoerceMat
    :: Mat shapeIn  channelsIn  depthIn
    -> Mat shapeOut channelsOut depthOut
unsafeCoerceMat = unsafeCoerce

unsafeCoerceMatM
    :: MutMat shapeIn  channelsIn  depthIn  s
    -> MutMat shapeOut channelsOut depthOut s
unsafeCoerceMatM = unsafeCoerce

--------------------------------------------------------------------------------

matFromPtr :: IO (Ptr C'Mat) -> IO (Mat 'D 'D 'D)
matFromPtr = objFromPtr Mat $ \ptr -> [CU.exp| void { delete $(Mat * ptr) }|]

withMatPtr :: Mat shape channels depth -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

withMbMatPtr :: Maybe (Mat shape channels depth) -> (Ptr C'Mat -> IO a) -> IO a
withMbMatPtr mbMat f =
    case mbMat of
      Just mat -> withMatPtr mat f
      Nothing  -> f nullPtr

-- | Similar to 'withMatPtr' in that it keeps the 'ForeignPtr' alive
-- during the execution of the given action but it doesn't extract the 'Ptr'
-- from the 'ForeignPtr'.
keepMatAliveDuring :: Mat shape channels depth -> IO a -> IO a
keepMatAliveDuring mat m = do
    x <- m
    touchForeignPtr $ unMat mat
    pure x

newEmptyMat :: IO (Mat 'D 'D 'D)
newEmptyMat = matFromPtr [CU.exp|Mat * { new Mat() }|]

-- TODO (RvD): what happens if we construct a mat with more than 4 channels?
-- A scalar is just 4 values. What would be the default value of the 5th channel?
newMat
    :: ( Convert shape    (V.Vector Int32)
       , Convert channels Int32
       , Convert depth    MatDepth
       , ToScalar scalar
       -- , MinLengthDS 2 shape
       -- , 1 .<=? channels
       -- , channels .<=? 512
       -- , 2 <= Length shape
       -- , 1 <= channels
       -- , channels <= 512
       )
    => shape
    -> channels
    -> depth
    -> scalar
    -> IO (Mat (ShapeT shape) (ChannelsT channels) (DepthT depth))
newMat shape channels depth defValue = fmap unsafeCoerceMat $
    withVector shape' $ \shapePtr ->
    withScalarPtr defValue $ \scalarPtr ->
      matFromPtr [CU.exp|Mat * {
        new Mat( $(int32_t c'ndims)
               , $(int32_t * shapePtr)
               , $(int32_t c'type)
               , *$(Scalar * scalarPtr)
               )
      }|]
  where
    c'ndims = fromIntegral $ VG.length shape'
    c'type  = marshalFlags depth' channels'

    shape' :: V.Vector Int32
    shape'    = convert shape
    channels' = convert channels
    depth'    = convert depth


-- TODO (BvD): Move to some Utility module.
withVector
    :: (VG.Vector v a, Storable a)
    => v a
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

--------------------------------------------------------------------------------

withMatData
    :: Mat shape channels depth
    -> ([CSize] -> Ptr Word8 -> IO a)
    -> IO a
withMatData mat f = withMatPtr mat $ \matPtr ->
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

--------------------------------------------------------------------------------

data MatInfo
   = MatInfo
     { miShape    :: ![Int32]
     , miDepth    :: !MatDepth
     , miChannels :: !Int32
     }
     deriving (Show, Eq)

matInfo :: Mat shape channels depth -> MatInfo
matInfo mat = unsafePerformIO $
    withMatPtr mat $ \matPtr ->
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
