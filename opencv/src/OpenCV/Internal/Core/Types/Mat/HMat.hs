{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Mat.HMat
    ( HMat(..)
    , HElems(..)
    , hElemsDepth
    , hElemsLength
    , ToHElems(toHElems)

    , matToHMat
    , hMatToMat
    ) where

import "base" Data.Foldable
import "base" Data.Int
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( Storable(..), peekElemOff, pokeElemOff )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "bytestring" Data.ByteString as B
import "linear" Linear.Vector ( zero )
import "linear" Linear.V4 ( V4(..) )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Generic as VG
import qualified "vector" Data.Vector.Unboxed as VU
import qualified "vector" Data.Vector.Unboxed.Mutable as VUM

--------------------------------------------------------------------------------

data HMat
   = HMat
     { hmShape    :: ![Int32]
     , hmChannels :: !Int32
     , hmElems    :: !HElems
     } deriving (Show, Eq)

data HElems
   = HElems_8U       !(VU.Vector Word8)
   | HElems_8S       !(VU.Vector Int8)
   | HElems_16U      !(VU.Vector Word16)
   | HElems_16S      !(VU.Vector Int16)
   | HElems_32S      !(VU.Vector Int32)
   | HElems_32F      !(VU.Vector Float)
   | HElems_64F      !(VU.Vector Double)
   | HElems_USRTYPE1 !(V.Vector B.ByteString)
     deriving (Show, Eq)

hElemsDepth :: HElems -> Depth
hElemsDepth = \case
    HElems_8U       _v -> Depth_8U
    HElems_8S       _v -> Depth_8S
    HElems_16U      _v -> Depth_16U
    HElems_16S      _v -> Depth_16S
    HElems_32S      _v -> Depth_32S
    HElems_32F      _v -> Depth_32F
    HElems_64F      _v -> Depth_64F
    HElems_USRTYPE1 _v -> Depth_USRTYPE1

hElemsLength :: HElems -> Int
hElemsLength = \case
    HElems_8U       v -> VG.length v
    HElems_8S       v -> VG.length v
    HElems_16U      v -> VG.length v
    HElems_16S      v -> VG.length v
    HElems_32S      v -> VG.length v
    HElems_32F      v -> VG.length v
    HElems_64F      v -> VG.length v
    HElems_USRTYPE1 v -> VG.length v

class ToHElems a where
    toHElems :: VU.Vector a -> HElems

instance ToHElems Word8  where toHElems = HElems_8U
instance ToHElems Int8   where toHElems = HElems_8S
instance ToHElems Word16 where toHElems = HElems_16U
instance ToHElems Int16  where toHElems = HElems_16S
instance ToHElems Int32  where toHElems = HElems_32S
instance ToHElems Float  where toHElems = HElems_32F
instance ToHElems Double where toHElems = HElems_64F

matToHMat :: Mat shape channels depth -> HMat
matToHMat mat = unsafePerformIO $ withMatData mat $ \step dataPtr -> do
    elems <- copyElems info (map fromIntegral step) dataPtr
    pure HMat
         { hmShape    = miShape    info
         , hmChannels = miChannels info
         , hmElems    = elems
         }
  where
    info = matInfo mat

    copyElems
        :: MatInfo
        -> [Int]     -- ^ step
        -> Ptr Word8 -- ^ data
        -> IO HElems
    copyElems (MatInfo shape depth channels) step dataPtr =
        case depth of
          Depth_8U  -> HElems_8U  <$> copyToVec
          Depth_8S  -> HElems_8S  <$> copyToVec
          Depth_16U -> HElems_16U <$> copyToVec
          Depth_16S -> HElems_16S <$> copyToVec
          Depth_32S -> HElems_32S <$> copyToVec
          Depth_32F -> HElems_32F <$> copyToVec
          Depth_64F -> HElems_64F <$> copyToVec
          Depth_USRTYPE1 -> HElems_USRTYPE1 <$> error "todo"
      where
        copyToVec :: (Storable a, VU.Unbox a) => IO (VU.Vector a)
        copyToVec = do
            v <- VUM.unsafeNew $ product0 (map fromIntegral shape) * (fromIntegral channels)
            forM_ (zip [0,channels..] $ dimPositions $ map fromIntegral shape) $ \(posIx, pos) -> do
                let elemPtr = matElemAddress dataPtr step pos
                forM_ [0 .. channels - 1] $ \channelIx -> do
                  e <- peekElemOff elemPtr $ fromIntegral channelIx
                  VUM.unsafeWrite v (fromIntegral $ posIx + channelIx) e
            VU.unsafeFreeze v

hMatToMat :: HMat -> Mat 'D 'D 'D
hMatToMat (HMat shape channels elems) = unsafePerformIO $ do
    mat <- exceptErrorIO $ newMat sizes channels depth scalar
    withMatData mat copyElems
    pure mat
  where
    sizes = V.fromList shape
    depth = hElemsDepth elems

    scalar :: Scalar
    scalar = toScalar (zero :: V4 CDouble)

    copyElems :: [CSize] -> Ptr Word8 -> IO ()
    copyElems step dataPtr = case elems of
        HElems_8U       v -> copyFromVec v
        HElems_8S       v -> copyFromVec v
        HElems_16U      v -> copyFromVec v
        HElems_16S      v -> copyFromVec v
        HElems_32S      v -> copyFromVec v
        HElems_32F      v -> copyFromVec v
        HElems_64F      v -> copyFromVec v
        HElems_USRTYPE1 _v -> error "todo"
      where
        copyFromVec :: (Storable a, VU.Unbox a) => VU.Vector a -> IO ()
        copyFromVec v =
            forM_ (zip [0, fromIntegral channels ..] $ dimPositions (fromIntegral <$> shape)) $ \(posIx, pos) -> do
              let elemPtr = matElemAddress dataPtr (fromIntegral <$> step) pos
              forM_ [0 .. channels - 1] $ \channelIx ->
                pokeElemOff elemPtr (fromIntegral channelIx) $ VU.unsafeIndex v (fromIntegral $ posIx + channelIx)

product0 :: (Num a) => [a] -> a
product0 [] = 0
product0 xs = product xs
