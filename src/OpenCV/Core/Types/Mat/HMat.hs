module OpenCV.Core.Types.Mat.HMat
    ( HMat
    , hmShape
    , hmChannels
    , hmElems
    , HElems(..)
    , hElemsDepth
    , hElemsLength
    , hmat
    ) where


import "base" Foreign.C.Types
import "base" Foreign.Ptr ( Ptr, plusPtr )
import "base" Foreign.Storable ( Storable(..), peekElemOff, pokeElemOff )
import qualified "bytestring" Data.ByteString as B
import "lens" Control.Lens hiding ( ix )
import "linear" Linear.Vector ( zero )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "this" OpenCV.Internal
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
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

hElemsDepth :: HElems -> MatDepth
hElemsDepth = \case
    HElems_8U       _v -> MatDepth_8U
    HElems_8S       _v -> MatDepth_8S
    HElems_16U      _v -> MatDepth_16U
    HElems_16S      _v -> MatDepth_16S
    HElems_32S      _v -> MatDepth_32S
    HElems_32F      _v -> MatDepth_32F
    HElems_64F      _v -> MatDepth_64F
    HElems_USRTYPE1 _v -> MatDepth_USRTYPE1

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

hmat :: Iso' Mat HMat
hmat = iso matToHMat hMatToMat

matToHMat :: Mat -> HMat
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
          MatDepth_8U  -> HElems_8U  <$> copyToVec
          MatDepth_8S  -> HElems_8S  <$> copyToVec
          MatDepth_16U -> HElems_16U <$> copyToVec
          MatDepth_16S -> HElems_16S <$> copyToVec
          MatDepth_32S -> HElems_32S <$> copyToVec
          MatDepth_32F -> HElems_32F <$> copyToVec
          MatDepth_64F -> HElems_64F <$> copyToVec
          MatDepth_USRTYPE1 -> HElems_USRTYPE1 <$> error "todo"
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

hMatToMat :: HMat -> Mat
hMatToMat (HMat shape channels elems) = unsafePerformIO $ do
    mat <- newMat sizes depth channels scalar
    withMatData mat copyElems
    pure mat
  where
    sizes = V.fromList shape
    depth = hElemsDepth elems
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

-- | All possible positions (indexes) for a given shape (list of
-- sizes per dimension).
dimPositions :: (Num a, Enum a) => [a] -> [[a]]
dimPositions shape = sequence $ map (enumFromTo 0) $ map pred shape

matElemAddress :: Ptr Word8 -> [Int] -> [Int] -> Ptr a
matElemAddress dataPtr step pos = dataPtr `plusPtr` offset
    where
      offset = sum $ zipWith (*) step pos


--------------------------------------------------------------------------------

product0 :: (Num a) => [a] -> a
product0 [] = 0
product0 xs = product xs
