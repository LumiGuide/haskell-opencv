{-# language MultiParamTypeClasses #-}

module OpenCV.Core.Types.Mat.Depth
    ( Depth(..)
    , ToDepth(toDepth)
    , DepthT
    ) where

import "base" Data.Int
import "base" Data.Word
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

data Depth =
     Depth_8U
   | Depth_8S
   | Depth_16U
   | Depth_16S
   | Depth_32S
   | Depth_32F
   | Depth_64F
   | Depth_USRTYPE1
     deriving (Show, Eq)

-- TODO (RvD): should users be allowed to make instances of ToDepth
-- for their own types?
class ToDepth a where
    toDepth :: a -> Depth

instance ToDepth Depth where toDepth = id
instance ToDepth (proxy Word8 ) where toDepth _proxy = Depth_8U
instance ToDepth (proxy Int8  ) where toDepth _proxy = Depth_8S
instance ToDepth (proxy Word16) where toDepth _proxy = Depth_16U
instance ToDepth (proxy Int16 ) where toDepth _proxy = Depth_16S
instance ToDepth (proxy Int32 ) where toDepth _proxy = Depth_32S
instance ToDepth (proxy Float ) where toDepth _proxy = Depth_32F
instance ToDepth (proxy Double) where toDepth _proxy = Depth_64F
-- TODO (BvD): instance ToDepth ? where toDepth = const Depth_USRTYPE1
-- RvD: perhaps ByteString? Or a fixed size (statically) vector of bytes

instance (ToDepth a) => Convert a Depth where
    convert = toDepth

type family DepthT a :: DS * where
    DepthT Depth     = 'D
    DepthT (proxy d) = 'S d
