{-# language MultiParamTypeClasses #-}

module OpenCV.Core.Types.Mat.Depth
    ( Depth(..)
    , DepthT
    ) where

import "base" Data.Int
import "base" Data.Proxy
import "base" Data.Word
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------0

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

instance Convert (Proxy Word8 ) Depth where convert _p = Depth_8U
instance Convert (Proxy Int8  ) Depth where convert _p = Depth_8S
instance Convert (Proxy Word16) Depth where convert _p = Depth_16U
instance Convert (Proxy Int16 ) Depth where convert _p = Depth_16S
instance Convert (Proxy Int32 ) Depth where convert _p = Depth_32S
instance Convert (Proxy Float ) Depth where convert _p = Depth_32F
instance Convert (Proxy Double) Depth where convert _p = Depth_64F
-- TODO (BvD): instance ToDepth ? where toDepth = const Depth_USRTYPE1
-- RvD: perhaps ByteString? Or a fixed size (statically) vector of bytes

type family DepthT a :: DS * where
    DepthT Depth     = 'D
    DepthT (Proxy d) = 'S d
