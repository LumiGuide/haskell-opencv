{-# language MultiParamTypeClasses #-}

module OpenCV.Internal.Core.Types.Mat.Depth
    ( Depth(..)
    , ToDepth(toDepth)
    , ToDepthDS(toDepthDS)
    , DepthT
    ) where

import "base" Data.Int
import "base" Data.Proxy
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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

class ToDepthDS a where
    toDepthDS :: a -> DS Depth

instance ToDepthDS Depth      where toDepthDS _depth = D
instance ToDepthDS (proxy 'D) where toDepthDS _proxy = D

instance ToDepthDS (proxy ('S Word8 )) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Word8 )
instance ToDepthDS (proxy ('S Int8  )) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Int8  )
instance ToDepthDS (proxy ('S Word16)) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Word16)
instance ToDepthDS (proxy ('S Int16 )) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Int16 )
instance ToDepthDS (proxy ('S Int32 )) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Int32 )
instance ToDepthDS (proxy ('S Float )) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Float )
instance ToDepthDS (proxy ('S Double)) where toDepthDS _proxy = S $ toDepth (Proxy :: Proxy Double)

--------------------------------------------------------------------------------

type family DepthT a :: DS * where
    DepthT Depth     = 'D
    DepthT (proxy d) = 'S d
