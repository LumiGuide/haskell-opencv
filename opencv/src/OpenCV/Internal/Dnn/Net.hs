{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Internal.Dnn.Net
    ( Net
    , newNet
    , coerceNet
    , netIsEmpty
    ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.C.FinalizerTH
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/dnn.hpp"
C.using "namespace cv"
C.using "namespace cv::dnn"

--------------------------------------------------------------------------------

{- | Artificial neural network.

Neural network is presented as directed acyclic graph (DAG), where
vertices are Layer instances, and edges specify relationships between
layers inputs and outputs.

Each network layer has unique integer id and unique string name inside
its network.
-}
newtype Net s = Net {unNet :: ForeignPtr (C (Net s))}

type instance C (Net s) = C'Net

instance WithPtr (Net s) where
    withPtr = withForeignPtr . unNet

mkFinalizer DeletePtr "deleteNet" "cv::dnn::Net" ''C'Net

instance FromPtr (Net s) where fromPtr = objFromPtr Net deleteNet

newNet :: (PrimMonad m) => m (Net (PrimState m))
newNet = unsafePrimToPrim $ fromPtr [CU.exp| Net * { new cv::dnn::Net() } |]

coerceNet :: Net s1 -> Net s2
coerceNet (Net ptr) = Net ptr

-- | Returns true if there are no layers in the network.
netIsEmpty :: (PrimMonad m) => Net (PrimState m) -> m Bool
netIsEmpty net = unsafePrimToPrim $
    withPtr net $ \netPtr -> toBool <$>
    [CU.exp| bool { $(Net * netPtr)->empty() } |]
