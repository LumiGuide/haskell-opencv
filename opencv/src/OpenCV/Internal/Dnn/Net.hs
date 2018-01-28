{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Internal.Dnn.Net
    ( LayerId
    , Net
    , newNet
    , coerceNet
    , netIsEmpty
    , netConnectByDescriptor
    , netConnectByIdentifier
    , netEnableFusion
    , netForward
    ) where

import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( fromBool, toBool )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Core.Types.String
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.C.FinalizerTH
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Dnn.DictValue ( DictValue )
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except ( ExceptT(..) )


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/dnn.hpp"
C.using "namespace cv"
C.using "namespace cv::dnn"

--------------------------------------------------------------------------------

type LayerId = DictValue

{- | Artificial neural network.

Neural network is presented as directed acyclic graph (DAG), where
vertices are Layer instances, and edges specify relationships between
layers inputs and outputs.

Each network layer has unique integer id and unique string name inside
its network.
-}
newtype Net s = Net {unNet :: ForeignPtr (C (Net s))}

type instance C (Net s) = C'Net

instance WithPtr (Net s) where withPtr = withForeignPtr . unNet

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

{- | Connects output of the first layer to input of the second layer.

Descriptors have the following template @<layer_name>[.input_number]@:

 - The first part of the template @layer_name@ is sting name of the added
   layer. If this part is empty then the network input pseudo layer will be
   used;

 - The second optional part of the template @input_number@ is either number of
   the layer input, either label one. If this part is omitted then the first
   layer input will be used.
-}
netConnectByDescriptor
    :: (PrimMonad m, IsCvString string)
    => Net (PrimState m)
    -> string -- ^ Descriptor of the first layer output.
    -> string -- ^ Descriptor of the second layer input.
    -> CvExceptT m ()
netConnectByDescriptor net outPin inPin = ExceptT $ unsafePrimToPrim $
    withPtr net    $ \netPtr    ->
    withPtr (toCvString outPin) $ \outPinPtr ->
    withPtr (toCvString inPin ) $ \inPinPtr  ->
    handleCvException (pure ()) $
    [cvExcept|
      $(Net * netPtr)->
        connect( *$(String * outPinPtr)
               , *$(String * inPinPtr)
               );
    |]

{- | Connects #@outNum@ output of the first layer to #@inNum@ input of the second
layer.
-}
netConnectByIdentifier
    :: (PrimMonad m)
    => Net (PrimState m)
    -> Int -- ^ Identifier of the first layer.
    -> Int -- ^ Identifier of the second layer.
    -> Int -- ^ Number of the first layer output.
    -> Int -- ^ Number of the second layer input.
    -> CvExceptT m ()
netConnectByIdentifier net outLayerId outNum inpLayerId inpNum =
    ExceptT $ unsafePrimToPrim $
    withPtr net $ \netPtr ->
    handleCvException (pure ()) $
    [cvExcept|
      $(Net * netPtr)->
        connect( $(int c'outLayerId)
               , $(int c'outNum    )
               , $(int c'inpLayerId)
               , $(int c'inpNum    )
               );
    |]
  where
    c'outLayerId = fromIntegral outLayerId
    c'outNum     = fromIntegral outNum
    c'inpLayerId = fromIntegral inpLayerId
    c'inpNum     = fromIntegral inpNum

{- | Enables or disables layer fusion in the network.

The fusion is enabled by default.
-}
netEnableFusion
    :: (PrimMonad m)
    => Net (PrimState m)
    -> Bool -- ^ 'True' to enable the fusion, 'False' to disable.
    -> m ()
netEnableFusion net fusion = unsafePrimToPrim $
    withPtr net $ \netPtr ->
      [CU.exp| void { $(Net * netPtr)->enableFusion($(bool c'fusion)) } |]
  where
    c'fusion = fromBool fusion

{- | Runs forward pass.

By default runs forward pass for the whole network.

Returns blob for first output of specified layer.
-}
netForward
    :: (PrimMonad m, IsCvString string)
    => Net (PrimState m)
    -> Maybe string -- ^ Name for layer which output is needed to get.
    -> CvExceptT m (Mat 'D 'D 'D)
netForward net mbOutputName = ExceptT $ unsafePrimToPrim $ do
    result <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat result) $
      withPtr net $ \netPtr ->
      withPtr outputName $ \outputNamePtr ->
      withPtr result $ \resultPtr ->
      [cvExcept|
        *$(Mat * resultPtr) =
          $(Net * netPtr)->forward(*$(String * outputNamePtr));
      |]
  where
    outputName :: CvString
    outputName = maybe cvStringEmpty toCvString mbOutputName
