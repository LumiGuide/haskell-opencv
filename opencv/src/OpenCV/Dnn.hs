{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Dnn
    ( -- * Net
      LayerId
    , Net

    , readNetFromCaffe
    , readNetFromCaffeFile

    , netIsEmpty

      -- * DictValue
    , DictValue
    , ToDictValue
    , toDictValue
    , GetDictValue
    , getDictValue
    , dictValueIsInt
    , dictValueIsString
    , dictValueIsReal
    , dictValueSize
    ) where

import "base" Foreign.C.String ( withCString )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "bytestring" Data.ByteString as B
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "this" OpenCV.Internal.Dnn.DictValue
import "this" OpenCV.Internal.Dnn.Net
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types ( withPtr )
import "this" OpenCV.Internal.Exception
import "transformers" Control.Monad.Trans.Except ( ExceptT(..) )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/dnn.hpp"
C.using "namespace cv"
C.using "namespace cv::dnn"

--------------------------------------------------------------------------------

readNetFromCaffe
    :: (PrimMonad m)
    => B.ByteString -- ^ Content of the .prototxt file.
    -> B.ByteString -- ^ Content of the .caffemodel file.
    -> CvExceptT m (Net (PrimState m))
readNetFromCaffe proto model = ExceptT $ unsafePrimToPrim $ do
    net <- newNet
    handleCvException (pure $ coerceNet net) $
      B.useAsCStringLen proto $ \(protoStr, protoLen) ->
      B.useAsCStringLen model $ \(modelStr, modelLen) ->
      withPtr net $ \netPtr -> do
        let c'protoLen = fromIntegral protoLen
            c'modelLen = fromIntegral modelLen
        [cvExcept|
          *$(Net * netPtr) =
            cv::dnn::readNetFromCaffe
              ( $(char * protoStr)
              , $(int c'protoLen)
              , $(char * modelStr)
              , $(int c'modelLen)
              );
        |]

readNetFromCaffeFile
    :: FilePath -- ^ Path to the .prototxt file with text description of the
                -- network architecture.
    -> FilePath -- ^ Path to the .caffemodel file with learned network.
    -> CvExceptT IO (Net (PrimState IO))
readNetFromCaffeFile prototxt caffeModel = ExceptT $ do
    net <- newNet
    handleCvException (pure net) $
      withCString prototxt   $ \c'prototxt   ->
      withCString caffeModel $ \c'caffeModel ->
      withPtr net $ \netPtr ->
        [cvExcept|
          *$(Net * netPtr) =
            cv::dnn::readNetFromCaffe
              ( $(char * c'prototxt)
              , $(char * c'caffeModel)
              );
        |]
