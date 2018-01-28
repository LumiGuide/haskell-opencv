{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Internal.Dnn.DictValue
    ( DictValue
    , ToDictValue(..)
    , GetDictValue(..)
    , getDictValue
    , dictValueIsInt
    , dictValueIsString
    , dictValueIsReal
    , dictValueSize
    ) where

import "base" Data.Int ( Int32, Int64 )
import "base" Data.Word ( Word32 )
import "base" Foreign.C.Types ( CInt )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, unsafePrimToPrim )
import "this" OpenCV.Core.Types.String ( toCvString )
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.C.FinalizerTH
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Exception
import "transformers" Control.Monad.Trans.Except ( ExceptT(..) )

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/dnn.hpp"
C.using "namespace cv"
C.using "namespace cv::dnn"

--------------------------------------------------------------------------------

{- | Stores the scalar value (or array) of one of the following type:
double, cv::String or int64.
-}
newtype DictValue = DictValue {unDictValue :: ForeignPtr (C DictValue)}

type instance C DictValue = C'DictValue

instance WithPtr DictValue where
    withPtr = withForeignPtr . unDictValue

mkFinalizer DeletePtr "deleteDictValue" "cv::dnn::DictValue" ''C'DictValue

instance FromPtr DictValue where fromPtr = objFromPtr DictValue deleteDictValue

--------------------------------------------------------------------------------

class ToDictValue a where
    toDictValue :: (PrimMonad m) => a -> m DictValue

instance ToDictValue DictValue where
    toDictValue = pure

instance ToDictValue Double where
    toDictValue x = unsafePrimToPrim $ fromPtr
        [CU.exp| DictValue * { new cv::dnn::DictValue($(double c'x)) } |]
      where
        c'x = toCDouble x

instance ToDictValue Int64 where
    toDictValue x = unsafePrimToPrim $ fromPtr
        [CU.exp| DictValue * { new cv::dnn::DictValue($(int64_t x)) } |]

instance ToDictValue String where
    toDictValue string = unsafePrimToPrim $ fromPtr $
        withPtr (toCvString string) $ \stringPtr ->
          [CU.exp| DictValue * {
            new cv::dnn::DictValue(*$(String * stringPtr))
          } |]

--------------------------------------------------------------------------------

class GetDictValue a where
    getDictValue' :: Ptr (C DictValue) -> CInt -> IO (Either CvException a)

instance GetDictValue Int32 where
    getDictValue' dictValuePtr idx =
        alloca $ \resultPtr -> handleCvException (peek resultPtr) $
        [cvExcept|
          *$(int32_t * resultPtr) =
            $(DictValue * dictValuePtr)->get<int32_t>($(int idx));
        |]


instance GetDictValue Int64 where
    getDictValue' dictValuePtr idx =
        alloca $ \resultPtr -> handleCvException (peek resultPtr) $
        [cvExcept|
          *$(int64_t * resultPtr) =
            $(DictValue * dictValuePtr)->get<int64_t>($(int idx));
        |]

instance GetDictValue Word32 where
    getDictValue' dictValuePtr idx =
        alloca $ \resultPtr -> handleCvException (peek resultPtr) $
        [cvExcept|
          *$(uint32_t * resultPtr) =
            $(DictValue * dictValuePtr)->get<uint32_t>($(int idx));
        |]

getDictValue
    :: (PrimMonad m, GetDictValue a)
    => DictValue
    -> Maybe Int -- ^ Optional array index.
    -> CvExceptT m a
getDictValue dictValue mbIdx = ExceptT $ unsafePrimToPrim $
    withPtr dictValue $ \dictValuePtr ->
      getDictValue' dictValuePtr c'idx
  where
    c'idx :: CInt
    c'idx = maybe (-1) fromIntegral mbIdx

--------------------------------------------------------------------------------

dictValueIsInt :: (PrimMonad m) => DictValue -> m Bool
dictValueIsInt dictValue = unsafePrimToPrim $
    withPtr dictValue $ \dictValuePtr -> toBool <$>
      [CU.exp| bool { $(DictValue * dictValuePtr)->isInt() } |]

dictValueIsString :: (PrimMonad m) => DictValue -> m Bool
dictValueIsString dictValue = unsafePrimToPrim $
    withPtr dictValue $ \dictValuePtr -> toBool <$>
      [CU.exp| bool { $(DictValue * dictValuePtr)->isString() } |]

dictValueIsReal :: (PrimMonad m) => DictValue -> m Bool
dictValueIsReal dictValue = unsafePrimToPrim $
    withPtr dictValue $ \dictValuePtr -> toBool <$>
      [CU.exp| bool { $(DictValue * dictValuePtr)->isReal() } |]

dictValueSize :: (PrimMonad m) => DictValue -> m Int32
dictValueSize dictValue = unsafePrimToPrim $
    withPtr dictValue $ \dictValuePtr ->
      [CU.exp| int32_t { $(DictValue * dictValuePtr)->size() } |]
