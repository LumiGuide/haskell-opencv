{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module OpenCV.Core.Types.String
    ( CvString
    , IsCvString(..)
    , cvStringIsEmpty
    , cvStringEmpty
    ) where

import "base" Foreign.C.String ( CStringLen, withCStringLen )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "bytestring" Data.ByteString as B
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.Internal ( objFromPtr )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.C.FinalizerTH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

newtype CvString = CvString {unCvString :: ForeignPtr (C CvString)}

type instance C CvString = C'CvString

instance WithPtr CvString where withPtr = withForeignPtr . unCvString

mkFinalizer DeletePtr "deleteCvString" "cv::String" ''C'CvString

instance FromPtr CvString where fromPtr = objFromPtr CvString deleteCvString

cStringLenToCvString :: CStringLen -> IO CvString
cStringLenToCvString (haskellString, stringLength) = fromPtr $
    [C.block| String * {
      int strLength = $(int c'stringLength);
      // -- Point to string managed by Haskell runtime.
      char * haskellString = $(char * haskellString);
      // -- Assign memory on C++ heap for string managed by cv::String.
      char * cvString = new char[strLength];
      // -- Copy temporary Haskell string to C++ heap.
      strncpy(cvString, haskellString, strLength);
      // -- cv::String is responsible for freeing the string memory.
      return new cv::String(cvString);
    } |]
  where
    c'stringLength = fromIntegral stringLength

class IsCvString a where
    toCvString :: a -> CvString

instance IsCvString CvString where
    toCvString = id

instance IsCvString String where
    toCvString s = unsafePerformIO $ withCStringLen s cStringLenToCvString

instance IsCvString B.ByteString where
    toCvString s = unsafePerformIO $ B.useAsCStringLen s cStringLenToCvString

cvStringIsEmpty :: CvString -> Bool
cvStringIsEmpty cvString = toBool $
    unsafePerformIO $ withPtr cvString $ \cvStringPtr ->
      [CU.exp| bool { $(String * cvStringPtr)->empty() } |]

cvStringEmpty :: CvString
cvStringEmpty = unsafePerformIO $ fromPtr
    [CU.exp| String * { new cv::String() } |]
