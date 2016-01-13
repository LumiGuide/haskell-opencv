{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Internal where

import "base" Foreign.C.String ( peekCString )
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Ptr ( Ptr, nullPtr )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "template-haskell" Language.Haskell.TH.Quote ( QuasiQuoter, quoteExp )
import "this" Language.C.Inline.OpenCV


--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

newtype CvException = CvException { unCvException :: ForeignPtr C'Exception }

instance Exception CvException

instance Show CvException where
    show cvException = unsafePerformIO $
        withCvExceptionPtr cvException $ \cvExceptionPtr -> do
          charPtr <- [CU.exp| const char * { $(Exception * cvExceptionPtr)->what() } |]
          peekCString charPtr

withCvExceptionPtr :: CvException -> (Ptr C'Exception -> IO a) -> IO a
withCvExceptionPtr = withForeignPtr . unCvException

cvExceptionFromPtr :: IO (Ptr C'Exception) -> IO CvException
cvExceptionFromPtr = objFromPtr CvException $ \ptr -> [CU.exp| void { delete $(Exception * ptr) }|]

handleCvException :: IO a -> IO (Ptr C'Exception) -> IO (Either CvException a)
handleCvException okAct act = mask_ $ do
    exceptionPtr <- act
    if exceptionPtr /= nullPtr
      then Left <$> cvExceptionFromPtr (pure exceptionPtr)
      else Right <$> okAct

cvExcept :: QuasiQuoter
cvExcept = C.block {quoteExp = \s -> quoteExp C.block $ cvExceptWrap s}

cvExceptU :: QuasiQuoter
cvExceptU = CU.block {quoteExp = \s -> quoteExp CU.block $ cvExceptWrap s}

cvExceptWrap :: String -> String
cvExceptWrap s =
    "Exception * {\n\
    \  try\n\
    \  {\n   " <> s <> "\n\
    \    return NULL;\n\
    \  }\n\
    \  catch (const cv::Exception & e)\n\
    \  {\n\
    \    return new cv::Exception(e);\n\
    \  }\n\
    \}"

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr objPtr (finalizer objPtr)
