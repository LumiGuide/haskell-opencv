{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Internal where

import "base" Foreign.C.String ( peekCString )
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, touchForeignPtr )
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
--  Matrix
--------------------------------------------------------------------------------

newtype Mat = Mat {unMat :: ForeignPtr C'Mat}

matFromPtr :: IO (Ptr C'Mat) -> IO Mat
matFromPtr = objFromPtr Mat $ \ptr -> [CU.exp| void { delete $(Mat * ptr) }|]

withMatPtr :: Mat -> (Ptr C'Mat -> IO a) -> IO a
withMatPtr = withForeignPtr . unMat

withMbMatPtr :: Maybe Mat -> (Ptr C'Mat -> IO a) -> IO a
withMbMatPtr mbMat f =
    case mbMat of
      Just mat -> withMatPtr mat f
      Nothing  -> f nullPtr

-- | Similar to 'withMatPtr' in that it keeps the 'ForeignPtr' alive
-- during the execution of the given action but it doesn't extract the 'Ptr'
-- from the 'ForeignPtr'.
keepMatAliveDuring :: Mat -> IO a -> IO a
keepMatAliveDuring mat m = do
    x <- m
    touchForeignPtr $ unMat mat
    pure x


--------------------------------------------------------------------------------
-- Mutable Matrix
--------------------------------------------------------------------------------

newtype MutMat s = MutMat { unMutMat :: Mat }


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr objPtr (finalizer objPtr)
