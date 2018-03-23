{-# language CPP #-}
{-# language DeriveFunctor #-}
{-# language QuasiQuotes #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Exception
    ( -- * Exception type
      CvException(..)
    , CoerceMatError(..)
    , ExpectationError(..)
    , CvCppException

      -- * Handling C++ exceptions
    , handleCvException
    , wrapException

      -- * Quasi quoters
    , cvExcept
    , cvExceptU

      -- * Promoting exceptions to errors
    , exceptError
    , exceptErrorIO
    , exceptErrorM
    , runCvExceptST

      -- * Unsafe stuff
    , unsafeCvExcept
    , unsafeWrapException
    ) where

import "base" Control.Monad.ST ( ST, runST )
import "base" Control.Exception ( Exception(displayException), mask_, throw, throwIO )
import "base" Control.Monad ( (<=<) )
import "base" Data.List ( intercalate )
import qualified "base" Data.List.NonEmpty as NE
import "base" Data.Monoid ( (<>) )
import "base" Foreign.C.String ( peekCString )
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Ptr ( Ptr, nullPtr )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "mtl" Control.Monad.Error.Class ( MonadError, throwError )
import "template-haskell" Language.Haskell.TH.Quote ( QuasiQuoter, quoteExp )
import "this" OpenCV.Internal.C.FinalizerTH
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat.Depth
import "this" OpenCV.Internal ( objFromPtr )
import "transformers" Control.Monad.Trans.Except

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"


--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data CvException
   = BindingException !CvCppException
     -- ^ An exception was thrown by the underlying opencv c++ library.
   | CoerceMatError !(NE.NonEmpty CoerceMatError)
     -- ^ A 'Mat' couldn't be coerced to a different type because that
     -- type does not match the actual shape of the matrix.
     deriving Show

data CoerceMatError
   = ShapeError        !(ExpectationError Int)
   | SizeError    !Int !(ExpectationError Int)
   | ChannelError      !(ExpectationError Int)
   | DepthError        !(ExpectationError Depth)
     deriving Show

data ExpectationError a
   = ExpectationError
     { expectedValue :: !a
     , actualValue   :: !a
     } deriving (Show, Functor)

instance Exception CvException where
    displayException = \case
      BindingException cppException ->
        "Exception thrown by opencv c++ library:\n" ++ show cppException
      CoerceMatError coerceMatErrors ->
        "A matrix can't be converted to the desired type:\n  - " ++
          intercalate "\n  - " (map displayCoerceMatError (NE.toList coerceMatErrors))

displayCoerceMatError :: CoerceMatError -> String
displayCoerceMatError = \case
    ShapeError expErr ->
        "Expected a "
           ++ show (expectedValue expErr)
           ++ "-D shape but got a "
           ++ show (actualValue expErr) ++ "-D shape"
    SizeError dimIx expErr ->
        "Expected dimension "
           ++ show dimIx ++ " to be of size "
           ++ show (expectedValue expErr)
           ++ " but got " ++ show (actualValue expErr)
    ChannelError expErr ->
        "Expected "
           ++ show (expectedValue expErr)
           ++ " channels but got "
           ++ show (actualValue expErr) ++ " channels"
    DepthError expErr ->
        "Expected depth "
           ++ displayDepth (expectedValue expErr)
           ++ " but got depth "
           ++ displayDepth (actualValue expErr)

displayDepth :: Depth -> String
displayDepth depth = show depth ++ " (" ++ haskellDepth ++ ")"
  where
    haskellDepth = case depth of
        Depth_8U  -> "Word8"
        Depth_8S  -> "Int8"
        Depth_16U -> "Word16"
        Depth_16S -> "Int16"
        Depth_32S -> "Int32"
        Depth_32F -> "Float"
        Depth_64F -> "Double"
        Depth_USRTYPE1 -> "not supported in Haskell"

newtype CvCppException = CvCppException { unCvCppException :: ForeignPtr (C CvCppException) }

type instance C CvCppException = C'CvCppException

instance Show CvCppException where
    show cvException = unsafePerformIO $
        withPtr cvException $ \cvExceptionPtr -> do
          charPtr <- [CU.exp| const char * { $(Exception * cvExceptionPtr)->what() } |]
          peekCString charPtr

instance WithPtr CvCppException where
    withPtr = withForeignPtr . unCvCppException

mkFinalizer DeletePtr "deleteException" "cv::Exception" ''C'CvCppException

instance FromPtr CvCppException where
    fromPtr = objFromPtr CvCppException deleteException

handleCvException
    :: IO a
    -> IO (Ptr (C CvCppException))
    -> IO (Either CvException a)
handleCvException okAct act = mask_ $ do
    exceptionPtr <- act
    if exceptionPtr /= nullPtr
      then do cppErr <- fromPtr (pure exceptionPtr)
              pure $ Left $ BindingException cppErr
      else Right <$> okAct

cvExcept :: QuasiQuoter
cvExcept = C.block {quoteExp = \s -> quoteExp C.block $ cvExceptWrap s}

cvExceptU :: QuasiQuoter
cvExceptU = CU.block {quoteExp = \s -> quoteExp CU.block $ cvExceptWrap s}

cvExceptWrap :: String -> String
cvExceptWrap s = unlines
   [ "Exception * {"
   , "  try"
   , "  {   " <> s <> ""
   , "    return NULL;"
   , "  }"
   , "  catch (const cv::Exception & e)"
   , "  {"
   , "    return new cv::Exception(e);"
   , "  }"
   , "}"
   ]

type CvExcept    a = Except  CvException   a
type CvExceptT m a = ExceptT CvException m a

exceptError :: CvExcept a -> a
exceptError = either throw id . runExcept

exceptErrorIO :: CvExceptT IO a -> IO a
exceptErrorIO = either throwIO pure <=< runExceptT

exceptErrorM :: (Monad m) => CvExceptT m a -> m a
exceptErrorM = either throw pure <=< runExceptT

runCvExceptST :: MonadError CvException m => (forall s. CvExceptT (ST s) a) -> m a
runCvExceptST act = either throwError return $ runST $ runExceptT act

unsafeCvExcept :: MonadError CvException m => CvExceptT IO a -> m a
unsafeCvExcept = either throwError return . unsafePerformIO . runExceptT

unsafeWrapException :: MonadError CvException m => IO (Either CvException a) -> m a
unsafeWrapException = unsafeCvExcept . ExceptT

wrapException :: ( MonadError CvException m ) => m (Either CvException a) -> m a
wrapException m = m >>= either throwError return
