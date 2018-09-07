{-# language DeriveFunctor #-}
{-# language QuasiQuotes #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}

module OpenCV.Exception
    ( -- * Exception type
      CvException(..)
    , CoerceMatError(..)
    , ExpectationError(..)
    , CvCppException

      -- * Promoting exceptions to errors
    , exceptError
    , exceptErrorIO
    , exceptErrorM
    , maybeThrowError
    ) where

import "this" OpenCV.Internal.Exception
