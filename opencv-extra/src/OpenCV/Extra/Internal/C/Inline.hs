{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

-- | Interface between OpenCV (extra modules) and inline-c(pp) (Haskell)
module OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx ) where

import "base" Foreign.Ptr ( FunPtr )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "opencv" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Extra.Internal.C.Types

-- | Context useful to work with the OpenCV library's extra modules.
--
-- Based on 'C.cppCtx', 'C.bsCtx', 'C.vecCtx' and most importantly 'openCvCtx'.
--
-- 'C.ctxTypesTable': converts OpenCV basic types to their counterparts in
-- "OpenCV.Internal.C.Inline".
--
-- No 'C.ctxAntiQuoters'.
openCvExtraCtx :: C.Context
openCvExtraCtx = openCvCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTAble = openCvExtraTypesTable }

openCvExtraTypesTable :: C.TypesTable
openCvExtraTypesTable = M.fromList
  [ ( C.TypeName "Ptr_BackgroundSubtractorGMG", [t| C'Ptr_BackgroundSubtractorGMG |] )
  , ( C.TypeName "Ptr_BackgroundSubtractorMOG", [t| C'Ptr_BackgroundSubtractorMOG |] )
  ]
