{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

-- | Interface between OpenCV (extra modules) and inline-c(pp) (Haskell)
module OpenCV.Extra.Internal.C.Inline ( openCvExtraCtx ) where

import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
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
    ctx = mempty { C.ctxTypesTable = openCvExtraTypesTable }

openCvExtraTypesTable :: C.TypesTable
openCvExtraTypesTable = M.fromList
  [ ( C.TypeName "Ptr_BackgroundSubtractorGMG", [t| C'Ptr_BackgroundSubtractorGMG |] )
  , ( C.TypeName "Ptr_BackgroundSubtractorMOG", [t| C'Ptr_BackgroundSubtractorMOG |] )
  , ( C.TypeName "Ptr_Tracker"                , [t| C'Ptr_Tracker                 |] )
  , ( C.TypeName "Ptr_TrackerFeature"         , [t| C'Ptr_TrackerFeature          |] )
  , ( C.TypeName "Ptr_MultiTracker"           , [t| C'Ptr_MultiTracker            |] )
  , ( C.TypeName "Ptr_MultiTrackerAlt"        , [t| C'Ptr_MultiTrackerAlt         |] )
  , ( C.TypeName "Ptr_SURF"                   , [t| C'Ptr_SURF                    |] )
  , ( C.TypeName "Ptr_GrayworldWB"            , [t| C'Ptr_GrayworldWB             |] )
  , ( C.TypeName "Ptr_LearningBasedWB"        , [t| C'Ptr_LearningBasedWB         |] )
  , ( C.TypeName "Ptr_SimpleWB"               , [t| C'Ptr_SimpleWB                |] )
  , ( C.TypeName "Ptr_CharucoBoard"           , [t| C'Ptr'CharucoBoard            |] )
  , ( C.TypeName "Ptr_Dictionary"             , [t| C'Ptr'Dictionary              |] )
  , ( C.TypeName "VectorVectorPoint2f"        , [t| C'Vector'Vector'Point2f       |] )
  , ( C.TypeName "VectorInt"                  , [t| C'Vector'Int                  |] )
  ]
