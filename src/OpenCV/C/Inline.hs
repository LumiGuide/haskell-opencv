{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface between OpenCV and inline-c(pp) (Haskell)
module OpenCV.C.Inline ( openCvCtx ) where

import "base" Foreign.Ptr ( FunPtr )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.C.Types

-- | Context useful to work with the OpenCV library
--
-- Based on 'C.cppCtx', 'C.bsCtx' and 'C.vecCtx'.
--
-- 'C.ctxTypesTable': converts OpenCV basic types to their counterparts in
-- "OpenCV.C.Inline".
--
-- No 'C.ctxAntiQuoters'.
openCvCtx :: C.Context
openCvCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty
      { C.ctxTypesTable = openCvTypesTable
      }

openCvTypesTable :: C.TypesTable
openCvTypesTable = M.fromList
  [ ( C.TypeName "bool"        , [t| C.CInt        |] )

  , ( C.TypeName "Exception"   , [t| C'CvCppException |] )

  , ( C.TypeName "Point2i"     , [t| C'Point2i     |] )
  , ( C.TypeName "Point2f"     , [t| C'Point2f     |] )
  , ( C.TypeName "Point2d"     , [t| C'Point2d     |] )
  , ( C.TypeName "Point3i"     , [t| C'Point3i     |] )
  , ( C.TypeName "Point3f"     , [t| C'Point3f     |] )
  , ( C.TypeName "Point3d"     , [t| C'Point3d     |] )
  , ( C.TypeName "Vec2i"       , [t| C'Vec2i       |] )
  , ( C.TypeName "Vec2f"       , [t| C'Vec2f       |] )
  , ( C.TypeName "Vec2d"       , [t| C'Vec2d       |] )
  , ( C.TypeName "Vec3i"       , [t| C'Vec3i       |] )
  , ( C.TypeName "Vec3f"       , [t| C'Vec3f       |] )
  , ( C.TypeName "Vec3d"       , [t| C'Vec3d       |] )
  , ( C.TypeName "Vec4i"       , [t| C'Vec4i       |] )
  , ( C.TypeName "Vec4f"       , [t| C'Vec4f       |] )
  , ( C.TypeName "Vec4d"       , [t| C'Vec4d       |] )
  , ( C.TypeName "Size2i"      , [t| C'Size2i      |] )
  , ( C.TypeName "Size2f"      , [t| C'Size2f      |] )
  , ( C.TypeName "Size2d"      , [t| C'Size2d      |] )
  , ( C.TypeName "Rect2i"      , [t| C'Rect2i      |] )
  , ( C.TypeName "Rect2f"      , [t| C'Rect2f      |] )
  , ( C.TypeName "Rect2d"      , [t| C'Rect2d      |] )
  , ( C.TypeName "RotatedRect" , [t| C'RotatedRect |] )
  , ( C.TypeName "TermCriteria", [t| C'TermCriteria|] )
  , ( C.TypeName "Scalar"      , [t| C'Scalar      |] )
  , ( C.TypeName "Mat"         , [t| C'Mat         |] )
  , ( C.TypeName "Range"       , [t| C'Range       |] )

  , ( C.TypeName "KeyPoint"    , [t| C'KeyPoint    |] )
  , ( C.TypeName "DMatch"      , [t| C'DMatch      |] )

--, ( C.TypeName "MSER"        , [t| C'MSER        |] )
  , ( C.TypeName "Ptr_ORB"     , [t| C'Ptr_ORB     |] )
--, ( C.TypeName "BRISK"       , [t| C'BRISK       |] )
--, ( C.TypeName "KAZE"        , [t| C'KAZE        |] )
--, ( C.TypeName "AKAZE"       , [t| C'AKAZE       |] )

  , ( C.TypeName "BFMatcher"   , [t| C'BFMatcher   |] )

  , ( C.TypeName "Ptr_BackgroundSubtractorKNN" , [t| C'Ptr_BackgroundSubtractorKNN  |] )
  , ( C.TypeName "Ptr_BackgroundSubtractorMOG2", [t| C'Ptr_BackgroundSubtractorMOG2 |] )

  , ( C.TypeName "MouseCallback"   , [t| FunPtr C'MouseCallback    |] )
  , ( C.TypeName "TrackbarCallback", [t| FunPtr C'TrackbarCallback |] )
  ]
