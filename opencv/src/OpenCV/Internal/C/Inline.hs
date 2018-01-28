{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

-- | Interface between OpenCV and inline-c(pp) (Haskell)
module OpenCV.Internal.C.Inline ( openCvCtx ) where

import "base" Foreign.Ptr ( FunPtr )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Types

-- | Context useful to work with the OpenCV library
--
-- Based on 'C.cppCtx', 'C.bsCtx' and 'C.vecCtx'.
--
-- 'C.ctxTypesTable': converts OpenCV basic types to their counterparts in
-- "OpenCV.Internal.C.Inline".
--
-- No 'C.ctxAntiQuoters'.
openCvCtx :: C.Context
openCvCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = openCvTypesTable }

openCvTypesTable :: C.TypesTable
openCvTypesTable = M.fromList
  [ ( C.TypeName "bool"        , [t| C.CInt        |] )

  , ( C.TypeName "Exception"   , [t| C'CvCppException |] )

  , ( C.TypeName "String"      , [t| C'CvString    |] )

  , ( C.TypeName "Matx12f"     , [t| C'Matx12f     |] )
  , ( C.TypeName "Matx12d"     , [t| C'Matx12d     |] )
  , ( C.TypeName "Matx13f"     , [t| C'Matx13f     |] )
  , ( C.TypeName "Matx13d"     , [t| C'Matx13d     |] )
  , ( C.TypeName "Matx14f"     , [t| C'Matx14f     |] )
  , ( C.TypeName "Matx14d"     , [t| C'Matx14d     |] )
  , ( C.TypeName "Matx16f"     , [t| C'Matx16f     |] )
  , ( C.TypeName "Matx16d"     , [t| C'Matx16d     |] )
  , ( C.TypeName "Matx21f"     , [t| C'Matx21f     |] )
  , ( C.TypeName "Matx21d"     , [t| C'Matx21d     |] )
  , ( C.TypeName "Matx22f"     , [t| C'Matx22f     |] )
  , ( C.TypeName "Matx22d"     , [t| C'Matx22d     |] )
  , ( C.TypeName "Matx23f"     , [t| C'Matx23f     |] )
  , ( C.TypeName "Matx23d"     , [t| C'Matx23d     |] )
  , ( C.TypeName "Matx31f"     , [t| C'Matx31f     |] )
  , ( C.TypeName "Matx31d"     , [t| C'Matx31d     |] )
  , ( C.TypeName "Matx32f"     , [t| C'Matx32f     |] )
  , ( C.TypeName "Matx32d"     , [t| C'Matx32d     |] )
  , ( C.TypeName "Matx33f"     , [t| C'Matx33f     |] )
  , ( C.TypeName "Matx33d"     , [t| C'Matx33d     |] )
  , ( C.TypeName "Matx34f"     , [t| C'Matx34f     |] )
  , ( C.TypeName "Matx34d"     , [t| C'Matx34d     |] )
  , ( C.TypeName "Matx41f"     , [t| C'Matx41f     |] )
  , ( C.TypeName "Matx41d"     , [t| C'Matx41d     |] )
  , ( C.TypeName "Matx43f"     , [t| C'Matx43f     |] )
  , ( C.TypeName "Matx43d"     , [t| C'Matx43d     |] )
  , ( C.TypeName "Matx44f"     , [t| C'Matx44f     |] )
  , ( C.TypeName "Matx44d"     , [t| C'Matx44d     |] )
  , ( C.TypeName "Matx51f"     , [t| C'Matx51f     |] )
  , ( C.TypeName "Matx51d"     , [t| C'Matx51d     |] )
  , ( C.TypeName "Matx61f"     , [t| C'Matx61f     |] )
  , ( C.TypeName "Matx61d"     , [t| C'Matx61d     |] )
  , ( C.TypeName "Matx66f"     , [t| C'Matx66f     |] )
  , ( C.TypeName "Matx66d"     , [t| C'Matx66d     |] )

  , ( C.TypeName "Vec2i"       , [t| C'Vec2i       |] )
  , ( C.TypeName "Vec2f"       , [t| C'Vec2f       |] )
  , ( C.TypeName "Vec2d"       , [t| C'Vec2d       |] )
  , ( C.TypeName "Vec3i"       , [t| C'Vec3i       |] )
  , ( C.TypeName "Vec3f"       , [t| C'Vec3f       |] )
  , ( C.TypeName "Vec3d"       , [t| C'Vec3d       |] )
  , ( C.TypeName "Vec4i"       , [t| C'Vec4i       |] )
  , ( C.TypeName "Vec4f"       , [t| C'Vec4f       |] )
  , ( C.TypeName "Vec4d"       , [t| C'Vec4d       |] )

  , ( C.TypeName "Point2i"     , [t| C'Point2i     |] )
  , ( C.TypeName "Point2f"     , [t| C'Point2f     |] )
  , ( C.TypeName "Point2d"     , [t| C'Point2d     |] )
  , ( C.TypeName "Point3i"     , [t| C'Point3i     |] )
  , ( C.TypeName "Point3f"     , [t| C'Point3f     |] )
  , ( C.TypeName "Point3d"     , [t| C'Point3d     |] )

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

--, ( C.TypeName "MSER"                  , [t| C'MSER                   |] )
  , ( C.TypeName "Ptr_ORB"               , [t| C'Ptr_ORB                |] )
--, ( C.TypeName "BRISK"                 , [t| C'BRISK                  |] )
--, ( C.TypeName "KAZE"                  , [t| C'KAZE                   |] )
--, ( C.TypeName "AKAZE"                 , [t| C'AKAZE                  |] )
  , ( C.TypeName "Ptr_SimpleBlobDetector", [t| C'Ptr_SimpleBlobDetector |] )

  , ( C.TypeName "DescriptorMatcher"     , [t| C'DescriptorMatcher      |] )
  , ( C.TypeName "BFMatcher"             , [t| C'BFMatcher              |] )
  , ( C.TypeName "FlannBasedMatcher"     , [t| C'FlannBasedMatcher      |] )

  , ( C.TypeName "Ptr_BackgroundSubtractorKNN" , [t| C'Ptr_BackgroundSubtractorKNN  |] )
  , ( C.TypeName "Ptr_BackgroundSubtractorMOG2", [t| C'Ptr_BackgroundSubtractorMOG2 |] )

  , ( C.TypeName "VideoCapture", [t| C'VideoCapture |] )
  , ( C.TypeName "VideoWriter" , [t| C'VideoWriter  |] )

  , ( C.TypeName "CascadeClassifier", [t| C'CascadeClassifier |] )

  , ( C.TypeName "MouseCallback"   , [t| FunPtr C'MouseCallback    |] )
  , ( C.TypeName "TrackbarCallback", [t| FunPtr C'TrackbarCallback |] )

  , ( C.TypeName "Net"      , [t| C'Net       |] )
  , ( C.TypeName "DictValue", [t| C'DictValue |] )
  ]
