{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface between OpenCV and inline-c(pp) (Haskell)
module Language.C.Inline.OpenCV
    ( C'Exception
    , C'Point2i
    , C'Point2f
    , C'Point2d
    , C'Point3i
    , C'Point3f
    , C'Point3d
    , C'Size2i
    , C'Size2f
    , C'Rect
    , C'RotatedRect
    , C'TermCriteria
    , C'Range
    , C'Scalar
    , C'Mat

    , C'MouseCallback
    , C'TrackbarCallback

    , openCvCtx
    ) where

import "base" Foreign.Ptr ( Ptr, FunPtr )
import "base" Data.Int ( Int32 )
import "base" Data.Monoid ( (<>) )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C

-- | Haskell representation of an OpenCV exception
data C'Exception
-- | Haskell representation of an OpenCV @cv::Point_\<int>@ object
data C'Point2i
-- | Haskell representation of an OpenCV @cv::Point_\<float>@ object
data C'Point2f
-- | Haskell representation of an OpenCV @cv::Point_\<double>@ object
data C'Point2d
-- | Haskell representation of an OpenCV @cv::Point3_\<int>@ object
data C'Point3i
-- | Haskell representation of an OpenCV @cv::Point3_\<float>@ object
data C'Point3f
-- | Haskell representation of an OpenCV @cv::Point3_\<double>@ object
data C'Point3d
-- | Haskell representation of an OpenCV @cv::Size_\<int>@ object
data C'Size2i
-- | Haskell representation of an OpenCV @cv::Size_\<float>@ object
data C'Size2f
-- | Haskell representation of an OpenCV @cv::Rect_\<int>@ object
data C'Rect
-- | Haskell representation of an OpenCV @cv::RotatedRect@ object
data C'RotatedRect
-- | Haskell representation of an OpenCV @cv::TermCriteria@ object
data C'TermCriteria
-- | Haskell representation of an OpenCV @cv::Range@ object
data C'Range
-- | Haskell representation of an OpenCV @cv::Scalar_\<double>@ object
data C'Scalar
-- | Haskell representation of an OpenCV @cv::Mat@ object
data C'Mat

-- | Callback function for mouse events
type C'MouseCallback
   =  Int32 -- ^ One of the @cv::MouseEvenTypes@ constants.
   -> Int32 -- ^ The x-coordinate of the mouse event.
   -> Int32 -- ^ The y-coordinate of the mouse event.
   -> Int32 -- ^ One of the @cv::MouseEventFlags@ constants.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

-- | Callback function for trackbars
type C'TrackbarCallback
   =  Int32 -- ^ Current position of the specified trackbar.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

-- | Context useful to work with the OpenCV library
--
-- Based on 'C.cppCtx', 'C.bsCtx' and 'C.vecCtx'.
--
-- 'C.ctxTypesTable': converts OpenCV basic types to their counterparts in
-- "Language.C.Inline.OpenCV".
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
  [ ( C.TypeName "bool"        , [t| C.CInt |] )

  , ( C.TypeName "Exception"   , [t| C'Exception    |] )
  , ( C.TypeName "Point2i"     , [t| C'Point2i      |] )
  , ( C.TypeName "Point2f"     , [t| C'Point2f      |] )
  , ( C.TypeName "Point2d"     , [t| C'Point2d      |] )
  , ( C.TypeName "Point3i"     , [t| C'Point3i      |] )
  , ( C.TypeName "Point3f"     , [t| C'Point3f      |] )
  , ( C.TypeName "Point3d"     , [t| C'Point3d      |] )
  , ( C.TypeName "Size2i"      , [t| C'Size2i       |] )
  , ( C.TypeName "Size2f"      , [t| C'Size2f       |] )
  , ( C.TypeName "Rect"        , [t| C'Rect         |] )
  , ( C.TypeName "RotatedRect" , [t| C'RotatedRect  |] )
  , ( C.TypeName "TermCriteria", [t| C'TermCriteria |] )
  , ( C.TypeName "Scalar"      , [t| C'Scalar       |] )
  , ( C.TypeName "Mat"         , [t| C'Mat          |] )
  , ( C.TypeName "Range"       , [t| C'Range        |] )

  , ( C.TypeName "MouseCallback"   , [t| FunPtr C'MouseCallback    |] )
  , ( C.TypeName "TrackbarCallback", [t| FunPtr C'TrackbarCallback |] )
  ]
