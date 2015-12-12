{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.OpenCV
    ( C'Exception
    , C'Point
    , C'Point2i
    , C'Point2f
    , C'Point2d
    , C'Point3i
    , C'Point3f
    , C'Point3d
    , C'Size
    , C'Size2i
    , C'Size2f
    , C'Rect
    , C'RotatedRect
    , C'TermCriteria
    , C'Scalar
    , C'Mat

    , C'MouseCallback
    , C'TrackbarCallback

    , openCvCtx
    ) where

import "base" Foreign.Ptr ( Ptr, FunPtr )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Types  as C
import qualified "inline-c" Language.C.Inline.Context as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude hiding ( yield )

data C'Exception
type C'Point = C'Point2i
data C'Point2i
data C'Point2f
data C'Point2d
data C'Point3i
data C'Point3f
data C'Point3d
type C'Size = C'Size2i
data C'Size2i
data C'Size2f
data C'Rect
data C'RotatedRect
data C'TermCriteria
data C'Scalar
data C'Mat

type C'MouseCallback = C.CInt -> C.CInt -> C.CInt -> C.CInt -> Ptr () -> IO ()
type C'TrackbarCallback = C.CInt -> Ptr () -> IO ()

openCvCtx :: C.Context
openCvCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty
      { C.ctxTypesTable = openCvTypesTable
      }

openCvTypesTable :: C.TypesTable
openCvTypesTable = M.fromList
  [ (C.TypeName "bool"        , [t| C.CInt |] )

  , (C.TypeName "Exception"   , [t| C'Exception    |] )
  , (C.TypeName "Point"       , [t| C'Point        |] )
  , (C.TypeName "Point2i"     , [t| C'Point2i      |] )
  , (C.TypeName "Point2f"     , [t| C'Point2f      |] )
  , (C.TypeName "Point2d"     , [t| C'Point2d      |] )
  , (C.TypeName "Point3i"     , [t| C'Point3i      |] )
  , (C.TypeName "Point3f"     , [t| C'Point3f      |] )
  , (C.TypeName "Point3d"     , [t| C'Point3d      |] )
  , (C.TypeName "Size"        , [t| C'Size         |] )
  , (C.TypeName "Size2i"      , [t| C'Size2i       |] )
  , (C.TypeName "Size2f"      , [t| C'Size2f       |] )
  , (C.TypeName "Rect"        , [t| C'Rect         |] )
  , (C.TypeName "RotatedRect" , [t| C'RotatedRect  |] )
  , (C.TypeName "TermCriteria", [t| C'TermCriteria |] )
  , (C.TypeName "Scalar"      , [t| C'Scalar       |] )
  , (C.TypeName "Mat"         , [t| C'Mat          |] )

  , (C.TypeName "MouseCallback"   , [t| FunPtr C'MouseCallback    |] )
  , (C.TypeName "TrackbarCallback", [t| FunPtr C'TrackbarCallback |] )
  ]
