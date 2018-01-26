{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Point
    ( Point
    , PointDim
    , IsPoint(..)
    , IsPoint2
    , IsPoint3

    , Point2i, Point2f, Point2d
    , Point3i, Point3f, Point3d
    ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import qualified "inline-c"     Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C ( using )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Point
import "this" OpenCV.Internal.Core.Types.Point.TH

--------------------------------------------------------------------------------

C.context openCvCtx
C.include "opencv2/core.hpp"
C.using "namespace cv"

mkPointType "Point2i" 2 "Point_"  ''Int32   ''C'Point2i "int32_t"
mkPointType "Point2f" 2 "Point_"  ''CFloat  ''C'Point2f "float"
mkPointType "Point2d" 2 "Point_"  ''CDouble ''C'Point2d "double"

mkPointType "Point3i" 3 "Point3_" ''Int32   ''C'Point3i "int32_t"
mkPointType "Point3f" 3 "Point3_" ''CFloat  ''C'Point3f "float"
mkPointType "Point3d" 3 "Point3_" ''CDouble ''C'Point3d "double"
