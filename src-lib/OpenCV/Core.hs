module OpenCV.Core
    ( -- * Point
      -- ** 2D types
      Point2i
    , Point
    , Point2f
    , Point2d
      -- ** 3D types
    , Point3i
    , Point3f
    , Point3d
      -- ** 2D point construction
    , mkPoint2i
    , mkPoint
    , mkPoint2f
    , mkPoint2d
      -- ** 3D point construction
    , mkPoint3i
    , mkPoint3f
    , mkPoint3d
      -- * Size
    , Size2i
    , Size
    , Size2f
    , mkSize2i
    , mkSize
    , mkSize2f
      -- * Rect
    , Rect
    , mkRect
    , rectTopLeft
    , rectBottomRight
    , rectSize
    , rectArea
    , rectContains
      -- * RotatedRect
    , RotatedRect
    , mkRotatedRect
    , rotatedRectCenter
    , rotatedRectSize
    , rotatedRectAngle
    , rotatedRectBoundingRect
    , rotatedRectPoints
      -- * TermCriteria
    , TermCriteria
    , mkTermCriteria
      -- * Scalar
    , Scalar
    , mkScalar
      -- * Matrix
    , Mat
    , newEmptyMat
    , cloneMat
      -- * Mutable Matrix
    , MutMat
    , IOMat
    , STMat
    , freeze
    , thaw
    , createMat
      -- * Exception
    , CvException
    ) where

import "this" OpenCV.Internal
