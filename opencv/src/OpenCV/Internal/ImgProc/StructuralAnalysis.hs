{-# language CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgProc.StructuralAnalysis
  ( convexHull
  , ConvexHull(..)
  ) where

import "base" Data.Int
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.PlacementNew ( PlacementNew )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Exception
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

{- | Finds the convex hull of a point set.

Finds the convex hull of a 2D point set using the Sklansky's algorithm
that has \( O(n \log n) \) complexity in the current implementation.
-}
convexHull
    :: forall point2 depth
     . ( IsPoint2 point2 depth
       , ConvexHull depth
       )
    => V.Vector (point2 depth)
       -- ^ Input 2D point set.
    -> Bool
       -- ^ Orientation flag. If it is true, the output convex hull is oriented
       -- clockwise. Otherwise, it is oriented counter-clockwise. The assumed
       -- coordinate system has its X axis pointing to the right, and its Y axis
       -- pointing upwards.
    -> CvExcept (V.Vector (Point 2 depth))
       -- ^ Output convex hull.
convexHull points clockwise = unsafeWrapException $
    withArrayPtr (V.map toPoint points) $ \(pointsPtr :: Ptr (C (Point 2 depth))) ->
    alloca $ \(hullPointsPtrPtr :: Ptr (Ptr (Ptr (C (Point 2 depth))))) ->
    alloca $ \(numHullPointsPtr :: Ptr Int32) ->
    handleCvException
      ( do numHullPoints <- fromIntegral <$> peek numHullPointsPtr
           hullPointsPtr :: Ptr (Ptr (C (Point 2 depth))) <- peek hullPointsPtrPtr
           hullPointsList :: [Ptr (C (Point 2 depth))] <- peekArray numHullPoints hullPointsPtr
           hullPointsVec <- V.fromList <$> mapM (fromPtr . pure) hullPointsList
           convexHull_deletePtrArray hullPointsPtrPtr
           pure hullPointsVec
      ) $
      convexHull_internal
        (fromIntegral $ V.length points)
        (fromBool clockwise)
        pointsPtr
        hullPointsPtrPtr
        numHullPointsPtr

-- | Internal class used to overload the 'convexHull' depth.
class ( FromPtr      (Point   2 depth)
      , CSizeOf      (C'Point 2 depth)
      , PlacementNew (C'Point 2 depth)
      ) => ConvexHull depth where
    convexHull_internal
        :: Int32
        -> CInt
        -> Ptr (C (Point 2 depth))
        -> Ptr (Ptr (Ptr (C (Point 2 depth))))
        -> Ptr Int32
        -> IO (Ptr (C CvCppException))

    convexHull_deletePtrArray
        :: Ptr (Ptr (Ptr (C (Point 2 depth))))
        -> IO ()

instance ConvexHull CFloat where
    convexHull_internal numPoints clockwise pointsPtr hullPointsPtrPtr numHullPointsPtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2f * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<cv::Point2f> hull;
          cv::convexHull
            ( points
            , hull
            , $(bool clockwise)
            , true
            );

          *$(int32_t * numHullPointsPtr) = hull.size();

          cv::Point2f * * * hullPointsPtrPtr = $(Point2f * * * hullPointsPtrPtr);
          cv::Point2f * * hullPointsPtr = new cv::Point2f * [hull.size()];
          *hullPointsPtrPtr = hullPointsPtr;

          for (std::vector<cv::Point2i>::size_type i = 0; i < hull.size(); i++)
          {
            cv::Point2f & hullPt = hull[i];
            cv::Point2f * newHullPt = new cv::Point2f(hullPt.x, hullPt.y);
            hullPointsPtr[i] = newHullPt;
          }
        |]

    convexHull_deletePtrArray hullPointsPtrPtr =
        [CU.block| void {
          delete [] *$(Point2f * * * hullPointsPtrPtr);
        }|]

instance ConvexHull Int32 where
    convexHull_internal numPoints clockwise pointsPtr hullPointsPtrPtr numHullPointsPtr =
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2i * pointsPtr)
                           , $(int32_t numPoints)
                           );
          std::vector<cv::Point2i> hull;
          cv::convexHull
            ( points
            , hull
            , $(bool clockwise)
            , true
            );

          *$(int32_t * numHullPointsPtr) = hull.size();

          cv::Point2i * * * hullPointsPtrPtr = $(Point2i * * * hullPointsPtrPtr);
          cv::Point2i * * hullPointsPtr = new cv::Point2i * [hull.size()];
          *hullPointsPtrPtr = hullPointsPtr;

          for (std::vector<cv::Point2i>::size_type i = 0; i < hull.size(); i++)
          {
            cv::Point2i & hullPt = hull[i];
            cv::Point2i * newHullPt = new cv::Point2i(hullPt.x, hullPt.y);
            hullPointsPtr[i] = newHullPt;
          }
        |]

    convexHull_deletePtrArray hullPointsPtrPtr =
        [CU.block| void {
          delete [] *$(Point2i * * * hullPointsPtrPtr);
        }|]
