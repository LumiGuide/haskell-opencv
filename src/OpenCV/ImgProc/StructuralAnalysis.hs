{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.StructuralAnalysis
    ( contourArea
    , pointPolygonTest
    ) where

import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Exception ( CvException, handleCvException, cvExcept )
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Structural Analysis and Shape Descriptors
--------------------------------------------------------------------------------

{- | Calculates a contour area.

The function computes a contour area. Similarly to `moments`, the area is
computed using the <https://en.wikipedia.org/wiki/Green%27s_theorem Green formula>.
Thus, the returned area and the number of non-zero pixels, if you draw the
contour using `drawContours` or `fillPoly`, can be different. Also, the function
will most certainly give a wrong results for contours with self-intersections.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/structural_analysis_and_shape_descriptors.html?highlight=contourarea#cv2.contourArea OpenCV Sphinx doc>
-}
contourArea
    :: (Convert point2f Point2f)
    => V.Vector point2f
       -- ^ Input vector of 2D points (contour vertices).
    -> Bool
       -- ^ Oriented area flag. If it is true, the function returns a signed
       -- area value, depending on the contour orientation (clockwise or
       -- counter-clockwise). Using this feature you can determine orientation
       -- of a contour by taking the sign of an area.
    -> Either CvException Double
contourArea contour oriented = unsafePerformIO $
    withArrayPtr (V.map convert contour :: V.Vector Point2f) $ \contourPtr ->
    alloca $ \c'area ->
    handleCvException (realToFrac <$> peek c'area) $
      [cvExcept|
        cv::_InputArray contour =
          cv::_InputArray( $(Point2f * contourPtr)
                         , $(int32_t c'numPoints)
                         );
        *$(double * c'area) = cv::contourArea(contour, $(bool c'oriented));
      |]
  where
    c'numPoints = fromIntegral $ V.length contour
    c'oriented = fromBool oriented

-- | Performs a point-in-contour test.
--
-- The function determines whether the point is inside a contour, outside, or
-- lies on an edge (or coincides with a vertex). It returns positive (inside),
-- negative (outside), or zero (on an edge) value, correspondingly. When
-- measureDist=false , the return value is +1, -1, and 0,
-- respectively. Otherwise, the return value is a signed distance between the
-- point and the nearest contour edge.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/structural_analysis_and_shape_descriptors.html#pointpolygontest OpenCV Sphinx doc>
pointPolygonTest
    :: ( Convert contourPoint2f Point2f
       , Convert testPoint2f    Point2f
       )
    => V.Vector contourPoint2f -- ^ Contour.
    -> testPoint2f -- ^ Point tested against the contour.
    -> Bool
       -- ^ If true, the function estimates the signed distance from the point
       -- to the nearest contour edge. Otherwise, the function only checks if
       -- the point is inside a contour or not.
    -> Either CvException Double
pointPolygonTest contour pt measureDist = unsafePerformIO $
    withArrayPtr (V.map convert contour :: V.Vector Point2f) $ \contourPtr ->
    withPtr (convert pt :: Point2f) $ \ptPtr ->
    alloca $ \c'resultPtr ->
    handleCvException (realToFrac <$> peek c'resultPtr) $
      [cvExcept|
        cv::_InputArray contour =
          cv::_InputArray( $(Point2f * contourPtr)
                         , $(int32_t c'numPoints)
                         );
        *$(double * c'resultPtr) =
          cv::pointPolygonTest( contour
                              , *$(Point2f * ptPtr)
                              , $(bool c'measureDist)
                              );
      |]
  where
    c'numPoints = fromIntegral $ V.length contour
    c'measureDist = fromBool measureDist
