{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.StructuralAnalysis
    ( contourArea
    , pointPolygonTest
    , findContours
    , Contour(..)
    , ContourAreaOriented(..)
    , ContourRetrievalMode(..)
    , ContourApproximationMethod(..)
    , minAreaRect
    ) where

import "base" Control.Exception ( mask_ )
import "base" Control.Monad (guard)
import "base" Data.Functor (($>))
import "base" Data.Int
import "base" Data.Maybe (mapMaybe)
import "base" Data.Traversable (for)
import qualified "vector" Data.Vector as V
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear.V4 ( V4(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Exception.Internal
import "this" OpenCV.TypeLevel
import "base" System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include "opencv2/imgproc.hpp"

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
    :: (ToPoint2f point2f)
    => V.Vector point2f
       -- ^ Input vector of 2D points (contour vertices).
    -> ContourAreaOriented
       -- ^ Signed or unsigned area
    -> CvExcept Double
contourArea contour areaOriented = unsafeWrapException $
    withArrayPtr (V.map toPoint2f contour) $ \contourPtr ->
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
    oriented =
      case areaOriented of
        ContourAreaOriented -> True
        ContourAreaAbsoluteValue -> False
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
    :: ( ToPoint2f contourPoint2f
       , ToPoint2f testPoint2f
       )
    => V.Vector contourPoint2f -- ^ Contour.
    -> testPoint2f -- ^ Point tested against the contour.
    -> Bool
       -- ^ If true, the function estimates the signed distance from the point
       -- to the nearest contour edge. Otherwise, the function only checks if
       -- the point is inside a contour or not.
    -> CvExcept Double
pointPolygonTest contour pt measureDist = unsafeWrapException $
    withArrayPtr (V.map toPoint2f contour) $ \contourPtr ->
    withPtr (toPoint2f pt) $ \ptPtr ->
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

-- | Oriented area flag.
data ContourAreaOriented
  = ContourAreaOriented
    -- ^ Return a signed area value, depending on the contour orientation (clockwise or
    -- counter-clockwise). Using this feature you can determine orientation
    -- of a contour by taking the sign of an area.
  | ContourAreaAbsoluteValue
    -- ^ Return the area as an absolute value.

data ContourRetrievalMode
  = ContourRetrievalExternal
    -- ^ Retrieves only the extreme outer contours.
  | ContourRetrievalList
    -- ^ Retrieves all of the contours without establishing any hierarchical relationships.
  | ContourRetrievalCComp
    -- ^ Retrieves all of the contours and organizes them into a two-level hierarchy. At the top level, there are external boundaries of the components. At the second level, there are boundaries of the holes. If there is another contour inside a hole of a connected component, it is still put at the top level.
  | ContourRetrievalTree
    -- ^ Retrieves all of the contours and reconstructs a full hierarchy of nested contours.

data ContourApproximationMethod
  = ContourApproximationNone
    -- ^ Stores absolutely all the contour points. That is, any 2 subsequent points @(x1,y1)@ and @(x2,y2)@ of the contour will be either horizontal, vertical or diagonal neighbors, that is, @max(abs(x1-x2),abs(y2-y1)) == 1@.
  | ContourApproximationSimple
    -- ^ Compresses horizontal, vertical, and diagonal segments and leaves only their end points. For example, an up-right rectangular contour is encoded with 4 points.
  | ContourApproximationTC89L1
  | ContourApproximationTC89KCOS

#num CV_RETR_EXTERNAL
#num CV_RETR_LIST
#num CV_RETR_CCOMP
#num CV_RETR_TREE
#num CV_CHAIN_APPROX_NONE
#num CV_CHAIN_APPROX_SIMPLE
#num CV_CHAIN_APPROX_TC89_L1
#num CV_CHAIN_APPROX_TC89_KCOS

marshalContourRetrievalMode
  :: ContourRetrievalMode -> Int32
marshalContourRetrievalMode = \case
  ContourRetrievalExternal -> c'CV_RETR_EXTERNAL
  ContourRetrievalList -> c'CV_RETR_LIST
  ContourRetrievalCComp -> c'CV_RETR_CCOMP
  ContourRetrievalTree -> c'CV_RETR_TREE

marshalContourApproximationMethod
  :: ContourApproximationMethod -> Int32
marshalContourApproximationMethod = \case
  ContourApproximationNone -> c'CV_CHAIN_APPROX_NONE
  ContourApproximationSimple -> c'CV_CHAIN_APPROX_SIMPLE
  ContourApproximationTC89L1 -> c'CV_CHAIN_APPROX_TC89_L1
  ContourApproximationTC89KCOS -> c'CV_CHAIN_APPROX_TC89_KCOS

data Contour =
  Contour {contourPoints :: !(V.Vector Point2i)
          ,contourChildren :: !(V.Vector Contour)}

findContours
  :: ContourRetrievalMode
  -> ContourApproximationMethod
  -> Mat ('S [h, w]) ('S 1) ('S Word8)
  -> V.Vector Contour
findContours mode method src = unsafePerformIO $
  withPtr src $ \srcPtr ->
  alloca $ \(contourLengthsPtrPtr :: Ptr (Ptr Int32)) ->
  alloca $ \(contoursPtrPtr :: Ptr (Ptr (Ptr (Ptr C'Point2i)))) ->
  alloca $ \(hierarchyPtrPtr :: Ptr (Ptr (Ptr C'Vec4i))) ->
  alloca $ \(numContoursPtr :: Ptr Int32) -> mask_ $ do
    [C.block| void {
      std::vector<std::vector<cv::Point>> contours;
      std::vector<cv::Vec4i> hierarchy;
      cv::findContours(
        *$(Mat * srcPtr),
        contours,
        hierarchy,
        $(int32_t c'mode),
        $(int32_t c'method)
      );

      *$(int32_t * numContoursPtr) = contours.size();

      cv::Point * * * * contoursPtrPtr = $(Point2i * * * * contoursPtrPtr);
      cv::Point * * * contoursPtr = new cv::Point * * [contours.size()];
      *contoursPtrPtr = contoursPtr;

      cv::Vec4i * * * hierarchyPtrPtr = $(Vec4i * * * hierarchyPtrPtr);
      cv::Vec4i * * hierarchyPtr = new cv::Vec4i * [contours.size()];
      *hierarchyPtrPtr = hierarchyPtr;

      int32_t * * contourLengthsPtrPtr = $(int32_t * * contourLengthsPtrPtr);
      int32_t * contourLengthsPtr = new int32_t [contours.size()];
      *contourLengthsPtrPtr = contourLengthsPtr;

      for (std::vector<std::vector<cv::Point>>::size_type i = 0; i < contours.size(); i++) {
        std::vector<cv::Point> & contourPoints = contours[i];
        cv::Vec4i hierarchyInfo = hierarchy[i];

        contourLengthsPtr[i] = contourPoints.size();

        cv::Point * * newContourPoints = new cv::Point * [contourPoints.size()];
        for (std::vector<cv::Point>::size_type j = 0; j < contourPoints.size(); j++) {
          cv::Point & orig = contourPoints[j];
          cv::Point * newPt = new cv::Point(orig.x, orig.y);
          newContourPoints[j] = newPt;
        }
        contoursPtr[i] = newContourPoints;

        hierarchyPtr[i] = new cv::Vec4i(
          hierarchyInfo[0],
          hierarchyInfo[1],
          hierarchyInfo[2],
          hierarchyInfo[3]
        );
      }
    }|]

    numContours <- fromIntegral <$> peek numContoursPtr

    contourLengthsPtr <- peek contourLengthsPtrPtr
    contourLengths <- peekArray numContours contourLengthsPtr

    contoursPtr <- peek contoursPtrPtr
    unmarshalledContours <- peekArray numContours contoursPtr

    allContours <- for (zip unmarshalledContours contourLengths) $ \(contourPointsPtr,n) ->
      fmap V.fromList
           (peekArray (fromIntegral n) contourPointsPtr >>= mapM (fromPtr . pure))

    hierarchyPtr <- peek hierarchyPtrPtr
    (hierarchy :: [V4 Int32]) <-
        peekArray numContours hierarchyPtr >>=
        mapM (fmap fromVec4i . fromPtr . pure)

    let treeHierarchy =
          zipWith (\(V4 nextSibling previousSibling firstChild parent) points ->
                (Contour { contourPoints = points
                         , contourChildren = if firstChild < 0
                                               then mempty
                                               else V.fromList (map fst treeHierarchy !! fromIntegral firstChild)
                         } : if nextSibling < 0
                               then []
                               else map fst treeHierarchy !! fromIntegral nextSibling
                ,parent < 0 && previousSibling < 0)
              )
              hierarchy
              allContours

    [CU.block| void {
      delete [] *$(Point2i * * * * contoursPtrPtr);
      delete [] *$(Vec4i * * * hierarchyPtrPtr);
      delete [] *$(int32_t * * contourLengthsPtrPtr);
    } |]

    return (V.fromList
              (concat (mapMaybe (\(contours,isRoot) -> guard isRoot $> contours) treeHierarchy)))
  where
    c'mode = marshalContourRetrievalMode mode
    c'method = marshalContourApproximationMethod method

minAreaRect :: (ToPoint2i point2i)
            => V.Vector point2i -> RotatedRect
minAreaRect points =
  unsafePerformIO $ fromPtr $
  withArrayPtr (V.map toPoint2i points) $
  \pointsPtr ->
    [CU.exp|
      RotatedRect * {
        new RotatedRect(
          cv::minAreaRect(
              cv::_InputArray( $(Point2i * pointsPtr)
                             , $(int32_t c'numPoints))))
      }
    |]
  where c'numPoints = fromIntegral (V.length points)
