{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.StructuralAnalysis
    ( Contour(..)
    , ContourAreaOriented(..)
    , ContourRetrievalMode(..)
    , ContourApproximationMethod(..)

    , approxPolyDP, ApproxPolyDP
    , arcLength
    , boundingRect, BoundingRect
    , contourArea
    , convexHull, ConvexHull
    , convexHullIndices, ConvexHullIndices
    , convexityDefects
    , findContours
    , isContourConvex
    , minAreaRect
    , pointPolygonTest
    ) where

import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "base" Control.Exception ( mask_ )
import "base" Control.Monad ( guard )
import "base" Data.Functor ( ($>) )
import "base" Data.Int
import "base" Data.Maybe ( mapMaybe )
import "base" Data.Traversable (for)
import "base" Data.Word
import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( fromBool, toBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import "linear" Linear.V4 ( V4(..) )
import "mtl" Control.Monad.Error.Class ( MonadError )
import "this" OpenCV.Core.Types ( Mut )
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Core.Types.Vec ( fromVec, Vec4i )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.Internal.ImgProc.StructuralAnalysis
import "this" OpenCV.TypeLevel
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as VS

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

-- | Oriented area flag.
data ContourAreaOriented
   = ContourAreaOriented
     -- ^ Return a signed area value, depending on the contour orientation
     -- (clockwise or counter-clockwise). Using this feature you can determine
     -- orientation of a contour by taking the sign of an area.
   | ContourAreaAbsoluteValue
     -- ^ Return the area as an absolute value.

data ContourRetrievalMode
   = ContourRetrievalExternal
     -- ^ Retrieves only the extreme outer contours.
   | ContourRetrievalList
     -- ^ Retrieves all of the contours without establishing any hierarchical
     -- relationships.
   | ContourRetrievalCComp
     -- ^ Retrieves all of the contours and organizes them into a two-level
     -- hierarchy. At the top level, there are external boundaries of the
     -- components. At the second level, there are boundaries of the holes. If
     -- there is another contour inside a hole of a connected component, it is
     -- still put at the top level.
   | ContourRetrievalTree
     -- ^ Retrieves all of the contours and reconstructs a full hierarchy of
     -- nested contours.

data ContourApproximationMethod
   = ContourApproximationNone
     -- ^ Stores absolutely all the contour points. That is, any 2 subsequent
     -- points @(x1,y1)@ and @(x2,y2)@ of the contour will be either horizontal,
     -- vertical or diagonal neighbors, that is, @max(abs(x1-x2),abs(y2-y1)) ==
     -- 1@.
   | ContourApproximationSimple
     -- ^ Compresses horizontal, vertical, and diagonal segments and leaves only
     -- their end points. For example, an up-right rectangular contour is
     -- encoded with 4 points.
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

marshalContourRetrievalMode :: ContourRetrievalMode -> Int32
marshalContourRetrievalMode = \case
    ContourRetrievalExternal -> c'CV_RETR_EXTERNAL
    ContourRetrievalList     -> c'CV_RETR_LIST
    ContourRetrievalCComp    -> c'CV_RETR_CCOMP
    ContourRetrievalTree     -> c'CV_RETR_TREE

marshalContourApproximationMethod :: ContourApproximationMethod -> Int32
marshalContourApproximationMethod = \case
    ContourApproximationNone     -> c'CV_CHAIN_APPROX_NONE
    ContourApproximationSimple   -> c'CV_CHAIN_APPROX_SIMPLE
    ContourApproximationTC89L1   -> c'CV_CHAIN_APPROX_TC89_L1
    ContourApproximationTC89KCOS -> c'CV_CHAIN_APPROX_TC89_KCOS

data Contour
   = Contour
     { contourPoints   :: !(V.Vector Point2i)
     , contourChildren :: !(V.Vector Contour)
     } deriving Show

arcLength
    :: (IsPoint2 point2 Int32, MonadError CvException m)
    => V.Vector (point2 Int32)
    -> Bool -- ^ is closed
    -> m Double
arcLength curve isClosed = unsafeWrapException $
    withArrayPtr (V.map toPoint curve) $ \curvePtr ->
    alloca $ \c'resultPtr ->
    handleCvException (realToFrac <$> peek c'resultPtr) $
        [cvExcept|
            cv::_InputArray curve =
              cv::_InputArray ( $(Point2i * curvePtr)
                              , $(int32_t c'numCurvePoints)
                              );
            *$(double * c'resultPtr) =
               cv::arcLength( curve
                            , $(bool c'isClosed)
                            );
        |]
    where
      c'isClosed = fromBool isClosed
      c'numCurvePoints = fromIntegral $ V.length curve

{- | Calculates a contour area.

The function computes a contour area. Similarly to `moments`, the area is
computed using the <https://en.wikipedia.org/wiki/Green%27s_theorem Green formula>.
Thus, the returned area and the number of non-zero pixels, if you draw the
contour using `drawContours` or `fillPoly`, can be different. Also, the function
will most certainly give a wrong results for contours with self-intersections.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/structural_analysis_and_shape_descriptors.html?highlight=contourarea#cv2.contourArea OpenCV Sphinx doc>
-}
contourArea
    :: (IsPoint2 point2 CFloat, MonadError CvException m)
    => V.Vector (point2 CFloat)
       -- ^ Input vector of 2D points (contour vertices).
    -> ContourAreaOriented
       -- ^ Signed or unsigned area
    -> m Double
contourArea contour areaOriented = unsafeWrapException $
    withArrayPtr (V.map toPoint contour) $ \contourPtr ->
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

{- | Finds the convexity defects of a contour.

Example:

@
handDefectsImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Hand)
    => IO (Mat ('S ['S height, 'S width]) ('S channels) ('S depth))
handDefectsImg = do
    handContour <- mkHandContour
    let -- Indices of points in \'handContour\' that are part of the convex hull.
        handHullIndices :: VS.Vector Int32
        handHullIndices = exceptError $ convexHullIndices handContour True

        handHullIndices' :: V.Vector Int
        handHullIndices' = V.map fromIntegral $ V.convert handHullIndices

        handHull :: V.Vector Point2i
        handHull = V.backpermute handContour handHullIndices'

        handDefects :: V.Vector Vec4i
        handDefects = exceptError $ convexityDefects handContour handHullIndices

    pure $ exceptError $
      withMatM (Proxy :: Proxy [height, width])
               (Proxy :: Proxy channels)
               (Proxy :: Proxy depth)
               transparent $ \\imgM -> do
        -- Draw contour.
        polylines imgM (V.singleton handContour) True blue 2 LineType_AA 0
        -- Draw convex hull of contour.
        polylines imgM (V.singleton handHull) True red 2 LineType_AA 0
        -- Draw defects.
        forM_ handDefects $ \defect -> do
          let V4 _start_index _end_index farthest_pt_index _fixpt_depth = fromVec defect
          let farthest_pt = handContour V.! (fromIntegral farthest_pt_index)
          circle imgM farthest_pt 7 green (-1) LineType_AA 0
@

<<doc/generated/examples/handDefectsImg.png handDefectsImg>>
-}
convexityDefects
    :: (IsPoint2 point2 Int32, MonadError CvException m)
    => V.Vector (point2 Int32) -- ^ Input contour
    -> VS.Vector Int32
       -- ^ Indices (0 based) of those points in the input contour
       -- that form a convex hull.
    -> m (V.Vector Vec4i)
       -- ^ The output vector of convexity defects.
convexityDefects contour hull = unsafeWrapException $
    withArrayPtr (V.map toPoint contour) $ \contourPtr ->
    alloca $ \(defectsPtrPtr :: Ptr (Ptr (Ptr (C Vec4i)))) ->
    alloca $ \(defectsSizePtr :: Ptr Int32) ->
    handleCvException
      ( do defectsSize <- fromIntegral <$> peek defectsSizePtr
           defectsPtr :: Ptr (Ptr (C Vec4i)) <- peek defectsPtrPtr
           defectsList :: [Ptr (C Vec4i)] <- peekArray defectsSize defectsPtr
           defectsVec <- V.fromList <$> mapM (fromPtr . pure) defectsList
           [CU.block| void { delete [] *$(Vec4i * * * defectsPtrPtr); } |]
           pure defectsVec
      ) $
      [cvExcept|
        cv::_InputArray contour =
          cv::_InputArray( $(Point2i * contourPtr)
                         , $(int32_t contourSize)
                         );
        cv::_InputArray hull =
          cv::_InputArray( $vec-ptr:(int32_t * hull)
                         , $vec-len:hull
                         );
        std::vector<cv::Vec4i> defects;

        convexityDefects(contour, hull, defects);

        *$(int32_t * defectsSizePtr) = defects.size();

        cv::Vec4i * * * defectsPtrPtr = $(Vec4i * * * defectsPtrPtr);
        cv::Vec4i * * defectsPtr = new cv::Vec4i * [defects.size()];
        *defectsPtrPtr = defectsPtr;

        for (std::vector<cv::Vec4i>::size_type i = 0; i < defects.size(); i++)
        {
            cv::Vec4i & defectRef = defects[i];
            cv::Vec4i * newDefect =
              new cv::Vec4i( defectRef[0]
                           , defectRef[1]
                           , defectRef[2]
                           , defectRef[3]
                           );
            defectsPtr[i] = newDefect;
        }
      |]
  where
    contourSize :: Int32
    contourSize = fromIntegral $ V.length contour

{- | Finds contours in a binary image.

Example:

@
handContourImg
    :: forall (width    :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *  )
     . (Mat ('S ['S height, 'S width]) ('S channels) ('S depth) ~ Hand)
    => IO (Mat ('S ['S height, 'S width]) ('S channels) ('S depth))
handContourImg = do
    handContour <- mkHandContour
    pure $ exceptError $
      withMatM (Proxy :: Proxy [height, width])
               (Proxy :: Proxy channels)
               (Proxy :: Proxy depth)
               white $ \\imgM -> do
        -- Original colour image.
        void $ matCopyToM imgM (V2 0 0) hand Nothing
        -- Draw contour.
        polylines imgM (V.singleton handContour) True blue 2 LineType_AA 0

mkHandContour :: IO (V.Vector Point2i)
mkHandContour = do
    handBinaryM <- thaw handBinary
    contours <- findContours ContourRetrievalExternal ContourApproximationTC89L1 handBinaryM
    -- Assume there is at least 1 contour.
    pure $ contourPoints $ contours V.! 0
  where
    handBinary :: Mat ('S ['S 543, 'S 400]) ('S 1) ('S Word8)
    handBinary = exceptError $ do
        handGray <- cvtColor bgr gray hand
        -- The hand is surrounded by white pixels. We convert all white pixels
        -- (> 234) to black and all other pixels (the hand itself) to white.
        fst \<$> threshold (ThreshVal_Abs 234) (Thresh_BinaryInv 255) handGray
@

<<doc/generated/examples/handContour.png handContourImg>>
-}
findContours
    :: (PrimMonad m)
    => ContourRetrievalMode -- ^ Contour retrieval mode.
    -> ContourApproximationMethod -- ^ Contour approximation method.
    -> Mut (Mat ('S [h, w]) ('S 1) ('S Word8)) (PrimState m)
       -- ^ Source. Non-zero pixels are treated as 1's. Zero pixels remain 0's,
       -- so the image is treated as binary.
    -> m (V.Vector Contour)
findContours mode method src = unsafePrimToPrim $
    withPtr src $ \srcPtr ->
    alloca $ \(contourLengthsPtrPtr :: Ptr (Ptr Int32)) ->
    alloca $ \(contoursPtrPtr :: Ptr (Ptr (Ptr (Ptr C'Point2i)))) ->
    alloca $ \(hierarchyPtrPtr :: Ptr (Ptr (Ptr C'Vec4i))) ->
    alloca $ \(numContoursPtr :: Ptr Int32) -> mask_ $ do
      [C.block| void {
        std::vector< std::vector<cv::Point> > contours;
        std::vector<cv::Vec4i> hierarchy;
        cv::findContours
          ( *$(Mat * srcPtr)
          , contours
          , hierarchy
          , $(int32_t c'mode)
          , $(int32_t c'method)
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

        for (std::vector< std::vector<cv::Point> >::size_type i = 0; i < contours.size(); i++)
        {
          std::vector<cv::Point> & contourPoints = contours[i];
          cv::Vec4i hierarchyInfo = hierarchy[i];

          contourLengthsPtr[i] = contourPoints.size();

          cv::Point * * newContourPoints = new cv::Point * [contourPoints.size()];
          for (std::vector<cv::Point>::size_type j = 0; j < contourPoints.size(); j++)
          {
            cv::Point & orig = contourPoints[j];
            cv::Point * newPt = new cv::Point(orig.x, orig.y);
            newContourPoints[j] = newPt;
          }
          contoursPtr[i] = newContourPoints;

          hierarchyPtr[i] =
            new cv::Vec4i( hierarchyInfo[0]
                         , hierarchyInfo[1]
                         , hierarchyInfo[2]
                         , hierarchyInfo[3]
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
          mapM (fmap fromVec . fromPtr . pure)

      let treeHierarchy :: V.Vector ([Contour], Bool)
          treeHierarchy = V.fromList $
            zipWith
              (\(V4 nextSibling previousSibling firstChild parent) points ->
                ( Contour { contourPoints = points
                          , contourChildren =
                              if firstChild < 0
                              then mempty
                              else V.fromList $ fst $ treeHierarchy V.! fromIntegral firstChild
                          } : if nextSibling < 0
                              then []
                              else fst $ treeHierarchy V.! fromIntegral nextSibling
                , parent < 0 && previousSibling < 0
                )
              )
              hierarchy
              allContours

      [CU.block| void {
        delete [] *$(Point2i * * * * contoursPtrPtr);
        delete [] *$(Vec4i * * * hierarchyPtrPtr);
        delete [] *$(int32_t * * contourLengthsPtrPtr);
      } |]

      return $ V.fromList $ concat
             $ mapMaybe (\(contours,isRoot) -> guard isRoot $> contours)
             $ V.toList treeHierarchy
  where
    c'mode = marshalContourRetrievalMode mode
    c'method = marshalContourApproximationMethod method

{- | Tests a contour convexity.

The function tests whether the input contour is convex or not. The contour must
be simple, that is, without self-intersections. Otherwise, the function output
is undefined.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/structural_analysis_and_shape_descriptors.html?highlight=contourarea#iscontourconvex OpenCV Sphinx doc>
-}
-- TODO (RvD): support Int32 points
isContourConvex
    :: (IsPoint2 point2 CFloat, MonadError CvException m)
    => V.Vector (point2 CFloat)
    -> m Bool
isContourConvex contour = unsafeWrapException $
    alloca $ \c'resultPtr ->
    handleCvException (toBool <$> peek c'resultPtr) $
    withArrayPtr (V.map toPoint contour) $ \contourPtr ->
      [cvExcept|
        *$(bool * c'resultPtr) =
          cv::isContourConvex
          (cv::_InputArray($(Point2f * contourPtr), $(int32_t c'numPoints)));
      |]
  where
    c'numPoints = fromIntegral $ V.length contour

minAreaRect :: (IsPoint2 point2 Int32)
            => V.Vector (point2 Int32)
            -> RotatedRect
minAreaRect points =
    unsafePerformIO $ fromPtr $
    withArrayPtr (V.map toPoint points) $ \pointsPtr ->
      [CU.exp|
        RotatedRect * {
          new RotatedRect(
            cv::minAreaRect(
                cv::_InputArray( $(Point2i * pointsPtr)
                               , $(int32_t c'numPoints))))
        }
      |]
  where
    c'numPoints = fromIntegral $ V.length points

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
    :: ( IsPoint2 contourPoint2 CFloat
       , IsPoint2 testPoint2    CFloat
       , MonadError CvException m
       )
    => V.Vector (contourPoint2 CFloat) -- ^ Contour.
    -> testPoint2 CFloat -- ^ Point tested against the contour.
    -> Bool
       -- ^ If true, the function estimates the signed distance from the point
       -- to the nearest contour edge. Otherwise, the function only checks if
       -- the point is inside a contour or not.
    -> m Double
pointPolygonTest contour pt measureDist = unsafeWrapException $
    withArrayPtr (V.map toPoint contour) $ \contourPtr ->
    withPtr (toPoint pt) $ \ptPtr ->
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
