{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc
    ( -- * Image Filtering
      module OpenCV.ImgProc.ImgFiltering
      -- * Geometric Image Transformations
    , module OpenCV.ImgProc.GeometricImgTransform
      -- * Miscellaneous Image Transformations
    , module OpenCV.ImgProc.MiscImgTransform
      -- * Drawing Functions
    , module OpenCV.ImgProc.Drawing
      -- * Color Maps
      -- * Histograms
      -- * Structural Analysis and Shape Descriptors
    , pointPolygonTest
      -- * Motion Analysis and Object Tracking
      -- * Feature Detection
      -- * Object Detection
    , MatchTemplateMethod(..)
    , matchTemplate
      -- * Types
    , module OpenCV.ImgProc.Types
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude hiding ( shift )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core
import "this" OpenCV.Internal
import "this" OpenCV.ImgProc.ImgFiltering
import "this" OpenCV.ImgProc.GeometricImgTransform
import "this" OpenCV.ImgProc.MiscImgTransform
import "this" OpenCV.ImgProc.Drawing
import "this" OpenCV.ImgProc.Types
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"
#include "macros.hpp"

--------------------------------------------------------------------------------
-- Color Maps
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Histograms
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Structural Analysis and Shape Descriptors
--------------------------------------------------------------------------------

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
    :: V.Vector Point2f -- ^ Contour.
    -> Point2f -- ^ Point tested against the contour.
    -> Bool
       -- ^ If true, the function estimates the signed distance from the point
       -- to the nearest contour edge. Otherwise, the function only checks if
       -- the point is inside a contour or not.
    -> Either CvException Double
pointPolygonTest contour pt measureDist = unsafePerformIO $
    withPoint2fs contour $ \contourPtr ->
    withPoint2fPtr pt $ \ptPtr ->
    alloca $ \(c'resultPtr) ->
    handleCvException (realToFrac <$> peek c'resultPtr) $
      [cvExcept|
        cv::_InputArray contour =
          cv::_InputArray( $(Point2f * contourPtr)
                         , $(int c'numPoints)
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


--------------------------------------------------------------------------------
-- Motion Analysis and Object Tracking
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Feature Detection
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Object Detection
--------------------------------------------------------------------------------

-- | <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/object_detection.html#matchtemplate OpenCV Sphinx doc>
data MatchTemplateMethod
   = MatchTemplateSqDiff
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/f096a706cb9499736423f10d901c7fe13a1e6926.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/6d6a720237b3a4c1365c8e86a9cfcf0895d5e265.png>>
   | MatchTemplateCCorr
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/93f1747a86a3c5095a0e6a187442c6e2a0ae0968.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/6a72ad9ae17c4dad88e33ed16308fc1cfba549b8.png>>
   | MatchTemplateCCoeff
       -- ^ * not normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/c9b62df96d0692d90cc1d8a5912a68a44461910c.png>>
       --   * where <<http://docs.opencv.org/3.0-last-rst/_images/math/ffb6954b6020b02e13b73c79bd852c1627cfb79c.png>>
       --   * normed: <<http://docs.opencv.org/3.0-last-rst/_images/math/235e42ec68d2d773899efcf0a4a9d35a7afedb64.png>>
     deriving Show

#num CV_TM_SQDIFF
#num CV_TM_SQDIFF_NORMED
#num CV_TM_CCORR
#num CV_TM_CCORR_NORMED
#num CV_TM_CCOEFF
#num CV_TM_CCOEFF_NORMED

marshallMatchTemplateMethod :: MatchTemplateMethod -> Bool -> CInt
marshallMatchTemplateMethod m n =
    case (m, n) of
      (MatchTemplateSqDiff, False) -> c'CV_TM_SQDIFF
      (MatchTemplateSqDiff, True ) -> c'CV_TM_SQDIFF_NORMED
      (MatchTemplateCCorr , False) -> c'CV_TM_CCORR
      (MatchTemplateCCorr , True ) -> c'CV_TM_CCORR_NORMED
      (MatchTemplateCCoeff, False) -> c'CV_TM_CCOEFF
      (MatchTemplateCCoeff, True ) -> c'CV_TM_CCOEFF_NORMED

-- | <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/object_detection.html#matchtemplate OpenCV Sphinx doc>
--
-- Compares a template against overlapped image regions.
--
-- The function slides through image, compares the overlapped patches
-- of size
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/d47153257f0243694e5632bb23b85009eb9e5599.png w \times h>>
-- against templ using the specified method and stores the comparison
-- results in result . Here are the formulae for the available
-- comparison methods
-- (<<http://docs.opencv.org/3.0-last-rst/_images/math/06f9f0fcaa8d96a6a23b0f7d1566fe5efaa789ad.png I>> denotes image,
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/87804527283a4539e1e17c5861df8cb92a97fd6d.png T>> template,
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/8fa391da5431a5d6eaba1325c3e7cb3da22812b5.png R>> result).
-- The summation is done over template and/or the image patch:
-- <<http://docs.opencv.org/3.0-last-rst/_images/math/ff90cafd4a71d85875237787b54815ee8ac77bff.png x' = 0...w-1, y' = 0...h-1>>
matchTemplate
    :: Mat
       -- ^ Image where the search is running. It must be 8-bit or 32-bit floating-point.
    -> Mat
       -- ^ Searched template. It must be not greater than the source image and have the same data type.
    -> MatchTemplateMethod
       -- ^ Parameter specifying the comparison method.
    -> Bool
       -- ^ Normalise. See 'MatchTemplateMethod'.
    -> Either CvException Mat
       -- ^ Map of comparison results. It must be single-channel 32-bit floating-point.
       -- If image is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/e4926c3d97c3f7434c6317ba24b8b9294a0aba64.png>>
       -- and templ is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/d47153257f0243694e5632bb23b85009eb9e5599.png>>
       -- , then result is
       -- <<http://docs.opencv.org/3.0-last-rst/_images/math/e318d7237b57e08135e689fd9136b9ac8e4a4102.png>>.
matchTemplate image templ method normed = unsafePerformIO $ do
    result <- newEmptyMat
    handleCvException (pure result) $
      withMatPtr result $ \resultPtr ->
      withMatPtr image $ \imagePtr ->
      withMatPtr templ $ \templPtr ->
        [cvExcept|
          cv::matchTemplate( *$(Mat * imagePtr)
                           , *$(Mat * templPtr)
                           , *$(Mat * resultPtr)
                           , $(int c'method)
                           );
        |]
  where
    c'method = marshallMatchTemplateMethod method normed
