{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc
    ( -- * Image Filtering
      medianBlur
      -- * Geometric Image Transformations
    , warpAffine
    , warpPerspective
    , invertAffineTransform
      -- * Miscellaneous Image Transformations
      -- * Drawing Functions
    , LineType(..)
    , FontFace(..)

    , arrowedLine
    , circle
    , ellipse
    , fillConvexPoly
    , fillPoly
    , getTextSize
    , line
    , putText
    , rectangle
      -- * Color Maps
      -- * Histograms
      -- * Structural Analysis and Shape Descriptors
      -- * Motion Analysis and Object Tracking
      -- * Feature Detection
      -- * Object Detection
    , MatchTemplateMethod(..)
    , matchTemplate
      -- * Types
    , InterpolationMethod(..)
    , BorderMode(..)
    ) where

import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude hiding ( shift )
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( append )
import qualified "text" Data.Text.Foreign as T ( withCStringLen )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core
import "this" OpenCV.Internal
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as VS

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
-- Image Filtering
--------------------------------------------------------------------------------

-- | Blurs an image using the median filter.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/filtering.html#medianblur OpenCV Sphinx doc>
medianBlur
    :: Mat
       -- ^ Input 1-, 3-, or 4-channel image; when ksize is 3 or 5, the
       -- image depth should be CV_8U, CV_16U, or CV_32F, for larger
       -- aperture sizes, it can only be CV_8U.
    -> Int
       -- ^ Aperture linear size; it must be odd and greater than 1, for
       -- example: 3, 5, 7...
    -> Either CvException Mat
medianBlur matIn ksize = unsafePerformIO $ c'medianBlur $ fromIntegral ksize
  where
    c'medianBlur :: C.CInt -> IO (Either CvException Mat)
    c'medianBlur c'ksize = do
      matOut <- newEmptyMat
      handleCvException matOut $
        withMatPtr matOut $ \matOutPtr ->
        withMatPtr matIn $ \matInPtr ->
          [cvExcept| cv::medianBlur(*$(Mat * matInPtr), *$(Mat * matOutPtr), $(int c'ksize)); |]


--------------------------------------------------------------------------------
-- Geometric Image Transformations
--------------------------------------------------------------------------------

#num WARP_FILL_OUTLIERS
#num WARP_INVERSE_MAP

-- | Applies an affine transformation to an image
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#warpaffine OpenCV Sphinx doc>
warpAffine
    :: Mat -- ^ Source image.
    -> Mat -- ^ Affine transformation matrix.
    -> InterpolationMethod
    -> Bool -- ^ Perform the inverse transformation.
    -> Bool -- ^ Fill outliers.
    -> BorderMode -- ^ Pixel extrapolation method.
    -> Either CvException Mat -- ^ Transformed source image.
warpAffine src transform interpolationMethod inverse fillOutliers borderMode =
    unsafePerformIO $ do
      dst <- newEmptyMat
      handleCvException dst $
        withMatPtr src $ \srcPtr ->
        withMatPtr dst $ \dstPtr ->
        withMatPtr transform $ \transformPtr ->
        withScalarPtr borderValue $ \borderValuePtr ->
          [cvExcept|
            Mat * src = $(Mat * srcPtr);
            cv::warpAffine
              ( *src
              , *$(Mat * dstPtr)
              , *$(Mat * transformPtr)
              , src->size()
              , $(int c'interpolationMethod) | $(int c'inverse) | $(int c'fillOutliers)
              , $(int c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshallInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshallBorderMode borderMode

-- | Applies a perspective transformation to an image
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#warpperspective OpenCV Sphinx doc>
warpPerspective
    :: Mat -- ^ Source image.
    -> Mat -- ^ Perspective transformation matrix.
    -> InterpolationMethod
    -> Bool -- ^ Perform the inverse transformation.
    -> Bool -- ^ Fill outliers.
    -> BorderMode -- ^ Pixel extrapolation method.
    -> Either CvException Mat -- ^ Transformed source image.
warpPerspective src transform interpolationMethod inverse fillOutliers borderMode =
    unsafePerformIO $ do
      dst <- newEmptyMat
      handleCvException dst $
        withMatPtr src $ \srcPtr ->
        withMatPtr dst $ \dstPtr ->
        withMatPtr transform $ \transformPtr ->
        withScalarPtr borderValue $ \borderValuePtr ->
          [cvExcept|
            Mat * src = $(Mat * srcPtr);
            cv::warpPerspective
              ( *src
              , *$(Mat * dstPtr)
              , *$(Mat * transformPtr)
              , src->size()
              , $(int c'interpolationMethod) | $(int c'inverse) | $(int c'fillOutliers)
              , $(int c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshallInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshallBorderMode borderMode

-- | Inverts an affine transformation
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#invertaffinetransform OpenCV Sphinx doc>
invertAffineTransform :: Mat -> Either CvException Mat
invertAffineTransform matIn = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException matOut $
      withMatPtr matIn  $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
        [cvExcept|
           cv::invertAffineTransform(*$(Mat * matInPtr), *$(Mat * matOutPtr));
        |]


--------------------------------------------------------------------------------
-- Miscellaneous Image Transformations
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Drawing Functions
--------------------------------------------------------------------------------

data LineType
   = LineType_8  -- ^ 8-connected line.
   | LineType_4  -- ^ 4-connected line.
   | LineType_AA -- ^ Antialiased line.
     deriving Show

#num LINE_8
#num LINE_4
#num LINE_AA

marshallLineType :: LineType -> C.CInt
marshallLineType = \case
  LineType_8  -> c'LINE_8
  LineType_4  -> c'LINE_4
  LineType_AA -> c'LINE_AA

data FontFace
   = FontHersheySimplex
     -- ^ Normal size sans-serif font
   | FontHersheyPlain
     -- ^ Small size sans-serif font
   | FontHersheyDuplex
     -- ^ Normal size sans-serif font (more complex than 'FontHersheySimplex')
   | FontHersheyComplex
     -- ^ Normal size serif font
   | FontHersheyTriplex
     -- ^ Normal size serif font (more complex than 'FontHersheyComplex')
   | FontHersheyComplexSmall
     -- ^ Smaller version of 'FontHersheyComplex'
   | FontHersheyScriptSimplex
     -- ^ Hand-writing style font
   | FontHersheyScriptComplex
     -- ^ More complex variant of 'FontHersheyScriptSimplex'
   | FontItalic
     -- ^ Flag for italic font

#num FONT_HERSHEY_SIMPLEX
#num FONT_HERSHEY_PLAIN
#num FONT_HERSHEY_DUPLEX
#num FONT_HERSHEY_COMPLEX
#num FONT_HERSHEY_TRIPLEX
#num FONT_HERSHEY_COMPLEX_SMALL
#num FONT_HERSHEY_SCRIPT_SIMPLEX
#num FONT_HERSHEY_SCRIPT_COMPLEX
#num FONT_ITALIC

marshallFontFace :: FontFace -> C.CInt
marshallFontFace = \case
   FontHersheySimplex       -> c'FONT_HERSHEY_SIMPLEX
   FontHersheyPlain         -> c'FONT_HERSHEY_PLAIN
   FontHersheyDuplex        -> c'FONT_HERSHEY_DUPLEX
   FontHersheyComplex       -> c'FONT_HERSHEY_COMPLEX
   FontHersheyTriplex       -> c'FONT_HERSHEY_TRIPLEX
   FontHersheyComplexSmall  -> c'FONT_HERSHEY_COMPLEX_SMALL
   FontHersheyScriptSimplex -> c'FONT_HERSHEY_SCRIPT_SIMPLEX
   FontHersheyScriptComplex -> c'FONT_HERSHEY_SCRIPT_COMPLEX
   FontItalic               -> c'FONT_ITALIC


-- | Draws a arrow segment pointing from the first point to the second one
--
-- <http://docs.opencv.org/3.0.0/d6/d6e/group__imgproc__draw.html#ga0a165a3ca093fd488ac709fdf10c05b2 OpenCV Doxygen doc>
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#arrowedline OpenCV Sphinx doc>
arrowedLine
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> Point2i -- ^ The point the arrow starts from.
    -> Point2i -- ^ The point the arrow points to.
    -> Scalar -- ^ Line color.
    -> Int -- ^ Line thickness.
    -> LineType
    -> Int -- ^ Number of fractional bits in the point coordinates.
    -> Double -- ^ The length of the arrow tip in relation to the arrow length.
    -> m ()
arrowedLine img pt1 pt2 color thickness lineType shift tipLength =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2iPtr pt1 $ \pt1Ptr ->
    withPoint2iPtr pt2 $ \pt2Ptr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::arrowedLine( *$(Mat * matPtr)
                       , *$(Point2i * pt1Ptr)
                       , *$(Point2i * pt2Ptr)
                       , *$(Scalar * colorPtr)
                       , $(int c'thickness)
                       , $(int c'lineType)
                       , $(int c'shift)
                       , $(double c'tipLength)
                       )
      }|]
  where
    c'thickness = fromIntegral thickness
    c'lineType  = marshallLineType lineType
    c'shift     = fromIntegral shift
    c'tipLength = realToFrac tipLength

-- | Draws a circle.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#circle OpenCV Sphinx doc>
circle
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image where the circle is drawn.
    -> Point2i -- ^ Center of the circle.
    -> Int -- ^ Radius of the circle.
    -> Scalar -- ^ Circle color.
    -> Int -- ^ Thickness of the circle outline, if positive. Negative thickness means that a filled circle is to be drawn.
    -> LineType -- ^ Type of the circle boundary.
    -> Int -- ^ Number of fractional bits in the coordinates of the center and in the radius value.
    -> m ()
circle img center radius color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2iPtr center $ \centerPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::circle( *$(Mat * matPtr)
                  , *$(Point2i * centerPtr)
                  , $(int c'radius)
                  , *$(Scalar * colorPtr)
                  , $(int c'thickness)
                  , $(int c'lineType)
                  , $(int c'shift)
                  )
      }|]
  where
    c'radius    = fromIntegral radius
    c'thickness = fromIntegral thickness
    c'lineType  = marshallLineType lineType
    c'shift     = fromIntegral shift

-- | Draws a simple or thick elliptic arc or fills an ellipse sector
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#ellipse OpenCV Sphinx doc>
ellipse
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> Point2i -- ^ Center of the ellipse.
    -> Size2i -- ^ Half of the size of the ellipse main axes.
    -> Double -- ^ Ellipse rotation angle in degrees.
    -> Double -- ^ Starting angle of the elliptic arc in degrees.
    -> Double -- ^ Ending angle of the elliptic arc in degrees.
    -> Scalar -- ^ Ellipse color.
    -> Int
       -- ^ Thickness of the ellipse arc outline, if
       -- positive. Otherwise, this indicates that a filled ellipse
       -- sector is to be drawn.
    -> LineType -- ^ Type of the ellipse boundary.
    -> Int -- ^ Number of fractional bits in the coordinates of the center and values of axes.
    -> m ()
ellipse img center axes angle startAngle endAngle color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2iPtr center $ \centerPtr ->
    withSize2iPtr axes $ \axesPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::ellipse( *$(Mat * matPtr)
                   , *$(Point2i * centerPtr)
                   , *$(Size2i * axesPtr)
                   , $(double c'angle)
                   , $(double c'startAngle)
                   , $(double c'endAngle)
                   , *$(Scalar * colorPtr)
                   , $(int c'thickness)
                   , $(int c'lineType)
                   , $(int c'shift)
                   )
      }|]
  where
    c'angle      = realToFrac angle
    c'startAngle = realToFrac startAngle
    c'endAngle   = realToFrac endAngle
    c'thickness  = fromIntegral thickness
    c'lineType   = marshallLineType lineType
    c'shift      = fromIntegral shift

-- | Fills a convex polygon.
--
-- The function 'fillConvexPoly' draws a filled convex polygon. This
-- function is much faster than the function 'fillPoly' . It can fill
-- not only convex polygons but any monotonic polygon without
-- self-intersections, that is, a polygon whose contour intersects
-- every horizontal line (scan line) twice at the most (though, its
-- top-most and/or the bottom edge could be horizontal).
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillconvexpoly OpenCV Sphinx doc>
fillConvexPoly
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> V.Vector Point2i -- ^ Polygon vertices.
    -> Scalar -- ^ Polygon color.
    -> LineType
    -> Int -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillConvexPoly img points color lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoints points $ \pointsPtr ->
    withScalarPtr color $ \colorPtr -> do
      [C.exp|void {
        cv::fillConvexPoly( *$(Mat * matPtr)
                          , $(Point2i * pointsPtr)
                          , $(int c'numPoints)
                          , *$(Scalar * colorPtr)
                          , $(int c'lineType)
                          , $(int c'shift)
                          )
      }|]
  where
    c'numPoints = fromIntegral $ V.length points
    c'lineType  = marshallLineType lineType
    c'shift     = fromIntegral shift

-- | Fills the area bounded by one or more polygons.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillpoly OpenCV Sphinx doc>
fillPoly
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector Point2i) -- ^ Polygons.
    -> Scalar -- ^ Polygon color.
    -> LineType
    -> Int -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillPoly img polygons color lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPolygons polygons $ \polygonsPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withScalarPtr color $ \colorPtr -> do
      [C.exp|void {
        cv::fillPoly( *$(Mat * matPtr)
                    , $(const Point2i * * polygonsPtr)
                    , $(int * nptsPtr)
                    , $(int c'numPolygons)
                    , *$(Scalar * colorPtr)
                    , $(int c'lineType)
                    , $(int c'shift)
                    )
      }|]
  where
    c'numPolygons = fromIntegral $ V.length polygons
    c'lineType    = marshallLineType lineType
    c'shift       = fromIntegral shift

    npts :: VS.Vector C.CInt
    npts = VS.convert $ V.map (fromIntegral . V.length) polygons

-- | Calculates the width and height of a text string.
--
--  Calculates and returns the size of a box that contains the specified text.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#gettextsize OpenCV Sphinx doc>
getTextSize
    :: Text
    -> FontFace
    -> Double  -- ^ Font scale.
    -> Int -- ^ Thickness of lines used to render the text.
    -> (Size2i, Int)
getTextSize text fontFace fontScale thickness = unsafePerformIO $
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    alloca $ \(c'baseLinePtr :: Ptr C.CInt) -> do
      size <- size2iFromPtr $
        [C.block|Size2i * {
          Size size = cv::getTextSize( $(char * c'text)
                                     , $(int c'fontFace)
                                     , $(double c'fontScale)
                                     , $(int c'thickness)
                                     , $(int * c'baseLinePtr)
                                     );
          return new Size(size);
        }|]
      baseLine <- peek c'baseLinePtr
      pure (size, fromIntegral baseLine)
  where
    c'fontFace  = marshallFontFace fontFace
    c'fontScale = realToFrac fontScale
    c'thickness = fromIntegral thickness



-- | Draws a line segment connecting two points.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#line OpenCV Sphinx doc>
line
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> Point2i -- ^ First point of the line segment.
    -> Point2i -- ^ Scond point of the line segment.
    -> Scalar -- ^ Line color.
    -> Int -- ^ Line thickness.
    -> LineType
    -> Int -- ^ Number of fractional bits in the point coordinates.
    -> m ()
line img pt1 pt2 color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2iPtr pt1 $ \pt1Ptr ->
    withPoint2iPtr pt2 $ \pt2Ptr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::line( *$(Mat * matPtr)
                , *$(Point2i * pt1Ptr)
                , *$(Point2i * pt2Ptr)
                , *$(Scalar * colorPtr)
                , $(int c'thickness)
                , $(int c'lineType)
                , $(int c'shift)
                )
      }|]
  where
    c'thickness = fromIntegral thickness
    c'lineType  = marshallLineType lineType
    c'shift     = fromIntegral shift

-- | Draws a text string.
--
-- The function putText renders the specified text string in the
-- image. Symbols that cannot be rendered using the specified font are
-- replaced by question marks.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#puttext OpenCV Sphinx doc>
putText
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> Text -- ^ Text string to be drawn.
    -> Point2i -- ^ Bottom-left corner of the text string in the image.
    -> FontFace
    -> Double -- ^ Font scale factor that is multiplied by the font-specific base size.
    -> Scalar -- ^ Text color.
    -> Int -- ^ Thickness of the lines used to draw a text.
    -> LineType
    -> Bool -- ^ When 'True', the image data origin is at the bottom-left corner. Otherwise, it is at the top-left corner.
    -> m ()
putText img text org fontFace fontScale color thickness lineType bottomLeftOrigin =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    withPoint2iPtr org $ \orgPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::putText( *$(Mat * matPtr)
                   , $(char * c'text)
                   , *$(Point2i * orgPtr)
                   , $(int c'fontFace)
                   , $(double c'fontScale)
                   , *$(Scalar * colorPtr)
                   , $(int c'thickness)
                   , $(int c'lineType)
                   , $(bool c'bottomLeftOrigin)
                   )
      }|]
  where
    c'fontFace         = marshallFontFace fontFace
    c'fontScale        = realToFrac fontScale
    c'thickness        = fromIntegral thickness
    c'lineType         = marshallLineType lineType
    c'bottomLeftOrigin = fromBool bottomLeftOrigin

-- | Draws a simple, thick, or filled up-right rectangle
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#rectangle OpenCV Sphinx doc>
rectangle
    :: (PrimMonad m)
    => MutMat (PrimState m) -- ^ Image.
    -> Rect
    -> Scalar -- ^ Rectangle color or brightness (grayscale image).
    -> Int -- ^ Line thickness.
    -> LineType
    -> Int -- ^ Number of fractional bits in the point coordinates.
    -> m ()
rectangle img rect color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withRectPtr rect $ \rectPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::rectangle( *$(Mat * matPtr)
                     , *$(Rect * rectPtr)
                     , *$(Scalar * colorPtr)
                     , $(int c'thickness)
                     , $(int c'lineType)
                     , $(int c'shift)
                     )
      }|]
  where
    c'thickness = fromIntegral thickness
    c'lineType  = marshallLineType lineType
    c'shift     = fromIntegral shift


--------------------------------------------------------------------------------
-- Color Maps
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Histograms
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Structural Analysis and Shape Descriptors
--------------------------------------------------------------------------------


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
       -- ^ * not <http://docs.opencv.org/3.0-last-rst/_images/math/f096a706cb9499736423f10d901c7fe13a1e6926.png>>
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

marshallMatchTemplateMethod :: MatchTemplateMethod -> Bool -> C.CInt
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
    handleCvException result $
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


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data InterpolationMethod
   = InterNearest -- ^ Nearest neighbor interpolation.
   | InterLinear -- ^ Bilinear interpolation.
   | InterCubic -- ^ Bicubic interpolation.
   | InterArea
     -- ^ Resampling using pixel area relation. It may be a preferred method for
     -- image decimation, as it gives moire'-free results. But when the image is
     -- zoomed, it is similar to the 'InterNearest' method.
   | InterLanczos4 -- ^ Lanczos interpolation over 8x8 neighborhood
     deriving Show

#num INTER_NEAREST
#num INTER_LINEAR
#num INTER_CUBIC
#num INTER_AREA
#num INTER_LANCZOS4

marshallInterpolationMethod :: InterpolationMethod -> C.CInt
marshallInterpolationMethod = \case
   InterNearest  -> c'INTER_NEAREST
   InterLinear   -> c'INTER_LINEAR
   InterCubic    -> c'INTER_CUBIC
   InterArea     -> c'INTER_AREA
   InterLanczos4 -> c'INTER_LANCZOS4

-- TODO (RvD): Show instance
-- Needs a Show instance for Scalar
data BorderMode
   = BorderConstant Scalar -- ^ 1D example: @iiiiii|abcdefgh|iiiiiii@  with some specified @i@
   | BorderReplicate   -- ^ 1D example: @aaaaaa|abcdefgh|hhhhhhh@
   | BorderReflect     -- ^ 1D example: @fedcba|abcdefgh|hgfedcb@
   | BorderWrap        -- ^ 1D example: @cdefgh|abcdefgh|abcdefg@
   | BorderReflect101  -- ^ 1D example: @gfedcb|abcdefgh|gfedcba@
   | BorderTransparent -- ^ 1D example: @uvwxyz|absdefgh|ijklmno@
   | BorderIsolated    -- ^ do not look outside of ROI

#num BORDER_CONSTANT
#num BORDER_REPLICATE
#num BORDER_REFLECT
#num BORDER_WRAP
#num BORDER_REFLECT_101
#num BORDER_TRANSPARENT
#num BORDER_ISOLATED

marshallBorderMode :: BorderMode -> (C.CInt, Scalar)
marshallBorderMode = \case
    BorderConstant scalar -> (c'BORDER_CONSTANT    , scalar    )
    BorderReplicate       -> (c'BORDER_REPLICATE   , zeroScalar)
    BorderReflect         -> (c'BORDER_REFLECT     , zeroScalar)
    BorderWrap            -> (c'BORDER_WRAP        , zeroScalar)
    BorderReflect101      -> (c'BORDER_REFLECT_101 , zeroScalar)
    BorderTransparent     -> (c'BORDER_TRANSPARENT , zeroScalar)
    BorderIsolated        -> (c'BORDER_ISOLATED    , zeroScalar)
  where
    zeroScalar = mkScalar 0 0 0 0
