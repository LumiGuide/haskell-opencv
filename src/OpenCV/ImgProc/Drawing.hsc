{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.Drawing
    ( LineType(..)
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
    ) where

import "base" Foreign.C.Types
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
import "this" OpenCV.Core.Types
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

--------------------------------------------------------------------------------


data LineType
   = LineType_8  -- ^ 8-connected line.
   | LineType_4  -- ^ 4-connected line.
   | LineType_AA -- ^ Antialiased line.
     deriving Show

#num LINE_8
#num LINE_4
#num LINE_AA

marshallLineType :: LineType -> CInt
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

marshallFontFace :: FontFace -> CInt
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
    withPoint2is points $ \pointsPtr ->
    withScalarPtr color $ \colorPtr ->
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
    withScalarPtr color $ \colorPtr ->
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

    npts :: VS.Vector CInt
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
    alloca $ \(c'baseLinePtr :: Ptr CInt) -> do
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
