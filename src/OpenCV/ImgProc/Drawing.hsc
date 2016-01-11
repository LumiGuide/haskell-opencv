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
    , polylines
    , getTextSize
    , line
    , putText
    , rectangle
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
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Internal
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
     deriving (Show, Enum, Bounded)

#num LINE_8
#num LINE_4
#num LINE_AA

marshalLineType :: LineType -> Int32
marshalLineType = \case
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
     deriving (Show, Enum, Bounded)

#num FONT_HERSHEY_SIMPLEX
#num FONT_HERSHEY_PLAIN
#num FONT_HERSHEY_DUPLEX
#num FONT_HERSHEY_COMPLEX
#num FONT_HERSHEY_TRIPLEX
#num FONT_HERSHEY_COMPLEX_SMALL
#num FONT_HERSHEY_SCRIPT_SIMPLEX
#num FONT_HERSHEY_SCRIPT_COMPLEX
#num FONT_ITALIC

marshalFontFace :: FontFace -> Int32
marshalFontFace = \case
   FontHersheySimplex       -> c'FONT_HERSHEY_SIMPLEX
   FontHersheyPlain         -> c'FONT_HERSHEY_PLAIN
   FontHersheyDuplex        -> c'FONT_HERSHEY_DUPLEX
   FontHersheyComplex       -> c'FONT_HERSHEY_COMPLEX
   FontHersheyTriplex       -> c'FONT_HERSHEY_TRIPLEX
   FontHersheyComplexSmall  -> c'FONT_HERSHEY_COMPLEX_SMALL
   FontHersheyScriptSimplex -> c'FONT_HERSHEY_SCRIPT_SIMPLEX
   FontHersheyScriptComplex -> c'FONT_HERSHEY_SCRIPT_COMPLEX
   FontItalic               -> c'FONT_ITALIC


{- | Draws a arrow segment pointing from the first point to the second one

Example:

@
arrowedLineImg :: 'Mat'
arrowedLineImg = 'createMat' $ do
  imgM <- 'mkMatM' ('V.fromList' [200, 300]) 'MatDepth_8U' 4 transparent
  'arrowedLine' imgM (V2  10 130 :: V2 'Int32') (V2 190  40 :: V2 'Int32') blue 5 'LineType_8' 0 0.15
  'arrowedLine' imgM (V2 210  50 :: V2 'Int32') (V2 250 180 :: V2 'Int32') red  8 'LineType_8' 0 0.4
  'pure' imgM
@

<<doc/arrowedLine.png>>

<http://docs.opencv.org/3.0.0/d6/d6e/group__imgproc__draw.html#ga0a165a3ca093fd488ac709fdf10c05b2 OpenCV Doxygen doc>
<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#arrowedline OpenCV Sphinx doc>
-}
arrowedLine
    :: (PrimMonad m, ToPoint2i fromPoint2i, ToPoint2i toPoint2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> fromPoint2i -- ^ The point the arrow starts from.
    -> toPoint2i -- ^ The point the arrow points to.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
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
                       , $(int32_t thickness)
                       , $(int32_t c'lineType)
                       , $(int32_t shift)
                       , $(double c'tipLength)
                       )
      }|]
  where
    c'lineType  = marshalLineType lineType
    c'tipLength = realToFrac tipLength

{- | Draws a circle.

Example:

@
circleImg :: 'Mat'
circleImg = 'createMat' $ do
  imgM <- 'mkMatM' ('V.fromList' [200, 400]) 'MatDepth_8U' 4 transparent
  'circle' imgM (V2 100 100 :: V2 'Int32') 90 blue  5  'LineType_AA' 0
  'circle' imgM (V2 300 100 :: V2 'Int32') 45 red (-1) 'LineType_AA' 0
  'pure' imgM
@

<<doc/circle.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#circle OpenCV Sphinx doc>
-}
circle
    :: (PrimMonad m, ToPoint2i point2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image where the circle is drawn.
    -> point2i -- ^ Center of the circle.
    -> Int32 -- ^ Radius of the circle.
    -> color -- ^ Circle color.
    -> Int32 -- ^ Thickness of the circle outline, if positive. Negative thickness means that a filled circle is to be drawn.
    -> LineType -- ^ Type of the circle boundary.
    -> Int32 -- ^ Number of fractional bits in the coordinates of the center and in the radius value.
    -> m ()
circle img center radius color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2iPtr center $ \centerPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::circle( *$(Mat * matPtr)
                  , *$(Point2i * centerPtr)
                  , $(int32_t radius)
                  , *$(Scalar * colorPtr)
                  , $(int32_t thickness)
                  , $(int32_t c'lineType)
                  , $(int32_t shift)
                  )
      }|]
  where
    c'lineType  = marshalLineType lineType

{- | Draws a simple or thick elliptic arc or fills an ellipse sector

Example:

@
ellipseImg :: 'Mat'
ellipseImg = 'createMat' $ do
  imgM <- 'mkMatM' ('V.fromList' [200, 400]) 'MatDepth_8U' 4 transparent
  'ellipse' imgM (V2 100 100 :: V2 'Int32') (V2 90 60 :: V2 'Int32')  30  0 360 blue  5  'LineType_AA' 0
  'ellipse' imgM (V2 300 100 :: V2 'Int32') (V2 80 40 :: V2 'Int32') 160 40 290 red (-1) 'LineType_AA' 0
  'pure' imgM
@

<<doc/ellipse.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#ellipse OpenCV Sphinx doc>
-}
ellipse
    :: (PrimMonad m, ToPoint2i point2i, ToSize2i size2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> point2i -- ^ Center of the ellipse.
    -> size2i -- ^ Half of the size of the ellipse main axes.
    -> Double -- ^ Ellipse rotation angle in degrees.
    -> Double -- ^ Starting angle of the elliptic arc in degrees.
    -> Double -- ^ Ending angle of the elliptic arc in degrees.
    -> color -- ^ Ellipse color.
    -> Int32
       -- ^ Thickness of the ellipse arc outline, if
       -- positive. Otherwise, this indicates that a filled ellipse
       -- sector is to be drawn.
    -> LineType -- ^ Type of the ellipse boundary.
    -> Int32 -- ^ Number of fractional bits in the coordinates of the center and values of axes.
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
                   , $(int32_t thickness)
                   , $(int32_t c'lineType)
                   , $(int32_t shift)
                   )
      }|]
  where
    c'angle      = realToFrac angle
    c'startAngle = realToFrac startAngle
    c'endAngle   = realToFrac endAngle
    c'lineType   = marshalLineType lineType

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
    :: (PrimMonad m, ToPoint2i point2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> V.Vector point2i -- ^ Polygon vertices.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillConvexPoly img points color lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPoint2is points $ \pointsPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::fillConvexPoly( *$(Mat * matPtr)
                          , $(Point2i * pointsPtr)
                          , $(int32_t c'numPoints)
                          , *$(Scalar * colorPtr)
                          , $(int32_t c'lineType)
                          , $(int32_t shift)
                          )
      }|]
  where
    c'numPoints = fromIntegral $ V.length points
    c'lineType  = marshalLineType lineType


{- | Fills the area bounded by one or more polygons.

Example:

@
fillPolyImg :: 'Mat'
fillPolyImg = 'createMat' $ do
    imgM <- 'mkMatM' ('V.fromList' [h, w]) 'MatDepth_8U' 4 transparent
    'fillPoly' imgM pts light_blue 'LineType_AA' 0
    'polylines' imgM pts True blue 2 'LineType_AA' 0
    pure imgM
  where
    w = 300
    h = 300
    pts = 'V.singleton' $ 'V.fromList'
          [ V2 (    w \`div`  4) ( 7*h \`div`  8)
          , V2 (  3*w \`div`  4) ( 7*h \`div`  8)
          , V2 (  3*w \`div`  4) (13*h \`div` 16)
          , V2 ( 11*w \`div` 16) (13*h \`div` 16)
          , V2 ( 19*w \`div` 32) ( 3*h \`div`  8)
          , V2 (  3*w \`div`  4) ( 3*h \`div`  8)
          , V2 (  3*w \`div`  4) (   h \`div`  8)
          , V2 ( 26*w \`div` 40) (   h \`div`  8)
          , V2 ( 26*w \`div` 40) (   h \`div`  4)
          , V2 ( 22*w \`div` 40) (   h \`div`  4)
          , V2 ( 22*w \`div` 40) (   h \`div`  8)
          , V2 ( 18*w \`div` 40) (   h \`div`  8)
          , V2 ( 18*w \`div` 40) (   h \`div`  4)
          , V2 ( 14*w \`div` 40) (   h \`div`  4)
          , V2 ( 14*w \`div` 40) (   h \`div`  8)
          , V2 (    w \`div`  4) (   h \`div`  8)
          , V2 (    w \`div`  4) ( 3*h \`div`  8)
          , V2 ( 13*w \`div` 32) ( 3*h \`div`  8)
          , V2 (  5*w \`div` 16) (13*h \`div` 16)
          , V2 (    w \`div`  4) (13*h \`div` 16)
          ]
@

<<doc/fillPoly.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillpoly OpenCV Sphinx doc>
-}
fillPoly
    :: (PrimMonad m, ToPoint2i point2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector point2i) -- ^ Polygons.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
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
                    , $(int32_t * nptsPtr)
                    , $(int32_t c'numPolygons)
                    , *$(Scalar * colorPtr)
                    , $(int32_t c'lineType)
                    , $(int32_t shift)
                    )
      }|]
  where
    c'numPolygons = fromIntegral $ V.length polygons
    c'lineType    = marshalLineType lineType

    npts :: VS.Vector Int32
    npts = VS.convert $ V.map (fromIntegral . V.length) polygons

{- | Draws several polygonal curves

See 'fillPoly' for an example.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#polylines OpenCV Sphinx doc>
-}
polylines
    :: (PrimMonad m, ToPoint2i point2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector point2i) -- ^ Vertices.
    -> Bool
       -- ^ Flag indicating whether the drawn polylines are closed or not. If
       -- they are closed, the function draws a line from the last vertex of
       -- each curve to its first vertex.
    -> color
    -> Int32 -- ^ Thickness of the polyline edges.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
polylines img curves isClosed color thickness lineType shift =
    unsafePrimToPrim $
    withMatPtr (unMutMat img) $ \matPtr ->
    withPolygons curves $ \curvesPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withScalarPtr color $ \colorPtr ->
      [C.exp|void {
        cv::polylines
        ( *$(Mat * matPtr)
        , $(const Point2i * * curvesPtr)
        , $(int32_t * nptsPtr)
        , $(int32_t c'numCurves)
        , $(bool c'isClosed)
        , *$(Scalar * colorPtr)
        , $(int32_t thickness)
        , $(int32_t c'lineType)
        , $(int32_t shift)
        );
      }|]
  where
    c'numCurves = fromIntegral $ V.length curves
    c'isClosed  = fromBool isClosed
    c'lineType  = marshalLineType lineType

    npts :: VS.Vector Int32
    npts = VS.convert $ V.map (fromIntegral . V.length) curves

-- | Calculates the width and height of a text string.
--
--  Calculates and returns the size of a box that contains the specified text.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#gettextsize OpenCV Sphinx doc>
getTextSize
    :: Text
    -> FontFace
    -> Double  -- ^ Font scale.
    -> Int32 -- ^ Thickness of lines used to render the text.
    -> (Size2i, Int)
getTextSize text fontFace fontScale thickness = unsafePerformIO $
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    alloca $ \(c'baseLinePtr :: Ptr Int32) -> do
      size <- size2iFromPtr $
        [C.block|Size2i * {
          Size size = cv::getTextSize( $(char * c'text)
                                     , $(int32_t c'fontFace)
                                     , $(double c'fontScale)
                                     , $(int32_t thickness)
                                     , $(int32_t * c'baseLinePtr)
                                     );
          return new Size(size);
        }|]
      baseLine <- peek c'baseLinePtr
      pure (size, fromIntegral baseLine)
  where
    c'fontFace  = marshalFontFace fontFace
    c'fontScale = realToFrac fontScale



{- | Draws a line segment connecting two points.

Example:

@
lineImg :: 'Mat'
lineImg = 'createMat' $ do
  imgM <- 'mkMatM' ('V.fromList' [200, 300]) 'MatDepth_8U' 4 transparent
  'line' imgM (V2  10 130 :: V2 'Int32') (V2 190  40 :: V2 'Int32') blue 5 'LineType_8' 0
  'line' imgM (V2 210  50 :: V2 'Int32') (V2 250 180 :: V2 'Int32') red  8 'LineType_8' 0
  'pure' imgM
@

<<doc/line.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#line OpenCV Sphinx doc>
-}
line
    :: (PrimMonad m, ToPoint2i fromPoint2i, ToPoint2i toPoint2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> fromPoint2i -- ^ First point of the line segment.
    -> toPoint2i -- ^ Scond point of the line segment.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
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
                , $(int32_t thickness)
                , $(int32_t c'lineType)
                , $(int32_t shift)
                )
      }|]
  where
    c'lineType = marshalLineType lineType

{- | Draws a text string.

The function putText renders the specified text string in the
image. Symbols that cannot be rendered using the specified font are
replaced by question marks.

Example:

@
putTextImg :: 'Mat'
putTextImg = 'createMat' $ do
  let dims = 'V.fromList' [50 + 'fromIntegral' (30 * 'fromEnum' ('maxBound' :: 'FontFace')), 400]
  imgM <- 'mkMatM' dims 'MatDepth_8U' 4 transparent
  'forM_' ('zip' [0..] ['minBound' .. 'maxBound']) $ \(n, fontFace) ->
    'putText' imgM (T.pack $ 'show' fontFace) (V2 10 (35 + n * 30) :: V2 'Int32') fontFace 1.0 black 1 'LineType_AA' False
  'pure' imgM
@

<<doc/putText.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#puttext OpenCV Sphinx doc>
-}
putText
    :: (PrimMonad m, ToPoint2i point2i, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> Text -- ^ Text string to be drawn.
    -> point2i -- ^ Bottom-left corner of the text string in the image.
    -> FontFace
    -> Double -- ^ Font scale factor that is multiplied by the font-specific base size.
    -> color -- ^ Text color.
    -> Int32 -- ^ Thickness of the lines used to draw a text.
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
                   , $(int32_t c'fontFace)
                   , $(double c'fontScale)
                   , *$(Scalar * colorPtr)
                   , $(int32_t thickness)
                   , $(int32_t c'lineType)
                   , $(bool c'bottomLeftOrigin)
                   )
      }|]
  where
    c'fontFace         = marshalFontFace fontFace
    c'fontScale        = realToFrac fontScale
    c'lineType         = marshalLineType lineType
    c'bottomLeftOrigin = fromBool bottomLeftOrigin

{- | Draws a simple, thick, or filled up-right rectangle

Example:

@
rectangleImg :: 'Mat'
rectangleImg = 'createMat' $ do
  imgM <- 'mkMatM' ('V.fromList' [200, 400]) 'MatDepth_8U' 4 transparent
  'rectangle' imgM ('mkRect' (V2  10 10) (V2 180 180)) blue  5  'LineType_8' 0
  'rectangle' imgM ('mkRect' (V2 260 30) (V2  80 140)) red (-1) 'LineType_8' 0
  'pure' imgM
@

<<doc/rectangle.png>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#rectangle OpenCV Sphinx doc>
-}
rectangle
    :: (PrimMonad m, ToScalar color)
    => MutMat (PrimState m) -- ^ Image.
    -> Rect
    -> color -- ^ Rectangle color or brightness (grayscale image).
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
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
                     , $(int32_t thickness)
                     , $(int32_t c'lineType)
                     , $(int32_t shift)
                     )
      }|]
  where
    c'lineType = marshalLineType lineType
