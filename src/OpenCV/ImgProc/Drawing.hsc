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
    , line
    , getTextSize
    , putText
    , rectangle
    ) where

import "base" Data.Int
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( append )
import qualified "text" Data.Text.Foreign as T ( withCStringLen )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.TypeLevel
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
   = LineType_8
     -- ^ 8-connected line.
     --
     -- <<doc/generated/LineType_8.png 8-connected line>>
   | LineType_4
     -- ^ 4-connected line.
     --
     -- <<doc/generated/LineType_4.png 4-connected line>>
   | LineType_AA
     -- ^ Antialiased line.
     --
     -- <<doc/generated/LineType_AA.png Antialised line>>
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
     -- ^ Normal size sans-serif font.
     --
     -- <<doc/generated/FontHersheySimplex.png FontHersheySimplex>>
   | FontHersheyPlain
     -- ^ Small size sans-serif font.
     --
     -- <<doc/generated/FontHersheyPlain.png FontHersheyPlain>>
   | FontHersheyDuplex
     -- ^ Normal size sans-serif font (more complex than 'FontHersheySimplex').
     --
     -- <<doc/generated/FontHersheyDuplex.png FontHersheyDuplex>>
   | FontHersheyComplex
     -- ^ Normal size serif font.
     --
     -- <<doc/generated/FontHersheyComplex.png FontHersheyComplex>>
   | FontHersheyTriplex
     -- ^ Normal size serif font (more complex than 'FontHersheyComplex').
     --
     -- <<doc/generated/FontHersheyTriplex.png FontHersheyTriplex>>
   | FontHersheyComplexSmall
     -- ^ Smaller version of 'FontHersheyComplex'.
     --
     -- <<doc/generated/FontHersheyComplexSmall.png FontHersheyComplexSmall>>
   | FontHersheyScriptSimplex
     -- ^ Hand-writing style font.
     --
     -- <<doc/generated/FontHersheyScriptSimplex.png FontHersheyScriptSimplex>>
   | FontHersheyScriptComplex
     -- ^ More complex variant of 'FontHersheyScriptSimplex'.
     --
     -- <<doc/generated/FontHersheyScriptComplex.png FontHersheyScriptComplex>>
   | FontItalic
     -- ^ Flag for italic font.
     --
     -- <<doc/generated/FontItalic.png FontItalic>>
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
arrowedLineImg :: Mat (ShapeT [200, 300]) ('S 4) ('S Word8)
arrowedLineImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [200, 300])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    arrowedLine imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 LineType_AA 0 0.15
    arrowedLine imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 LineType_AA 0 0.4
    pure imgM
@

<<doc/generated/examples/arrowedLineImg.png arrowedLineImg>>

<http://docs.opencv.org/3.0.0/d6/d6e/group__imgproc__draw.html#ga0a165a3ca093fd488ac709fdf10c05b2 OpenCV Doxygen doc>
<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#arrowedline OpenCV Sphinx doc>
-}
arrowedLine
    :: ( Convert fromPoint2i Point2i
       , Convert toPoint2i   Point2i
       , Convert color       Scalar
       , PrimMonad m
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
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
    withPtr (unMutMat img) $ \matPtr ->
    withPtr (convert pt1   :: Point2i) $ \pt1Ptr   ->
    withPtr (convert pt2   :: Point2i) $ \pt2Ptr   ->
    withPtr (convert color :: Scalar)  $ \colorPtr ->
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
circleImg :: Mat (ShapeT [200, 400]) ('S 4) ('S Word8)
circleImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [200, 400])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    circle imgM (V2 100 100 :: V2 Int32) 90 blue  5  LineType_AA 0
    circle imgM (V2 300 100 :: V2 Int32) 45 red (-1) LineType_AA 0
    pure imgM
@

<<doc/generated/examples/circleImg.png circleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#circle OpenCV Sphinx doc>
-}
circle
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image where the circle is drawn.
    -> point2i -- ^ Center of the circle.
    -> Int32 -- ^ Radius of the circle.
    -> color -- ^ Circle color.
    -> Int32 -- ^ Thickness of the circle outline, if positive. Negative thickness means that a filled circle is to be drawn.
    -> LineType -- ^ Type of the circle boundary.
    -> Int32 -- ^ Number of fractional bits in the coordinates of the center and in the radius value.
    -> m ()
circle img center radius color thickness lineType shift =
    unsafePrimToPrim $
    withPtr (unMutMat img) $ \matPtr ->
    withPtr (convert center :: Point2i) $ \centerPtr ->
    withPtr (convert color  :: Scalar ) $ \colorPtr  ->
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
ellipseImg :: Mat (ShapeT [200, 400]) ('S 4) ('S Word8)
ellipseImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [200, 400])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    ellipse imgM (V2 100 100 :: V2 Int32) (V2 90 60 :: V2 Int32)  30  0 360 blue  5  LineType_AA 0
    ellipse imgM (V2 300 100 :: V2 Int32) (V2 80 40 :: V2 Int32) 160 40 290 red (-1) LineType_AA 0
    pure imgM
@

<<doc/generated/examples/ellipseImg.png ellipseImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#ellipse OpenCV Sphinx doc>
-}
ellipse
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert size2i  Size2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
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
    withPtr (unMutMat img) $ \matPtr ->
    withPtr (convert center :: Point2i) $ \centerPtr ->
    withPtr (convert axes   :: Size2i ) $ \axesPtr   ->
    withPtr (convert color  :: Scalar ) $ \colorPtr  ->
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

{- | Fills a convex polygon.

The function 'fillConvexPoly' draws a filled convex polygon. This
function is much faster than the function 'fillPoly' . It can fill
not only convex polygons but any monotonic polygon without
self-intersections, that is, a polygon whose contour intersects
every horizontal line (scan line) twice at the most (though, its
top-most and/or the bottom edge could be horizontal).

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillconvexpoly OpenCV Sphinx doc>
-}
fillConvexPoly
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
    -> V.Vector point2i -- ^ Polygon vertices.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillConvexPoly img points color lineType shift =
    unsafePrimToPrim $
    withPtr (unMutMat img) $ \matPtr ->
    withArrayPtr (V.map convert points :: V.Vector Point2i) $ \pointsPtr ->
    withPtr (convert color :: Scalar) $ \colorPtr ->
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
rookPts :: Int32 -> Int32 -> V.Vector (V.Vector (V2 Int32))
rookPts w h = V.singleton $ V.fromList
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

fillPolyImg
    :: forall (h :: Nat) (w :: Nat)
     . (h ~ 300, w ~ 300)
    => Mat (ShapeT [h, w]) ('S 4) ('S Word8)
fillPolyImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [h, w])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    fillPoly imgM (rookPts w h) blue LineType_AA 0
    pure imgM
  where
    h = fromInteger $ natVal (Proxy :: Proxy h)
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/fillPolyImg.png fillPolyImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillpoly OpenCV Sphinx doc>
-}
fillPoly
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector point2i) -- ^ Polygons.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillPoly img polygons color lineType shift =
    unsafePrimToPrim $
    withPtr (unMutMat img) $ \matPtr ->
    withPolygons polygons $ \polygonsPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withPtr (convert color :: Scalar) $ \colorPtr ->
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

Example:

@
polylinesImg
    :: forall (h :: Nat) (w :: Nat)
     . (h ~ 300, w ~ 300)
    => Mat (ShapeT [h, w]) ('S 4) ('S Word8)
polylinesImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [h, w])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    polylines imgM (rookPts w h) True blue 2 LineType_AA 0
    pure imgM
  where
    h = fromInteger $ natVal (Proxy :: Proxy h)
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/polylinesImg.png polylinesImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#polylines OpenCV Sphinx doc>
-}
polylines
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
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
    withPtr (unMutMat img) $ \matPtr ->
    withPolygons curves $ \curvesPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withPtr (convert color :: Scalar) $ \colorPtr ->
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

{- | Draws a line segment connecting two points.

Example:

@
lineImg :: Mat (ShapeT [200, 300]) ('S 4) ('S Word8)
lineImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [200, 300])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    line imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 LineType_AA 0
    line imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 LineType_AA 0
    pure imgM
@

<<doc/generated/examples/lineImg.png lineImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#line OpenCV Sphinx doc>
-}
line
    :: ( PrimMonad m
       , Convert fromPoint2i Point2i
       , Convert toPoint2i   Point2i
       , Convert color       Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
    -> fromPoint2i -- ^ First point of the line segment.
    -> toPoint2i -- ^ Scond point of the line segment.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
    -> m ()
line img pt1 pt2 color thickness lineType shift =
    unsafePrimToPrim $
    withPtr (unMutMat img) $ \matPtr ->
    withPtr (convert pt1   :: Point2i) $ \pt1Ptr   ->
    withPtr (convert pt2   :: Point2i) $ \pt2Ptr   ->
    withPtr (convert color :: Scalar ) $ \colorPtr ->
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

{- | Calculates the size of a box that contains the specified text

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#gettextsize OpenCV Sphinx doc>
-}
getTextSize
    :: Text
    -> FontFace
    -> Double  -- ^ Font scale.
    -> Int32 -- ^ Thickness of lines used to render the text.
    -> (Size2i, Int32)
       -- ^ (size, baseLine) =
       -- (The size of a box that contains the specified text.
       -- , y-coordinate of the baseline relative to the bottom-most text point)
getTextSize text fontFace fontScale thickness = unsafePerformIO $
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    alloca $ \(c'baseLinePtr :: Ptr Int32) -> do
      size <- fromPtr $
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
      pure (size, baseLine)
  where
    c'fontFace  = marshalFontFace fontFace
    c'fontScale = realToFrac fontScale


{- | Draws a text string.

The function putText renders the specified text string in the
image. Symbols that cannot be rendered using the specified font are
replaced by question marks.

Example:

@
putTextImg :: Mat ('S ['D, 'S 400]) ('S 4) ('S Word8)
putTextImg = createMat $ do
    imgM <- mkMatM (height ::: (Proxy :: Proxy 400) ::: Z)
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    forM_ (zip [0..] [minBound .. maxBound]) $ \(n, fontFace) ->
      putText imgM (T.pack $ show fontFace) (V2 10 (35 + n * 30) :: V2 Int32) fontFace 1.0 black 1 LineType_AA False
    pure imgM
  where
    height :: Int32
    height = 50 + fromIntegral (30 * fromEnum (maxBound :: FontFace))
@

<<doc/generated/examples/putTextImg.png putTextImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#puttext OpenCV Sphinx doc>
-}
putText
    :: ( PrimMonad m
       , Convert point2i Point2i
       , Convert color   Scalar
       )
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
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
    withPtr (unMutMat img) $ \matPtr ->
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    withPtr (convert org   :: Point2i) $ \orgPtr   ->
    withPtr (convert color :: Scalar ) $ \colorPtr ->
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
rectangleImg :: Mat (ShapeT [200, 400]) ('S 4) ('S Word8)
rectangleImg = createMat $ do
    imgM <- mkMatM (Proxy :: Proxy [200, 400])
                   (Proxy :: Proxy 4)
                   (Proxy :: Proxy Word8)
                   transparent
    rectangle imgM (mkRect (V2  10 10) (V2 180 180)) blue  5  LineType_8 0
    rectangle imgM (mkRect (V2 260 30) (V2  80 140)) red (-1) LineType_8 0
    pure imgM
@

<<doc/generated/examples/rectangleImg.png rectangleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#rectangle OpenCV Sphinx doc>
-}
rectangle
    :: (PrimMonad m, Convert color Scalar)
    => MutMat ('S [height, width]) channels depth (PrimState m) -- ^ Image.
    -> Rect
    -> color -- ^ Rectangle color or brightness (grayscale image).
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
    -> m ()
rectangle img rect color thickness lineType shift =
    unsafePrimToPrim $
    withPtr (unMutMat img) $ \matPtr ->
    withPtr rect $ \rectPtr ->
    withPtr (convert color :: Scalar) $ \colorPtr ->
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
