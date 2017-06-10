{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.Drawing
    ( LineType(..)
    , Font(..)
    , FontFace(..)
    , FontSlant(..)
    , ContourDrawMode(..)
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
    , drawContours
    , marker
    ) where

import "base" Data.Int
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Storable as VS
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( withArray )
import "base" Foreign.Marshal.Utils ( fromBool )
import "base" Foreign.Ptr ( Ptr )
import "base" Foreign.Storable ( peek )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimMonad, PrimState, unsafePrimToPrim )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( append )
import qualified "text" Data.Text.Foreign as T ( withCStringLen )
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.TypeLevel
import "base" System.IO.Unsafe ( unsafePerformIO )

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

data Font
   = Font
     { _fontFace  :: !FontFace
     , _fontSlant :: !FontSlant
     , _fontScale :: !Double
     } deriving (Show)

data FontFace
   = FontHersheySimplex
     -- ^ Normal size sans-serif font. Does not have a 'Slanted' variant.
     --
     -- <<doc/generated/FontHersheySimplex.png FontHersheySimplex>>
   | FontHersheyPlain
     -- ^ Small size sans-serif font.
     --
     -- <<doc/generated/FontHersheyPlain.png FontHersheyPlain>>
     --
     -- <<doc/generated/FontHersheyPlain_slanted.png FontHersheyPlain>>
   | FontHersheyDuplex
     -- ^ Normal size sans-serif font (more complex than
     -- 'FontHersheySimplex'). Does not have a 'Slanted' variant.
     --
     -- <<doc/generated/FontHersheyDuplex.png FontHersheyDuplex>>
   | FontHersheyComplex
     -- ^ Normal size serif font.
     --
     -- <<doc/generated/FontHersheyComplex.png FontHersheyComplex>>
     --
     -- <<doc/generated/FontHersheyComplex_slanted.png FontHersheyComplex>>
   | FontHersheyTriplex
     -- ^ Normal size serif font (more complex than 'FontHersheyComplex').
     --
     -- <<doc/generated/FontHersheyTriplex.png FontHersheyTriplex>>
     --
     -- <<doc/generated/FontHersheyTriplex_slanted.png FontHersheyTriplex>>
   | FontHersheyComplexSmall
     -- ^ Smaller version of 'FontHersheyComplex'.
     --
     -- <<doc/generated/FontHersheyComplexSmall.png FontHersheyComplexSmall>>
     --
     -- <<doc/generated/FontHersheyComplexSmall_slanted.png FontHersheyComplexSmall>>
   | FontHersheyScriptSimplex
     -- ^ Hand-writing style font. Does not have a 'Slanted' variant.
     --
     -- <<doc/generated/FontHersheyScriptSimplex.png FontHersheyScriptSimplex>>
   | FontHersheyScriptComplex
     -- ^ More complex variant of 'FontHersheyScriptSimplex'. Does not have a
     -- 'Slanted' variant.
     --
     -- <<doc/generated/FontHersheyScriptComplex.png FontHersheyScriptComplex>>
     deriving (Show, Enum, Bounded)

data FontSlant
   = NotSlanted
   | Slanted
     deriving (Show)

marshalFont :: Font -> (Int32, C.CDouble)
marshalFont (Font face slant scale) =
    ( marshalFontFace face + marshalFontSlant slant
    , realToFrac scale
    )

#num FONT_ITALIC

marshalFontSlant :: FontSlant -> Int32
marshalFontSlant = \case
   NotSlanted -> 0
   Slanted    -> c'FONT_ITALIC

#num FONT_HERSHEY_SIMPLEX
#num FONT_HERSHEY_PLAIN
#num FONT_HERSHEY_DUPLEX
#num FONT_HERSHEY_COMPLEX
#num FONT_HERSHEY_TRIPLEX
#num FONT_HERSHEY_COMPLEX_SMALL
#num FONT_HERSHEY_SCRIPT_SIMPLEX
#num FONT_HERSHEY_SCRIPT_COMPLEX

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


{- | Draws a arrow segment pointing from the first point to the second one

Example:

@
arrowedLineImg :: Mat (ShapeT [200, 300]) ('S 4) ('S Word8)
arrowedLineImg = exceptError $
    withMatM
      (Proxy :: Proxy [200, 300])
      (Proxy :: Proxy 4)
      (Proxy :: Proxy Word8)
      transparent $ \\imgM -> do
        arrowedLine imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 LineType_AA 0 0.15
        arrowedLine imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 LineType_AA 0 0.4
@

<<doc/generated/examples/arrowedLineImg.png arrowedLineImg>>

<http://docs.opencv.org/3.0.0/d6/d6e/group__imgproc__draw.html#ga0a165a3ca093fd488ac709fdf10c05b2 OpenCV Doxygen doc>
<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#arrowedline OpenCV Sphinx doc>
-}
arrowedLine
    :: ( IsPoint2 fromPoint2 Int32
       , IsPoint2 toPoint2   Int32
       , ToScalar  color
       , PrimMonad m
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> fromPoint2 Int32 -- ^ The point the arrow starts from.
    -> toPoint2 Int32 -- ^ The point the arrow points to.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
    -> Double -- ^ The length of the arrow tip in relation to the arrow length.
    -> m ()
arrowedLine img pt1 pt2 color thickness lineType shift tipLength =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPtr (toPoint pt1) $ \pt1Ptr   ->
    withPtr (toPoint pt2) $ \pt2Ptr   ->
    withPtr (toScalar color) $ \colorPtr ->
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
circleImg = exceptError $
    withMatM
      (Proxy :: Proxy [200, 400])
      (Proxy :: Proxy 4)
      (Proxy :: Proxy Word8)
      transparent $ \\imgM -> do
        lift $ circle imgM (V2 100 100 :: V2 Int32) 90 blue  5  LineType_AA 0
        lift $ circle imgM (V2 300 100 :: V2 Int32) 45 red (-1) LineType_AA 0
@

<<doc/generated/examples/circleImg.png circleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#circle OpenCV Sphinx doc>
-}
circle
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image where the circle is drawn.
    -> point2 Int32 -- ^ Center of the circle.
    -> Int32 -- ^ Radius of the circle.
    -> color -- ^ Circle color.
    -> Int32 -- ^ Thickness of the circle outline, if positive. Negative thickness means that a filled circle is to be drawn.
    -> LineType -- ^ Type of the circle boundary.
    -> Int32 -- ^ Number of fractional bits in the coordinates of the center and in the radius value.
    -> m ()
circle img center radius color thickness lineType shift =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPtr (toPoint center) $ \centerPtr ->
    withPtr (toScalar color) $ \colorPtr  ->
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
ellipseImg = exceptError $
    withMatM
      (Proxy :: Proxy [200, 400])
      (Proxy :: Proxy 4)
      (Proxy :: Proxy Word8)
      transparent $ \\imgM -> do
        lift $ ellipse imgM (V2 100 100 :: V2 Int32) (V2 90 60 :: V2 Int32)  30  0 360 blue  5  LineType_AA 0
        lift $ ellipse imgM (V2 300 100 :: V2 Int32) (V2 80 40 :: V2 Int32) 160 40 290 red (-1) LineType_AA 0
@

<<doc/generated/examples/ellipseImg.png ellipseImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#ellipse OpenCV Sphinx doc>
-}
ellipse
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , IsSize   size   Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> point2 Int32 -- ^ Center of the ellipse.
    -> size   Int32  -- ^ Half of the size of the ellipse main axes.
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
    withPtr img $ \matPtr ->
    withPtr (toPoint  center) $ \centerPtr ->
    withPtr (toSize   axes  ) $ \axesPtr   ->
    withPtr (toScalar color ) $ \colorPtr  ->
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

Example:

@
fillConvexPolyImg
    :: forall (h :: Nat) (w :: Nat)
     . (h ~ 300, w ~ 300)
    => Mat (ShapeT [h, w]) ('S 4) ('S Word8)
fillConvexPolyImg = exceptError $
    withMatM (Proxy :: Proxy [h, w])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ fillConvexPoly imgM pentagon blue LineType_AA 0
  where
    pentagon :: V.Vector (V2 Int32)
    pentagon = V.fromList
               [ V2 150   0
               , V2   7 104
               , V2  62 271
               , V2 238 271
               , V2 293 104
               ]
@

<<doc/generated/examples/fillConvexPolyImg.png fillConvexPolyImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillconvexpoly OpenCV Sphinx doc>
-}
fillConvexPoly
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , ToScalar  color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> V.Vector (point2 Int32) -- ^ Polygon vertices.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillConvexPoly img points color lineType shift =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withArrayPtr (V.map toPoint points) $ \pointsPtr ->
    withPtr (toScalar color) $ \colorPtr ->
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
fillPolyImg = exceptError $
    withMatM (Proxy :: Proxy [h, w])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ fillPoly imgM (rookPts w h) blue LineType_AA 0
  where
    h = fromInteger $ natVal (Proxy :: Proxy h)
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/fillPolyImg.png fillPolyImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#fillpoly OpenCV Sphinx doc>
-}
fillPoly
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector (point2 Int32)) -- ^ Polygons.
    -> color -- ^ Polygon color.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the vertex coordinates.
    -> m ()
fillPoly img polygons color lineType shift =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPolygons polygons $ \polygonsPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withPtr (toScalar color) $ \colorPtr ->
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
polylinesImg = exceptError $
    withMatM (Proxy :: Proxy [h, w])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ polylines imgM (rookPts w h) True blue 2 LineType_AA 0
  where
    h = fromInteger $ natVal (Proxy :: Proxy h)
    w = fromInteger $ natVal (Proxy :: Proxy w)
@

<<doc/generated/examples/polylinesImg.png polylinesImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#polylines OpenCV Sphinx doc>
-}
polylines
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> V.Vector (V.Vector (point2 Int32)) -- ^ Vertices.
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
    withPtr img $ \matPtr ->
    withPolygons curves $ \curvesPtr ->
    VS.unsafeWith npts $ \nptsPtr ->
    withPtr (toScalar color) $ \colorPtr ->
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
lineImg = exceptError $
    withMatM (Proxy :: Proxy [200, 300])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ line imgM (V2  10 130 :: V2 Int32) (V2 190  40 :: V2 Int32) blue 5 LineType_AA 0
      lift $ line imgM (V2 210  50 :: V2 Int32) (V2 250 180 :: V2 Int32) red  8 LineType_AA 0
@

<<doc/generated/examples/lineImg.png lineImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#line OpenCV Sphinx doc>
-}
line
    :: ( PrimMonad m
       , IsPoint2 fromPoint2 Int32
       , IsPoint2 toPoint2   Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> fromPoint2 Int32 -- ^ First point of the line segment.
    -> toPoint2   Int32 -- ^ Scond point of the line segment.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
    -> m ()
line img pt1 pt2 color thickness lineType shift =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPtr (toPoint pt1) $ \pt1Ptr ->
    withPtr (toPoint pt2) $ \pt2Ptr ->
    withPtr (toScalar color) $ \colorPtr ->
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
    -> Font
    -> Int32 -- ^ Thickness of lines used to render the text.
    -> (Size2i, Int32)
       -- ^ (size, baseLine) =
       -- (The size of a box that contains the specified text.
       -- , y-coordinate of the baseline relative to the bottom-most text point)
getTextSize text font thickness = unsafePerformIO $
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
    (c'fontFace, c'fontScale) = marshalFont font

{- | Draws a text string.

The function putText renders the specified text string in the
image. Symbols that cannot be rendered using the specified font are
replaced by question marks.

Example:

@
putTextImg :: Mat ('S ['D, 'S 400]) ('S 4) ('S Word8)
putTextImg = exceptError $
    withMatM (height ::: (Proxy :: Proxy 400) ::: Z)
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      forM_ (zip [0..] [minBound .. maxBound]) $ \\(n, fontFace) ->
        lift $ putText imgM
                       (T.pack $ show fontFace)
                       (V2 10 (35 + n * 30) :: V2 Int32)
                       (Font fontFace NotSlanted 1.0)
                       black
                       1
                       LineType_AA
                       False
  where
    height :: Int32
    height = 50 + fromIntegral (30 * fromEnum (maxBound :: FontFace))
@

<<doc/generated/examples/putTextImg.png putTextImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#puttext OpenCV Sphinx doc>
-}
putText
    :: ( PrimMonad m
       , IsPoint2 point2 Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> Text -- ^ Text string to be drawn.
    -> point2 Int32 -- ^ Bottom-left corner of the text string in the image.
    -> Font
    -> color -- ^ Text color.
    -> Int32 -- ^ Thickness of the lines used to draw a text.
    -> LineType
    -> Bool -- ^ When 'True', the image data origin is at the bottom-left corner. Otherwise, it is at the top-left corner.
    -> m ()
putText img text org font color thickness lineType bottomLeftOrigin =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    T.withCStringLen (T.append text "\0") $ \(c'text, _textLength) ->
    withPtr (toPoint org) $ \orgPtr ->
    withPtr (toScalar color) $ \colorPtr ->
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
    (c'fontFace, c'fontScale) = marshalFont font
    c'lineType = marshalLineType lineType
    c'bottomLeftOrigin = fromBool bottomLeftOrigin

{- | Draws a simple, thick, or filled up-right rectangle

Example:

@
rectangleImg :: Mat (ShapeT [200, 400]) ('S 4) ('S Word8)
rectangleImg = exceptError $
    withMatM (Proxy :: Proxy [200, 400])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ rectangle imgM (toRect $ HRect (V2  10 10) (V2 180 180)) blue  5  LineType_8 0
      lift $ rectangle imgM (toRect $ HRect (V2 260 30) (V2  80 140)) red (-1) LineType_8 0
@

<<doc/generated/examples/rectangleImg.png rectangleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/drawing_functions.html#rectangle OpenCV Sphinx doc>
-}
rectangle
    :: (PrimMonad m, ToScalar color, IsRect rect Int32)
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> rect Int32
    -> color -- ^ Rectangle color or brightness (grayscale image).
    -> Int32 -- ^ Line thickness.
    -> LineType
    -> Int32 -- ^ Number of fractional bits in the point coordinates.
    -> m ()
rectangle img rect color thickness lineType shift =
    unsafePrimToPrim $
    withPtr img $ \matPtr ->
    withPtr (toRect rect) $ \rectPtr ->
    withPtr (toScalar color) $ \colorPtr ->
      [C.exp|void {
        cv::rectangle( *$(Mat * matPtr)
                     , *$(Rect2i * rectPtr)
                     , *$(Scalar * colorPtr)
                     , $(int32_t thickness)
                     , $(int32_t c'lineType)
                     , $(int32_t shift)
                     )
      }|]
  where
    c'lineType = marshalLineType lineType


data ContourDrawMode
  = OutlineContour LineType
                   Int32 -- ^ Thickness of lines the contours are drawn with.
  | FillContours -- ^ Draw the contour, filling in the area.

marshalContourDrawMode
  :: ContourDrawMode -> (Int32, Int32)
marshalContourDrawMode = \case
  OutlineContour lineType thickness -> (marshalLineType lineType, thickness)
  FillContours -> (marshalLineType LineType_4, -1)

{-|

Draw contours onto a black image.

Example:

@
flowerContours :: Mat ('S ['S 512, 'S 768]) ('S 3) ('S Word8)
flowerContours = exceptError $
  withMatM (Proxy :: Proxy [512,768])
           (Proxy :: Proxy 3)
           (Proxy :: Proxy Word8)
           black $ \\imgM -> do
    edges <- thaw $ exceptError $
             cvtColor bgr gray flower_768x512 >>=
             canny 30 20 Nothing CannyNormL1
    contours <- findContours ContourRetrievalList
                             ContourApproximationSimple edges
    lift $ drawContours (V.map contourPoints contours)
                        red
                        (OutlineContour LineType_AA 1)
                        imgM
@

<<doc/generated/examples/flowerContours.png flowerContours>>

-}
drawContours :: (ToScalar color, PrimMonad m)
             => V.Vector (V.Vector Point2i)
             -> color -- ^ Color of the contours.
             -> ContourDrawMode
             -> Mut (Mat ('S [h, w]) channels depth) (PrimState m) -- ^ Image.
             -> m ()
drawContours contours color drawMode img = unsafePrimToPrim $
  withArrayPtr (V.concat (V.toList contours)) $ \contoursPtrPtr ->
  withArray (V.toList (V.map (fromIntegral . V.length) contours)) $ \(contourLengthsPtr :: Ptr Int32) ->
  withPtr (toScalar color) $ \colorPtr ->
  withPtr img $ \dstPtr ->
    [C.exp|void {
      int32_t *contourLengths = $(int32_t * contourLengthsPtr);
      Point2i * contoursPtr = $(Point2i * contoursPtrPtr);
      std::vector<std::vector<cv::Point>> contours;
      int32_t numContours = $(int32_t numContours);

      int k = 0;
      for(int i = 0; i < numContours; i++) {
        std::vector<cv::Point> contour;
        for(int j = 0; j < contourLengths[i]; j++) {
          contour.push_back( contoursPtr[k] );
          k++;
        }
        contours.push_back(contour);
      }

      cv::drawContours(
        *$(Mat * dstPtr),
        contours,
        -1,
        *$(Scalar * colorPtr),
        $(int32_t c'thickness),
        $(int32_t c'lineType)
      );
    }|]
  where
    numContours = fromIntegral (V.length contours)
    (c'lineType, c'thickness) = marshalContourDrawMode drawMode

{-| Draws a marker on a predefined position in an image.

The marker will be drawn as as a 20-pixel cross.

Example:

@
markerImg :: Mat (ShapeT [100, 100]) ('S 4) ('S Word8)
markerImg = exceptError $
    withMatM (Proxy :: Proxy [100, 100])
             (Proxy :: Proxy 4)
             (Proxy :: Proxy Word8)
             transparent $ \\imgM -> do
      lift $ marker imgM (50 :: V2 Int32) blue
@

<<doc/generated/examples/markerImg.png markerImg>>
-}
marker
  :: (PrimMonad m, IsPoint2 point2 Int32, ToScalar color)
  => Mut (Mat ('S '[ height, width]) channels depth) (PrimState m)
    -- ^ The image to draw the marker on.
  -> point2 Int32
    -- ^ The point where the crosshair is positioned.
  -> color
    -- ^ Line color.
  -> m ()
marker img center color =
  unsafePrimToPrim $
  withPtr img $ \matPtr ->
  withPtr (toPoint center) $ \centerPtr ->
  withPtr (toScalar color) $ \colorPtr  ->
    [C.exp|void {
      cv::drawMarker( *$(Mat * matPtr)
                    , *$(Point2i * centerPtr)
                    , *$(Scalar * colorPtr))
    }|]
