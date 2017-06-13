{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.ColorMaps
    ( ColorMap(..)
    , applyColorMap
    ) where

import "base" Data.Int
import "base" Data.Word
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel

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

data ColorMap
   = ColorMapAutumn  -- ^ <<doc/generated/examples/colorMapAutumImg.png   colorMapAutumImg  >>
   | ColorMapBone    -- ^ <<doc/generated/examples/colorMapBoneImg.png    colorMapBoneImg   >>
   | ColorMapJet     -- ^ <<doc/generated/examples/colorMapJetImg.png     colorMapJetImg    >>
   | ColorMapWinter  -- ^ <<doc/generated/examples/colorMapWinterImg.png  colorMapWinterImg >>
   | ColorMapRainbow -- ^ <<doc/generated/examples/colorMapRainbowImg.png colorMapRainbowImg>>
   | ColorMapOcean   -- ^ <<doc/generated/examples/colorMapOceanImg.png   colorMapOceanImg  >>
   | ColorMapSummer  -- ^ <<doc/generated/examples/colorMapSummerImg.png  colorMapSummerImg >>
   | ColorMapSpring  -- ^ <<doc/generated/examples/colorMapSpringImg.png  colorMapSpringImg >>
   | ColorMapCool    -- ^ <<doc/generated/examples/colorMapCoolImg.png    colorMapCoolImg   >>
   | ColorMapHsv     -- ^ <<doc/generated/examples/colorMapHsvImg.png     colorMapHsvImg    >>
   | ColorMapPink    -- ^ <<doc/generated/examples/colorMapPinkImg.png    colorMapPinkImg   >>
   | ColorMapHot     -- ^ <<doc/generated/examples/colorMapHotImg.png     colorMapHotImg    >>
   | ColorMapParula  -- ^ <<doc/generated/examples/colorMapParulaImg.png  colorMapParulaImg >>

#num COLORMAP_AUTUMN
#num COLORMAP_BONE
#num COLORMAP_JET
#num COLORMAP_WINTER
#num COLORMAP_RAINBOW
#num COLORMAP_OCEAN
#num COLORMAP_SUMMER
#num COLORMAP_SPRING
#num COLORMAP_COOL
#num COLORMAP_HSV
#num COLORMAP_PINK
#num COLORMAP_HOT
#num COLORMAP_PARULA

marshalColorMap :: ColorMap -> Int32
marshalColorMap = \case
   ColorMapAutumn  -> c'COLORMAP_AUTUMN
   ColorMapBone    -> c'COLORMAP_BONE
   ColorMapJet     -> c'COLORMAP_JET
   ColorMapWinter  -> c'COLORMAP_WINTER
   ColorMapRainbow -> c'COLORMAP_RAINBOW
   ColorMapOcean   -> c'COLORMAP_OCEAN
   ColorMapSummer  -> c'COLORMAP_SUMMER
   ColorMapSpring  -> c'COLORMAP_SPRING
   ColorMapCool    -> c'COLORMAP_COOL
   ColorMapHsv     -> c'COLORMAP_HSV
   ColorMapPink    -> c'COLORMAP_PINK
   ColorMapHot     -> c'COLORMAP_HOT
   ColorMapParula  -> c'COLORMAP_PARULA

{- | Applies a GNU Octave/MATLAB equivalent colormap on a given image

The human perception isn’t built for observing fine changes in grayscale
images. Human eyes are more sensitive to observing changes between colors, so
you often need to recolor your grayscale images to get a clue about
them. OpenCV now comes with various colormaps to enhance the visualization in
your computer vision application.

Example:

@
grayscaleImg
    :: forall (height :: Nat) (width :: Nat) depth
     . (height ~ 30, width ~ 256, depth ~ Word8)
    => Mat (ShapeT [height, width]) ('S 1) ('S depth)
grayscaleImg = exceptError $
    matFromFunc
      (Proxy :: Proxy [height, width])
      (Proxy :: Proxy 1)
      (Proxy :: Proxy depth)
      grayscale
  where
    grayscale :: [Int] -> Int -> Word8
    grayscale [_y, x] 0 = fromIntegral x
    grayscale _pos _channel = error "impossible"

type ColorMapImg = Mat (ShapeT [30, 256]) ('S 3) ('S Word8)

mkColorMapImg :: ColorMap -> ColorMapImg
mkColorMapImg cmap = exceptError $ applyColorMap cmap grayscaleImg

colorMapAutumImg   :: ColorMapImg
colorMapBoneImg    :: ColorMapImg
colorMapJetImg     :: ColorMapImg
colorMapWinterImg  :: ColorMapImg
colorMapRainbowImg :: ColorMapImg
colorMapOceanImg   :: ColorMapImg
colorMapSummerImg  :: ColorMapImg
colorMapSpringImg  :: ColorMapImg
colorMapCoolImg    :: ColorMapImg
colorMapHsvImg     :: ColorMapImg
colorMapPinkImg    :: ColorMapImg
colorMapHotImg     :: ColorMapImg
colorMapParulaImg  :: ColorMapImg

colorMapAutumImg   = mkColorMapImg ColorMapAutumn
colorMapBoneImg    = mkColorMapImg ColorMapBone
colorMapJetImg     = mkColorMapImg ColorMapJet
colorMapWinterImg  = mkColorMapImg ColorMapWinter
colorMapRainbowImg = mkColorMapImg ColorMapRainbow
colorMapOceanImg   = mkColorMapImg ColorMapOcean
colorMapSummerImg  = mkColorMapImg ColorMapSummer
colorMapSpringImg  = mkColorMapImg ColorMapSpring
colorMapCoolImg    = mkColorMapImg ColorMapCool
colorMapHsvImg     = mkColorMapImg ColorMapHsv
colorMapPinkImg    = mkColorMapImg ColorMapPink
colorMapHotImg     = mkColorMapImg ColorMapHot
colorMapParulaImg  = mkColorMapImg ColorMapParula
@

<<doc/generated/examples/grayscaleImg.png grayscaleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/colormaps.html#applycolormap OpenCV Sphinx doc>
-}
applyColorMap
    :: ColorMap
    -> Mat shape ('S 1) ('S Word8)
    -> CvExcept (Mat shape ('S 3) ('S Word8))
applyColorMap colorMap src = unsafeWrapException $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withPtr src $ \srcPtr ->
      withPtr dst $ \dstPtr ->
        [cvExcept|
          cv::applyColorMap( *$(Mat * srcPtr)
                           , *$(Mat * dstPtr)
                           , $(int32_t c'colorMap)
                           );
        |]
  where
    c'colorMap = marshalColorMap colorMap
