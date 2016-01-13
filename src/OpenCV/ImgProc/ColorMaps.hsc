{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.ColorMaps
    ( ColorMap(..)
    , applyColorMap
    ) where

import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Internal

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
   = ColorMapAutumn  -- ^ <<doc/generated/colorMapAutumImg.png   colorMapAutumImg  >>
   | ColorMapBone    -- ^ <<doc/generated/colorMapBoneImg.png    colorMapBoneImg   >>
   | ColorMapJet     -- ^ <<doc/generated/colorMapJetImg.png     colorMapJetImg    >>
   | ColorMapWinter  -- ^ <<doc/generated/colorMapWinterImg.png  colorMapWinterImg >>
   | ColorMapRainbow -- ^ <<doc/generated/colorMapRainbowImg.png colorMapRainbowImg>>
   | ColorMapOcean   -- ^ <<doc/generated/colorMapOceanImg.png   colorMapOceanImg  >>
   | ColorMapSummer  -- ^ <<doc/generated/colorMapSummerImg.png  colorMapSummerImg >>
   | ColorMapSpring  -- ^ <<doc/generated/colorMapSpringImg.png  colorMapSpringImg >>
   | ColorMapCool    -- ^ <<doc/generated/colorMapCoolImg.png    colorMapCoolImg   >>
   | ColorMapHsv     -- ^ <<doc/generated/colorMapHsvImg.png     colorMapHsvImg    >>
   | ColorMapPink    -- ^ <<doc/generated/colorMapPinkImg.png    colorMapPinkImg   >>
   | ColorMapHot     -- ^ <<doc/generated/colorMapHotImg.png     colorMapHotImg    >>
   | ColorMapParula  -- ^ <<doc/generated/colorMapParulaImg.png  colorMapParulaImg >>

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
grayscaleImg :: 'Mat'
grayscaleImg = 'createMat' $ do
    imgM <- 'mkMatM' (V.fromList ['fromIntegral' h, 'fromIntegral' w]) 'MatDepth_8U' 1 black
    forM_ [0..w-1] $ \x ->
      forM_ [0..h-1] $ \y ->
        'unsafeWrite' imgM [y,x] ('fromIntegral' x :: 'Word8')
    pure imgM
  where
    w = 256
    h = 30

mkColorMapImg :: 'ColorMap' -> 'Mat'
mkColorMapImg cmap = either throw id $ 'applyColorMap' cmap grayscaleImg

colorMapAutumImg   :: 'Mat'
colorMapBoneImg    :: 'Mat'
colorMapJetImg     :: 'Mat'
colorMapWinterImg  :: 'Mat'
colorMapRainbowImg :: 'Mat'
colorMapOceanImg   :: 'Mat'
colorMapSummerImg  :: 'Mat'
colorMapSpringImg  :: 'Mat'
colorMapCoolImg    :: 'Mat'
colorMapHsvImg     :: 'Mat'
colorMapPinkImg    :: 'Mat'
colorMapHotImg     :: 'Mat'
colorMapParulaImg  :: 'Mat'

colorMapAutumImg   = mkColorMapImg 'ColorMapAutumn'
colorMapBoneImg    = mkColorMapImg 'ColorMapBone'
colorMapJetImg     = mkColorMapImg 'ColorMapJet'
colorMapWinterImg  = mkColorMapImg 'ColorMapWinter'
colorMapRainbowImg = mkColorMapImg 'ColorMapRainbow'
colorMapOceanImg   = mkColorMapImg 'ColorMapOcean'
colorMapSummerImg  = mkColorMapImg 'ColorMapSummer'
colorMapSpringImg  = mkColorMapImg 'ColorMapSpring'
colorMapCoolImg    = mkColorMapImg 'ColorMapCool'
colorMapHsvImg     = mkColorMapImg 'ColorMapHsv'
colorMapPinkImg    = mkColorMapImg 'ColorMapPink'
colorMapHotImg     = mkColorMapImg 'ColorMapHot'
colorMapParulaImg  = mkColorMapImg 'ColorMapParula'
@

<<doc/generated/grayscaleImg.png grayscaleImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/colormaps.html#applycolormap OpenCV Sphinx doc>
-}
applyColorMap :: ColorMap -> Mat -> Either CvException Mat
applyColorMap colorMap src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          cv::applyColorMap( *$(Mat * srcPtr)
                           , *$(Mat * dstPtr)
                           , $(int32_t c'colorMap)
                           );
        |]
  where
    c'colorMap = marshalColorMap colorMap
