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
import "this" OpenCV.Core.Types.Mat.Internal ( newEmptyMat )
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
   = ColorMapAutumn  -- ^ <<doc/colorscale_autumn.jpg>>
   | ColorMapBone    -- ^ <<doc/colorscale_bone.jpg>>
   | ColorMapJet     -- ^ <<doc/colorscale_jet.jpg>>
   | ColorMapWinter  -- ^ <<doc/colorscale_winter.jpg>>
   | ColorMapRainbow -- ^ <<doc/colorscale_rainbow.jpg>>
   | ColorMapOcean   -- ^ <<doc/colorscale_ocean.jpg>>
   | ColorMapSummer  -- ^ <<doc/colorscale_summer.jpg>>
   | ColorMapSpring  -- ^ <<doc/colorscale_spring.jpg>>
   | ColorMapCool    -- ^ <<doc/colorscale_cool.jpg>>
   | ColorMapHsv     -- ^ <<doc/colorscale_hsv.jpg>>
   | ColorMapPink    -- ^ <<doc/colorscale_pink.jpg>>
   | ColorMapHot     -- ^ <<doc/colorscale_hot.jpg>>
   | ColorMapParula  -- ^ <<doc/colorscale_parula.jpg>>

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

-- | Applies a GNU Octave/MATLAB equivalent colormap on a given image
--
-- The human perception isn’t built for observing fine changes in grayscale
-- images. Human eyes are more sensitive to observing changes between colors, so
-- you often need to recolor your grayscale images to get a clue about
-- them. OpenCV now comes with various colormaps to enhance the visualization in
-- your computer vision application.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/colormaps.html#applycolormap OpenCV Sphinx doc>
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
