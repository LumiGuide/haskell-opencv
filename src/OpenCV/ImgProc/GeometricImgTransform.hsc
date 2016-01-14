{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |

The functions in this section perform various geometrical transformations of 2D
images. They do not change the image content but deform the pixel grid and map
this deformed grid to the destination image. In fact, to avoid sampling
artifacts, the mapping is done in the reverse order, from destination to the
source. That is, for each pixel @(x,y)@ of the destination image, the functions
compute coordinates of the corresponding "donor" pixel in the source image and
copy the pixel value:

@dst(x,y) = src(fx(x,y), fy(x,y))@

In case when you specify the forward mapping @\<gx,gy> : src -> dst@, the OpenCV
functions first compute the corresponding inverse mapping @\<fx,fy>:dst->src@
and then use the above formula.

The actual implementations of the geometrical transformations, from the most
generic remap and to the simplest and the fastest resize, need to solve two main
problems with the above formula:

* Extrapolation of non-existing pixels.
Similarly to the filtering functions described in the previous section, for some
@(x,y)@, either one of @fx(x,y)@, or @fy(x,y)@, or both of them may fall outside
of the image. In this case, an extrapolation method needs to be used. OpenCV
provides the same selection of extrapolation methods as in the filtering
functions. In addition, it provides the method 'BorderTransparent'. This means
that the corresponding pixels in the destination image will not be modified at
all.

* Interpolation of pixel values.
Usually @fx(x,y)@ and @fy(x,y)@ are floating-point numbers. This means that
@\<fx,fy>@ can be either an affine or perspective transformation, or radial lens
distortion correction, and so on. So, a pixel value at fractional coordinates
needs to be retrieved. In the simplest case, the coordinates can be just rounded
to the nearest integer coordinates and the corresponding pixel can be used. This
is called a nearest-neighbor interpolation. However, a better result can be
achieved by using more sophisticated interpolation methods , where a polynomial
function is fit into some neighborhood of the computed pixel
@(fx(x,y),fy(x,y))@, and then the value of the polynomial at @(fx(x,y),fy(x,y))@
is taken as the interpolated pixel value. In OpenCV, you can choose between
several interpolation methods. See resize for details.
-}
module OpenCV.ImgProc.GeometricImgTransform
    ( ResizeAbsRel(..)
    , resize
    , warpAffine
    , warpPerspective
    , invertAffineTransform
    ) where

import "base" Foreign.C.Types ( CDouble )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.Vector ( zero )
import "lumi-hackage-extended" Lumi.Prelude hiding ( shift )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Internal
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Internal
import "this" OpenCV.ImgProc.Types
import "this" OpenCV.ImgProc.Types.Internal

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

data ResizeAbsRel
   = ResizeAbs Size2i -- ^ Resize to an absolute size.
   | ResizeRel (V2 Double)
     -- ^ Resize with relative factors for both the width and the height.
     deriving Show

marshalResizeAbsRel
    :: ResizeAbsRel
    -> (Size2i, CDouble, CDouble)
marshalResizeAbsRel (ResizeAbs s) = (s, 0   , 0   )
marshalResizeAbsRel (ResizeRel f) = (s, c'fx, c'fy)
  where
    s = toSize2i (zero :: V2 Int32)
    (V2 c'fx c'fy) = realToFrac <$> f

{- | Resizes an image

To shrink an image, it will generally look best with 'InterArea' interpolation,
whereas to enlarge an image, it will generally look best with 'InterCubic'
(slow) or 'InterLinear' (faster but still looks OK).

Example:

@
resizeInterAreaImg :: Mat
resizeInterAreaImg = createMat $ do
    imgM <- mkMatM (V.fromList [h, w + (w `div` 2)]) MatDepth_8U 3 transparent
    void $ matCopyToM imgM (V2 0 0) birds_768x512
    void $ matCopyToM imgM (V2 w 0) birds_resized
    arrowedLine imgM (V2 startX y) (V2 pointX y) red 4 LineType_8 0 0.15
    pure imgM
  where
    birds_resized = either throw id $ 'resize' ('ResizeRel' $ pure 0.5) 'InterArea' birds_768x512

    [h, w] = miShape $ matInfo birds_768x512
    startX = round $ fromIntegral w * (0.95 :: Double)
    pointX = round $ fromIntegral w * (1.05 :: Double)
    y = h \`div\` 4
@

<<doc/generated/resizeInterAreaImg.png resizeInterAreaImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#resize OpenCV Sphinx doc>
-}
resize
    :: ResizeAbsRel
    -> InterpolationMethod
    -> Mat
    -> Either CvException Mat
resize factor interpolationMethod src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
      withSize2iPtr dsize $ \dsizePtr ->
        [cvExcept|
          cv::resize
          ( *$(Mat * srcPtr)
          , *$(Mat * dstPtr)
          , *$(Size2i * dsizePtr)
          , $(double fx)
          , $(double fy)
          , $(int32_t c'interpolation)
          );
        |]
  where
    (dsize, fx, fy) = marshalResizeAbsRel factor
    c'interpolation = marshalInterpolationMethod interpolationMethod

#num WARP_FILL_OUTLIERS
#num WARP_INVERSE_MAP

{- | Applies an affine transformation to an image

Example:

@
warpAffineImg :: 'Mat'
warpAffineImg = either throw id $
    'warpAffine' birds_512x341 transform 'InterLinear' False False 'BorderReplicate'
  where
    -- TODO: more interesting transformation (use cv::getRotationMatrix2D)
    transform :: Mat
    transform = eyeMat 2 3 MatDepth_32F 1
@

<<doc/generated/warpAffineImg.png warpAffineImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#warpaffine OpenCV Sphinx doc>
-}
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
      handleCvException (pure dst) $
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
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              , $(int32_t c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshalBorderMode borderMode

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
      handleCvException (pure dst) $
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
              , $(int32_t c'interpolationMethod) | $(int32_t c'inverse) | $(int32_t c'fillOutliers)
              , $(int32_t c'borderMode)
              , *$(Scalar * borderValuePtr)
              );
          |]
  where
    c'interpolationMethod = marshalInterpolationMethod interpolationMethod
    c'inverse      = if inverse      then c'WARP_INVERSE_MAP   else 0
    c'fillOutliers = if fillOutliers then c'WARP_FILL_OUTLIERS else 0
    (c'borderMode, borderValue) = marshalBorderMode borderMode

-- | Inverts an affine transformation
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/geometric_transformations.html#invertaffinetransform OpenCV Sphinx doc>
invertAffineTransform :: Mat -> Either CvException Mat
invertAffineTransform matIn = unsafePerformIO $ do
    matOut <- newEmptyMat
    handleCvException (pure matOut) $
      withMatPtr matIn  $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
        [cvExcept|
           cv::invertAffineTransform(*$(Mat * matInPtr), *$(Mat * matOutPtr));
        |]
