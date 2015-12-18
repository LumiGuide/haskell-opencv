{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module OpenCV.ImgProc
    ( -- * Image Filtering
      medianBlur
      -- * Geometric Image Transformations
    , warpAffine
    , warpPerspective
    , invertAffineTransform
      -- * Miscellaneous Image Transformations
    , ColorCode(..)
    , ColorConversion
    , cvtColor
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
    c'medianBlur :: CInt -> IO (Either CvException Mat)
    c'medianBlur c'ksize = do
      matOut <- newEmptyMat
      handleCvException (pure matOut) $
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
    handleCvException (pure matOut) $
      withMatPtr matIn  $ \matInPtr ->
      withMatPtr matOut $ \matOutPtr ->
        [cvExcept|
           cv::invertAffineTransform(*$(Mat * matInPtr), *$(Mat * matOutPtr));
        |]


--------------------------------------------------------------------------------
-- Miscellaneous Image Transformations
--------------------------------------------------------------------------------

data ColorCode
    = BayerBG
    | BayerGB
    | BayerGR
    | BayerRG

    | BGR
    | BGR555
    | BGR565

    | BGRA
    | BGRA_I420
    | BGRA_IYUV
    | BGRA_NV12
    | BGRA_NV21
    | BGRA_UYNV
    | BGRA_UYVY
    | BGRA_Y422
    | BGRA_YUNV
    | BGRA_YUY2
    | BGRA_YUYV
    | BGRA_YV12
    | BGRA_YVYU

    | BGR_EA
    | BGR_FULL
    | BGR_I420
    | BGR_IYUV
    | BGR_NV12
    | BGR_NV21
    | BGR_UYNV
    | BGR_UYVY
    | BGR_VNG
    | BGR_Y422
    | BGR_YUNV
    | BGR_YUY2
    | BGR_YUYV
    | BGR_YV12
    | BGR_YVYU

    | GRAY
    | GRAY_420
    | GRAY_I420
    | GRAY_IYUV
    | GRAY_NV12
    | GRAY_NV21
    | GRAY_UYNV
    | GRAY_UYVY
    | GRAY_Y422
    | GRAY_YUNV
    | GRAY_YUY2
    | GRAY_YUYV
    | GRAY_YV12
    | GRAY_YVYU

    | HLS
    | HLS_FULL
    | HSV
    | HSV_FULL
    | Lab
    | LBGR
    | LRGB
    | Luv
    | MRGBA
    | RGB

    | RGBA
    | RGBA_I420
    | RGBA_IYUV
    | RGBA_NV12
    | RGBA_NV21
    | RGBA_UYNV
    | RGBA_UYVY
    | RGBA_Y422
    | RGBA_YUNV
    | RGBA_YUY2
    | RGBA_YUYV
    | RGBA_YV12
    | RGBA_YVYU

    | RGB_EA
    | RGB_FULL
    | RGB_I420
    | RGB_IYUV
    | RGB_NV12
    | RGB_NV21
    | RGB_UYNV
    | RGB_UYVY
    | RGB_VNG
    | RGB_Y422
    | RGB_YUNV
    | RGB_YUY2
    | RGB_YUYV
    | RGB_YV12
    | RGB_YVYU

    | XYZ
    | YCrCb

    | YUV
    | YUV420p
    | YUV420sp
    | YUV_I420
    | YUV_IYUV
    | YUV_YV12

#num COLOR_BGR2BGRA
#num COLOR_RGB2RGBA
#num COLOR_BGRA2BGR
#num COLOR_RGBA2RGB
#num COLOR_BGR2RGBA
#num COLOR_RGB2BGRA
#num COLOR_RGBA2BGR
#num COLOR_BGRA2RGB
#num COLOR_BGR2RGB
#num COLOR_RGB2BGR
#num COLOR_BGRA2RGBA
#num COLOR_RGBA2BGRA
#num COLOR_BGR2GRAY
#num COLOR_RGB2GRAY
#num COLOR_GRAY2BGR
#num COLOR_GRAY2RGB
#num COLOR_GRAY2BGRA
#num COLOR_GRAY2RGBA
#num COLOR_BGRA2GRAY
#num COLOR_RGBA2GRAY
#num COLOR_BGR2BGR565
#num COLOR_RGB2BGR565
#num COLOR_BGR5652BGR
#num COLOR_BGR5652RGB
#num COLOR_BGRA2BGR565
#num COLOR_RGBA2BGR565
#num COLOR_BGR5652BGRA
#num COLOR_BGR5652RGBA
#num COLOR_GRAY2BGR565
#num COLOR_BGR5652GRAY
#num COLOR_BGR2BGR555
#num COLOR_RGB2BGR555
#num COLOR_BGR5552BGR
#num COLOR_BGR5552RGB
#num COLOR_BGRA2BGR555
#num COLOR_RGBA2BGR555
#num COLOR_BGR5552BGRA
#num COLOR_BGR5552RGBA
#num COLOR_GRAY2BGR555
#num COLOR_BGR5552GRAY
#num COLOR_BGR2XYZ
#num COLOR_RGB2XYZ
#num COLOR_XYZ2BGR
#num COLOR_XYZ2RGB
#num COLOR_BGR2YCrCb
#num COLOR_RGB2YCrCb
#num COLOR_YCrCb2BGR
#num COLOR_YCrCb2RGB
#num COLOR_BGR2HSV
#num COLOR_RGB2HSV
#num COLOR_BGR2Lab
#num COLOR_RGB2Lab
#num COLOR_BGR2Luv
#num COLOR_RGB2Luv
#num COLOR_BGR2HLS
#num COLOR_RGB2HLS
#num COLOR_HSV2BGR
#num COLOR_HSV2RGB
#num COLOR_Lab2BGR
#num COLOR_Lab2RGB
#num COLOR_Luv2BGR
#num COLOR_Luv2RGB
#num COLOR_HLS2BGR
#num COLOR_HLS2RGB
#num COLOR_BGR2HSV_FULL
#num COLOR_RGB2HSV_FULL
#num COLOR_BGR2HLS_FULL
#num COLOR_RGB2HLS_FULL
#num COLOR_HSV2BGR_FULL
#num COLOR_HSV2RGB_FULL
#num COLOR_HLS2BGR_FULL
#num COLOR_HLS2RGB_FULL
#num COLOR_LBGR2Lab
#num COLOR_LRGB2Lab
#num COLOR_LBGR2Luv
#num COLOR_LRGB2Luv
#num COLOR_Lab2LBGR
#num COLOR_Lab2LRGB
#num COLOR_Luv2LBGR
#num COLOR_Luv2LRGB
#num COLOR_BGR2YUV
#num COLOR_RGB2YUV
#num COLOR_YUV2BGR
#num COLOR_YUV2RGB
#num COLOR_YUV2RGB_NV12
#num COLOR_YUV2BGR_NV12
#num COLOR_YUV2RGB_NV21
#num COLOR_YUV2BGR_NV21
#num COLOR_YUV420sp2RGB
#num COLOR_YUV420sp2BGR
#num COLOR_YUV2RGBA_NV12
#num COLOR_YUV2BGRA_NV12
#num COLOR_YUV2RGBA_NV21
#num COLOR_YUV2BGRA_NV21
#num COLOR_YUV420sp2RGBA
#num COLOR_YUV420sp2BGRA
#num COLOR_YUV2RGB_YV12
#num COLOR_YUV2BGR_YV12
#num COLOR_YUV2RGB_IYUV
#num COLOR_YUV2BGR_IYUV
#num COLOR_YUV2RGB_I420
#num COLOR_YUV2BGR_I420
#num COLOR_YUV420p2RGB
#num COLOR_YUV420p2BGR
#num COLOR_YUV2RGBA_YV12
#num COLOR_YUV2BGRA_YV12
#num COLOR_YUV2RGBA_IYUV
#num COLOR_YUV2BGRA_IYUV
#num COLOR_YUV2RGBA_I420
#num COLOR_YUV2BGRA_I420
#num COLOR_YUV420p2RGBA
#num COLOR_YUV420p2BGRA
#num COLOR_YUV2GRAY_420
#num COLOR_YUV2GRAY_NV21
#num COLOR_YUV2GRAY_NV12
#num COLOR_YUV2GRAY_YV12
#num COLOR_YUV2GRAY_IYUV
#num COLOR_YUV2GRAY_I420
#num COLOR_YUV420sp2GRAY
#num COLOR_YUV420p2GRAY
#num COLOR_YUV2RGB_UYVY
#num COLOR_YUV2BGR_UYVY
-- #num COLOR_YUV2RGB_VYUY
-- #num COLOR_YUV2BGR_VYUY
#num COLOR_YUV2RGB_Y422
#num COLOR_YUV2BGR_Y422
#num COLOR_YUV2RGB_UYNV
#num COLOR_YUV2BGR_UYNV
#num COLOR_YUV2RGBA_UYVY
#num COLOR_YUV2BGRA_UYVY
-- #num COLOR_YUV2RGBA_VYUY
-- #num COLOR_YUV2BGRA_VYUY
#num COLOR_YUV2RGBA_Y422
#num COLOR_YUV2BGRA_Y422
#num COLOR_YUV2RGBA_UYNV
#num COLOR_YUV2BGRA_UYNV
#num COLOR_YUV2RGB_YUY2
#num COLOR_YUV2BGR_YUY2
#num COLOR_YUV2RGB_YVYU
#num COLOR_YUV2BGR_YVYU
#num COLOR_YUV2RGB_YUYV
#num COLOR_YUV2BGR_YUYV
#num COLOR_YUV2RGB_YUNV
#num COLOR_YUV2BGR_YUNV
#num COLOR_YUV2RGBA_YUY2
#num COLOR_YUV2BGRA_YUY2
#num COLOR_YUV2RGBA_YVYU
#num COLOR_YUV2BGRA_YVYU
#num COLOR_YUV2RGBA_YUYV
#num COLOR_YUV2BGRA_YUYV
#num COLOR_YUV2RGBA_YUNV
#num COLOR_YUV2BGRA_YUNV
#num COLOR_YUV2GRAY_UYVY
#num COLOR_YUV2GRAY_YUY2
-- #num CV_YUV2GRAY_VYUY
#num COLOR_YUV2GRAY_Y422
#num COLOR_YUV2GRAY_UYNV
#num COLOR_YUV2GRAY_YVYU
#num COLOR_YUV2GRAY_YUYV
#num COLOR_YUV2GRAY_YUNV
#num COLOR_RGBA2mRGBA
#num COLOR_mRGBA2RGBA
#num COLOR_RGB2YUV_I420
#num COLOR_BGR2YUV_I420
#num COLOR_RGB2YUV_IYUV
#num COLOR_BGR2YUV_IYUV
#num COLOR_RGBA2YUV_I420
#num COLOR_BGRA2YUV_I420
#num COLOR_RGBA2YUV_IYUV
#num COLOR_BGRA2YUV_IYUV
#num COLOR_RGB2YUV_YV12
#num COLOR_BGR2YUV_YV12
#num COLOR_RGBA2YUV_YV12
#num COLOR_BGRA2YUV_YV12
#num COLOR_BayerBG2BGR
#num COLOR_BayerGB2BGR
#num COLOR_BayerRG2BGR
#num COLOR_BayerGR2BGR
#num COLOR_BayerBG2RGB
#num COLOR_BayerGB2RGB
#num COLOR_BayerRG2RGB
#num COLOR_BayerGR2RGB
#num COLOR_BayerBG2GRAY
#num COLOR_BayerGB2GRAY
#num COLOR_BayerRG2GRAY
#num COLOR_BayerGR2GRAY
#num COLOR_BayerBG2BGR_VNG
#num COLOR_BayerGB2BGR_VNG
#num COLOR_BayerRG2BGR_VNG
#num COLOR_BayerGR2BGR_VNG
#num COLOR_BayerBG2RGB_VNG
#num COLOR_BayerGB2RGB_VNG
#num COLOR_BayerRG2RGB_VNG
#num COLOR_BayerGR2RGB_VNG
#num COLOR_BayerBG2BGR_EA
#num COLOR_BayerGB2BGR_EA
#num COLOR_BayerRG2BGR_EA
#num COLOR_BayerGR2BGR_EA
#num COLOR_BayerBG2RGB_EA
#num COLOR_BayerGB2RGB_EA
#num COLOR_BayerRG2RGB_EA
#num COLOR_BayerGR2RGB_EA

class ColorConversion (fromColor :: ColorCode) (toColor :: ColorCode) where
    colorCode :: Proxy fromColor -> Proxy toColor -> C.CInt

instance ColorConversion 'BGR      'BGRA      where colorCode _ _ = c'COLOR_BGR2BGRA
instance ColorConversion 'RGB      'RGBA      where colorCode _ _ = c'COLOR_RGB2RGBA
instance ColorConversion 'BGRA     'BGR       where colorCode _ _ = c'COLOR_BGRA2BGR
instance ColorConversion 'RGBA     'RGB       where colorCode _ _ = c'COLOR_RGBA2RGB
instance ColorConversion 'BGR      'RGBA      where colorCode _ _ = c'COLOR_BGR2RGBA
instance ColorConversion 'RGB      'BGRA      where colorCode _ _ = c'COLOR_RGB2BGRA
instance ColorConversion 'RGBA     'BGR       where colorCode _ _ = c'COLOR_RGBA2BGR
instance ColorConversion 'BGRA     'RGB       where colorCode _ _ = c'COLOR_BGRA2RGB
instance ColorConversion 'BGR      'RGB       where colorCode _ _ = c'COLOR_BGR2RGB
instance ColorConversion 'RGB      'BGR       where colorCode _ _ = c'COLOR_RGB2BGR
instance ColorConversion 'BGRA     'RGBA      where colorCode _ _ = c'COLOR_BGRA2RGBA
instance ColorConversion 'RGBA     'BGRA      where colorCode _ _ = c'COLOR_RGBA2BGRA
instance ColorConversion 'BGR      'GRAY      where colorCode _ _ = c'COLOR_BGR2GRAY
instance ColorConversion 'RGB      'GRAY      where colorCode _ _ = c'COLOR_RGB2GRAY
instance ColorConversion 'GRAY     'BGR       where colorCode _ _ = c'COLOR_GRAY2BGR
instance ColorConversion 'GRAY     'RGB       where colorCode _ _ = c'COLOR_GRAY2RGB
instance ColorConversion 'GRAY     'BGRA      where colorCode _ _ = c'COLOR_GRAY2BGRA
instance ColorConversion 'GRAY     'RGBA      where colorCode _ _ = c'COLOR_GRAY2RGBA
instance ColorConversion 'BGRA     'GRAY      where colorCode _ _ = c'COLOR_BGRA2GRAY
instance ColorConversion 'RGBA     'GRAY      where colorCode _ _ = c'COLOR_RGBA2GRAY
instance ColorConversion 'BGR      'BGR565    where colorCode _ _ = c'COLOR_BGR2BGR565
instance ColorConversion 'RGB      'BGR565    where colorCode _ _ = c'COLOR_RGB2BGR565
instance ColorConversion 'BGR565   'BGR       where colorCode _ _ = c'COLOR_BGR5652BGR
instance ColorConversion 'BGR565   'RGB       where colorCode _ _ = c'COLOR_BGR5652RGB
instance ColorConversion 'BGRA     'BGR565    where colorCode _ _ = c'COLOR_BGRA2BGR565
instance ColorConversion 'RGBA     'BGR565    where colorCode _ _ = c'COLOR_RGBA2BGR565
instance ColorConversion 'BGR565   'BGRA      where colorCode _ _ = c'COLOR_BGR5652BGRA
instance ColorConversion 'BGR565   'RGBA      where colorCode _ _ = c'COLOR_BGR5652RGBA
instance ColorConversion 'GRAY     'BGR565    where colorCode _ _ = c'COLOR_GRAY2BGR565
instance ColorConversion 'BGR565   'GRAY      where colorCode _ _ = c'COLOR_BGR5652GRAY
instance ColorConversion 'BGR      'BGR555    where colorCode _ _ = c'COLOR_BGR2BGR555
instance ColorConversion 'RGB      'BGR555    where colorCode _ _ = c'COLOR_RGB2BGR555
instance ColorConversion 'BGR555   'BGR       where colorCode _ _ = c'COLOR_BGR5552BGR
instance ColorConversion 'BGR555   'RGB       where colorCode _ _ = c'COLOR_BGR5552RGB
instance ColorConversion 'BGRA     'BGR555    where colorCode _ _ = c'COLOR_BGRA2BGR555
instance ColorConversion 'RGBA     'BGR555    where colorCode _ _ = c'COLOR_RGBA2BGR555
instance ColorConversion 'BGR555   'BGRA      where colorCode _ _ = c'COLOR_BGR5552BGRA
instance ColorConversion 'BGR555   'RGBA      where colorCode _ _ = c'COLOR_BGR5552RGBA
instance ColorConversion 'GRAY     'BGR555    where colorCode _ _ = c'COLOR_GRAY2BGR555
instance ColorConversion 'BGR555   'GRAY      where colorCode _ _ = c'COLOR_BGR5552GRAY
instance ColorConversion 'BGR      'XYZ       where colorCode _ _ = c'COLOR_BGR2XYZ
instance ColorConversion 'RGB      'XYZ       where colorCode _ _ = c'COLOR_RGB2XYZ
instance ColorConversion 'XYZ      'BGR       where colorCode _ _ = c'COLOR_XYZ2BGR
instance ColorConversion 'XYZ      'RGB       where colorCode _ _ = c'COLOR_XYZ2RGB
instance ColorConversion 'BGR      'YCrCb     where colorCode _ _ = c'COLOR_BGR2YCrCb
instance ColorConversion 'RGB      'YCrCb     where colorCode _ _ = c'COLOR_RGB2YCrCb
instance ColorConversion 'YCrCb    'BGR       where colorCode _ _ = c'COLOR_YCrCb2BGR
instance ColorConversion 'YCrCb    'RGB       where colorCode _ _ = c'COLOR_YCrCb2RGB
instance ColorConversion 'BGR      'HSV       where colorCode _ _ = c'COLOR_BGR2HSV
instance ColorConversion 'RGB      'HSV       where colorCode _ _ = c'COLOR_RGB2HSV
instance ColorConversion 'BGR      'Lab       where colorCode _ _ = c'COLOR_BGR2Lab
instance ColorConversion 'RGB      'Lab       where colorCode _ _ = c'COLOR_RGB2Lab
instance ColorConversion 'BGR      'Luv       where colorCode _ _ = c'COLOR_BGR2Luv
instance ColorConversion 'RGB      'Luv       where colorCode _ _ = c'COLOR_RGB2Luv
instance ColorConversion 'BGR      'HLS       where colorCode _ _ = c'COLOR_BGR2HLS
instance ColorConversion 'RGB      'HLS       where colorCode _ _ = c'COLOR_RGB2HLS
instance ColorConversion 'HSV      'BGR       where colorCode _ _ = c'COLOR_HSV2BGR
instance ColorConversion 'HSV      'RGB       where colorCode _ _ = c'COLOR_HSV2RGB
instance ColorConversion 'Lab      'BGR       where colorCode _ _ = c'COLOR_Lab2BGR
instance ColorConversion 'Lab      'RGB       where colorCode _ _ = c'COLOR_Lab2RGB
instance ColorConversion 'Luv      'BGR       where colorCode _ _ = c'COLOR_Luv2BGR
instance ColorConversion 'Luv      'RGB       where colorCode _ _ = c'COLOR_Luv2RGB
instance ColorConversion 'HLS      'BGR       where colorCode _ _ = c'COLOR_HLS2BGR
instance ColorConversion 'HLS      'RGB       where colorCode _ _ = c'COLOR_HLS2RGB
instance ColorConversion 'BGR      'HSV_FULL  where colorCode _ _ = c'COLOR_BGR2HSV_FULL
instance ColorConversion 'RGB      'HSV_FULL  where colorCode _ _ = c'COLOR_RGB2HSV_FULL
instance ColorConversion 'BGR      'HLS_FULL  where colorCode _ _ = c'COLOR_BGR2HLS_FULL
instance ColorConversion 'RGB      'HLS_FULL  where colorCode _ _ = c'COLOR_RGB2HLS_FULL
instance ColorConversion 'HSV      'BGR_FULL  where colorCode _ _ = c'COLOR_HSV2BGR_FULL
instance ColorConversion 'HSV      'RGB_FULL  where colorCode _ _ = c'COLOR_HSV2RGB_FULL
instance ColorConversion 'HLS      'BGR_FULL  where colorCode _ _ = c'COLOR_HLS2BGR_FULL
instance ColorConversion 'HLS      'RGB_FULL  where colorCode _ _ = c'COLOR_HLS2RGB_FULL
instance ColorConversion 'LBGR     'Lab       where colorCode _ _ = c'COLOR_LBGR2Lab
instance ColorConversion 'LRGB     'Lab       where colorCode _ _ = c'COLOR_LRGB2Lab
instance ColorConversion 'LBGR     'Luv       where colorCode _ _ = c'COLOR_LBGR2Luv
instance ColorConversion 'LRGB     'Luv       where colorCode _ _ = c'COLOR_LRGB2Luv
instance ColorConversion 'Lab      'LBGR      where colorCode _ _ = c'COLOR_Lab2LBGR
instance ColorConversion 'Lab      'LRGB      where colorCode _ _ = c'COLOR_Lab2LRGB
instance ColorConversion 'Luv      'LBGR      where colorCode _ _ = c'COLOR_Luv2LBGR
instance ColorConversion 'Luv      'LRGB      where colorCode _ _ = c'COLOR_Luv2LRGB
instance ColorConversion 'BGR      'YUV       where colorCode _ _ = c'COLOR_BGR2YUV
instance ColorConversion 'RGB      'YUV       where colorCode _ _ = c'COLOR_RGB2YUV
instance ColorConversion 'YUV      'BGR       where colorCode _ _ = c'COLOR_YUV2BGR
instance ColorConversion 'YUV      'RGB       where colorCode _ _ = c'COLOR_YUV2RGB
instance ColorConversion 'YUV      'RGB_NV12  where colorCode _ _ = c'COLOR_YUV2RGB_NV12
instance ColorConversion 'YUV      'BGR_NV12  where colorCode _ _ = c'COLOR_YUV2BGR_NV12
instance ColorConversion 'YUV      'RGB_NV21  where colorCode _ _ = c'COLOR_YUV2RGB_NV21
instance ColorConversion 'YUV      'BGR_NV21  where colorCode _ _ = c'COLOR_YUV2BGR_NV21
instance ColorConversion 'YUV420sp 'RGB       where colorCode _ _ = c'COLOR_YUV420sp2RGB
instance ColorConversion 'YUV420sp 'BGR       where colorCode _ _ = c'COLOR_YUV420sp2BGR
instance ColorConversion 'YUV      'RGBA_NV12 where colorCode _ _ = c'COLOR_YUV2RGBA_NV12
instance ColorConversion 'YUV      'BGRA_NV12 where colorCode _ _ = c'COLOR_YUV2BGRA_NV12
instance ColorConversion 'YUV      'RGBA_NV21 where colorCode _ _ = c'COLOR_YUV2RGBA_NV21
instance ColorConversion 'YUV      'BGRA_NV21 where colorCode _ _ = c'COLOR_YUV2BGRA_NV21
instance ColorConversion 'YUV420sp 'RGBA      where colorCode _ _ = c'COLOR_YUV420sp2RGBA
instance ColorConversion 'YUV420sp 'BGRA      where colorCode _ _ = c'COLOR_YUV420sp2BGRA
instance ColorConversion 'YUV      'RGB_YV12  where colorCode _ _ = c'COLOR_YUV2RGB_YV12
instance ColorConversion 'YUV      'BGR_YV12  where colorCode _ _ = c'COLOR_YUV2BGR_YV12
instance ColorConversion 'YUV      'RGB_IYUV  where colorCode _ _ = c'COLOR_YUV2RGB_IYUV
instance ColorConversion 'YUV      'BGR_IYUV  where colorCode _ _ = c'COLOR_YUV2BGR_IYUV
instance ColorConversion 'YUV      'RGB_I420  where colorCode _ _ = c'COLOR_YUV2RGB_I420
instance ColorConversion 'YUV      'BGR_I420  where colorCode _ _ = c'COLOR_YUV2BGR_I420
instance ColorConversion 'YUV420p  'RGB       where colorCode _ _ = c'COLOR_YUV420p2RGB
instance ColorConversion 'YUV420p  'BGR       where colorCode _ _ = c'COLOR_YUV420p2BGR
instance ColorConversion 'YUV      'RGBA_YV12 where colorCode _ _ = c'COLOR_YUV2RGBA_YV12
instance ColorConversion 'YUV      'BGRA_YV12 where colorCode _ _ = c'COLOR_YUV2BGRA_YV12
instance ColorConversion 'YUV      'RGBA_IYUV where colorCode _ _ = c'COLOR_YUV2RGBA_IYUV
instance ColorConversion 'YUV      'BGRA_IYUV where colorCode _ _ = c'COLOR_YUV2BGRA_IYUV
instance ColorConversion 'YUV      'RGBA_I420 where colorCode _ _ = c'COLOR_YUV2RGBA_I420
instance ColorConversion 'YUV      'BGRA_I420 where colorCode _ _ = c'COLOR_YUV2BGRA_I420
instance ColorConversion 'YUV420p  'RGBA      where colorCode _ _ = c'COLOR_YUV420p2RGBA
instance ColorConversion 'YUV420p  'BGRA      where colorCode _ _ = c'COLOR_YUV420p2BGRA
instance ColorConversion 'YUV      'GRAY_420  where colorCode _ _ = c'COLOR_YUV2GRAY_420
instance ColorConversion 'YUV      'GRAY_NV21 where colorCode _ _ = c'COLOR_YUV2GRAY_NV21
instance ColorConversion 'YUV      'GRAY_NV12 where colorCode _ _ = c'COLOR_YUV2GRAY_NV12
instance ColorConversion 'YUV      'GRAY_YV12 where colorCode _ _ = c'COLOR_YUV2GRAY_YV12
instance ColorConversion 'YUV      'GRAY_IYUV where colorCode _ _ = c'COLOR_YUV2GRAY_IYUV
instance ColorConversion 'YUV      'GRAY_I420 where colorCode _ _ = c'COLOR_YUV2GRAY_I420
instance ColorConversion 'YUV420sp 'GRAY      where colorCode _ _ = c'COLOR_YUV420sp2GRAY
instance ColorConversion 'YUV420p  'GRAY      where colorCode _ _ = c'COLOR_YUV420p2GRAY
instance ColorConversion 'YUV      'RGB_UYVY  where colorCode _ _ = c'COLOR_YUV2RGB_UYVY
instance ColorConversion 'YUV      'BGR_UYVY  where colorCode _ _ = c'COLOR_YUV2BGR_UYVY
instance ColorConversion 'YUV      'RGB_Y422  where colorCode _ _ = c'COLOR_YUV2RGB_Y422
instance ColorConversion 'YUV      'BGR_Y422  where colorCode _ _ = c'COLOR_YUV2BGR_Y422
instance ColorConversion 'YUV      'RGB_UYNV  where colorCode _ _ = c'COLOR_YUV2RGB_UYNV
instance ColorConversion 'YUV      'BGR_UYNV  where colorCode _ _ = c'COLOR_YUV2BGR_UYNV
instance ColorConversion 'YUV      'RGBA_UYVY where colorCode _ _ = c'COLOR_YUV2RGBA_UYVY
instance ColorConversion 'YUV      'BGRA_UYVY where colorCode _ _ = c'COLOR_YUV2BGRA_UYVY
instance ColorConversion 'YUV      'RGBA_Y422 where colorCode _ _ = c'COLOR_YUV2RGBA_Y422
instance ColorConversion 'YUV      'BGRA_Y422 where colorCode _ _ = c'COLOR_YUV2BGRA_Y422
instance ColorConversion 'YUV      'RGBA_UYNV where colorCode _ _ = c'COLOR_YUV2RGBA_UYNV
instance ColorConversion 'YUV      'BGRA_UYNV where colorCode _ _ = c'COLOR_YUV2BGRA_UYNV
instance ColorConversion 'YUV      'RGB_YUY2  where colorCode _ _ = c'COLOR_YUV2RGB_YUY2
instance ColorConversion 'YUV      'BGR_YUY2  where colorCode _ _ = c'COLOR_YUV2BGR_YUY2
instance ColorConversion 'YUV      'RGB_YVYU  where colorCode _ _ = c'COLOR_YUV2RGB_YVYU
instance ColorConversion 'YUV      'BGR_YVYU  where colorCode _ _ = c'COLOR_YUV2BGR_YVYU
instance ColorConversion 'YUV      'RGB_YUYV  where colorCode _ _ = c'COLOR_YUV2RGB_YUYV
instance ColorConversion 'YUV      'BGR_YUYV  where colorCode _ _ = c'COLOR_YUV2BGR_YUYV
instance ColorConversion 'YUV      'RGB_YUNV  where colorCode _ _ = c'COLOR_YUV2RGB_YUNV
instance ColorConversion 'YUV      'BGR_YUNV  where colorCode _ _ = c'COLOR_YUV2BGR_YUNV
instance ColorConversion 'YUV      'RGBA_YUY2 where colorCode _ _ = c'COLOR_YUV2RGBA_YUY2
instance ColorConversion 'YUV      'BGRA_YUY2 where colorCode _ _ = c'COLOR_YUV2BGRA_YUY2
instance ColorConversion 'YUV      'RGBA_YVYU where colorCode _ _ = c'COLOR_YUV2RGBA_YVYU
instance ColorConversion 'YUV      'BGRA_YVYU where colorCode _ _ = c'COLOR_YUV2BGRA_YVYU
instance ColorConversion 'YUV      'RGBA_YUYV where colorCode _ _ = c'COLOR_YUV2RGBA_YUYV
instance ColorConversion 'YUV      'BGRA_YUYV where colorCode _ _ = c'COLOR_YUV2BGRA_YUYV
instance ColorConversion 'YUV      'RGBA_YUNV where colorCode _ _ = c'COLOR_YUV2RGBA_YUNV
instance ColorConversion 'YUV      'BGRA_YUNV where colorCode _ _ = c'COLOR_YUV2BGRA_YUNV
instance ColorConversion 'YUV      'GRAY_UYVY where colorCode _ _ = c'COLOR_YUV2GRAY_UYVY
instance ColorConversion 'YUV      'GRAY_YUY2 where colorCode _ _ = c'COLOR_YUV2GRAY_YUY2
instance ColorConversion 'YUV      'GRAY_Y422 where colorCode _ _ = c'COLOR_YUV2GRAY_Y422
instance ColorConversion 'YUV      'GRAY_UYNV where colorCode _ _ = c'COLOR_YUV2GRAY_UYNV
instance ColorConversion 'YUV      'GRAY_YVYU where colorCode _ _ = c'COLOR_YUV2GRAY_YVYU
instance ColorConversion 'YUV      'GRAY_YUYV where colorCode _ _ = c'COLOR_YUV2GRAY_YUYV
instance ColorConversion 'YUV      'GRAY_YUNV where colorCode _ _ = c'COLOR_YUV2GRAY_YUNV
instance ColorConversion 'RGBA     'MRGBA     where colorCode _ _ = c'COLOR_RGBA2mRGBA
instance ColorConversion 'MRGBA    'RGBA      where colorCode _ _ = c'COLOR_mRGBA2RGBA
instance ColorConversion 'RGB      'YUV_I420  where colorCode _ _ = c'COLOR_RGB2YUV_I420
instance ColorConversion 'BGR      'YUV_I420  where colorCode _ _ = c'COLOR_BGR2YUV_I420
instance ColorConversion 'RGB      'YUV_IYUV  where colorCode _ _ = c'COLOR_RGB2YUV_IYUV
instance ColorConversion 'BGR      'YUV_IYUV  where colorCode _ _ = c'COLOR_BGR2YUV_IYUV
instance ColorConversion 'RGBA     'YUV_I420  where colorCode _ _ = c'COLOR_RGBA2YUV_I420
instance ColorConversion 'BGRA     'YUV_I420  where colorCode _ _ = c'COLOR_BGRA2YUV_I420
instance ColorConversion 'RGBA     'YUV_IYUV  where colorCode _ _ = c'COLOR_RGBA2YUV_IYUV
instance ColorConversion 'BGRA     'YUV_IYUV  where colorCode _ _ = c'COLOR_BGRA2YUV_IYUV
instance ColorConversion 'RGB      'YUV_YV12  where colorCode _ _ = c'COLOR_RGB2YUV_YV12
instance ColorConversion 'BGR      'YUV_YV12  where colorCode _ _ = c'COLOR_BGR2YUV_YV12
instance ColorConversion 'RGBA     'YUV_YV12  where colorCode _ _ = c'COLOR_RGBA2YUV_YV12
instance ColorConversion 'BGRA     'YUV_YV12  where colorCode _ _ = c'COLOR_BGRA2YUV_YV12
instance ColorConversion 'BayerBG  'BGR       where colorCode _ _ = c'COLOR_BayerBG2BGR
instance ColorConversion 'BayerGB  'BGR       where colorCode _ _ = c'COLOR_BayerGB2BGR
instance ColorConversion 'BayerRG  'BGR       where colorCode _ _ = c'COLOR_BayerRG2BGR
instance ColorConversion 'BayerGR  'BGR       where colorCode _ _ = c'COLOR_BayerGR2BGR
instance ColorConversion 'BayerBG  'RGB       where colorCode _ _ = c'COLOR_BayerBG2RGB
instance ColorConversion 'BayerGB  'RGB       where colorCode _ _ = c'COLOR_BayerGB2RGB
instance ColorConversion 'BayerRG  'RGB       where colorCode _ _ = c'COLOR_BayerRG2RGB
instance ColorConversion 'BayerGR  'RGB       where colorCode _ _ = c'COLOR_BayerGR2RGB
instance ColorConversion 'BayerBG  'GRAY      where colorCode _ _ = c'COLOR_BayerBG2GRAY
instance ColorConversion 'BayerGB  'GRAY      where colorCode _ _ = c'COLOR_BayerGB2GRAY
instance ColorConversion 'BayerRG  'GRAY      where colorCode _ _ = c'COLOR_BayerRG2GRAY
instance ColorConversion 'BayerGR  'GRAY      where colorCode _ _ = c'COLOR_BayerGR2GRAY
instance ColorConversion 'BayerBG  'BGR_VNG   where colorCode _ _ = c'COLOR_BayerBG2BGR_VNG
instance ColorConversion 'BayerGB  'BGR_VNG   where colorCode _ _ = c'COLOR_BayerGB2BGR_VNG
instance ColorConversion 'BayerRG  'BGR_VNG   where colorCode _ _ = c'COLOR_BayerRG2BGR_VNG
instance ColorConversion 'BayerGR  'BGR_VNG   where colorCode _ _ = c'COLOR_BayerGR2BGR_VNG
instance ColorConversion 'BayerBG  'RGB_VNG   where colorCode _ _ = c'COLOR_BayerBG2RGB_VNG
instance ColorConversion 'BayerGB  'RGB_VNG   where colorCode _ _ = c'COLOR_BayerGB2RGB_VNG
instance ColorConversion 'BayerRG  'RGB_VNG   where colorCode _ _ = c'COLOR_BayerRG2RGB_VNG
instance ColorConversion 'BayerGR  'RGB_VNG   where colorCode _ _ = c'COLOR_BayerGR2RGB_VNG
instance ColorConversion 'BayerBG  'BGR_EA    where colorCode _ _ = c'COLOR_BayerBG2BGR_EA
instance ColorConversion 'BayerGB  'BGR_EA    where colorCode _ _ = c'COLOR_BayerGB2BGR_EA
instance ColorConversion 'BayerRG  'BGR_EA    where colorCode _ _ = c'COLOR_BayerRG2BGR_EA
instance ColorConversion 'BayerGR  'BGR_EA    where colorCode _ _ = c'COLOR_BayerGR2BGR_EA
instance ColorConversion 'BayerBG  'RGB_EA    where colorCode _ _ = c'COLOR_BayerBG2RGB_EA
instance ColorConversion 'BayerGB  'RGB_EA    where colorCode _ _ = c'COLOR_BayerGB2RGB_EA
instance ColorConversion 'BayerRG  'RGB_EA    where colorCode _ _ = c'COLOR_BayerRG2RGB_EA
instance ColorConversion 'BayerGR  'RGB_EA    where colorCode _ _ = c'COLOR_BayerGR2RGB_EA

-- | Converts an image from one color space to another
--
-- The function converts an input image from one color space to
-- another. In case of a transformation to-from RGB color space, the
-- order of the channels should be specified explicitly (RGB or
-- BGR). Note that the default color format in OpenCV is often
-- referred to as RGB but it is actually BGR (the bytes are
-- reversed). So the first byte in a standard (24-bit) color image
-- will be an 8-bit Blue component, the second byte will be Green, and
-- the third byte will be Red. The fourth, fifth, and sixth bytes
-- would then be the second pixel (Blue, then Green, then Red), and so
-- on.
--
-- The conventional ranges for R, G, and B channel values are:
--
--   * 0 to 255 for CV_8U images
--
--   * 0 to 65535 for CV_16U images
--
--   * 0 to 1 for CV_32F images
--
-- In case of linear transformations, the range does not matter. But
-- in case of a non-linear transformation, an input RGB image should
-- be normalized to the proper value range to get the correct results,
-- for example, for RGB ￼ L*u*v* transformation. For example, if you
-- have a 32-bit floating-point image directly converted from an 8-bit
-- image without any scaling, then it will have the 0..255 value range
-- instead of 0..1 assumed by the function. So, before calling
-- 'cvtColor', you need first to scale the image down:
--
-- >  cvtColor (img * 1/255) 'ColorConvBGR2Luv'
--
-- If you use 'cvtColor' with 8-bit images, the conversion will have
-- some information lost. For many applications, this will not be
-- noticeable but it is recommended to use 32-bit images in
-- applications that need the full range of colors or that convert an
-- image before an operation and then convert back.
--
-- If conversion adds the alpha channel, its value will set to the
-- maximum of corresponding channel range: 255 for CV_8U, 65535 for
-- CV_16U, 1 for CV_32F.
--
-- <http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor OpenCV Sphinx Doc>
cvtColor :: forall (fromColor :: ColorCode) (toColor :: ColorCode)
         . (ColorConversion fromColor toColor)
         => Proxy fromColor
         -> Proxy toColor
         -> Mat
         -> Either CvException Mat
cvtColor fromColor toColor src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          cv::cvtColor( *$(Mat * srcPtr)
                      , *$(Mat * dstPtr)
                      , $(int c'code)
                      );
        |]
  where
    c'code = colorCode fromColor toColor


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

marshallInterpolationMethod :: InterpolationMethod -> CInt
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

marshallBorderMode :: BorderMode -> (CInt, Scalar)
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
