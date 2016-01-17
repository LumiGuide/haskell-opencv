{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenCV.ImgProc.MiscImgTransform
    ( -- * Color conversion
      cvtColor
    , ColorConversion
    , ColorCode(..)

      -- ** Color code proxies
    , bayerBG
    , bayerGB
    , bayerGR
    , bayerRG
    , bgr
    , bgr555
    , bgr565
    , bgra
    , bgra_I420
    , bgra_IYUV
    , bgra_NV12
    , bgra_NV21
    , bgra_UYNV
    , bgra_UYVY
    , bgra_Y422
    , bgra_YUNV
    , bgra_YUY2
    , bgra_YUYV
    , bgra_YV12
    , bgra_YVYU
    , bgr_EA
    , bgr_FULL
    , bgr_I420
    , bgr_IYUV
    , bgr_NV12
    , bgr_NV21
    , bgr_UYNV
    , bgr_UYVY
    , bgr_VNG
    , bgr_Y422
    , bgr_YUNV
    , bgr_YUY2
    , bgr_YUYV
    , bgr_YV12
    , bgr_YVYU
    , gray
    , gray_420
    , gray_I420
    , gray_IYUV
    , gray_NV12
    , gray_NV21
    , gray_UYNV
    , gray_UYVY
    , gray_Y422
    , gray_YUNV
    , gray_YUY2
    , gray_YUYV
    , gray_YV12
    , gray_YVYU
    , hls
    , hls_FULL
    , hsv
    , hsv_FULL
    , lab
    , lbgr
    , lrgb
    , luv
    , mrgba
    , rgb
    , rgba
    , rgba_I420
    , rgba_IYUV
    , rgba_NV12
    , rgba_NV21
    , rgba_UYNV
    , rgba_UYVY
    , rgba_Y422
    , rgba_YUNV
    , rgba_YUY2
    , rgba_YUYV
    , rgba_YV12
    , rgba_YVYU
    , rgb_EA
    , rgb_FULL
    , rgb_I420
    , rgb_IYUV
    , rgb_NV12
    , rgb_NV21
    , rgb_UYNV
    , rgb_UYVY
    , rgb_VNG
    , rgb_Y422
    , rgb_YUNV
    , rgb_YUY2
    , rgb_YUYV
    , rgb_YV12
    , rgb_YVYU
    , xyz
    , yCrCb
    , yuv
    , yuv420p
    , yuv420sp
    , yuv_I420
    , yuv_IYUV
    , yuv_YV12

      -- * Thresholding
    , ThreshType(..)
    , ThreshValue(..)
    , threshold
    ) where

import "base" Foreign.C.Types
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" GHC.TypeLits
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude hiding ( shift )
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.ImgProc.MiscImgTransform.ColorCodes
import "this" OpenCV.Internal
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
    colorCode :: Proxy fromColor -> Proxy toColor -> Int32

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

{- | Converts an image from one color space to another

The function converts an input image from one color space to
another. In case of a transformation to-from RGB color space, the
order of the channels should be specified explicitly (RGB or
BGR). Note that the default color format in OpenCV is often
referred to as RGB but it is actually BGR (the bytes are
reversed). So the first byte in a standard (24-bit) color image
will be an 8-bit Blue component, the second byte will be Green, and
the third byte will be Red. The fourth, fifth, and sixth bytes
would then be the second pixel (Blue, then Green, then Red), and so
on.

The conventional ranges for R, G, and B channel values are:

  * 0 to 255 for CV_8U images

  * 0 to 65535 for CV_16U images

  * 0 to 1 for CV_32F images

In case of linear transformations, the range does not matter. But
in case of a non-linear transformation, an input RGB image should
be normalized to the proper value range to get the correct results,
for example, for RGB ￼ L*u*v* transformation. For example, if you
have a 32-bit floating-point image directly converted from an 8-bit
image without any scaling, then it will have the 0..255 value range
instead of 0..1 assumed by the function. So, before calling
'cvtColor', you need first to scale the image down:

>  cvtColor (img * 1/255) 'ColorConvBGR2Luv'

If you use 'cvtColor' with 8-bit images, the conversion will have
some information lost. For many applications, this will not be
noticeable but it is recommended to use 32-bit images in
applications that need the full range of colors or that convert an
image before an operation and then convert back.

If conversion adds the alpha channel, its value will set to the
maximum of corresponding channel range: 255 for CV_8U, 65535 for
CV_16U, 1 for CV_32F.

Example:

@
cvtColorImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width2 ~ ((*) width 2)   -- TODO (RvD): HSE parse error with infix type operator
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
cvtColorImg = createMat $ do
    imgM <- mkMatM ((Proxy :: Proxy height) ::: (Proxy :: Proxy width2) ::: Z)
                   (Proxy :: Proxy channels)
                   (Proxy :: Proxy depth)
                   white
    void $ matCopyToM imgM (V2 0 0) birds_512x341
    void $ matCopyToM imgM (V2 w 0) (unsafeCoerceMat birds_gray)
    arrowedLine imgM (V2 startX midY) (V2 pointX midY) red 4 LineType_8 0 0.15
    pure imgM
  where
    birds_gray = either throw id $ cvtColor gray bgr =<< cvtColor bgr gray birds_512x341

    h, w :: Int32
    h = fromInteger $ natVal (Proxy :: Proxy height)
    w = fromInteger $ natVal (Proxy :: Proxy width)

    startX, pointX :: Int32
    startX = round $ fromIntegral w * (0.95 :: Double)
    pointX = round $ fromIntegral w * (1.05 :: Double)
    midY = h \`div\` 2
@

<<doc/generated/examples/cvtColorImg.png cvtColorImg>>

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor OpenCV Sphinx Doc>
-}
cvtColor :: forall (fromColor :: ColorCode)
                   (toColor   :: ColorCode)
                   (shape     :: DS [DS Nat])
                   srcChannels srcDepth
         . (ColorConversion fromColor toColor)
         => Proxy fromColor -- ^ Convert from 'ColorCode'. Make sure the source image has this 'ColorCode'
         -> Proxy toColor   -- ^ Convert to 'ColorCode'.
         -> Mat shape srcChannels srcDepth -- ^ Source image
         -> Either CvException (Mat shape 'D 'D)
cvtColor fromColor toColor src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          cv::cvtColor( *$(Mat * srcPtr)
                      , *$(Mat * dstPtr)
                      , $(int32_t c'code)
                      );
        |]
  where
    c'code = colorCode fromColor toColor

data ThreshType
   = Thresh_Binary    !Double
   | Thresh_BinaryInv !Double
   | Thresh_Truncate
   | Thresh_ToZero
   | Thresh_ToZeroInv
     deriving (Show, Eq)

#num THRESH_BINARY
#num THRESH_BINARY_INV
#num THRESH_TRUNC
#num THRESH_TOZERO
#num THRESH_TOZERO_INV

marshalThreshType :: ThreshType -> (Int32, CDouble)
marshalThreshType = \case
    Thresh_Binary    maxVal -> (c'THRESH_BINARY    , realToFrac maxVal)
    Thresh_BinaryInv maxVal -> (c'THRESH_BINARY_INV, realToFrac maxVal)
    Thresh_Truncate         -> (c'THRESH_TRUNC     , 0)
    Thresh_ToZero           -> (c'THRESH_TOZERO    , 0)
    Thresh_ToZeroInv        -> (c'THRESH_TOZERO_INV, 0)

data ThreshValue
   = ThreshVal_Abs !Double
   | ThreshVal_Otsu
   | ThreshVal_Triangle
     deriving (Show, Eq)

#num THRESH_OTSU
#num THRESH_TRIANGLE

marshalThreshValue :: ThreshValue -> (Int32, CDouble)
marshalThreshValue = \case
    ThreshVal_Abs val  -> (0                , realToFrac val)
    ThreshVal_Otsu     -> (c'THRESH_OTSU    , 0)
    ThreshVal_Triangle -> (c'THRESH_TRIANGLE, 0)

-- TODO (RvD): Otsu and triangle are only implemented for 8 bit images.

{- | Applies a fixed-level threshold to each array element

The function applies fixed-level thresholding to a single-channel array. The
function is typically used to get a bi-level (binary) image out of a
grayscale image or for removing a noise, that is, filtering out pixels with
too small or too large values. There are several types of thresholding
supported by the function.

<http://docs.opencv.org/3.0-last-rst/modules/imgproc/doc/miscellaneous_transformations.html#threshold OpenCV Sphinx doc>
-}
threshold
    :: (depth `In` [Word8, Float])
    => ThreshValue -- ^
    -> ThreshType
    -> (Mat shape ('S 1) ('S depth))
    -> Either CvException (Mat shape ('S 1) ('S depth), Double)
threshold threshVal threshType src = unsafePerformIO $ do
    dst <- newEmptyMat
    alloca $ \calcThreshPtr ->
      handleCvException ((unsafeCoerceMat dst, ) . realToFrac <$> peek calcThreshPtr) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          *$(double * calcThreshPtr) =
            cv::threshold( *$(Mat * srcPtr)
                         , *$(Mat * dstPtr)
                         , $(double c'threshVal)
                         , $(double c'maxVal)
                         , $(int32_t c'type)
                         );
        |]
  where
    c'type = c'threshType .|. c'threshValMode
    (c'threshType, c'maxVal) = marshalThreshType threshType
    (c'threshValMode, c'threshVal) = marshalThreshValue threshVal
