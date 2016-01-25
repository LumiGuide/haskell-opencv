{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.ImgProc.MiscImgTransform
    ( -- * Color conversion
      cvtColor
    , ColorConversion
    , ColorCode(..)
    , ColorCodeChannels
    , ColorCodeDepth
    , ColorCodeMatchesChannels

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

import "base" Data.Bits
import "base" Data.Int
import "base" Data.Proxy ( Proxy(..) )
import "base" Data.Word
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Storable ( peek )
import "base" GHC.TypeLits
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" Language.C.Inline.OpenCV ( openCvCtx )
import "this" OpenCV.Exception
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.ImgProc.MiscImgTransform.Internal
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/imgproc.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

-- | Names of color encodings
data ColorCode
    = BayerBG   -- ^ ('bayerBG') Bayer pattern with BG in the second row, second and third column
    | BayerGB   -- ^ ('bayerGB') Bayer pattern with GB in the second row, second and third column
    | BayerGR   -- ^ ('bayerGR') Bayer pattern with GR in the second row, second and third column
    | BayerRG   -- ^ ('bayerRG') Bayer pattern with RG in the second row, second and third column

    | BGR       -- ^ ('bgr') 24 bit RGB color space with channels: (B8:G8:R8)
    | BGR555    -- ^ ('bgr555') 15 bit RGB color space
    | BGR565    -- ^ ('bgr565') 16 bit RGB color space

    | BGRA      -- ^ ('bgra') 32 bit RGBA color space with channels: (B8:G8:R8:A8)
    | BGRA_I420 -- ^ ('bgra_I420')
    | BGRA_IYUV -- ^ ('bgra_IYUV')
    | BGRA_NV12 -- ^ ('bgra_NV12')
    | BGRA_NV21 -- ^ ('bgra_NV21')
    | BGRA_UYNV -- ^ ('bgra_UYNV')
    | BGRA_UYVY -- ^ ('bgra_UYVY')
    | BGRA_Y422 -- ^ ('bgra_Y422')
    | BGRA_YUNV -- ^ ('bgra_YUNV')
    | BGRA_YUY2 -- ^ ('bgra_YUY2')
    | BGRA_YUYV -- ^ ('bgra_YUYV')
    | BGRA_YV12 -- ^ ('bgra_YV12')
    | BGRA_YVYU -- ^ ('bgra_YVYU')

    | BGR_EA    -- ^ ('bgr_EA') Edge-Aware
    | BGR_FULL  -- ^ ('bgr_FULL')
    | BGR_I420  -- ^ ('bgr_I420')
    | BGR_IYUV  -- ^ ('bgr_IYUV')
    | BGR_NV12  -- ^ ('bgr_NV12')
    | BGR_NV21  -- ^ ('bgr_NV21')
    | BGR_UYNV  -- ^ ('bgr_UYNV')
    | BGR_UYVY  -- ^ ('bgr_UYVY')
    | BGR_VNG   -- ^ ('bgr_VNG')
    | BGR_Y422  -- ^ ('bgr_Y422')
    | BGR_YUNV  -- ^ ('bgr_YUNV')
    | BGR_YUY2  -- ^ ('bgr_YUY2')
    | BGR_YUYV  -- ^ ('bgr_YUYV')
    | BGR_YV12  -- ^ ('bgr_YV12')
    | BGR_YVYU  -- ^ ('bgr_YVYU')

    | GRAY      -- ^ ('gray') 8 bit single channel color space
    | GRAY_420  -- ^ ('gray_420')
    | GRAY_I420 -- ^ ('gray_I420')
    | GRAY_IYUV -- ^ ('gray_IYUV')
    | GRAY_NV12 -- ^ ('gray_NV12')
    | GRAY_NV21 -- ^ ('gray_NV21')
    | GRAY_UYNV -- ^ ('gray_UYNV')
    | GRAY_UYVY -- ^ ('gray_UYVY')
    | GRAY_Y422 -- ^ ('gray_Y422')
    | GRAY_YUNV -- ^ ('gray_YUNV')
    | GRAY_YUY2 -- ^ ('gray_YUY2')
    | GRAY_YUYV -- ^ ('gray_YUYV')
    | GRAY_YV12 -- ^ ('gray_YV12')
    | GRAY_YVYU -- ^ ('gray_YVYU')

    | HLS       -- ^ ('hls')
    | HLS_FULL  -- ^ ('hls_FULL')
    | HSV       -- ^ ('hsv')
    | HSV_FULL  -- ^ ('hsv_FULL')
    | Lab       -- ^ ('lab')
    | LBGR      -- ^ ('lbgr')
    | LRGB      -- ^ ('lrgb')
    | Luv       -- ^ ('luv')
    | MRGBA     -- ^ ('mrgba')
    | RGB       -- ^ ('rgb') 24 bit RGB color space with channels: (R8:G8:B8)

    | RGBA      -- ^ ('rgba')
    | RGBA_I420 -- ^ ('rgba_I420')
    | RGBA_IYUV -- ^ ('rgba_IYUV')
    | RGBA_NV12 -- ^ ('rgba_NV12')
    | RGBA_NV21 -- ^ ('rgba_NV21')
    | RGBA_UYNV -- ^ ('rgba_UYNV')
    | RGBA_UYVY -- ^ ('rgba_UYVY')
    | RGBA_Y422 -- ^ ('rgba_Y422')
    | RGBA_YUNV -- ^ ('rgba_YUNV')
    | RGBA_YUY2 -- ^ ('rgba_YUY2')
    | RGBA_YUYV -- ^ ('rgba_YUYV')
    | RGBA_YV12 -- ^ ('rgba_YV12')
    | RGBA_YVYU -- ^ ('rgba_YVYU')

    | RGB_EA    -- ^ ('rgb_EA') Edge-Aware
    | RGB_FULL  -- ^ ('rgb_FULL')
    | RGB_I420  -- ^ ('rgb_I420')
    | RGB_IYUV  -- ^ ('rgb_IYUV')
    | RGB_NV12  -- ^ ('rgb_NV12')
    | RGB_NV21  -- ^ ('rgb_NV21')
    | RGB_UYNV  -- ^ ('rgb_UYNV')
    | RGB_UYVY  -- ^ ('rgb_UYVY')
    | RGB_VNG   -- ^ ('rgb_VNG')
    | RGB_Y422  -- ^ ('rgb_Y422')
    | RGB_YUNV  -- ^ ('rgb_YUNV')
    | RGB_YUY2  -- ^ ('rgb_YUY2')
    | RGB_YUYV  -- ^ ('rgb_YUYV')
    | RGB_YV12  -- ^ ('rgb_YV12')
    | RGB_YVYU  -- ^ ('rgb_YVYU')

    | XYZ       -- ^ ('xyz')
    | YCrCb     -- ^ ('yCrCb')

    | YUV       -- ^ ('yuv')
    | YUV420p   -- ^ ('yuv420p')
    | YUV420sp  -- ^ ('yuv420sp')
    | YUV_I420  -- ^ ('yuv_I420')
    | YUV_IYUV  -- ^ ('yuv_IYUV')
    | YUV_YV12  -- ^ ('yuv_YV12')

bayerBG    :: Proxy 'BayerBG
bayerGB    :: Proxy 'BayerGB
bayerGR    :: Proxy 'BayerGR
bayerRG    :: Proxy 'BayerRG
bgr        :: Proxy 'BGR
bgr555     :: Proxy 'BGR555
bgr565     :: Proxy 'BGR565
bgra       :: Proxy 'BGRA
bgra_I420  :: Proxy 'BGRA_I420
bgra_IYUV  :: Proxy 'BGRA_IYUV
bgra_NV12  :: Proxy 'BGRA_NV12
bgra_NV21  :: Proxy 'BGRA_NV21
bgra_UYNV  :: Proxy 'BGRA_UYNV
bgra_UYVY  :: Proxy 'BGRA_UYVY
bgra_Y422  :: Proxy 'BGRA_Y422
bgra_YUNV  :: Proxy 'BGRA_YUNV
bgra_YUY2  :: Proxy 'BGRA_YUY2
bgra_YUYV  :: Proxy 'BGRA_YUYV
bgra_YV12  :: Proxy 'BGRA_YV12
bgra_YVYU  :: Proxy 'BGRA_YVYU
bgr_EA     :: Proxy 'BGR_EA
bgr_FULL   :: Proxy 'BGR_FULL
bgr_I420   :: Proxy 'BGR_I420
bgr_IYUV   :: Proxy 'BGR_IYUV
bgr_NV12   :: Proxy 'BGR_NV12
bgr_NV21   :: Proxy 'BGR_NV21
bgr_UYNV   :: Proxy 'BGR_UYNV
bgr_UYVY   :: Proxy 'BGR_UYVY
bgr_VNG    :: Proxy 'BGR_VNG
bgr_Y422   :: Proxy 'BGR_Y422
bgr_YUNV   :: Proxy 'BGR_YUNV
bgr_YUY2   :: Proxy 'BGR_YUY2
bgr_YUYV   :: Proxy 'BGR_YUYV
bgr_YV12   :: Proxy 'BGR_YV12
bgr_YVYU   :: Proxy 'BGR_YVYU
gray       :: Proxy 'GRAY
gray_420   :: Proxy 'GRAY_420
gray_I420  :: Proxy 'GRAY_I420
gray_IYUV  :: Proxy 'GRAY_IYUV
gray_NV12  :: Proxy 'GRAY_NV12
gray_NV21  :: Proxy 'GRAY_NV21
gray_UYNV  :: Proxy 'GRAY_UYNV
gray_UYVY  :: Proxy 'GRAY_UYVY
gray_Y422  :: Proxy 'GRAY_Y422
gray_YUNV  :: Proxy 'GRAY_YUNV
gray_YUY2  :: Proxy 'GRAY_YUY2
gray_YUYV  :: Proxy 'GRAY_YUYV
gray_YV12  :: Proxy 'GRAY_YV12
gray_YVYU  :: Proxy 'GRAY_YVYU
hls        :: Proxy 'HLS
hls_FULL   :: Proxy 'HLS_FULL
hsv        :: Proxy 'HSV
hsv_FULL   :: Proxy 'HSV_FULL
lab        :: Proxy 'Lab
lbgr       :: Proxy 'LBGR
lrgb       :: Proxy 'LRGB
luv        :: Proxy 'Luv
mrgba      :: Proxy 'MRGBA
rgb        :: Proxy 'RGB
rgba       :: Proxy 'RGBA
rgba_I420  :: Proxy 'RGBA_I420
rgba_IYUV  :: Proxy 'RGBA_IYUV
rgba_NV12  :: Proxy 'RGBA_NV12
rgba_NV21  :: Proxy 'RGBA_NV21
rgba_UYNV  :: Proxy 'RGBA_UYNV
rgba_UYVY  :: Proxy 'RGBA_UYVY
rgba_Y422  :: Proxy 'RGBA_Y422
rgba_YUNV  :: Proxy 'RGBA_YUNV
rgba_YUY2  :: Proxy 'RGBA_YUY2
rgba_YUYV  :: Proxy 'RGBA_YUYV
rgba_YV12  :: Proxy 'RGBA_YV12
rgba_YVYU  :: Proxy 'RGBA_YVYU
rgb_EA     :: Proxy 'RGB_EA
rgb_FULL   :: Proxy 'RGB_FULL
rgb_I420   :: Proxy 'RGB_I420
rgb_IYUV   :: Proxy 'RGB_IYUV
rgb_NV12   :: Proxy 'RGB_NV12
rgb_NV21   :: Proxy 'RGB_NV21
rgb_UYNV   :: Proxy 'RGB_UYNV
rgb_UYVY   :: Proxy 'RGB_UYVY
rgb_VNG    :: Proxy 'RGB_VNG
rgb_Y422   :: Proxy 'RGB_Y422
rgb_YUNV   :: Proxy 'RGB_YUNV
rgb_YUY2   :: Proxy 'RGB_YUY2
rgb_YUYV   :: Proxy 'RGB_YUYV
rgb_YV12   :: Proxy 'RGB_YV12
rgb_YVYU   :: Proxy 'RGB_YVYU
xyz        :: Proxy 'XYZ
yCrCb      :: Proxy 'YCrCb
yuv        :: Proxy 'YUV
yuv420p    :: Proxy 'YUV420p
yuv420sp   :: Proxy 'YUV420sp
yuv_I420   :: Proxy 'YUV_I420
yuv_IYUV   :: Proxy 'YUV_IYUV
yuv_YV12   :: Proxy 'YUV_YV12

bayerBG    = Proxy
bayerGB    = Proxy
bayerGR    = Proxy
bayerRG    = Proxy
bgr        = Proxy
bgr555     = Proxy
bgr565     = Proxy
bgra       = Proxy
bgra_I420  = Proxy
bgra_IYUV  = Proxy
bgra_NV12  = Proxy
bgra_NV21  = Proxy
bgra_UYNV  = Proxy
bgra_UYVY  = Proxy
bgra_Y422  = Proxy
bgra_YUNV  = Proxy
bgra_YUY2  = Proxy
bgra_YUYV  = Proxy
bgra_YV12  = Proxy
bgra_YVYU  = Proxy
bgr_EA     = Proxy
bgr_FULL   = Proxy
bgr_I420   = Proxy
bgr_IYUV   = Proxy
bgr_NV12   = Proxy
bgr_NV21   = Proxy
bgr_UYNV   = Proxy
bgr_UYVY   = Proxy
bgr_VNG    = Proxy
bgr_Y422   = Proxy
bgr_YUNV   = Proxy
bgr_YUY2   = Proxy
bgr_YUYV   = Proxy
bgr_YV12   = Proxy
bgr_YVYU   = Proxy
gray       = Proxy
gray_420   = Proxy
gray_I420  = Proxy
gray_IYUV  = Proxy
gray_NV12  = Proxy
gray_NV21  = Proxy
gray_UYNV  = Proxy
gray_UYVY  = Proxy
gray_Y422  = Proxy
gray_YUNV  = Proxy
gray_YUY2  = Proxy
gray_YUYV  = Proxy
gray_YV12  = Proxy
gray_YVYU  = Proxy
hls        = Proxy
hls_FULL   = Proxy
hsv        = Proxy
hsv_FULL   = Proxy
lab        = Proxy
lbgr       = Proxy
lrgb       = Proxy
luv        = Proxy
mrgba      = Proxy
rgb        = Proxy
rgba       = Proxy
rgba_I420  = Proxy
rgba_IYUV  = Proxy
rgba_NV12  = Proxy
rgba_NV21  = Proxy
rgba_UYNV  = Proxy
rgba_UYVY  = Proxy
rgba_Y422  = Proxy
rgba_YUNV  = Proxy
rgba_YUY2  = Proxy
rgba_YUYV  = Proxy
rgba_YV12  = Proxy
rgba_YVYU  = Proxy
rgb_EA     = Proxy
rgb_FULL   = Proxy
rgb_I420   = Proxy
rgb_IYUV   = Proxy
rgb_NV12   = Proxy
rgb_NV21   = Proxy
rgb_UYNV   = Proxy
rgb_UYVY   = Proxy
rgb_VNG    = Proxy
rgb_Y422   = Proxy
rgb_YUNV   = Proxy
rgb_YUY2   = Proxy
rgb_YUYV   = Proxy
rgb_YV12   = Proxy
rgb_YVYU   = Proxy
xyz        = Proxy
yCrCb      = Proxy
yuv        = Proxy
yuv420p    = Proxy
yuv420sp   = Proxy
yuv_I420   = Proxy
yuv_IYUV   = Proxy
yuv_YV12   = Proxy

--------------------------------------------------------------------------------

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

-- | Gives the number of channels associated with a particular color encoding
type family ColorCodeChannels (cc :: ColorCode) :: Nat where
  ColorCodeChannels 'BayerBG   = 1
  ColorCodeChannels 'BayerGB   = 1
  ColorCodeChannels 'BayerGR   = 1
  ColorCodeChannels 'BayerRG   = 1
  ColorCodeChannels 'BGR       = 3
  ColorCodeChannels 'BGR555    = 2
  ColorCodeChannels 'BGR565    = 2
  ColorCodeChannels 'BGRA      = 4
  ColorCodeChannels 'BGRA_I420 = 4
  ColorCodeChannels 'BGRA_IYUV = 4
  ColorCodeChannels 'BGRA_NV12 = 4
  ColorCodeChannels 'BGRA_NV21 = 4
  ColorCodeChannels 'BGRA_UYNV = 4
  ColorCodeChannels 'BGRA_UYVY = 4
  ColorCodeChannels 'BGRA_Y422 = 4
  ColorCodeChannels 'BGRA_YUNV = 4
  ColorCodeChannels 'BGRA_YUY2 = 4
  ColorCodeChannels 'BGRA_YUYV = 4
  ColorCodeChannels 'BGRA_YV12 = 4
  ColorCodeChannels 'BGRA_YVYU = 4
  ColorCodeChannels 'BGR_EA    = 3
  ColorCodeChannels 'BGR_FULL  = 3
  ColorCodeChannels 'BGR_I420  = 3
  ColorCodeChannels 'BGR_IYUV  = 3
  ColorCodeChannels 'BGR_NV12  = 3
  ColorCodeChannels 'BGR_NV21  = 3
  ColorCodeChannels 'BGR_UYNV  = 3
  ColorCodeChannels 'BGR_UYVY  = 3
  ColorCodeChannels 'BGR_VNG   = 3
  ColorCodeChannels 'BGR_Y422  = 3
  ColorCodeChannels 'BGR_YUNV  = 3
  ColorCodeChannels 'BGR_YUY2  = 3
  ColorCodeChannels 'BGR_YUYV  = 3
  ColorCodeChannels 'BGR_YV12  = 3
  ColorCodeChannels 'BGR_YVYU  = 3
  ColorCodeChannels 'GRAY      = 1
  ColorCodeChannels 'GRAY_420  = 1
  ColorCodeChannels 'GRAY_I420 = 1
  ColorCodeChannels 'GRAY_IYUV = 1
  ColorCodeChannels 'GRAY_NV12 = 1
  ColorCodeChannels 'GRAY_NV21 = 1
  ColorCodeChannels 'GRAY_UYNV = 1
  ColorCodeChannels 'GRAY_UYVY = 1
  ColorCodeChannels 'GRAY_Y422 = 1
  ColorCodeChannels 'GRAY_YUNV = 1
  ColorCodeChannels 'GRAY_YUY2 = 1
  ColorCodeChannels 'GRAY_YUYV = 1
  ColorCodeChannels 'GRAY_YV12 = 1
  ColorCodeChannels 'GRAY_YVYU = 1
  ColorCodeChannels 'HLS       = 3
  ColorCodeChannels 'HLS_FULL  = 3
  ColorCodeChannels 'HSV       = 3
  ColorCodeChannels 'HSV_FULL  = 3
  ColorCodeChannels 'Lab       = 3
  ColorCodeChannels 'LBGR      = 3
  ColorCodeChannels 'LRGB      = 3
  ColorCodeChannels 'Luv       = 3
  ColorCodeChannels 'MRGBA     = 4
  ColorCodeChannels 'RGB       = 3
  ColorCodeChannels 'RGBA      = 4
  ColorCodeChannels 'RGBA_I420 = 4
  ColorCodeChannels 'RGBA_IYUV = 4
  ColorCodeChannels 'RGBA_NV12 = 4
  ColorCodeChannels 'RGBA_NV21 = 4
  ColorCodeChannels 'RGBA_UYNV = 4
  ColorCodeChannels 'RGBA_UYVY = 4
  ColorCodeChannels 'RGBA_Y422 = 4
  ColorCodeChannels 'RGBA_YUNV = 4
  ColorCodeChannels 'RGBA_YUY2 = 4
  ColorCodeChannels 'RGBA_YUYV = 4
  ColorCodeChannels 'RGBA_YV12 = 4
  ColorCodeChannels 'RGBA_YVYU = 4
  ColorCodeChannels 'RGB_EA    = 3
  ColorCodeChannels 'RGB_FULL  = 3
  ColorCodeChannels 'RGB_I420  = 3
  ColorCodeChannels 'RGB_IYUV  = 3
  ColorCodeChannels 'RGB_NV12  = 3
  ColorCodeChannels 'RGB_NV21  = 3
  ColorCodeChannels 'RGB_UYNV  = 3
  ColorCodeChannels 'RGB_UYVY  = 3
  ColorCodeChannels 'RGB_VNG   = 3
  ColorCodeChannels 'RGB_Y422  = 3
  ColorCodeChannels 'RGB_YUNV  = 3
  ColorCodeChannels 'RGB_YUY2  = 3
  ColorCodeChannels 'RGB_YUYV  = 3
  ColorCodeChannels 'RGB_YV12  = 3
  ColorCodeChannels 'RGB_YVYU  = 3
  ColorCodeChannels 'XYZ       = 3
  ColorCodeChannels 'YCrCb     = 3
  ColorCodeChannels 'YUV       = 3
  ColorCodeChannels 'YUV420p   = 3
  ColorCodeChannels 'YUV420sp  = 3
  ColorCodeChannels 'YUV_I420  = 1
  ColorCodeChannels 'YUV_IYUV  = 1
  ColorCodeChannels 'YUV_YV12  = 1

class ColorCodeMatchesChannels (code :: ColorCode) (channels :: DS Nat)

instance ColorCodeMatchesChannels code 'D
instance (ColorCodeChannels code ~ channels) => ColorCodeMatchesChannels code ('S channels)

type family ColorCodeDepth (srcCode :: ColorCode) (dstCode :: ColorCode) (srcDepth :: DS *) :: DS * where
  ColorCodeDepth 'BGR     'BGRA       ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'BGRA       ('S depth)  = 'S depth
  ColorCodeDepth 'BGRA    'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'RGBA    'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'BGRA    'RGBA       ('S depth)  = 'S depth

  ColorCodeDepth 'BGR     'BGR565     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'BGR555     ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'BGR565     ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'BGR555     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGRA    'BGR565     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGRA    'BGR555     ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGBA    'BGR565     ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGBA    'BGR555     ('S Word8)  = 'S Word8

  ColorCodeDepth 'BGR565  'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR555  'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR565  'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR555  'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR565  'BGRA       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR555  'BGRA       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR565  'RGBA       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR555  'RGBA       ('S Word8)  = 'S Word8

  ColorCodeDepth 'BGR     'GRAY       ('S depth)  = 'S depth
  ColorCodeDepth 'BGRA    'GRAY       ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'GRAY       ('S depth)  = 'S depth
  ColorCodeDepth 'RGBA    'GRAY       ('S depth)  = 'S depth

  ColorCodeDepth 'BGR565  'GRAY       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR555  'GRAY       ('S Word8)  = 'S Word8

  ColorCodeDepth 'GRAY    'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'GRAY    'BGRA       ('S depth)  = 'S depth

  ColorCodeDepth 'GRAY    'BGR565     ('S Word8)  = 'S Word8
  ColorCodeDepth 'GRAY    'BGR555     ('S Word8)  = 'S Word8

  ColorCodeDepth 'BGR     'YCrCb      ('S depth)  = 'S depth
  ColorCodeDepth 'BGR     'YUV        ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'YCrCb      ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'YUV        ('S depth)  = 'S depth

  ColorCodeDepth 'YCrCb   'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'YCrCb   'RGB        ('S depth)  = 'S depth
  ColorCodeDepth 'YUV     'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'YUV     'RGB        ('S depth)  = 'S depth

  ColorCodeDepth 'BGR     'XYZ        ('S depth)  = 'S depth
  ColorCodeDepth 'RGB     'XYZ        ('S depth)  = 'S depth

  ColorCodeDepth 'XYZ     'BGR        ('S depth)  = 'S depth
  ColorCodeDepth 'XYZ     'RGB        ('S depth)  = 'S depth

  ColorCodeDepth 'BGR     'HSV        ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'HSV        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'HSV_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'HSV_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'HLS        ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'HLS        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'HLS_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'HLS_FULL   ('S Word8)  = 'S Word8

  ColorCodeDepth 'BGR     'HSV        ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'HSV        ('S Float)  = 'S Float
  ColorCodeDepth 'BGR     'HSV_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'HSV_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'BGR     'HLS        ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'HLS        ('S Float)  = 'S Float
  ColorCodeDepth 'BGR     'HLS_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'HLS_FULL   ('S Float)  = 'S Float

  ColorCodeDepth 'HSV     'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'HSV     'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'HSV     'BGR_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'HSV     'RGB_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'HLS     'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'HLS     'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'HLS     'BGR_FULL   ('S Word8)  = 'S Word8
  ColorCodeDepth 'HLS     'RGB_FULL   ('S Word8)  = 'S Word8

  ColorCodeDepth 'HSV     'BGR        ('S Float)  = 'S Float
  ColorCodeDepth 'HSV     'RGB        ('S Float)  = 'S Float
  ColorCodeDepth 'HSV     'BGR_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'HSV     'RGB_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'HLS     'BGR        ('S Float)  = 'S Float
  ColorCodeDepth 'HLS     'RGB        ('S Float)  = 'S Float
  ColorCodeDepth 'HLS     'BGR_FULL   ('S Float)  = 'S Float
  ColorCodeDepth 'HLS     'RGB_FULL   ('S Float)  = 'S Float

  ColorCodeDepth 'BGR     'Lab        ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'Lab        ('S Word8)  = 'S Word8
  ColorCodeDepth 'LBGR    'Lab        ('S Word8)  = 'S Word8
  ColorCodeDepth 'LRGB    'Lab        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'Luv        ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'Luv        ('S Word8)  = 'S Word8
  ColorCodeDepth 'LBGR    'Luv        ('S Word8)  = 'S Word8
  ColorCodeDepth 'LRGB    'Luv        ('S Word8)  = 'S Word8

  ColorCodeDepth 'BGR     'Lab        ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'Lab        ('S Float)  = 'S Float
  ColorCodeDepth 'LBGR    'Lab        ('S Float)  = 'S Float
  ColorCodeDepth 'LRGB    'Lab        ('S Float)  = 'S Float
  ColorCodeDepth 'BGR     'Luv        ('S Float)  = 'S Float
  ColorCodeDepth 'RGB     'Luv        ('S Float)  = 'S Float
  ColorCodeDepth 'LBGR    'Luv        ('S Float)  = 'S Float
  ColorCodeDepth 'LRGB    'Luv        ('S Float)  = 'S Float

  ColorCodeDepth 'Lab     'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'Lab     'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'Lab     'LBGR       ('S Word8)  = 'S Word8
  ColorCodeDepth 'Lab     'LRGB       ('S Word8)  = 'S Word8
  ColorCodeDepth 'Luv     'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'Luv     'RGB        ('S Word8)  = 'S Word8
  ColorCodeDepth 'Luv     'LBGR       ('S Word8)  = 'S Word8
  ColorCodeDepth 'Luv     'LRGB       ('S Word8)  = 'S Word8

  ColorCodeDepth 'Lab     'BGR        ('S Float)  = 'S Float
  ColorCodeDepth 'Lab     'RGB        ('S Float)  = 'S Float
  ColorCodeDepth 'Lab     'LBGR       ('S Float)  = 'S Float
  ColorCodeDepth 'Lab     'LRGB       ('S Float)  = 'S Float
  ColorCodeDepth 'Luv     'BGR        ('S Float)  = 'S Float
  ColorCodeDepth 'Luv     'RGB        ('S Float)  = 'S Float
  ColorCodeDepth 'Luv     'LBGR       ('S Float)  = 'S Float
  ColorCodeDepth 'Luv     'LRGB       ('S Float)  = 'S Float

  ColorCodeDepth 'BayerBG 'GRAY       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerBG 'GRAY       ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGB 'GRAY       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGB 'GRAY       ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGR 'GRAY       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGR 'GRAY       ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerRG 'GRAY       ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerRG 'GRAY       ('S Word16) = 'S Word16

  ColorCodeDepth 'BayerBG 'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerBG 'BGR        ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGB 'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGB 'BGR        ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGR 'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGR 'BGR        ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerRG 'BGR        ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerRG 'BGR        ('S Word16) = 'S Word16

  ColorCodeDepth 'BayerBG 'BGR_VNG    ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerBG 'BGR_VNG    ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGB 'BGR_VNG    ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGB 'BGR_VNG    ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGR 'BGR_VNG    ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGR 'BGR_VNG    ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerRG 'BGR_VNG    ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerRG 'BGR_VNG    ('S Word16) = 'S Word16

  ColorCodeDepth 'BayerBG 'BGR_EA     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerBG 'BGR_EA     ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGB 'BGR_EA     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGB 'BGR_EA     ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerGR 'BGR_EA     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerGR 'BGR_EA     ('S Word16) = 'S Word16
  ColorCodeDepth 'BayerRG 'BGR_EA     ('S Word8)  = 'S Word8
  ColorCodeDepth 'BayerRG 'BGR_EA     ('S Word16) = 'S Word16

  ColorCodeDepth 'YUV     'BGR_NV21   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_NV21   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGR_NV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_NV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_NV21  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_NV21  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_NV12  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_NV12  ('S Word8)  = 'S Word8

  ColorCodeDepth 'YUV     'BGR_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_YV12  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_YV12  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGR_IYUV   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_IYUV   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_IYUV  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_IYUV  ('S Word8)  = 'S Word8

  ColorCodeDepth 'YUV     'GRAY_420   ('S Word8)  = 'S Word8

  ColorCodeDepth 'RGB     'YUV_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'YUV_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGBA    'YUV_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGRA    'YUV_YV12   ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGB     'YUV_IYUV   ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGR     'YUV_IYUV   ('S Word8)  = 'S Word8
  ColorCodeDepth 'RGBA    'YUV_IYUV   ('S Word8)  = 'S Word8
  ColorCodeDepth 'BGRA    'YUV_IYUV   ('S Word8)  = 'S Word8

  ColorCodeDepth 'YUV     'RGB_UYVY   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGR_UYVY   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_UYVY  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_UYVY  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_YUY2   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGR_YUY2   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGB_YVYU   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGR_YVYU   ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_YUY2  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_YUY2  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'RGBA_YVYU  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'BGRA_YVYU  ('S Word8)  = 'S Word8

  ColorCodeDepth 'YUV     'GRAY_UYVY  ('S Word8)  = 'S Word8
  ColorCodeDepth 'YUV     'GRAY_YUY2  ('S Word8)  = 'S Word8

  ColorCodeDepth 'RGBA    'MRGBA      ('S Word8)  = 'S Word8
  ColorCodeDepth 'MRGBA   'RGBA       ('S Word8)  = 'S Word8

  ColorCodeDepth srcCode  dstCode      'D         = 'D

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

  * 0 to 255 for 'Word8' images

  * 0 to 65535 for 'Word16' images

  * 0 to 1 for 'Float' images

In case of linear transformations, the range does not matter. But
in case of a non-linear transformation, an input RGB image should
be normalized to the proper value range to get the correct results,
for example, for RGB to L*u*v* transformation. For example, if you
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
maximum of corresponding channel range: 255 for 'Word8', 65535 for
'Word16', 1 for 'Float'.

Example:

@
cvtColorImg
    :: forall (width    :: Nat)
              (width2   :: Nat)
              (height   :: Nat)
              (channels :: Nat)
              (depth    :: *)
     . ( Mat (ShapeT [height, width]) ('S channels) ('S depth) ~ Birds_512x341
       , width2 ~ (width + width)
       )
    => Mat (ShapeT [height, width2]) ('S channels) ('S depth)
cvtColorImg = createMat $ do
    imgM <- mkMatM ((Proxy :: Proxy height) ::: (Proxy :: Proxy width2) ::: Z)
                   (Proxy :: Proxy channels)
                   (Proxy :: Proxy depth)
                   white
    void $ matCopyToM imgM (V2 0 0) birds_512x341
    void $ matCopyToM imgM (V2 w 0) birds_gray
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
-- TODO (RvD): Allow value level color codes
-- Allow statically unknown color codes: fromColor :: DS ColorCode
cvtColor :: forall (fromColor   :: ColorCode)
                   (toColor     :: ColorCode)
                   (shape       :: DS [DS Nat])
                   (srcChannels :: DS Nat)
                   (dstChannels :: DS Nat)
                   (srcDepth    :: DS *)
                   (dstDepth    :: DS *)
          . ( ColorConversion fromColor toColor
            , ColorCodeMatchesChannels fromColor srcChannels
            , dstChannels ~ 'S (ColorCodeChannels toColor)
            , srcDepth `In` ['D, 'S Word8, 'S Word16, 'S Float]
            , dstDepth ~ ColorCodeDepth fromColor toColor srcDepth
            )
         => Proxy fromColor -- ^ Convert from 'ColorCode'. Make sure the source image has this 'ColorCode'
         -> Proxy toColor   -- ^ Convert to 'ColorCode'.
         -> Mat shape srcChannels srcDepth -- ^ Source image
         -> Either CvException (Mat shape dstChannels dstDepth)
cvtColor fromColor toColor src = unsafePerformIO $ do
    dst <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat dst) $
      withMatPtr src $ \srcPtr ->
      withMatPtr dst $ \dstPtr ->
        [cvExcept|
          cv::cvtColor( *$(Mat * srcPtr)
                      , *$(Mat * dstPtr)
                      , $(int32_t c'code)
                      , 0
                      );
        |]
  where
    c'code = colorCode fromColor toColor

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
