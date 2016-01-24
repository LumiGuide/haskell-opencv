{-# LANGUAGE DataKinds #-}

module OpenCV.ImgProc.MiscImgTransform.ColorCodes where

import "base" Data.Proxy ( Proxy(..) )

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
