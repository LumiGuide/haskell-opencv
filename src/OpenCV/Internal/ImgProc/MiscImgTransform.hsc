{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgProc.MiscImgTransform where

import "base" Data.Bits
import "base" Data.Int
import "base" Foreign.C.Types
import "linear" Linear.V2 ( V2(..) )
import "this" OpenCV.Core.Types.Rect

--------------------------------------------------------------------------------

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "namespace.hpp"

--------------------------------------------------------------------------------

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

#num FLOODFILL_FIXED_RANGE
#num FLOODFILL_MASK_ONLY

--------------------------------------------------------------------------------

data GrabCutOperationMode
    = GrabCut_InitWithRect (Rect Int32)
        -- ^ Initialize the state and the mask using the provided rectangle. After that, run iterCount iterations of the algorithm.
        -- The rectangle represents a ROI containing a segmented object. The pixels outside of the ROI are marked as “obvious background”.
    | GrabCut_InitWithMask
        -- ^ Initialize the state using the provided mask.
    | GrabCut_InitWithRectAndMask (Rect Int32)
        -- ^ Combination of 'GCInitWithRect' and 'GCInitWithMask'. All the pixels outside of the ROI are automatically initialized with GC_BGD.
    | GrabCut_Eval
        -- ^ Just resume the algorithm.
      deriving (Show)

#num GC_INIT_WITH_RECT
#num GC_INIT_WITH_MASK
#num GC_EVAL

marshalGrabCutOperationMode :: GrabCutOperationMode -> Int32
marshalGrabCutOperationMode = \case
    GrabCut_InitWithRect _        -> c'GC_INIT_WITH_RECT
    GrabCut_InitWithMask          -> c'GC_INIT_WITH_MASK
    GrabCut_InitWithRectAndMask _ -> c'GC_INIT_WITH_RECT .|. c'GC_INIT_WITH_MASK
    GrabCut_Eval                  -> c'GC_EVAL

marshalGrabCutOperationModeRect :: GrabCutOperationMode -> Rect Int32
marshalGrabCutOperationModeRect = \case
    GrabCut_InitWithRect r        -> r
    GrabCut_InitWithMask          -> emptyRect
    GrabCut_InitWithRectAndMask r -> r
    GrabCut_Eval                  -> emptyRect
  where
    emptyRect = toRect (HRect { hRectTopLeft = V2 0 0, hRectSize = V2 0 0 })

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
