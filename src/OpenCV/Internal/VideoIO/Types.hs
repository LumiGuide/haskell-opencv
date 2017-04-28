{-# language GeneralizedNewtypeDeriving #-}
{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.VideoIO.Types
    ( -- * VideoCodecs
      FourCC(..)
    , VideoCaptureProperties(..)
    , marshalCaptureProperties
    , VideoCaptureAPI(..)
    , marshalVideoCaptureAPI
    ) where

import "base" Data.Char ( ord )
import "base" Data.Int ( Int32 )
import "base" Data.Word ( Word32 )
import "base" Data.String ( IsString (..) )
import "base" Data.List( unfoldr )
import "this" OpenCV.Internal.VideoIO.Constants

--------------------------------------------------------------------------------
-- FourCC
--------------------------------------------------------------------------------

newtype FourCC = FourCC { unFourCC :: Int32 }

instance IsString FourCC where
  fromString "ask" = FourCC (-1)
  fromString fcc =
      FourCC $ foldr (\x acc -> acc * 256 + fromIntegral (ord x))
                     0
                     (take 4 fcc)

instance Show FourCC where
  show (FourCC (-1)) = "ask"
  show (FourCC fcc) = take 4 $ unfoldr worker (fromIntegral fcc :: Word32)
    where
      worker 0 = Nothing
      worker b = Just (toEnum . fromEnum $ b `mod` 256, b `div` 256)

--------------------------------------------------------------------------------
-- VideoCaptureProperties
-- more at https://github.com/opencv/opencv/blob/master/modules/videoio/include/opencv2/videoio.hpp
--------------------------------------------------------------------------------

data VideoCaptureProperties
   = VideoCapPropPosMsec
          -- ^ Current position of the video file in milliseconds.
   | VideoCapPropPosFrames
          -- ^ 0-based index of the frame to be decoded/captured next.
   | VideoCapPropPosAviRatio
          -- ^ Relative position of the video file: 0=start of the film,
          --   1=end of the film.
   | VideoCapPropFrameWidth     -- ^ Width of the frames in the video stream.
   | VideoCapPropFrameHeight    -- ^ Height of the frames in the video stream.
   | VideoCapPropFps            -- ^ Frame rate.
   | VideoCapPropFourCc         -- ^ 4-character code of codec.
   | VideoCapPropFrameCount     -- ^ Number of frames in the video file.
   | VideoCapPropFormat
          -- ^ Format of the Mat objects returned by VideoCapture::retrieve().
   | VideoCapPropMode
          -- ^ Backend-specific value indicating the current capture mode.
   | VideoCapPropBrightness     -- ^ Brightness of the image (only for cameras).
   | VideoCapPropContrast       -- ^ Contrast of the image (only for cameras).
   | VideoCapPropSaturation     -- ^ Saturation of the image (only for cameras).
   | VideoCapPropHue            -- ^ Hue of the image (only for cameras).
   | VideoCapPropGain           -- ^ Gain of the image (only for cameras).
   | VideoCapPropExposure       -- ^ Exposure (only for cameras).
   | VideoCapPropConvertRgb
        -- ^ Boolean flags indicating whether images should be converted to RGB.
   | VideoCapPropWhiteBalanceBlueU -- ^ Currently unsupported.
   | VideoCapPropRectification
        -- ^ Rectification flag for stereo cameras
        --  (note: only supported by DC1394 v 2.x backend currently).
   | VideoCapPropMonochrome        -- ^
   | VideoCapPropSharpness         -- ^
   | VideoCapPropAutoExposure
        -- ^ DC1394: exposure control done by camera, user can adjust reference
        -- level using this feature.
   | VideoCapPropGamma             -- ^
   | VideoCapPropTemperature       -- ^
   | VideoCapPropTrigger           -- ^
   | VideoCapPropTriggerDelay      -- ^
   | VideoCapPropWhiteBalanceRedV  -- ^
   | VideoCapPropZoom              -- ^
   | VideoCapPropFocus             -- ^
   | VideoCapPropGuid              -- ^
   | VideoCapPropIsoSpeed          -- ^
   | VideoCapPropBacklight         -- ^
   | VideoCapPropPan               -- ^
   | VideoCapPropTilt              -- ^
   | VideoCapPropRoll              -- ^
   | VideoCapPropIris              -- ^
   | VideoCapPropSettings
        -- ^ Pop up video/camera filter dialog (note: only supported by DSHOW
        --  backend currently. Property value is ignored)
   | VideoCapPropBuffersize        -- ^
   | VideoCapPropAutofocus         -- ^
   | VideoCapPropInt !Int32
       -- ^ Any property we need. Meaning of this property depends on
       -- the backend.

marshalCaptureProperties :: VideoCaptureProperties -> Int32
marshalCaptureProperties = \case
   VideoCapPropPosMsec           -> c'CAP_PROP_POS_MSEC
   VideoCapPropPosFrames         -> c'CAP_PROP_POS_FRAMES
   VideoCapPropPosAviRatio       -> c'CAP_PROP_POS_AVI_RATIO
   VideoCapPropFrameWidth        -> c'CAP_PROP_FRAME_WIDTH
   VideoCapPropFrameHeight       -> c'CAP_PROP_FRAME_HEIGHT
   VideoCapPropFps               -> c'CAP_PROP_FPS
   VideoCapPropFourCc            -> c'CAP_PROP_FOURCC
   VideoCapPropFrameCount        -> c'CAP_PROP_FRAME_COUNT
   VideoCapPropFormat            -> c'CAP_PROP_FORMAT
   VideoCapPropMode              -> c'CAP_PROP_MODE
   VideoCapPropBrightness        -> c'CAP_PROP_BRIGHTNESS
   VideoCapPropContrast          -> c'CAP_PROP_CONTRAST
   VideoCapPropSaturation        -> c'CAP_PROP_SATURATION
   VideoCapPropHue               -> c'CAP_PROP_HUE
   VideoCapPropGain              -> c'CAP_PROP_GAIN
   VideoCapPropExposure          -> c'CAP_PROP_EXPOSURE
   VideoCapPropConvertRgb        -> c'CAP_PROP_CONVERT_RGB
   VideoCapPropWhiteBalanceBlueU -> c'CAP_PROP_WHITE_BALANCE_BLUE_U
   VideoCapPropRectification     -> c'CAP_PROP_RECTIFICATION
   VideoCapPropMonochrome        -> c'CAP_PROP_MONOCHROME
   VideoCapPropSharpness         -> c'CAP_PROP_SHARPNESS
   VideoCapPropAutoExposure      -> c'CAP_PROP_AUTO_EXPOSURE
   VideoCapPropGamma             -> c'CAP_PROP_GAMMA
   VideoCapPropTemperature       -> c'CAP_PROP_TEMPERATURE
   VideoCapPropTrigger           -> c'CAP_PROP_TRIGGER
   VideoCapPropTriggerDelay      -> c'CAP_PROP_TRIGGER_DELAY
   VideoCapPropWhiteBalanceRedV  -> c'CAP_PROP_WHITE_BALANCE_RED_V
   VideoCapPropZoom              -> c'CAP_PROP_ZOOM
   VideoCapPropFocus             -> c'CAP_PROP_FOCUS
   VideoCapPropGuid              -> c'CAP_PROP_GUID
   VideoCapPropIsoSpeed          -> c'CAP_PROP_ISO_SPEED
   VideoCapPropBacklight         -> c'CAP_PROP_BACKLIGHT
   VideoCapPropPan               -> c'CAP_PROP_PAN
   VideoCapPropTilt              -> c'CAP_PROP_TILT
   VideoCapPropRoll              -> c'CAP_PROP_ROLL
   VideoCapPropIris              -> c'CAP_PROP_IRIS
   VideoCapPropSettings          -> c'CAP_PROP_SETTINGS
   VideoCapPropBuffersize        -> c'CAP_PROP_BUFFERSIZE
   VideoCapPropAutofocus         -> c'CAP_PROP_AUTOFOCUS
   VideoCapPropInt a             -> a

--------------------------------------------------------------------------------
-- VideoCaptureProperties
-- more at https://github.com/opencv/opencv/blob/master/modules/videoio/include/opencv2/videoio.hpp
--------------------------------------------------------------------------------
data VideoCaptureAPI
   = VideoCapAny          -- ^ Auto detect == 0
   | VideoCapVfw          -- ^ Video For Windows (platform native)
   | VideoCapV4l          -- ^ V4L/V4L2 capturing support via libv4l
   | VideoCapV4l2         -- ^ Same as CAP_V4L
   | VideoCapFirewire     -- ^ IEEE 1394 drivers
   | VideoCapFireware     -- ^ Same as CAP_FIREWIRE
   | VideoCapIeee1394     -- ^ Same as CAP_FIREWIRE
   | VideoCapDc1394       -- ^ Same as CAP_FIREWIRE
   | VideoCapCmu1394      -- ^ Same as CAP_FIREWIRE
   | VideoCapQt           -- ^ QuickTime
   | VideoCapUnicap       -- ^ Unicap drivers
   | VideoCapDshow        -- ^ DirectShow (via videoInput)
   | VideoCapPvapi        -- ^ PvAPI, Prosilica GigE SDK
   | VideoCapOpenni       -- ^ OpenNI (for Kinect)
   | VideoCapOpenniAsus   -- ^ OpenNI (for Asus Xtion)
   | VideoCapAndroid      -- ^ Android - not used
   | VideoCapXiapi        -- ^ XIMEA Camera API
   | VideoCapAvfoundation
        -- ^ AVFoundation framework for iOS (OS X Lion will have the same API)
   | VideoCapGiganetix    -- ^ Smartek Giganetix GigEVisionSDK
   | VideoCapMsmf         -- ^ Microsoft Media Foundation (via videoInput)
   | VideoCapWinrt        -- ^ Microsoft Windows Runtime using Media Foundation
   | VideoCapIntelperc    -- ^ Intel Perceptual Computing SDK
   | VideoCapOpenni2      -- ^ OpenNI2 (for Kinect)
   | VideoCapOpenni2Asus
        -- ^ OpenNI2 (for Asus Xtion and Occipital Structure sensors)
   | VideoCapGphoto2      -- ^ gPhoto2 connection
   | VideoCapGstreamer    -- ^ GStreamer
   | VideoCapFfmpeg
        -- ^ Open and record video file or stream using the FFMPEG library
   | VideoCapImages       -- ^ Image Sequence (e.g. img_%02d.jpg)

marshalVideoCaptureAPI :: VideoCaptureAPI -> Int32
marshalVideoCaptureAPI = \case
   VideoCapAny           -> c'CAP_ANY
   VideoCapVfw           -> c'CAP_VFW
   VideoCapV4l           -> c'CAP_V4L
   VideoCapV4l2          -> c'CAP_V4L2
   VideoCapFirewire      -> c'CAP_FIREWIRE
   VideoCapFireware      -> c'CAP_FIREWARE
   VideoCapIeee1394      -> c'CAP_IEEE1394
   VideoCapDc1394        -> c'CAP_DC1394
   VideoCapCmu1394       -> c'CAP_CMU1394
   VideoCapQt            -> c'CAP_QT
   VideoCapUnicap        -> c'CAP_UNICAP
   VideoCapDshow         -> c'CAP_DSHOW
   VideoCapPvapi         -> c'CAP_PVAPI
   VideoCapOpenni        -> c'CAP_OPENNI
   VideoCapOpenniAsus    -> c'CAP_OPENNI_ASUS
   VideoCapAndroid       -> c'CAP_ANDROID
   VideoCapXiapi         -> c'CAP_XIAPI
   VideoCapAvfoundation  -> c'CAP_AVFOUNDATION
   VideoCapGiganetix     -> c'CAP_GIGANETIX
   VideoCapMsmf          -> c'CAP_MSMF
   VideoCapWinrt         -> c'CAP_WINRT
   VideoCapIntelperc     -> c'CAP_INTELPERC
   VideoCapOpenni2       -> c'CAP_OPENNI2
   VideoCapOpenni2Asus   -> c'CAP_OPENNI2_ASUS
   VideoCapGphoto2       -> c'CAP_GPHOTO2
   VideoCapGstreamer     -> c'CAP_GSTREAMER
   VideoCapFfmpeg        -> c'CAP_FFMPEG
   VideoCapImages        -> c'CAP_IMAGES
