{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.HighGui
    ( -- * Window management
      WindowName
    , makeWindow

      -- * Event handling

      -- ** Keyboard
    , waitKey

      -- ** Mouse
    , Event(..)
    , EventFlags

    , hasLButton
    , hasRButton
    , hasMButton
    , hasCtrlKey
    , hasShiftKey
    , hasAltKey

    , EventFlagsRec(..)
    , flagsToRec

    , setMouseCallback

      -- * Trackbars
    , TrackbarName
    , createTrackbar

      -- * Drawing
    , imshow
    ) where

import qualified "base" Foreign.C.String as C
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context (C.cppCtx <> openCvCtx)

C.include "opencv2/core.hpp"
C.include "opencv2/highgui.hpp"
C.using "namespace cv"

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/highgui.hpp"

#include "namespace.hpp"


--------------------------------------------------------------------------------
-- Window management

type WindowName = String

makeWindow :: WindowName -> IO ()
makeWindow windowName =
    C.withCString windowName $ \c'wn ->
      [C.exp| void {
        cv::namedWindow($(char * c'wn), cv::WINDOW_NORMAL | cv::WINDOW_KEEPRATIO);
      }|]

--------------------------------------------------------------------------------
-- Keyboard

waitKey :: Int -> IO Int
waitKey delay = fromIntegral <$> f (fromIntegral delay)
  where
    f :: C.CInt -> IO C.CInt
    f c'delay = [C.exp| int { cv::waitKey($(int c'delay)) }|]


--------------------------------------------------------------------------------
-- Mouse

#num EVENT_MOUSEMOVE
#num EVENT_LBUTTONDOWN
#num EVENT_RBUTTONDOWN
#num EVENT_MBUTTONDOWN
#num EVENT_LBUTTONUP
#num EVENT_RBUTTONUP
#num EVENT_MBUTTONUP
#num EVENT_LBUTTONDBLCLK
#num EVENT_RBUTTONDBLCLK
#num EVENT_MBUTTONDBLCLK
#num EVENT_MOUSEWHEEL
#num EVENT_MOUSEHWHEEL

data Event
   = EventMouseMove
   | EventLButtonDown
   | EventRButtonDown
   | EventMButtonDown
   | EventLButtonUp
   | EventRButtonUp
   | EventMButtonUp
   | EventLButtonDbClick
   | EventRButtonDbClick
   | EventMButtonDbClick
   | EventMouseWheel
   | EventMouseHWheel
     deriving Show

#num EVENT_FLAG_LBUTTON
#num EVENT_FLAG_RBUTTON
#num EVENT_FLAG_MBUTTON
#num EVENT_FLAG_CTRLKEY
#num EVENT_FLAG_SHIFTKEY
#num EVENT_FLAG_ALTKEY

newtype EventFlags = EventFlags C.CInt

matchEventFlag :: C.CInt -> EventFlags -> Bool
matchEventFlag flag = \(EventFlags flags) -> flags .&. flag /= 0

hasLButton  :: EventFlags -> Bool
hasRButton  :: EventFlags -> Bool
hasMButton  :: EventFlags -> Bool
hasCtrlKey  :: EventFlags -> Bool
hasShiftKey :: EventFlags -> Bool
hasAltKey   :: EventFlags -> Bool

hasLButton  = matchEventFlag c'EVENT_FLAG_LBUTTON
hasRButton  = matchEventFlag c'EVENT_FLAG_RBUTTON
hasMButton  = matchEventFlag c'EVENT_FLAG_MBUTTON
hasCtrlKey  = matchEventFlag c'EVENT_FLAG_CTRLKEY
hasShiftKey = matchEventFlag c'EVENT_FLAG_SHIFTKEY
hasAltKey   = matchEventFlag c'EVENT_FLAG_ALTKEY

data EventFlagsRec
   = EventFlagsRec
     { flagsLButton  :: !Bool
     , flagsRButton  :: !Bool
     , flagsMButton  :: !Bool
     , flagsCtrlKey  :: !Bool
     , flagsShiftKey :: !Bool
     , flagsAltKey   :: !Bool
     } deriving Show

flagsToRec :: EventFlags -> EventFlagsRec
flagsToRec flags =
    EventFlagsRec
    { flagsLButton  = hasLButton  flags
    , flagsRButton  = hasRButton  flags
    , flagsMButton  = hasMButton  flags
    , flagsCtrlKey  = hasCtrlKey  flags
    , flagsShiftKey = hasShiftKey flags
    , flagsAltKey   = hasAltKey   flags
    }


unmarshallEvent :: C.CInt -> Event
unmarshallEvent event
   | event == c'EVENT_MOUSEMOVE     = EventMouseMove
   | event == c'EVENT_LBUTTONDOWN   = EventLButtonDown
   | event == c'EVENT_RBUTTONDOWN   = EventRButtonDown
   | event == c'EVENT_MBUTTONDOWN   = EventMButtonDown
   | event == c'EVENT_LBUTTONUP     = EventLButtonUp
   | event == c'EVENT_RBUTTONUP     = EventRButtonUp
   | event == c'EVENT_MBUTTONUP     = EventMButtonUp
   | event == c'EVENT_LBUTTONDBLCLK = EventLButtonDbClick
   | event == c'EVENT_RBUTTONDBLCLK = EventRButtonDbClick
   | event == c'EVENT_MBUTTONDBLCLK = EventMButtonDbClick
   | event == c'EVENT_MOUSEWHEEL    = EventMouseWheel
   | event == c'EVENT_MOUSEHWHEEL   = EventMouseHWheel
   | otherwise = error $ "unmarshallEvent - unknown event " <> show event


-- TODO (RvD): memory leak, fix with freeHaskellFunPtr
setMouseCallback
    :: WindowName
    -> (Event -> Int -> Int -> EventFlags -> IO ())
    -> IO ()
setMouseCallback windowName callback =
    C.withCString windowName $ \c'wn -> do
      callbackPtr <- $(C.mkFunPtr [t| C'MouseCallback |]) c'callback
      [C.exp| void { cv::setMouseCallback($(char * c'wn), $(MouseCallback callbackPtr)) }|]
  where
    c'callback :: C'MouseCallback
    c'callback c'event c'x c'y c'flags _c'userDataPtr = callback event x y flags
      where
        event = unmarshallEvent c'event
        x     = fromIntegral c'x
        y     = fromIntegral c'y
        flags = EventFlags $ fromIntegral c'flags

--------------------------------------------------------------------------------
-- Trackbars

type TrackbarName = String

createTrackbar
    :: TrackbarName
    -> WindowName
    -> Int -- ^ Initial value
    -> Int -- ^ Maximum value
    -> (Int -> IO ())
    -> IO ()
createTrackbar trackbarName windowName value count callback =
    C.withCString trackbarName $ \c'tn ->
      C.withCString windowName $ \c'wn -> do
        callbackPtr <- $(C.mkFunPtr [t| C'TrackbarCallback |]) c'callback
        [C.exp| void {
          cv::createTrackbar( $(char * c'tn)
                            , $(char * c'wn)
                            , &$(int c'value)
                            , $(int c'count)
                            , $(TrackbarCallback callbackPtr)
                            )
        }|]
  where
    c'value = fromIntegral value
    c'count = fromIntegral count

    c'callback :: C'TrackbarCallback
    c'callback c'pos _c'userDataPtr = callback pos
      where
        pos = fromIntegral c'pos


--------------------------------------------------------------------------------
-- Drawing

imshow :: WindowName -> Mat -> IO ()
imshow windowName mat =
    C.withCString windowName $ \c'wn ->
      withMatPtr mat $ \matPtr ->
        [C.exp| void { cv::imshow($(char * c'wn), *$(Mat * matPtr)); }|]
