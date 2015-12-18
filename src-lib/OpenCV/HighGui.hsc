{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.HighGui
    ( -- * Window management
      Window
    , WindowTitle
    , makeWindow
    , destroyWindow

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

    , MouseCallback
    , setMouseCallback

      -- * Trackbars
    , TrackbarName
    , TrackbarCallback
    , createTrackbar

      -- * Drawing
    , imshow
    ) where

import "base" Foreign.C.Types
import qualified "base" Foreign.C.String as C
import "base" Foreign.Ptr ( Ptr, FunPtr, freeHaskellFunPtr )
import "base" Foreign.Marshal.Alloc ( free )
import "base" Foreign.Marshal.Utils ( new )
import "containers" Data.Map ( Map )
import qualified "containers" Data.Map as M
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

type WindowTitle = String
type WindowName  = String

type TrackbarState = (FunPtr C'TrackbarCallback, Ptr CInt)

data Window
   = Window
     { windowName          :: WindowName
     , windowMouseCallback :: MVar (Maybe (FunPtr C'MouseCallback))
     , windowTrackbars     :: MVar (Map TrackbarName TrackbarState)
     }

freeTrackbar :: TrackbarState -> IO ()
freeTrackbar (callback, value) = do
    freeHaskellFunPtr callback
    free value

-- #num WINDOW_NORMAL
-- #num WINDOW_AUTOSIZE
-- #num WINDOW_OPENGL
-- #num WINDOW_FREERATIO
-- #num WINDOW_KEEPRATIO

-- marshallWindowFlags :: WindowFlags -> CInt
-- marshallWindowFlags WindowFlags{..} =

makeWindow :: WindowTitle -> IO Window
makeWindow title = do
    name <- show . hashUnique <$> newUnique
    mouseCallback <- newMVar Nothing
    trackbars <- newMVar M.empty
    C.withCString name $ \c'name ->
      C.withCString title $ \c'title ->
        [C.block| void {
          char * cname = $(char * c'name);
          cv::namedWindow(cname, cv::WINDOW_NORMAL | cv::WINDOW_KEEPRATIO);
          cv::setWindowTitle(cname, $(char * c'title));
        }|]
    pure Window
         { windowName          = name
         , windowMouseCallback = mouseCallback
         , windowTrackbars     = trackbars
         }

destroyWindow :: Window -> IO ()
destroyWindow window = mask_ $ do
  C.withCString (windowName window) $ \c'name ->
    [C.exp| void { cv::destroyWindow($(char * c'name)); }|]
  modifyMVar_ (windowMouseCallback window) $ \mbMouseCallback -> do
    mapM_ freeHaskellFunPtr mbMouseCallback
    pure Nothing
  modifyMVar_ (windowTrackbars window) $ \trackbars -> do
    mapM_ freeTrackbar trackbars
    pure M.empty

--------------------------------------------------------------------------------
-- Keyboard

waitKey :: Int -> IO Int
waitKey delay = fromIntegral <$> f (fromIntegral delay)
  where
    f :: CInt -> IO CInt
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

newtype EventFlags = EventFlags CInt

matchEventFlag :: CInt -> EventFlags -> Bool
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


unmarshallEvent :: CInt -> Event
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

type MouseCallback = Event -> Int -> Int -> EventFlags -> IO ()

setMouseCallback :: Window -> MouseCallback -> IO ()
setMouseCallback window callback =
    modifyMVar_ (windowMouseCallback window) $ \mbPrevCallback ->
      C.withCString (windowName window) $ \c'name -> mask_ $ do
        callbackPtr <- $(C.mkFunPtr [t| C'MouseCallback |]) c'callback
        [C.exp| void { cv::setMouseCallback($(char * c'name), $(MouseCallback callbackPtr)) }|]
        mapM_ freeHaskellFunPtr mbPrevCallback
        pure $ Just callbackPtr
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
type TrackbarCallback = Int -> IO ()

createTrackbar
    :: Window
    -> TrackbarName
    -> Int -- ^ Initial value
    -> Int -- ^ Maximum value
    -> TrackbarCallback
    -> IO ()
createTrackbar window trackbarName value count callback =
    modifyMVar_ (windowTrackbars window) $ \trackbars ->
    C.withCString trackbarName $ \c'tn ->
    C.withCString (windowName window) $ \c'wn -> mask_ $ do
      valuePtr <- new c'value
      callbackPtr <- $(C.mkFunPtr [t| C'TrackbarCallback |]) c'callback
      [C.exp| void {
        (void)cv::createTrackbar
          ( $(char * c'tn)
          , $(char * c'wn)
          , $(int * valuePtr)
          , $(int c'count)
          , $(TrackbarCallback callbackPtr)
          )
      }|]

      let (mbPrevCallback, trackbars') =
              M.updateLookupWithKey (\_k _v -> Just (callbackPtr, valuePtr))
                                    trackbarName
                                    trackbars
      mapM_ freeTrackbar mbPrevCallback
      pure trackbars'
  where
    c'value = fromIntegral value
    c'count = fromIntegral count

    c'callback :: C'TrackbarCallback
    c'callback c'pos _c'userDataPtr = callback pos
      where
        pos = fromIntegral c'pos


--------------------------------------------------------------------------------
-- Drawing

imshow :: Window -> Mat -> IO ()
imshow window mat =
    C.withCString (windowName window) $ \c'name ->
      withMatPtr mat $ \matPtr ->
        [C.exp| void { cv::imshow($(char * c'name), *$(Mat * matPtr)); }|]
