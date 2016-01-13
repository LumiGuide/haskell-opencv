{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
While OpenCV was designed for use in full-scale applications and can be used
within functionally rich UI frameworks (such as Qt*, WinForms*, or Cocoa*) or
without any UI at all, sometimes there it is required to try functionality
quickly and visualize the results. This is what the "OpenCV.HighGUI" module has
been designed for.

It provides easy interface to:

 * Create and manipulate windows that can display images and “remember” their
   content (no need to handle repaint events from OS).

 * Add trackbars to the windows, handle simple mouse events as well as keyboard
   commands.
-}
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
    , imshowM
    ) where

import qualified "base" Foreign.C.String as C
import "base" Foreign.Ptr ( Ptr, FunPtr, freeHaskellFunPtr )
import "base" Foreign.Marshal.Alloc ( free )
import "base" Foreign.Marshal.Utils ( new )
import "containers" Data.Map ( Map )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "lumi-hackage-extended" Lumi.Prelude
import "primitive" Control.Monad.Primitive ( PrimState )
import "this" Language.C.Inline.OpenCV
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Core.Types.Mat.Internal
import "this" OpenCV.Unsafe ( unsafeFreeze )

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

type TrackbarState = (FunPtr C'TrackbarCallback, Ptr Int32)

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

-- marshalWindowFlags :: WindowFlags -> Int32
-- marshalWindowFlags WindowFlags{..} =

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

waitKey :: Int32 -> IO Int32
waitKey delay = [C.exp| int32_t { cv::waitKey($(int32_t delay)) }|]


--------------------------------------------------------------------------------
-- Mouse

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

-- | Context for a mouse 'Event'
--
-- Information about which buttons and modifier keys where pressed during the
-- event.
newtype EventFlags = EventFlags Int32

matchEventFlag :: Int32 -> EventFlags -> Bool
matchEventFlag flag = \(EventFlags flags) -> flags .&. flag /= 0

hasLButton  :: EventFlags -> Bool
hasRButton  :: EventFlags -> Bool
hasMButton  :: EventFlags -> Bool
hasCtrlKey  :: EventFlags -> Bool
hasShiftKey :: EventFlags -> Bool
hasAltKey   :: EventFlags -> Bool

#num EVENT_FLAG_LBUTTON
#num EVENT_FLAG_RBUTTON
#num EVENT_FLAG_MBUTTON
#num EVENT_FLAG_CTRLKEY
#num EVENT_FLAG_SHIFTKEY
#num EVENT_FLAG_ALTKEY

hasLButton  = matchEventFlag c'EVENT_FLAG_LBUTTON
hasRButton  = matchEventFlag c'EVENT_FLAG_RBUTTON
hasMButton  = matchEventFlag c'EVENT_FLAG_MBUTTON
hasCtrlKey  = matchEventFlag c'EVENT_FLAG_CTRLKEY
hasShiftKey = matchEventFlag c'EVENT_FLAG_SHIFTKEY
hasAltKey   = matchEventFlag c'EVENT_FLAG_ALTKEY

-- | More convenient representation of 'EventFlags'
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

unmarshalEvent :: Int32 -> Event
unmarshalEvent event
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
   | otherwise = error $ "unmarshalEvent - unknown event " <> show event

-- | Callback function for mouse events
type MouseCallback
   =  Event -- ^ What happened to cause the callback to be fired.
   -> Int32 -- ^ The x-coordinate of the mouse event.
   -> Int32 -- ^ The y-coordinate of the mouse event.
   -> EventFlags
      -- ^ Context for the event, such as buttons and modifier keys pressed
      -- during the event.
   -> IO ()

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
    c'callback c'event x y c'flags _c'userDataPtr = callback event x y flags
      where
        event = unmarshalEvent c'event
        flags = EventFlags $ fromIntegral c'flags

--------------------------------------------------------------------------------
-- Trackbars

type TrackbarName = String

-- | Callback function for trackbars
type TrackbarCallback
   =  Int32 -- ^ Current position of the specified trackbar.
   -> IO ()

createTrackbar
    :: Window
    -> TrackbarName
    -> Int32 -- ^ Initial value
    -> Int32 -- ^ Maximum value
    -> TrackbarCallback
    -> IO ()
createTrackbar window trackbarName value count callback =
    modifyMVar_ (windowTrackbars window) $ \trackbars ->
    C.withCString trackbarName $ \c'tn ->
    C.withCString (windowName window) $ \c'wn -> mask_ $ do
      valuePtr <- new value
      callbackPtr <- $(C.mkFunPtr [t| C'TrackbarCallback |]) c'callback
      [C.exp| void {
        (void)cv::createTrackbar
          ( $(char * c'tn)
          , $(char * c'wn)
          , $(int32_t * valuePtr)
          , $(int32_t count)
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
    c'callback :: C'TrackbarCallback
    c'callback pos _c'userDataPtr = callback pos


--------------------------------------------------------------------------------
-- Drawing

imshow :: Window -> Mat -> IO ()
imshow window mat =
    C.withCString (windowName window) $ \c'name ->
      withMatPtr mat $ \matPtr ->
        [C.exp| void { cv::imshow($(char * c'name), *$(Mat * matPtr)); }|]

imshowM :: Window -> MutMat (PrimState IO) -> IO ()
imshowM window mat = imshow window =<< unsafeFreeze mat
