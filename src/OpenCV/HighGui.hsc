{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
While OpenCV was designed for use in full-scale applications and can be used
within functionally rich UI frameworks (such as Qt*, WinForms*, or Cocoa*) or
without any UI at all, sometimes there it is required to try functionality
quickly and visualize the results. This is what the "OpenCV.HighGui" module has
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
    , makeWindow
    , destroyWindow
    , withWindow
    , resizeWindow

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
    , TrackbarCallback
    , createTrackbar

      -- * Drawing
    , imshow
    , imshowM
    ) where

import "base" Control.Concurrent.MVar
import "base" Control.Exception ( mask_, bracket )
import "base" Data.Bits ( (.&.) )
import "base" Data.Int ( Int32 )
import "base" Data.Monoid ( (<>) )
import "base" Data.Unique ( newUnique, hashUnique )
import "base" Foreign.C.String ( CString, newCString, withCString )
import "base" Foreign.Ptr ( Ptr, FunPtr, freeHaskellFunPtr )
import "base" Foreign.Marshal.Alloc ( free )
import "base" Foreign.Marshal.Utils ( new )
import "containers" Data.Map ( Map )
import qualified "containers" Data.Map as M
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "primitive" Control.Monad.Primitive ( PrimState )
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Core.Types.Mat
import "this" OpenCV.Internal.Mutable
import "this" OpenCV.TypeLevel

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

data TrackbarState
   = TrackbarState
     { trackbarCallback :: !(FunPtr C'TrackbarCallback)
     , trackbarValuePtr :: !(Ptr Int32)
     }

data Window
   = Window
     { windowName          :: !CString
     , windowMouseCallback :: !(MVar (Maybe (FunPtr C'MouseCallback)))
     , windowTrackbars     :: !(MVar (Map String TrackbarState))
     }

freeTrackbar :: TrackbarState -> IO ()
freeTrackbar trackbar = do
    freeHaskellFunPtr $ trackbarCallback trackbar
    free $ trackbarValuePtr trackbar

-- #num WINDOW_NORMAL
-- #num WINDOW_AUTOSIZE
-- #num WINDOW_OPENGL
-- #num WINDOW_FREERATIO
-- #num WINDOW_KEEPRATIO

-- marshalWindowFlags :: WindowFlags -> Int32
-- marshalWindowFlags WindowFlags{..} =

-- | Create a window with the specified title.
--
-- Make sure to free the window when you're done with it using 'destroyWindow'
-- or better yet: use 'withWindow'.
makeWindow :: String -> IO Window
makeWindow title = do
    name <- show . hashUnique <$> newUnique
    c'name <- newCString name
    mouseCallback <- newMVar Nothing
    trackbars <- newMVar M.empty
    withCString title $ \c'title ->
      [C.block| void {
        char * cname = $(char * c'name);
        cv::namedWindow(cname, cv::WINDOW_NORMAL | cv::WINDOW_KEEPRATIO);
        cv::setWindowTitle(cname, $(char * c'title));
      }|]
    pure Window
         { windowName          = c'name
         , windowMouseCallback = mouseCallback
         , windowTrackbars     = trackbars
         }

-- | Close the window and free up all resources associated with the window.
destroyWindow :: Window -> IO ()
destroyWindow window = mask_ $ do
    [C.exp| void { cv::destroyWindow($(char * c'name)); }|]
    free c'name
    modifyMVar_ (windowMouseCallback window) $ \mbMouseCallback -> do
      mapM_ freeHaskellFunPtr mbMouseCallback
      pure Nothing
    modifyMVar_ (windowTrackbars window) $ \trackbars -> do
      mapM_ freeTrackbar trackbars
      pure M.empty
  where
    c'name :: CString
    c'name = windowName window

-- | @withWindow title act@ makes a window with the specified @title@ and passes
-- the resulting 'Window' to the computation @act@. The window will be destroyed
-- on exit from @withWindow@ whether by normal termination or by raising an
-- exception. Make sure not to use the @Window@ outside the @act@ computation!
withWindow :: String -> (Window -> IO a) -> IO a
withWindow title = bracket (makeWindow title) destroyWindow

-- | Resize a window to the specified size.
resizeWindow :: Window -> Int32 -> Int32 -> IO ()
resizeWindow window width height =
  [C.exp| void { cv::resizeWindow($(char * c'name), $(int32_t width), $(int32_t height)); }|]
    where
      c'name :: CString
      c'name = windowName window


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
    modifyMVar_ (windowMouseCallback window) $ \mbPrevCallback -> do
      callbackPtr <- $(C.mkFunPtr [t| C'MouseCallback |]) c'callback
      [C.exp| void { cv::setMouseCallback($(char * c'name), $(MouseCallback callbackPtr)) }|]
      mapM_ freeHaskellFunPtr mbPrevCallback
      pure $ Just callbackPtr
  where
    c'name :: CString
    c'name = windowName window

    c'callback :: C'MouseCallback
    c'callback c'event x y c'flags _c'userDataPtr = callback event x y flags
      where
        event = unmarshalEvent c'event
        flags = EventFlags $ fromIntegral c'flags

--------------------------------------------------------------------------------
-- Trackbars

-- | Callback function for trackbars
type TrackbarCallback
   =  Int32 -- ^ Current position of the specified trackbar.
   -> IO ()

createTrackbar
    :: Window
    -> String -- ^ Trackbar name
    -> Int32  -- ^ Initial value
    -> Int32  -- ^ Maximum value
    -> TrackbarCallback
    -> IO ()
createTrackbar window trackbarName value count callback =
    modifyMVar_ (windowTrackbars window) $ \trackbars ->
    withCString trackbarName $ \c'trackbarName -> mask_ $ do
      valuePtr <- new value
      callbackPtr <- $(C.mkFunPtr [t| C'TrackbarCallback |]) c'callback
      [C.exp| void {
        (void)cv::createTrackbar
          ( $(char * c'trackbarName)
          , $(char * c'name)
          , $(int32_t * valuePtr)
          , $(int32_t count)
          , $(TrackbarCallback callbackPtr)
          )
      }|]

      let (mbPrevCallback, trackbars') =
              M.updateLookupWithKey (\_k _v -> Just trackbar)
                                    trackbarName
                                    trackbars
          trackbar = TrackbarState
                     { trackbarCallback = callbackPtr
                     , trackbarValuePtr = valuePtr
                     }
      mapM_ freeTrackbar mbPrevCallback
      pure trackbars'
  where
    c'name :: CString
    c'name = windowName window

    c'callback :: C'TrackbarCallback
    c'callback pos _c'userDataPtr = callback pos


--------------------------------------------------------------------------------
-- Drawing

imshow
    :: Window -- ^
    -> Mat ('S [height, width]) channels depth
    -> IO ()
imshow window mat =
    withPtr mat $ \matPtr ->
      [C.exp| void { cv::imshow($(char * c'name), *$(Mat * matPtr)); }|]
  where
    c'name :: CString
    c'name = windowName window

imshowM
    :: Window -- ^
    -> Mut (Mat ('S [height, width]) channels depth) (PrimState IO)
    -> IO ()
imshowM window mat = imshow window =<< unsafeFreeze mat
