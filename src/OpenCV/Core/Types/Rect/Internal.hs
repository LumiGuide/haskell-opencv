{-# language CPP             #-}
{-# language ConstraintKinds #-}
{-# language QuasiQuotes     #-}
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.Core.Types.Rect.Internal
  ( Rect(..)
  , RectDepth
  , IsRect(..)
  , mkRect

  , Rect2i, Rect2f, Rect2d
  , IsRect2i, IsRect2f, IsRect2d
  ) where

import "base" Data.Int ( Int32 )
import "base" Foreign.C.Types
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "base" Foreign.Marshal.Utils ( toBool )
import "base" System.IO.Unsafe ( unsafePerformIO )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "linear" Linear.V2 ( V2(..) )
import "this" OpenCV.C.Inline ( openCvCtx )
import "this" OpenCV.C.PlacementNew.TH
import "this" OpenCV.C.Types
import "this" OpenCV.Core.Types.Point
import "this" OpenCV.Core.Types.Size
import "this" OpenCV.Internal

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------

newtype Rect (depth :: *)
      = Rect {unRect :: ForeignPtr (C'Rect depth)}

type instance C (Rect depth) = C'Rect depth

instance WithPtr (Rect depth) where withPtr = withForeignPtr . unRect

type family RectDepth (a :: *) :: *
type instance RectDepth (Rect depth) = depth

class IsRect a where
    toRect   :: a -> Rect (RectDepth a)
    fromRect :: Rect (RectDepth a) -> a

    newRect :: V2 (RectDepth a) -> V2 (RectDepth a) -> IO a

    rectTopLeft     :: a -> Point (RectDepth a) 2
    rectBottomRight :: a -> Point (RectDepth a) 2
    rectSize        :: a -> Size  (RectDepth a) 2
    rectArea        :: a -> RectDepth a
    rectContains    :: Point (RectDepth a) 2 -> a -> Bool

mkRect
    :: (IsRect a)
    => V2 (RectDepth a) -- ^ top left
    -> V2 (RectDepth a) -- ^ size
    -> a
mkRect pos size = unsafePerformIO $ newRect pos size

#define RECT_TYPE(NAME, IS_NAME, HDEPTH, CDEPTH, POINT, SIZE)                     \
type NAME = Rect HDEPTH;                                                          \
instance FromPtr NAME where {                                                     \
    fromPtr = objFromPtr Rect $ \ptr ->                                           \
                [CU.exp| void { delete $(NAME * ptr) }|];                         \
};                                                                                \
type IS_NAME p = (IsRect p, RectDepth p ~ HDEPTH);                                \
instance IsRect NAME where {                                                      \
    toRect = id;                                                                  \
    fromRect = id;                                                                \
    newRect (V2 x y) (V2 width height) = fromPtr $                                \
      [CU.exp|NAME * { new cv::Rect_<CDEPTH>                                      \
                           ( $(CDEPTH x)                                          \
                           , $(CDEPTH y)                                          \
                           , $(CDEPTH width)                                      \
                           , $(CDEPTH height)                                     \
                           )                                                      \
                     }|];                                                         \
    rectTopLeft rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr ->     \
      [CU.exp| POINT * { new POINT($(NAME * rectPtr)->tl()) }|];                  \
    rectBottomRight rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr -> \
      [CU.exp| POINT * { new POINT($(NAME * rectPtr)->br()) }|];                  \
    rectSize rect = unsafePerformIO $ fromPtr $ withPtr rect $ \rectPtr ->        \
      [CU.exp| SIZE * { new SIZE($(NAME * rectPtr)->size()) } |];                 \
    rectArea rect = unsafePerformIO $ withPtr rect $ \rectPtr ->                  \
      [CU.exp| CDEPTH { $(NAME * rectPtr)->area() }|];                            \
    rectContains point rect = toBool $ unsafePerformIO $                          \
      withPtr (toPoint point) $ \pointPtr ->                                      \
        withPtr rect $ \rectPtr ->                                                \
          [CU.exp| int { $(NAME * rectPtr)->contains(*$(POINT * pointPtr)) }|];   \
};

RECT_TYPE(Rect2i, IsRect2i, Int32  , int32_t, Point2i, Size2i)
RECT_TYPE(Rect2f, IsRect2f, CFloat , float  , Point2f, Size2f)
RECT_TYPE(Rect2d, IsRect2d, CDouble, double , Point2d, Size2d)

--------------------------------------------------------------------------------

instance Show Rect2i where
    showsPrec prec rect = showParen (prec >= 10) $
                              showString "mkRect "
                            . shows x . showString " "
                            . shows y . showString " "
                            . shows w . showString " "
                            . shows h
      where
        x, y, w, h :: Int32
        V2 x y = fromPoint $ rectTopLeft rect
        V2 w h = fromSize  $ rectSize    rect

--------------------------------------------------------------------------------

mkPlacementNewInstance ''Rect2i
mkPlacementNewInstance ''Rect2f
