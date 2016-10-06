{-# language CPP #-}
{-# language ConstraintKinds #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Core.Types.Rect
  ( Rect(..)
  , RectPoint
  , RectSize
  , HRect(..)
  , IsRect(..)
  ) where

import "aeson" Data.Aeson
import "base" Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import "linear" Linear.V2 ( V2(..) )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Core.Types.Point ( Point )
import "this" OpenCV.Core.Types.Size ( Size )
#if MIN_VERSION_base(4,9,0)
import "base" Data.Foldable ( Foldable )
import "base" Data.Traversable ( Traversable )
#endif

--------------------------------------------------------------------------------

newtype Rect (depth :: *)
      = Rect {unRect :: ForeignPtr (C'Rect depth)}

type instance C (Rect depth) = C'Rect depth

instance WithPtr (Rect depth) where withPtr = withForeignPtr . unRect

-- | Native Haskell represenation of a rectangle.
data HRect a
   = HRect
     { hRectTopLeft :: !(V2 a)
     , hRectSize    :: !(V2 a)
     } deriving (Foldable, Functor, Traversable, Show)

type family RectPoint (r :: * -> *) :: * -> *
type family RectSize  (r :: * -> *) :: * -> *

type instance RectPoint Rect = Point 2
type instance RectSize  Rect = Size

type instance RectPoint HRect = V2
type instance RectSize  HRect = V2

class IsRect (r :: * -> *) (depth :: *) where
    toRect   :: r depth -> Rect depth
    fromRect :: Rect depth -> r depth

    toRectIO :: r depth -> IO (Rect depth)
    toRectIO = pure . toRect

    rectTopLeft     :: r depth -> RectPoint r depth
    rectBottomRight :: r depth -> RectPoint r depth
    rectSize        :: r depth -> RectSize  r depth
    rectArea        :: r depth -> depth
    rectContains    :: RectPoint r depth -> r depth -> Bool

--------------------------------------------------------------------------------

instance (IsRect HRect a, Show a)
      => Show (Rect a) where
    showsPrec prec rect = showParen (prec >= 10) $
                              showString "toRect "
                            . showParen True (shows hr)
      where
        hr :: HRect a
        hr = fromRect rect

instance (ToJSON a) => ToJSON (HRect a) where
    toJSON hr = object [ "pos"  .= (x, y)
                       , "size" .= (w, h)
                       ]
      where
        V2 x y = hRectTopLeft hr
        V2 w h = hRectSize    hr

instance (FromJSON a) => FromJSON (HRect a) where
    parseJSON = withObject "HRect" $ \obj ->
                  HRect  <$> (uncurry V2 <$> obj .: "pos")
                         <*> (uncurry V2 <$> obj .: "size")

instance ( ToJSON a
         , IsRect HRect a
         )
      => ToJSON (Rect a) where
    toJSON = toJSON . (fromRect :: Rect a -> HRect a)

instance ( FromJSON a
         , IsRect HRect a
         )
      => FromJSON (Rect a) where
    parseJSON value = (toRect :: HRect a -> Rect a) <$> parseJSON value
