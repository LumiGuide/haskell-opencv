{-# language UndecidableInstances #-}
{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.ImgProc.MiscImgTransform.TypeLevel
    ( WidthAndHeightPlusTwo
    , PlusTwo
    ) where

import "base" GHC.TypeLits
import "this" OpenCV.TypeLevel

type family WidthAndHeightPlusTwo (dims :: DS [DS Nat]) :: DS [DS Nat] where
    WidthAndHeightPlusTwo 'D = 'D
    WidthAndHeightPlusTwo ('S '[w, h]) = 'S [PlusTwo w, PlusTwo h]

type family PlusTwo (n :: DS Nat) :: DS Nat where
    PlusTwo 'D = 'D
    PlusTwo ('S n) = 'S (2 + n)
