{-# language CPP #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.C.PlacementNew.TH ( mkPlacementNewInstance ) where

import "base" Data.Monoid ( (<>) )
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( quoteExp )
import "this" OpenCV.Internal.C.PlacementNew ( PlacementNew (..) )

mkPlacementNewInstance :: Name -> DecsQ
mkPlacementNewInstance name =
    [d|
      instance PlacementNew $(conT (mkName ctypeName)) where
          placementNew    = $(placementNewQ)
          placementDelete = $(placementDeleteQ)
    |]
  where
    typeName = nameBase name
    ctypeName = "C'"  <> typeName

    placementNewQ = do
      src <- newName "src"
      dst <- newName "dst"
      lamE [varP src, varP dst] $
        quoteExp C.exp $
          "void { new($(" <> typeName <> " * " <> nameBase dst <> ")) cv::" <> typeName <>
                   "(*$(" <> typeName <> " * " <> nameBase src <> ")) }"

    placementDeleteQ = do
      ptr <- newName "ptr"
      lamE [varP ptr] $
        quoteExp C.exp $
          "void { $(" <> typeName <> " * " <> nameBase ptr <> ")->~" <> typeName <> "() }"
